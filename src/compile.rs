// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Enable this to squash warnings due to exporting pieces of the representation
// for use with the regex! macro. See lib.rs for explanation.

use self::Inst::*;

use std::cmp;
use syntax::{self, Expr, Repeater};
use Error;

pub type InstIdx = usize;

/// An instruction, the underlying unit of a compiled regular expression
#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub enum Inst {
    /// When a Match instruction is executed, the current thread is successful.
    Match,
    OneChar { c: char, casei: bool },
    CharClass(syntax::CharClass),
    Any,
    AnyNoNL,
    StartLine,
    EndLine,
    StartText,
    EndText,
    WordBoundary,
    NotWordBoundary,
    Save(usize),
    Jump(InstIdx),
    Split(InstIdx, InstIdx),
}

/// Program represents a compiled regular expression. Once an expression is
/// compiled, its representation is immutable and will never change.
///
/// All of the data in a compiled expression is wrapped in "MaybeStatic" or
/// "MaybeOwned" types so that a `Program` can be represented as static data.
/// (This makes it convenient and efficient for use with the `regex!` macro.)
#[derive(Clone, Debug)]
pub struct Program {
    /// A sequence of instructions.
    pub insts: Vec<Inst>,
    /// If the regular expression requires a literal prefix in order to have a
    /// match, that prefix is stored here. (It's used in the VM to implement
    /// an optimization.)
    pub prefix: String,
}

impl Program {
    /// Compiles a Regex given its AST.
    pub fn new(ast: Expr, size: usize) -> Result<(Program, Vec<Option<String>>), Error> {
        let mut c = Compiler {
            insts: Vec::with_capacity(100),
            names: vec![None],
            size_limit: size,
        };

        c.insts.push(Save(0));
        try!(c.compile(ast));
        c.insts.push(Save(1));
        c.insts.push(Match);

        // Try to discover a literal string prefix.
        // This is a bit hacky since we have to skip over the initial
        // 'Save' instruction.
        let mut pre = String::with_capacity(5);
        for inst in c.insts[1..].iter() {
            match *inst {
                OneChar { c, casei: false } => pre.push(c),
                _ => break
            }
        }

        let Compiler { insts, names, .. } = c;
        let prog = Program {
            insts: insts,
            prefix: pre,
        };
        Ok((prog, names))
    }

    /// Returns the total number of capture groups in the regular expression.
    /// This includes the zeroth capture.
    pub fn num_captures(&self) -> usize {
        let mut n = 0;
        for inst in self.insts.iter() {
            match *inst {
                Save(c) => n = cmp::max(n, c+1),
                _ => {}
            }
        }
        // There's exactly 2 Save slots for every capture.
        n / 2
    }
}

struct Compiler {
    insts: Vec<Inst>,
    names: Vec<Option<String>>,
    size_limit: usize,
}

// The compiler implemented here is extremely simple. Most of the complexity
// in this crate is in the parser or the VM.
// The only tricky thing here is patching jump/split instructions to point to
// the right instruction.
impl Compiler {
    fn check_size(&self) -> Result<(), Error> {
        if self.insts.len() * ::std::mem::size_of::<Inst>() > self.size_limit {
            Err(Error::CompiledTooBig(self.size_limit))
        } else {
            Ok(())
        }
    }

    fn compile(&mut self, ast: Expr) -> Result<(), Error> {
        match ast {
            Expr::Empty => {},
            Expr::Literal { chars, casei } => {
                for c in chars {
                    self.push(OneChar { c: c, casei: casei });
                }
            }
            Expr::AnyChar => self.push(Any),
            Expr::AnyCharNoNL => self.push(AnyNoNL),
            Expr::Class(cls) => self.push(CharClass(cls)),
            Expr::StartLine => self.push(StartLine),
            Expr::EndLine => self.push(EndLine),
            Expr::StartText => self.push(StartText),
            Expr::EndText => self.push(EndText),
            Expr::WordBoundary => self.push(WordBoundary),
            Expr::NotWordBoundary => self.push(NotWordBoundary),
            Expr::Group { e, i: None, name: None } => try!(self.compile(*e)),
            Expr::Group { e, i, name } => {
                let i = i.expect("capture index");
                self.names.push(name);
                self.push(Save(2 * i));
                try!(self.compile(*e));
                self.push(Save(2 * i + 1));
            }
            Expr::Concat(es) => {
                for e in es {
                    try!(self.compile(e));
                }
            }
            Expr::Alternate(mut es) => {
                // TODO: Don't use recursion here. ---AG
                if es.len() == 0 {
                    return Ok(());
                }
                let e1 = es.remove(0);
                if es.len() == 0 {
                    try!(self.compile(e1));
                    return Ok(());
                }
                let e2 = Expr::Alternate(es); // this causes recursion

                let split = self.empty_split(); // push: split 0, 0
                let j1 = self.insts.len();
                try!(self.compile(e1));                // push: insts for x
                let jmp = self.empty_jump();    // push: jmp 0
                let j2 = self.insts.len();
                try!(self.compile(e2));                // push: insts for y
                let j3 = self.insts.len();

                self.set_split(split, j1, j2);  // split 0, 0 -> split j1, j2
                self.set_jump(jmp, j3);         // jmp 0      -> jmp j3
            }
            Expr::Repeat { e, r: Repeater::ZeroOrOne, greedy } => {
                let split = self.empty_split();
                let j1 = self.insts.len();
                try!(self.compile(*e));
                let j2 = self.insts.len();

                if greedy {
                    self.set_split(split, j1, j2);
                } else {
                    self.set_split(split, j2, j1);
                }
            }
            Expr::Repeat { e, r: Repeater::ZeroOrMore, greedy } => {
                let j1 = self.insts.len();
                let split = self.empty_split();
                let j2 = self.insts.len();
                try!(self.compile(*e));
                let jmp = self.empty_jump();
                let j3 = self.insts.len();

                self.set_jump(jmp, j1);
                if greedy {
                    self.set_split(split, j2, j3);
                } else {
                    self.set_split(split, j3, j2);
                }
            }
            Expr::Repeat { e, r: Repeater::OneOrMore, greedy } => {
                let j1 = self.insts.len();
                try!(self.compile(*e));
                let split = self.empty_split();
                let j2 = self.insts.len();

                if greedy {
                    self.set_split(split, j1, j2);
                } else {
                    self.set_split(split, j2, j1);
                }
            }
            Expr::Repeat { e, r: Repeater::Range { min, max: None }, greedy } => {
                let e = *e;
                for _ in 0..min {
                    try!(self.compile(e.clone()));
                }
                try!(self.compile(Expr::Repeat {
                    e: Box::new(e),
                    r: Repeater::ZeroOrMore,
                    greedy: greedy,
                }));
            }
            Expr::Repeat { e, r: Repeater::Range { min, max: Some(max) }, greedy } => {
                let e = *e;
                for _ in 0..min {
                    try!(self.compile(e.clone()));
                }
                for _ in min..max {
                    try!(self.compile(Expr::Repeat {
                        e: Box::new(e.clone()),
                        r: Repeater::ZeroOrOne,
                        greedy: greedy,
                    }));
                }
            }
        }
        self.check_size()
    }

    /// Appends the given instruction to the program.
    #[inline]
    fn push(&mut self, x: Inst) {
        self.insts.push(x)
    }

    /// Appends an *empty* `Split` instruction to the program and returns
    /// the index of that instruction. (The index can then be used to "patch"
    /// the actual locations of the split in later.)
    #[inline]
    fn empty_split(&mut self) -> InstIdx {
        self.insts.push(Split(0, 0));
        self.insts.len() - 1
    }

    /// Sets the left and right locations of a `Split` instruction at index
    /// `i` to `pc1` and `pc2`, respectively.
    /// If the instruction at index `i` isn't a `Split` instruction, then
    /// `panic!` is called.
    #[inline]
    fn set_split(&mut self, i: InstIdx, pc1: InstIdx, pc2: InstIdx) {
        let split = &mut self.insts[i];
        match *split {
            Split(_, _) => *split = Split(pc1, pc2),
            _ => panic!("BUG: Invalid split index."),
        }
    }

    /// Appends an *empty* `Jump` instruction to the program and returns the
    /// index of that instruction.
    #[inline]
    fn empty_jump(&mut self) -> InstIdx {
        self.insts.push(Jump(0));
        self.insts.len() - 1
    }

    /// Sets the location of a `Jump` instruction at index `i` to `pc`.
    /// If the instruction at index `i` isn't a `Jump` instruction, then
    /// `panic!` is called.
    #[inline]
    fn set_jump(&mut self, i: InstIdx, pc: InstIdx) {
        let jmp = &mut self.insts[i];
        match *jmp {
            Jump(_) => *jmp = Jump(pc),
            _ => panic!("BUG: Invalid jump index."),
        }
    }
}
