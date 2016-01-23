// Copyright 2014-2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::collections::HashSet;
use std::iter;

use syntax::{Expr, Repeater, CharClass, ClassRange};

use Error;
use inst::{
    EmptyLook,
    Inst, InstIdx,
    InstSave, InstSplit, InstEmptyLook, InstChar, InstRanges,
};

pub type Compiled = (Vec<Inst>, Vec<Option<String>>);

type CompileResult = Result<Hole, Error>;

pub struct Compiler {
    size_limit: usize,
    insts: Vec<MaybeInst>,
    cap_names: Vec<Option<String>>,
    seen_caps: HashSet<usize>,
}

impl Compiler {
    pub fn new(size_limit: usize) -> Compiler {
        Compiler {
            size_limit: size_limit,
            insts: vec![],
            cap_names: vec![None],
            seen_caps: HashSet::new(),
        }
    }

    pub fn compile(mut self, expr: &Expr) -> Result<Compiled, Error> {
        let hole = try!(self.c_capture(0, expr));
        self.fill_to_next(hole);
        self.push_compiled(Inst::Match);

        let insts = self.insts.into_iter().map(|inst| inst.unwrap()).collect();
        Ok((insts, self.cap_names))
    }

    fn c(&mut self, expr: &Expr) -> CompileResult {
        use inst;
        use syntax::Expr::*;

        try!(self.check_size());
        match *expr {
            Empty => Ok(Hole::None),
            Literal { ref chars, casei } => self.c_literal(chars, casei),
            AnyChar => self.c_class(Some(('\x00', '\u{10ffff}'))),
            AnyCharNoNL => {
                let ranges = &[('\x00', '\x09'), ('\x0b', '\u{10ffff}')];
                self.c_class(ranges.iter().cloned())
            }
            Class(ref cls) => {
                let ranges = cls.iter().map(|c| (c.start, c.end));
                self.c_class(ranges)
            }
            StartLine => self.c_empty_look(inst::EmptyLook::StartLine),
            EndLine => self.c_empty_look(inst::EmptyLook::EndLine),
            StartText => self.c_empty_look(inst::EmptyLook::StartText),
            EndText => self.c_empty_look(inst::EmptyLook::EndText),
            WordBoundary => self.c_empty_look(inst::EmptyLook::WordBoundary),
            NotWordBoundary => {
                self.c_empty_look(inst::EmptyLook::NotWordBoundary)
            }
            Group { ref e, i: None, name: None } => self.c(e),
            Group { ref e, i, ref name } => {
                // it's impossible to have a named capture without an index
                let i = i.expect("capture index");
                if !self.seen_caps.contains(&i) {
                    self.cap_names.push(name.clone());
                    self.seen_caps.insert(i);
                }
                self.c_capture(2 * i, e)
            }
            Concat(ref es) => self.c_concat(es.iter()),
            Alternate(ref es) => self.c_alternate(&**es),
            Repeat { ref e, r, greedy } => self.c_repeat(e, r, greedy),
        }
    }

    fn c_capture(&mut self, first_slot: usize, expr: &Expr) -> CompileResult {
        let hole = self.push_hole(MaybeInst::Save { slot: first_slot });
        self.fill_to_next(hole);

        let hole = try!(self.c(expr));
        self.fill_to_next(hole);

        Ok(self.push_hole(MaybeInst::Save { slot: first_slot + 1 }))
    }

    fn c_literal(&mut self, chars: &[char], casei: bool) -> CompileResult {
        assert!(!chars.is_empty());
        if casei {
            let mut prev_hole = Hole::None;
            for &c in chars {
                self.fill_to_next(prev_hole);
                let class = CharClass::new(vec![
                    ClassRange { start: c, end: c },
                ]);
                prev_hole = try!(self.c(&Expr::Class(class.case_fold())));
            }
            Ok(prev_hole)
        } else {
            let mut prev_hole = Hole::None;
            for &c in chars {
                self.fill_to_next(prev_hole);
                prev_hole = self.push_hole(MaybeInst::Char { c: c });
            }
            Ok(prev_hole)
        }
    }

    fn c_class<I>(&mut self, ranges: I) -> CompileResult
            where I: IntoIterator<Item=(char, char)> {
        let ranges: Vec<(char, char)> = ranges.into_iter().collect();
        Ok(if ranges.len() == 1 && ranges[0].0 == ranges[0].1 {
            self.push_hole(MaybeInst::Char { c: ranges[0].0 })
        } else {
            self.push_hole(MaybeInst::Ranges { ranges: ranges })
        })
    }

    fn c_empty_look(&mut self, look: EmptyLook) -> CompileResult {
        Ok(self.push_hole(MaybeInst::EmptyLook { look: look }))
    }

    fn c_concat<'a, I>(&mut self, exprs: I) -> CompileResult
            where I: IntoIterator<Item=&'a Expr> {
        let mut prev_hole = Hole::None;
        for e in exprs {
            self.fill_to_next(prev_hole);
            prev_hole = try!(self.c(e));
        }
        Ok(prev_hole)
    }

    fn c_alternate(&mut self, exprs: &[Expr]) -> CompileResult {
        assert!(exprs.len() >= 2, "alternates must have at least 2 exprs");
        let mut holes = vec![];
        for e in &exprs[0..exprs.len() - 1] {
            let split = self.push_split_hole();
            let goto1 = self.insts.len();
            holes.push(try!(self.c(e)));
            let goto2 = self.insts.len();
            self.fill_split(split, Some(goto1), Some(goto2));
        }
        holes.push(try!(self.c(&exprs[exprs.len() - 1])));
        Ok(Hole::Many(holes))
    }

    fn c_repeat(
        &mut self,
        expr: &Expr,
        kind: Repeater,
        greedy: bool,
    ) -> CompileResult {
        match kind {
            Repeater::ZeroOrOne => self.c_repeat_zero_or_one(expr, greedy),
            Repeater::ZeroOrMore => self.c_repeat_zero_or_more(expr, greedy),
            Repeater::OneOrMore => self.c_repeat_one_or_more(expr, greedy),
            Repeater::Range { min, max: None } => {
                self.c_repeat_range_min_or_more(expr, greedy, min)
            }
            Repeater::Range { min, max: Some(max) } => {
                self.c_repeat_range(expr, greedy, min, max)
            }
        }
    }

    fn c_repeat_zero_or_one(
        &mut self,
        expr: &Expr,
        greedy: bool,
    ) -> CompileResult {
        let split = self.push_split_hole();
        let goto1 = self.insts.len();
        let hole1 = try!(self.c(expr));

        let hole2 = if greedy {
            self.fill_split(split, Some(goto1), None)
        } else {
            self.fill_split(split, None, Some(goto1))
        };
        Ok(Hole::Many(vec![hole1, hole2]))
    }

    fn c_repeat_zero_or_more(
        &mut self,
        expr: &Expr,
        greedy: bool,
    ) -> CompileResult {
        let goto_split = self.insts.len();
        let split = self.push_split_hole();
        let goto_rep_expr = self.insts.len();
        let hole_rep_expr = try!(self.c(expr));

        self.fill(hole_rep_expr, goto_split);
        Ok(if greedy {
            self.fill_split(split, Some(goto_rep_expr), None)
        } else {
            self.fill_split(split, None, Some(goto_rep_expr))
        })
    }

    fn c_repeat_one_or_more(
        &mut self,
        expr: &Expr,
        greedy: bool,
    ) -> CompileResult {
        let goto_rep_expr = self.insts.len();
        let hole_rep_expr = try!(self.c(expr));
        self.fill_to_next(hole_rep_expr);
        let split = self.push_split_hole();

        Ok(if greedy {
            self.fill_split(split, Some(goto_rep_expr), None)
        } else {
            self.fill_split(split, None, Some(goto_rep_expr))
        })
    }

    fn c_repeat_range_min_or_more(
        &mut self,
        expr: &Expr,
        greedy: bool,
        min: u32,
    ) -> CompileResult {
        let min = u32_to_usize(min);
        if min == 0 {
            return self.c_repeat_zero_or_more(expr, greedy);
        }
        let hole = try!(self.c_concat(iter::repeat(expr).take(min - 1)));
        self.fill_to_next(hole);
        self.c_repeat_one_or_more(expr, greedy)
    }

    fn c_repeat_range(
        &mut self,
        expr: &Expr,
        greedy: bool,
        min: u32,
        max: u32,
    ) -> CompileResult {
        let (min, max) = (u32_to_usize(min), u32_to_usize(max));
        let hole = try!(self.c_concat(iter::repeat(expr).take(min)));
        if min == max {
            return Ok(hole);
        }
        self.fill_to_next(hole);
        // It is much simpler to compile, e.g., `a{2,5}` as:
        //
        //     aaa?a?a?
        //
        // But you end up with a sequence of instructions like this:
        //
        //     0: 'a'
        //     1: 'a',
        //     2: split(3, 4)
        //     3: 'a'
        //     4: split(5, 6)
        //     5: 'a'
        //     6: split(7, 8)
        //     7: 'a'
        //     8: MATCH
        //
        // This is *incredibly* inefficient because the splits end
        // up forming a chain. Given a much larger number than `5`,
        // it is easy cause perverse behavior in the matching engines
        // like stack overflows. We could fix the matching engine,
        // but instead, we should just make the program smarter.
        // Thus, we do a custom job here and instead of chaining the
        // splits together, we simply point them to the MATCH
        // instruction directly (for example).
        let mut holes = vec![];
        let mut prev_hole = Hole::None;
        for _ in min..max {
            self.fill_to_next(prev_hole);
            let split = self.push_split_hole();
            let goto_rep_expr = self.insts.len();
            prev_hole = try!(self.c(expr));
            if greedy {
                holes.push(self.fill_split(split, Some(goto_rep_expr), None));
            } else {
                holes.push(self.fill_split(split, None, Some(goto_rep_expr)));
            }
        }
        holes.push(prev_hole);
        Ok(Hole::Many(holes))
    }

    fn fill(&mut self, hole: Hole, goto: InstIdx) {
        match hole {
            Hole::None => {}
            Hole::One(pc) => {
                self.insts[pc].complete(goto);
            }
            Hole::Many(holes) => {
                for hole in holes {
                    self.fill(hole, goto);
                }
            }
        }
    }

    fn fill_to_next(&mut self, hole: Hole) {
        let next = self.insts.len();
        self.fill(hole, next);
    }

    fn fill_split(
        &mut self,
        hole: Hole,
        goto1: Option<InstIdx>,
        goto2: Option<InstIdx>,
    ) -> Hole {
        match hole {
            Hole::None => Hole::None,
            Hole::One(pc) => {
                match (goto1, goto2) {
                    (Some(goto1), Some(goto2)) => {
                        self.insts[pc].complete_split(goto1, goto2);
                        Hole::None
                    }
                    (Some(goto1), None) => {
                        self.insts[pc].complete_split_goto1(goto1);
                        Hole::One(pc)
                    }
                    (None, Some(goto2)) => {
                        self.insts[pc].complete_split_goto2(goto2);
                        Hole::One(pc)
                    }
                    (None, None) => unreachable!("at least one of the split \
                                                  holes must be filled"),
                }
            }
            Hole::Many(holes) => {
                let mut new_holes = vec![];
                for hole in holes {
                    new_holes.push(self.fill_split(hole, goto1, goto2));
                }
                if new_holes.is_empty() {
                    Hole::None
                } else if new_holes.len() == 1 {
                    new_holes.pop().unwrap()
                } else {
                    Hole::Many(new_holes)
                }
            }
        }
    }

    fn push_compiled(&mut self, inst: Inst) {
        self.insts.push(MaybeInst::Compiled(inst));
    }

    fn push_hole(&mut self, inst: MaybeInst) -> Hole {
        let hole = self.insts.len();
        self.insts.push(inst);
        Hole::One(hole)
    }

    fn push_split_hole(&mut self) -> Hole {
        let hole = self.insts.len();
        self.insts.push(MaybeInst::Split);
        Hole::One(hole)
    }

    fn check_size(&self) -> Result<(), Error> {
        use std::mem::size_of;

        if self.insts.len() * size_of::<Inst>() > self.size_limit {
            Err(Error::CompiledTooBig(self.size_limit))
        } else {
            Ok(())
        }
    }
}

/// Hole represents a pointer to zero or more instructions in a regex program
/// that need to have their goto fields set to the same location.
#[derive(Debug)]
enum Hole {
    None,
    One(InstIdx),
    Many(Vec<Hole>),
}

/// MaybeInst represents a possibly incomplete instruction in a regex program.
/// The nature of incompleteness is always determined by whether the
/// instruction's "goto" field has been set or not.
///
/// In the case of Split, since it has two goto fields, it can be "incomplete"
/// in three different ways: either none of its fields are set, only the first
/// is set or only the second is set. The reason why the first and second
/// fields are distinguished is because the order of the branch matters. (i.e.,
/// it's how "greedy" and "ungreedy" semantics are implemented.)
///
/// When the compiler is finished, *all* of its possibly incomplete
/// instructions must have been fully compiled where all goto fields in all
/// instructions are set. Violation of this invariant is a bug.
#[derive(Clone, Debug)]
enum MaybeInst {
    /// Compiled represents an instruction that is fully compiled. That is,
    /// all of its "goto" fields have been filled. When the compiler is done,
    /// all MaybeInsts must be of the Compiled form.
    Compiled(Inst),
    /// Split is a branch instruction where neither of its goto fields have
    /// been set.
    Split,
    /// Split1 is a branch instruction where only the first goto field has
    /// been set.
    Split1(InstIdx),
    /// Split2 is a branch instruction where only the second goto field has
    /// been set.
    Split2(InstIdx),
    /// Save is a capture instruction whose goto field has not been set.
    Save { slot: usize },
    /// EmptyLook is a zero-width assertion instruction whose goto field has
    /// not been set.
    EmptyLook { look: EmptyLook },
    /// Char is a character-match instruction whose goto field has not been
    /// set.
    Char { c: char },
    /// Ranges is a character-range-match instruction whose goto field has not
    /// been set.
    Ranges { ranges: Vec<(char, char)> },
}

impl MaybeInst {
    fn complete(&mut self, goto: InstIdx) {
        let filled = match *self {
            MaybeInst::Save { slot } => Inst::Save(InstSave {
                goto: goto,
                slot: slot,
            }),
            MaybeInst::EmptyLook { look } => Inst::EmptyLook(InstEmptyLook {
                goto: goto,
                look: look,
            }),
            MaybeInst::Char { c } => Inst::Char(InstChar {
                goto: goto,
                c: c,
            }),
            MaybeInst::Ranges { ref ranges } => Inst::Ranges(InstRanges {
                goto: goto,
                ranges: ranges.clone(),
            }),
            MaybeInst::Split1(goto1) => {
                Inst::Split(InstSplit { goto1: goto1, goto2: goto })
            }
            MaybeInst::Split2(goto2) => {
                Inst::Split(InstSplit { goto1: goto, goto2: goto2 })
            }
            _ => unreachable!("must be called on an uncompiled instruction \
                               with exactly one missing goto field, \
                               instead it was called on: {:?}", self),
        };
        *self = MaybeInst::Compiled(filled);
    }

    fn complete_split(&mut self, goto1: InstIdx, goto2: InstIdx) {
        let filled = match *self {
            MaybeInst::Split => {
                Inst::Split(InstSplit { goto1: goto1, goto2: goto2 })
            }
            _ => unreachable!("must be called on Split instruction, \
                               instead it was called on: {:?}", self),
        };
        *self = MaybeInst::Compiled(filled);
    }

    fn complete_split_goto1(&mut self, goto1: InstIdx) {
        let half_filled = match *self {
            MaybeInst::Split => goto1,
            _ => unreachable!("must be called on Split instruction, \
                               instead it was called on: {:?}", self),
        };
        *self = MaybeInst::Split1(half_filled);
    }

    fn complete_split_goto2(&mut self, goto2: InstIdx) {
        let half_filled = match *self {
            MaybeInst::Split => goto2,
            _ => unreachable!("must be called on Split instruction, \
                               instead it was called on: {:?}", self),
        };
        *self = MaybeInst::Split2(half_filled);
    }

    fn unwrap(self) -> Inst {
        match self {
            MaybeInst::Compiled(inst) => inst,
            _ => unreachable!("must be called on a compiled instruction, \
                               instead it was called on: {:?}", self),
        }
    }
}

fn u32_to_usize(n: u32) -> usize {
    if (n as u64) > (::std::usize::MAX as u64) {
        panic!("BUG: {} is too big to be pointer sized", n)
    }
    n as usize
}
