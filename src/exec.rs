// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use backtrack::{self, Backtrack};
use dfa::{self, Dfa};
use input::{ByteInput, CharInput};
use literals::Literals;
use nfa::Nfa;
use program::{Program, ProgramBuilder};
use re::CaptureIdxs;
use Error;

/// Executor manages the execution of a regular expression.
///
/// In particular, this manages the various compiled forms of a single regular
/// expression and the choice of which matching engine to use to execute a
/// regular expression.
#[derive(Clone, Debug)]
pub struct Executor {
    /// A compiled program that executes a regex on Unicode codepoints.
    /// This can be Unicode based, byte based or have both.
    prog: Prog,
    /// A preference for matching engine selection.
    ///
    /// This defaults to Automatic, which means the matching engine is selected
    /// based on heuristics (such as the nature and size of the compiled
    /// program, in addition to the size of the search text).
    ///
    /// If either Nfa or Backtrack is set, then it is always used because
    /// either is capable of executing every compiled program on any input
    /// size.
    ///
    /// If anything else is set, behaviour may be incorrect.
    match_engine: MatchEngine,
}

impl Executor {
    pub fn new(
        re: &str,
        match_engine: MatchEngine,
        size_limit: usize,
        bytes: bool,
    ) -> Result<Executor, Error> {
        let mut bprog = ProgramBuilder::new(re).size_limit(size_limit);
        let prog = if bytes {
            Prog::Bytes(try!(bprog.bytes(true).compile()))
        } else {
            let unicode = try!(bprog.compile());
            if dfa::can_exec(&unicode.insts) {
                Prog::Both {
                    unicode: unicode,
                    bytes: try!(ProgramBuilder::new(re)
                                               .size_limit(size_limit)
                                               .dfa(true)
                                               .compile()),
                }
            } else {
                Prog::Unicode(unicode)
            }
        };
        Ok(Executor {
            prog: prog,
            match_engine: match_engine,
        })
    }

    fn program(&self) -> &Program {
        match self.prog {
            Prog::Unicode(ref p) => p,
            Prog::Bytes(ref p) => p,
            Prog::Both { ref unicode, .. } => unicode,
        }
    }

    pub fn regex_str(&self) -> &str {
        &self.program().original
    }

    pub fn capture_names(&self) -> &[Option<String>] {
        &self.program().cap_names
    }

    pub fn alloc_captures(&self) -> Vec<Option<usize>> {
        self.program().alloc_captures()
    }

    pub fn exec(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        match self.match_engine {
            MatchEngine::Nfa => self.exec_nfa(caps, text, start),
            MatchEngine::Backtrack => self.exec_backtrack(caps, text, start),
            MatchEngine::Literals => {
                self.exec_literals(&self.program().prefixes, caps, text, start)
            }
            MatchEngine::Automatic => self.exec_auto(caps, text, start),
        }
    }

    fn exec_auto(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        if caps.len() == 0 {
            if let Prog::Both { ref unicode, ref bytes } = self.prog {
                return Dfa::exec(bytes, text.as_bytes(), start).is_some();
            }
        }

        let prog = self.program();
        if caps.len() <= 2 && prog.is_prefix_match() {
            self.exec_literals(&prog.prefixes, caps, text, start)
        } else if backtrack::should_exec(prog.insts.len(), text.len()) {
            self.exec_backtrack(caps, text, start)
        } else {
            self.exec_nfa(caps, text, start)
        }
    }

    fn exec_nfa(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        match self.prog {
            Prog::Unicode(ref p) => {
                Nfa::exec(p, caps, CharInput::new(text), start)
            }
            Prog::Bytes(ref p) => {
                Nfa::exec(p, caps, ByteInput::new(text), start)
            }
            Prog::Both { ref unicode, .. } => {
                Nfa::exec(unicode, caps, CharInput::new(text), start)
            }
        }
    }

    fn exec_backtrack(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        match self.prog {
            Prog::Unicode(ref p) => {
                Backtrack::exec(p, caps, CharInput::new(text), start)
            }
            Prog::Bytes(ref p) => {
                Backtrack::exec(p, caps, ByteInput::new(text), start)
            }
            Prog::Both { ref unicode, .. } => {
                Backtrack::exec(unicode, caps, CharInput::new(text), start)
            }
        }
    }

    fn exec_literals(
        &self,
        literals: &Literals,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        match literals.find(&text.as_bytes()[start..]) {
            None => false,
            Some((s, e)) => {
                if caps.len() == 2 {
                    caps[0] = Some(start + s);
                    caps[1] = Some(start + e);
                }
                true
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Prog {
    Unicode(Program),
    Bytes(Program),
    Both { unicode: Program, bytes: Program },
}

/// The matching engines offered by this regex implementation.
///
/// N.B. This is exported for use in testing.
#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
pub enum MatchEngine {
    /// Automatically choose the best matching engine based on heuristics.
    Automatic,
    /// A bounded backtracking implementation. About twice as fast as the
    /// NFA, but can only work on small regexes and small input.
    Backtrack,
    /// A full NFA simulation. Can always be employed but almost always the
    /// slowest choice.
    Nfa,
    /// If the entire regex is a literal and no capture groups have been
    /// requested, then we can degrade to a simple substring match.
    Literals,
}
