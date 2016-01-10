// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use syntax;

use Error;
use backtrack::{Backtrack, BackMachine};
use compile::Compiler;
use inst::{EmptyLook, Inst};
use nfa::{Nfa, NfaThreads};
use pool::Pool;
use prefix::Prefix;
use re::CaptureIdxs;

const NUM_PREFIX_LIMIT: usize = 30;
const PREFIX_LENGTH_LIMIT: usize = 15;

/// The matching engines offered by this regex implementation.
///
/// N.B. This is exported for use in testing.
#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
pub enum MatchEngine {
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

/// Program represents a compiled regular expression. Once an expression is
/// compiled, its representation is immutable and will never change.
/// (Well, almost. In fact, the matching engines cache state that can be
/// reused on subsequent searches. But this is interior mutability that
/// shouldn't be observable by the caller.)
#[derive(Debug)]
pub struct Program {
    /// The original regular expression string.
    pub original: String,
    /// A sequence of instructions.
    pub insts: Vec<Inst>,
    /// The sequence of capture group names. There is an entry for each capture
    /// group index and a name exists only if the capture group is named.
    pub cap_names: Vec<Option<String>>,
    /// If the regular expression requires a literal prefix in order to have a
    /// match, that prefix is stored here as a DFA.
    pub prefixes: Prefix,
    /// True iff matching any literal prefix indicates a match.
    pub prefixes_complete: bool,
    /// True iff program is anchored at the beginning.
    pub anchored_begin: bool,
    /// True iff program is anchored at the end.
    pub anchored_end: bool,
    /// The type of matching engine to use.
    /// When `None` (the default), pick an engine automatically.
    pub engine: Option<MatchEngine>,
    /// Cached NFA threads.
    pub nfa_threads: Pool<NfaThreads>,
    /// Cached backtracking memory.
    pub backtrack: Pool<BackMachine>,
}

impl Program {
    /// Compiles a Regex.
    pub fn new(
        engine: Option<MatchEngine>,
        size_limit: usize,
        re: &str,
    ) -> Result<Program, Error> {
        let expr = try!(syntax::Expr::parse(re));
        let compiler = Compiler::new(size_limit);
        let (insts, cap_names) = try!(compiler.compile(&expr));
        let (insts_len, ncaps) = (insts.len(), num_captures(&insts));
        let create_threads = move || NfaThreads::new(insts_len, ncaps);
        let create_backtrack = move || BackMachine::new();
        let mut prog = Program {
            original: re.into(),
            insts: insts,
            cap_names: cap_names,
            prefixes: Prefix::Empty,
            prefixes_complete: false,
            anchored_begin: false,
            anchored_end: false,
            engine: engine,
            nfa_threads: Pool::new(Box::new(create_threads)),
            backtrack: Pool::new(Box::new(create_backtrack)),
        };

        prog.find_prefixes();
        prog.anchored_begin = match prog.insts[1] {
            Inst::EmptyLook(ref inst) => inst.look == EmptyLook::StartText,
            _ => false,
        };
        prog.anchored_end = match prog.insts[prog.insts.len() - 3] {
            Inst::EmptyLook(ref inst) => inst.look == EmptyLook::EndText,
            _ => false,
        };
        Ok(prog)
    }

    /// Executes a compiled regex program.
    pub fn exec(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        match self.choose_engine(caps.len(), text) {
            MatchEngine::Backtrack => Backtrack::exec(self, caps, text, start),
            MatchEngine::Nfa => Nfa::exec(self, caps, text, start),
            MatchEngine::Literals => {
                match self.prefixes.find(&text[start..]) {
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
    }

    fn choose_engine(&self, cap_len: usize, text: &str) -> MatchEngine {
        // If the engine is already chosen, then we use it.
        // But that might not be a good idea. e.g., What if `Literals` is
        // chosen and it can't work? I guess we should probably check whether
        // the chosen engine is appropriate or not.
        self.engine.unwrap_or_else(|| {
            if cap_len <= 2
               && self.prefixes_complete
               && self.prefixes.preserves_priority() {
                MatchEngine::Literals
            } else if Backtrack::should_exec(self, text) {
                // We're only here if the input and regex combined are small.
                MatchEngine::Backtrack
            } else {
                MatchEngine::Nfa
            }
        })
    }

    /// Returns the total number of capture groups in the regular expression.
    /// This includes the zeroth capture.
    pub fn num_captures(&self) -> usize {
        num_captures(&self.insts)
    }

    /// Allocate new capture groups.
    pub fn alloc_captures(&self) -> Vec<Option<usize>> {
        vec![None; 2 * self.num_captures()]
    }

    /// Find and store a prefix machine for the current program.
    pub fn find_prefixes(&mut self) {
        // First, look for a standard literal prefix---this includes things
        // like `a+` and `[0-9]+`, but not `a|b`.
        let (ps, complete) = self.literals(self.skip(1));
        if !ps.is_empty() {
            self.prefixes = Prefix::new(ps);
            self.prefixes_complete = complete;
            return;
        }
        // Ok, now look for alternate prefixes, e.g., `a|b`.
        if let Some((pfxs, complete)) = self.alternate_prefixes() {
            self.prefixes = Prefix::new(pfxs);
            self.prefixes_complete = complete;
        }
    }

    fn alternate_prefixes(&self) -> Option<(Vec<String>, bool)> {
        let mut prefixes = vec![];
        let mut pcomplete = true;
        let mut stack = vec![self.skip(1)];
        while let Some(mut pc) = stack.pop() {
            pc = self.skip(pc);
            match self.insts[pc] {
                Inst::Split(ref inst) => {
                    stack.push(inst.goto2);
                    stack.push(inst.goto1);
                }
                _ => {
                    let (alt_prefixes, complete) = self.literals(pc);
                    if alt_prefixes.is_empty() {
                        // If no prefixes could be identified for this
                        // alternate, then we can't use a prefix machine to
                        // skip through the input. Thus, we fail and report
                        // nothing.
                        return None;
                    }
                    if prefixes.len() + alt_prefixes.len() > NUM_PREFIX_LIMIT {
                        // Arg. We've over-extended ourselves, quit with
                        // nothing to show for it.
                        //
                        // This could happen if the regex is `a|b|c|...`, where
                        // the number of alternates is too much for us to
                        // handle given an empirically defined threshold limit.
                        //
                        // When this happens, we can't capture all of the
                        // prefixes, so our prefix machine becomes useless.
                        // Thus, fail and report nothing.
                        return None;
                    }
                    pcomplete = pcomplete && complete;
                    prefixes.extend(alt_prefixes);
                }
            }
        }
        if prefixes.is_empty() {
            None
        } else {
            Some((prefixes, pcomplete))
        }
    }

    /// Find required literals starting at the given instruction.
    ///
    /// Returns `true` in the tuple if the end of the literal leads trivially
    /// to a match. (This may report false negatives, but being conservative
    /// is OK.)
    fn literals(&self, mut pc: usize) -> (Vec<String>, bool) {
        #![allow(unused_assignments)]
        use inst::Inst::*;

        let mut complete = true;
        let mut alts = vec![String::new()];
        loop {
            let inst = &self.insts[pc];

            // Each iteration adds one character to every alternate prefix *or*
            // it stops. Thus, the prefix alternates grow in lock step, and it
            // suffices to check one of them to see if the prefix limit has
            // been exceeded.
            if alts[0].len() > PREFIX_LENGTH_LIMIT {
                complete = false;
                break;
            }
            match *inst {
                Save(ref inst) => { pc = inst.goto; continue }
                Char(ref inst) => {
                    for alt in &mut alts {
                        alt.push(inst.c);
                    }
                    pc = inst.goto;
                }
                Ranges(ref inst) => {
                    // This adds a new literal for *each* character in this
                    // range. This has the potential to use way too much
                    // memory, so we bound it naively for now.
                    let nchars = num_chars_in_ranges(&inst.ranges);
                    if alts.len() * nchars > NUM_PREFIX_LIMIT {
                        complete = false;
                        break;
                    }

                    let orig = alts;
                    alts = Vec::with_capacity(orig.len());
                    for &(s, e) in &inst.ranges {
                        for c in (s as u32)..(e as u32 + 1){
                            for alt in &orig {
                                let mut alt = alt.clone();
                                alt.push(::std::char::from_u32(c).unwrap());
                                alts.push(alt);
                            }
                        }
                    }
                    pc = inst.goto;
                }
                _ => { complete = self.leads_to_match(pc); break }
            }
        }
        if alts[0].is_empty() {
            (vec![], false)
        } else {
            (alts, complete)
        }
    }

    fn leads_to_match(&self, pc: usize) -> bool {
        // I'm pretty sure this is conservative, so it might have some
        // false negatives.
        match self.insts[self.skip(pc)] {
            Inst::Match => true,
            _ => false,
        }
    }

    fn skip(&self, mut pc: usize) -> usize {
        loop {
            match self.insts[pc] {
                Inst::Save(_) => pc += 1,
                _ => return pc,
            }
        }
    }
}

impl Clone for Program {
    fn clone(&self) -> Program {
        let (insts_len, ncaps) = (self.insts.len(), self.num_captures());
        let create_threads = move || NfaThreads::new(insts_len, ncaps);
        let create_backtrack = move || BackMachine::new();
        Program {
            original: self.original.clone(),
            insts: self.insts.clone(),
            cap_names: self.cap_names.clone(),
            prefixes: self.prefixes.clone(),
            prefixes_complete: self.prefixes_complete,
            anchored_begin: self.anchored_begin,
            anchored_end: self.anchored_end,
            engine: self.engine,
            nfa_threads: Pool::new(Box::new(create_threads)),
            backtrack: Pool::new(Box::new(create_backtrack)),
        }
    }
}

/// Return the number of captures in the given sequence of instructions.
fn num_captures(insts: &[Inst]) -> usize {
    let mut n = 0;
    for inst in insts {
        if let Inst::Save(ref inst) = *inst {
            n = ::std::cmp::max(n, inst.slot + 1)
        }
    }
    // There's exactly 2 Save slots for every capture.
    n / 2
}

/// Count the number of characters in the given range.
///
/// This is useful for pre-emptively limiting the number of prefix literals
/// we extract from a regex program.
fn num_chars_in_ranges(ranges: &[(char, char)]) -> usize {
    ranges.iter()
          .map(|&(s, e)| 1 + (e as u32) - (s as u32))
          .fold(0, |acc, len| acc + len) as usize
}

#[cfg(test)]
mod tests {
    use super::Program;

    macro_rules! prog {
        ($re:expr) => { Program::new(None, 1 << 30, $re).unwrap() }
    }

    macro_rules! prefixes {
        ($re:expr) => {{
            let p = prog!($re);
            assert!(!p.prefixes_complete);
            p.prefixes.prefixes()
        }}
    }
    macro_rules! prefixes_complete {
        ($re:expr) => {{
            let p = prog!($re);
            assert!(p.prefixes_complete);
            p.prefixes.prefixes()
        }}
    }

    #[test]
    fn single() {
        assert_eq!(prefixes_complete!("a"), vec!["a"]);
        assert_eq!(prefixes_complete!("[a]"), vec!["a"]);
        assert_eq!(prefixes!("a+"), vec!["a"]);
        assert_eq!(prefixes!("(?:a)+"), vec!["a"]);
        assert_eq!(prefixes!("(a)+"), vec!["a"]);
    }

    #[test]
    fn single_alt() {
        assert_eq!(prefixes_complete!("a|b"), vec!["a", "b"]);
        assert_eq!(prefixes_complete!("b|a"), vec!["b", "a"]);
        assert_eq!(prefixes_complete!("[a]|[b]"), vec!["a", "b"]);
        assert_eq!(prefixes!("a+|b"), vec!["a", "b"]);
        assert_eq!(prefixes!("a|b+"), vec!["a", "b"]);
        assert_eq!(prefixes!("(?:a+)|b"), vec!["a", "b"]);
        assert_eq!(prefixes!("(a+)|b"), vec!["a", "b"]);
    }

    #[test]
    fn many() {
        assert_eq!(prefixes_complete!("abcdef"), vec!["abcdef"]);
        assert_eq!(prefixes!("abcdef+"), vec!["abcdef"]);
        assert_eq!(prefixes!("(?:abcdef)+"), vec!["abcdef"]);
        assert_eq!(prefixes!("(abcdef)+"), vec!["abcdef"]);
    }

    #[test]
    fn many_alt() {
        assert_eq!(prefixes_complete!("abc|def"), vec!["abc", "def"]);
        assert_eq!(prefixes_complete!("def|abc"), vec!["def", "abc"]);
        assert_eq!(prefixes!("abc+|def"), vec!["abc", "def"]);
        assert_eq!(prefixes!("abc|def+"), vec!["abc", "def"]);
        assert_eq!(prefixes!("(?:abc)+|def"), vec!["abc", "def"]);
        assert_eq!(prefixes!("(abc)+|def"), vec!["abc", "def"]);
    }

    #[test]
    fn class() {
        assert_eq!(prefixes_complete!("[0-9]"), vec![
            "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
        ]);
        assert_eq!(prefixes!("[0-9]+"), vec![
            "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
        ]);
    }

    #[test]
    fn preceding_alt() {
        assert_eq!(prefixes!("(?:a|b).+"), vec!["a", "b"]);
        assert_eq!(prefixes!("(a|b).+"), vec!["a", "b"]);
    }

    #[test]
    fn nested_alt() {
        assert_eq!(prefixes_complete!("(a|b|c|d)"),
                   vec!["a", "b", "c", "d"]);
        assert_eq!(prefixes_complete!("((a|b)|(c|d))"),
                   vec!["a", "b", "c", "d"]);
    }
}
