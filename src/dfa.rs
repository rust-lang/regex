// Copyright 2014-2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::collections::HashMap;
use std::fmt;
use std::mem;

use inst::{Insts, Inst, InstIdx};
use program::Program;
use sparse::SparseSet;

/// Return true if and only if the given program can be executed by a DFA.
///
/// Generally, a DFA is possible only when there are no zero-width assertions.
/// This restriction may be relaxed in the future.
pub fn can_exec(insts: &Insts) -> bool {
    use inst::EmptyLook::*;
    // N.B. This will return true on Unicode programs, which isn't technically
    // true. The caller must currently know to translate prog to byte based
    // first.
    for inst in insts {
        if let Inst::EmptyLook(ref inst) = *inst {
            match inst.look {
                WordBoundary | NotWordBoundary => return false,
                StartLine | EndLine | StartText | EndText => {}
            }
        }
    }
    true
}

type StateIdx = usize;

struct Byte(u16);

impl Byte {
    #[inline] fn input_byte(b: u8) -> Self { Byte(b as u16) }
    #[inline] fn eof() -> Self { Byte(256) }
    #[inline] fn is_eof(&self) -> bool { self.0 == 256 }

    #[inline]
    fn as_byte(&self) -> Option<u8> {
        if self.is_eof() {
            None
        } else {
            Some(self.0 as u8)
        }
    }
}

#[derive(Debug)]
pub struct DfaCache {
    compiled: HashMap<StateKey, StateIdx>,
    states: Vec<State>,
    qcur: SparseSet<InstIdx>,
    qnext: SparseSet<InstIdx>,
}

#[derive(Debug)]
pub struct Dfa<'r, 'c, 's> {
    prog: &'r Program,
    compiled: &'c mut HashMap<StateKey, StateIdx>,
    states: &'s mut Vec<State>,
}

struct State {
    next: [Option<StateIdx>; 257],
    insts: Vec<InstIdx>,
    given_flags: Flags,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct StateKey {
    insts: Vec<InstIdx>,
    given_flags: Flags,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Flags(u8);

impl DfaCache {
    /// Create new empty cache for the DFA engine.
    pub fn new() -> Self {
        DfaCache {
            compiled: HashMap::new(),
            states: vec![],
            qcur: SparseSet::new(0),
            qnext: SparseSet::new(0),
        }
    }

    fn resize(&mut self, num_insts: usize) {
        if num_insts != self.qcur.capacity() {
            self.qcur = SparseSet::new(num_insts);
            self.qnext = SparseSet::new(num_insts);
        }
    }
}

impl<'r, 'c, 's> Dfa<'r, 'c, 's> {
    pub fn exec(
        prog: &'r Program,
        text: &[u8],
        start: usize,
    ) -> Option<usize> {
        let mut _cache = prog.cache_dfa();
        let mut cache = &mut *_cache;
        cache.resize(prog.insts.len());
        let mut dfa = Dfa {
            prog: prog,
            compiled: &mut cache.compiled,
            states: &mut cache.states,
        };
        let r = if prog.reverse {
            dfa.exec_at_reverse(&mut cache.qcur, &mut cache.qnext, text, start)
        } else {
            dfa.exec_at(&mut cache.qcur, &mut cache.qnext, text, start)
        };
        // println!("{:?}", prog.insts);
        // println!("############################");
        // for (si, state) in dfa.states.iter().enumerate() {
            // println!("{:04} {:?}", si, state);
        // }
        // println!("############################");
        r
    }

    fn exec_at(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        text: &[u8],
        mut at: usize,
    ) -> Option<usize> {
        let mut flags = Flags::new();
        flags.set_start(at == 0).set_end(text.len() == 0);
        flags.set_start_line(at == 0 || text[at - 1] == b'\n');
        flags.set_end_line(text.len() == 0);
        self.follow_epsilon_transitions(0, qcur, flags);
        let mut si = match self.cached_state(qcur, flags) {
            None => return None,
            Some(si) => si,
        };
        let mut last_match = None;
        if self.states[si].is_match() {
            last_match = Some(at);
        }
        for (i, &b) in text[at..].iter().enumerate() {
            si = if let Some(nsi) = self.states[si].next[b as usize] {
                nsi
            } else if let Some(nsi) = self.exec_byte(qcur, qnext, si,
                                                     Byte::input_byte(b)) {
                nsi
            } else {
                return last_match;
            };
            if self.states[si].is_match() {
                last_match = Some(at + i);
            }
        }
        si = if let Some(nsi) = self.states[si].next[Byte::eof().0 as usize] {
            nsi
        } else if let Some(nsi) = self.exec_byte(qcur, qnext, si,
                                                 Byte::eof()) {
            nsi
        } else {
            return last_match;
        };
        if self.states[si].is_match() {
            last_match = Some(text.len());
        }
        last_match
    }

    fn exec_at_reverse(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        text: &[u8],
        mut at: usize,
    ) -> Option<usize> {
        let mut flags = Flags::new();
        flags.set_start(at == text.len()).set_end(text.len() == 0);
        flags.set_start_line(at == text.len() || text[at] == b'\n');
        flags.set_end_line(text.len() == 0);
        self.follow_epsilon_transitions(0, qcur, flags);
        let mut si = match self.cached_state(qcur, flags) {
            None => return None,
            Some(si) => si,
        };
        let mut last_match = None;
        if self.states[si].is_match() {
            last_match = Some(at);
        }
        for (i, &b) in text[..at].iter().rev().enumerate() {
            si = if let Some(nsi) = self.states[si].next[b as usize] {
                nsi
            } else if let Some(nsi) = self.exec_byte(qcur, qnext, si,
                                                     Byte::input_byte(b)) {
                nsi
            } else {
                return last_match;
            };
            if self.states[si].is_match() {
                last_match = Some(at - i);
            }
        }
        si = if let Some(nsi) = self.states[si].next[Byte::eof().0 as usize] {
            nsi
        } else if let Some(nsi) = self.exec_byte(qcur, qnext, si,
                                                 Byte::eof()) {
            nsi
        } else {
            return last_match;
        };
        if self.states[si].is_match() {
            last_match = Some(0);
        }
        last_match
    }

    fn exec_byte(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        si: StateIdx,
        b: Byte,
    ) -> Option<StateIdx> {
        use inst::Inst::*;

        let mut flags = Flags::new();
        if b.is_eof() {
            flags.set_end(true).set_end_line(true);
        } else if b.as_byte().map_or(false, |b| b == b'\n') {
            flags.set_end_line(true);
        }

        qcur.clear();
        for &ip in &self.states[si].insts {
            self.follow_epsilon_transitions(ip, qcur, flags);
        }
        flags.clear();
        qnext.clear();
        if b.as_byte().map_or(false, |b| b == b'\n') {
            flags.set_start_line(true);
        }
        for &ip in &*qcur {
            match self.prog.insts[ip] {
                Char(_) | Ranges(_) => unreachable!(),
                Save(_) | Split(_) | EmptyLook(_) => {}
                Match => { flags.set_match(true); }
                Bytes(ref inst) => {
                    if b.as_byte().map_or(false, |b| inst.matches(b)) {
                        self.follow_epsilon_transitions(
                            inst.goto, qnext, flags);
                    }
                }
            }
        }
        let next = self.cached_state(qnext, flags);
        self.states[si].next[b.0 as usize] = next;
        next
    }

    /// Follows the epsilon transitions starting at (and including) `ip`. The
    /// resulting states are inserted into the ordered set `q`.
    ///
    /// Conditional epsilon transitions (i.e., empty width assertions) are only
    /// followed if they are satisfied by the given flags, which should
    /// represent the flags set at the current location in the input.
    ///
    /// If the current location corresponds to the empty string, then only the
    /// end line and/or end text flags may be set. If the current location
    /// corresponds to a real byte in the input, then only the start line
    /// and/or start text flags may be set.
    ///
    /// As an exception to the above, when finding the initial state, any of
    /// the above flags may be set:
    ///
    /// If matching starts at the beginning of the input, then start text and
    /// start line should be set. If the input is empty, then end text and end
    /// line should also be set.
    ///
    /// If matching starts after the beginning of the input, then only start
    /// line should be set if the preceding byte is `\n`. End line should never
    /// be set in this case.
    fn follow_epsilon_transitions(
        &self,
        ip: InstIdx,
        q: &mut SparseSet<InstIdx>,
        flags: Flags,
    ) {
        use inst::Inst::*;
        use inst::EmptyLook::*;

        if q.contains_sparse_index(ip) {
            return;
        }
        // TODO(ag): use an explicit stack.
        q.add(ip);
        match self.prog.insts[ip] {
            Char(_) | Ranges(_) => unreachable!(),
            Match | Bytes(_) => {}
            EmptyLook(ref inst) => {
                match inst.look {
                    WordBoundary | NotWordBoundary => unreachable!(),
                    StartLine if flags.is_start_line() => {
                        self.follow_epsilon_transitions(inst.goto, q, flags);
                    }
                    EndLine if flags.is_end_line() => {
                        self.follow_epsilon_transitions(inst.goto, q, flags);
                    }
                    StartText if flags.is_start() => {
                        self.follow_epsilon_transitions(inst.goto, q, flags);
                    }
                    EndText if flags.is_end() => {
                        self.follow_epsilon_transitions(inst.goto, q, flags);
                    }
                    StartLine | EndLine | StartText | EndText => {}
                }
            }
            Save(ref inst) => {
                self.follow_epsilon_transitions(inst.goto, q, flags);
            }
            Split(ref inst) => {
                self.follow_epsilon_transitions(inst.goto1, q, flags);
                self.follow_epsilon_transitions(inst.goto2, q, flags);
            }
        }
    }

    /// Find a previously computed state matching the given set of instructions
    /// and flags.
    ///
    /// The given set of instructions should represent a single state in the
    /// NFA along with all states reachable without consuming any input.
    ///
    /// The given flags should correspond to the flags set immediately *after*
    /// the current byte of input. If there is no current byte (i.e., when
    /// computing the start state), then the start text and/or start line flags
    /// *may* be set.
    fn cached_state(
        &mut self,
        q: &SparseSet<InstIdx>,
        mut given_flags: Flags,
    ) -> Option<StateIdx> {
        use std::collections::hash_map::Entry;
        use inst::Inst::*;
        use inst::EmptyLook::*;

        // We need to build up enough information to recognize pre-built states
        // in the DFA. Generally speaking, this includes every instruction
        // except for those which are purely epsilon transitions, e.g., the
        // Save and Split instructions.
        //
        // Empty width assertions are also epsilon transitions, but since they
        // are conditional, we need to make them part of a state's key in the
        // cache. Note that not only do the empty width instructions make it
        // into the key, but the flags set at the current byte of input are
        // *also* added to the key. This ensures that distinct states are
        // generated based on which flags are set.
        //
        // As an optimization, if the various NFA states *don't* include any
        // empty width assertions, then we throw away the flags associated with
        // the current byte of input because they will never discriminate.
        let mut has_flags = false;
        let mut insts = vec![];
        for &ip in q {
            match self.prog.insts[ip] {
                Char(_) | Ranges(_) => unreachable!(),
                Save(_) => {}
                Split(_) => {}
                Bytes(_) => insts.push(ip),
                EmptyLook(ref inst) => {
                    match inst.look {
                        WordBoundary | NotWordBoundary => unreachable!(),
                        StartLine | EndLine | StartText | EndText => {
                            has_flags = true;
                            insts.push(ip);
                        }
                    }
                }
                Match => {
                    insts.push(ip);
                    // If this is a reverse program, then we want to continue
                    // executing to find the longest possible match. Otherwise,
                    // we only support leftmost-first semantics, so bail out.
                    if !self.prog.reverse {
                        break;
                    }
                }
            }
        }
        if !has_flags {
            // Wipe out the flags from the search text at the current byte,
            // except for whether this is a match state.
            given_flags = *Flags::new().set_match(given_flags.is_match());
        }
        if insts.len() == 0 && given_flags.is_empty() {
            return None;
        }
        let key = StateKey {
            insts: insts.clone(),
            given_flags: given_flags,
        };
        Some(match self.compiled.entry(key) {
            Entry::Occupied(v) => *v.get(),
            Entry::Vacant(v) => {
                self.states.push(State {
                    next: [None; 257],
                    insts: insts,
                    given_flags: given_flags,
                });
                *v.insert(self.states.len() - 1)
            }
        })
    }
}

impl State {
    fn is_match(&self) -> bool {
        self.given_flags.is_match()
    }
}

impl Flags {
    #[inline]
    fn new() -> Self {
        Flags(0)
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.0 == 0
    }

    #[inline]
    fn clear(&mut self) {
        self.0 = 0;
    }

    #[inline]
    fn set(&mut self, yes: bool, bit: u8) {
        if yes {
            self.0 = self.0 | bit;
        } else {
            self.0 = self.0 & !bit;
        }
    }

    #[inline]
    fn is_match(&self) -> bool { self.0 & 0b1_0000000 > 0 }

    #[inline]
    fn set_match(&mut self, yes: bool) -> &mut Self {
        self.set(yes, 0b1_0000000);
        self
    }

    #[inline]
    fn is_start(&self) -> bool { self.0 & 0b0_1_000000 > 0 }

    #[inline]
    fn set_start(&mut self, yes: bool) -> &mut Self {
        self.set(yes, 0b0_1_000000);
        self
    }

    #[inline]
    fn is_end(&self) -> bool { self.0 & 0b00_1_00000 > 0 }

    #[inline]
    fn set_end(&mut self, yes: bool) -> &mut Self {
        self.set(yes, 0b00_1_00000);
        self
    }

    #[inline]
    fn is_start_line(&self) -> bool { self.0 & 0b0000_1_000 > 0 }

    #[inline]
    fn set_start_line(&mut self, yes: bool) -> &mut Self {
        self.set(yes, 0b0000_1_000);
        self
    }

    #[inline]
    fn is_end_line(&self) -> bool { self.0 & 0b00000_1_00 > 0 }

    #[inline]
    fn set_end_line(&mut self, yes: bool) -> &mut Self {
        self.set(yes, 0b00000_1_00);
        self
    }
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut next = vec![];
        for (b, next_si) in self.next.iter().enumerate() {
            if let Some(si) = *next_si {
                next.push((vb(b as usize), si));
            }
        }
        f.debug_struct("State")
         .field("given_flags", &self.given_flags)
         .field("insts", &self.insts)
         .field("next", &next)
         .finish()
    }
}

impl fmt::Debug for Flags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Flags")
         .field("match", &if self.is_match() { 1 } else { 0 })
         .field("start", &if self.is_start() { 1 } else { 0 })
         .field("end", &if self.is_end() { 1 } else { 0 })
         .field("start_line", &if self.is_start_line() { 1 } else { 0 })
         .field("end_line", &if self.is_end_line() { 1 } else { 0 })
         .finish()
    }
}

fn vb(b: usize) -> String {
    use std::ascii::escape_default;

    if b > ::std::u8::MAX as usize {
        "EOF".to_owned()
    } else {
        let escaped = escape_default(b as u8).collect::<Vec<u8>>();
        String::from_utf8_lossy(&escaped).into_owned()
    }
}

#[cfg(test)]
mod tests {
    use exec::{Executor, MatchEngine};
    use program::ProgramBuilder;
    use super::Dfa;

    #[test]
    fn scratch() {
        // let re = ">.*\n|\n";
        // let text = " > abc\n";

        let re = "^[ \t]+|[ \t]+$";
        let text = " \t  trim me\t   \t";

        // let re = "a+b";
        // let text = "xyzaaab";

        // let re = r"(?m)^a+x$";
        // let text = "aaaa\naax";

        // let re = r"^a+x";
        // let text = "baa\naax";

        // let re = "(a|b)*a(a|b){10}";
        // let text = "ababbab";

        // println!("REGEX: {:?}", ::Regex::new(re).unwrap().find_iter(text).collect::<Vec<_>>());

        /*
        let prog = ProgramBuilder::new(re).dfa(true).compile().unwrap();
        let matched = Dfa::exec(&prog, text.as_bytes(), 4);
        println!("matched? {:?}", matched);

        println!("-------------------");
        println!("-------------------");
        println!("-------------------");

        if let Some(end) = matched {
            let prog = ProgramBuilder::new(re)
                                      .dfa(true)
                                      .reverse(true)
                                      .compile()
                                      .unwrap();
            let matched = Dfa::exec(&prog, text.as_bytes(), end);
            println!("reverse matched? {:?}, text: {}", matched, text.len());
        }

        println!("-------------------");
        println!("-------------------");
        println!("-------------------");
        */

        let exec = Executor::new(
            re, MatchEngine::Automatic, 10 * (1<<20), false).unwrap();
        let mut caps = [None; 2];
        println!("matched? {}", exec.exec_auto(&mut caps, text, 4));
        println!("caps: {:?}", &caps[..]);
    }
}
