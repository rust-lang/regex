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

use inst::{Insts, Inst};
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DfaResult {
    Match(usize),
    EarlyMatch,
    NoMatch,
    Quit,
}

#[derive(Debug)]
pub struct DfaCache {
    compiled: HashMap<StateKey, StateIdx>,
    states: Vec<State>,
    start_states: Vec<StatePtr>,
    stack: Vec<InstIdx>,
    qcur: SparseSet<InstIdx>,
    qnext: SparseSet<InstIdx>,
}

#[derive(Debug)]
pub struct Dfa<'r, 'c, 's, 'ss, 't> {
    prog: &'r Program,
    compiled: &'c mut HashMap<StateKey, StateIdx>,
    states: &'s mut Vec<State>,
    start_states: &'ss mut Vec<StatePtr>,
    stack: &'t mut Vec<InstIdx>,
    quit_on_first_match: bool,
}

struct State {
    next: Vec<StatePtr>,
    insts: Vec<InstIdx>,
    given_flags: Flags,
    inst_flags: Flags,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct StateKey {
    insts: Vec<InstIdx>,
    given_flags: Flags,
}

type InstIdx = u32;

type StateIdx = u32;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
enum StatePtr {
    Some(StateIdx),
    Dead,
    Unknown,
}

#[derive(Copy, Clone, Debug)]
struct Byte(u16);

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Flags(u8);

impl DfaCache {
    /// Create new empty cache for the DFA engine.
    pub fn new() -> Self {
        DfaCache {
            compiled: HashMap::new(),
            states: vec![],
            start_states: vec![StatePtr::Unknown; 256],
            stack: vec![],
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

impl<'r, 'c, 's, 'ss, 't> Dfa<'r, 'c, 's, 'ss, 't> {
    pub fn exec(
        prog: &'r Program,
        text: &[u8],
        start: usize,
        quit_on_first_match: bool,
    ) -> DfaResult {
        let mut _cache = prog.cache_dfa();
        let mut cache = &mut **_cache;
        cache.resize(prog.insts.len());
        let mut dfa = Dfa {
            prog: prog,
            compiled: &mut cache.compiled,
            states: &mut cache.states,
            start_states: &mut cache.start_states,
            stack: &mut cache.stack,
            quit_on_first_match: quit_on_first_match,
        };
        let r = if prog.insts.is_reversed() {
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
        println!("size: {:?} bytes", dfa.approximate_size());
        // for (i, cls) in prog.insts.byte_classes().iter().enumerate() {
            // println!("{}: {}", i, cls);
        // }
        r
    }

    fn exec_at(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        text: &[u8],
        mut at: usize,
    ) -> DfaResult {
        let start_flags = self.start_flags(text, at);
        let start = match self.start_state(qcur, start_flags) {
            None => return DfaResult::NoMatch,
            Some(si) => si,
        };
        let mut last_match = DfaResult::NoMatch;
        if self.states[start as usize].is_match() {
            if self.quit_on_first_match {
                return DfaResult::EarlyMatch;
            }
            last_match = DfaResult::Match(at);
        }
        let (mut si, mut i) = (start, at);
        while i < text.len() {
            if !self.prog.prefixes.is_empty() && si == start {
                i = match self.prefix_at(text, i) {
                    None => return DfaResult::NoMatch,
                    Some(i) => i,
                };
            }

            // Inline next_state to avoid an extra branch.
            let cls = self.prog.insts.byte_classes()[text[i] as usize];
            si = match self.states[si as usize].next[cls] {
                StatePtr::Some(nsi) => nsi,
                StatePtr::Dead => return last_match,
                StatePtr::Unknown => {
                    let b = Byte::input_byte(text[i]);
                    match self.exec_byte(qcur, qnext, si, b) {
                        Some(nsi) => nsi,
                        None => return last_match,
                    }
                }
            };
            if self.states[si as usize].is_match() {
                if self.quit_on_first_match {
                    return DfaResult::EarlyMatch;
                }
                last_match = DfaResult::Match(i);
            }
            i += 1;
        }
        si = match self.next_state(qcur, qnext, si, Byte::eof()) {
            Some(nsi) => nsi,
            None => return last_match,
        };
        if self.states[si as usize].is_match() {
            last_match = DfaResult::Match(text.len());
        }
        last_match
    }

    fn exec_at_reverse(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        text: &[u8],
        mut at: usize,
    ) -> DfaResult {
        let start_flags = self.start_flags_reverse(text, at);
        let start = match self.start_state(qcur, start_flags) {
            None => return DfaResult::NoMatch,
            Some(si) => si,
        };
        let mut last_match = DfaResult::NoMatch;
        if self.states[start as usize].is_match() {
            if self.quit_on_first_match {
                return DfaResult::EarlyMatch;
            }
            last_match = DfaResult::Match(at);
        }
        let (mut si, mut i) = (start, at);
        while i > 0 {
            i -= 1;

            // Inline next_state to avoid an extra branch.
            let cls = self.prog.insts.byte_classes()[text[i] as usize];
            si = match self.states[si as usize].next[cls] {
                StatePtr::Some(nsi) => nsi,
                StatePtr::Dead => return last_match,
                StatePtr::Unknown => {
                    let b = Byte::input_byte(text[i]);
                    match self.exec_byte(qcur, qnext, si, b) {
                        Some(nsi) => nsi,
                        None => return last_match,
                    }
                }
            };
            if self.states[si as usize].is_match() {
                if self.quit_on_first_match {
                    return DfaResult::EarlyMatch;
                }
                last_match = DfaResult::Match(i+1);
            }
        }
        si = match self.next_state(qcur, qnext, si, Byte::eof()) {
            Some(nsi) => nsi,
            None => return last_match,
        };
        if self.states[si as usize].is_match() {
            last_match = DfaResult::Match(0);
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

        // println!("STATE: {:?}, BYTE: {:?}", si, vb(b.0 as usize));

        qcur.clear();
        for &ip in &self.states[si as usize].insts {
            qcur.add(ip);
        }

        if self.states[si as usize].inst_flags.has_non_match_flags() {
            let mut flags = Flags::new();
            if b.is_eof() {
                flags.set_end(true).set_end_line(true);
            } else if b.as_byte().map_or(false, |b| b == b'\n') {
                flags.set_end_line(true);
            }
            if flags.has_non_match_flags() {
                qnext.clear();
                for &ip in &*qcur {
                    self.follow_epsilon_transitions(ip, qnext, flags);
                }
                mem::swap(qcur, qnext);
            }
        }

        let mut flags = Flags::new();
        if b.as_byte().map_or(false, |b| b == b'\n') {
            flags.set_start_line(true);
        }
        qnext.clear();
        for &ip in &*qcur {
            match self.prog.insts[ip as usize] {
                Char(_) | Ranges(_) => unreachable!(),
                Save(_) | Split(_) | EmptyLook(_) => {}
                Match => { flags.set_match(true); }
                Bytes(ref inst) => {
                    if b.as_byte().map_or(false, |b| inst.matches(b)) {
                        self.follow_epsilon_transitions(
                            inst.goto as InstIdx, qnext, flags);
                    }
                }
            }
        }
        let next = self.cached_state(qnext, flags);
        let cls = self.byte_class(b);
        self.states[si as usize].next[cls] = match next {
            Some(si) => StatePtr::Some(si),
            None => StatePtr::Dead,
        };
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
        &mut self,
        ip: InstIdx,
        q: &mut SparseSet<InstIdx>,
        flags: Flags,
    ) {
        use inst::Inst::*;
        use inst::EmptyLook::*;

        self.stack.push(ip);
        while let Some(ip) = self.stack.pop() {
            if q.contains_sparse_index(ip as usize) {
                continue;
            }
            q.add(ip);
            match self.prog.insts[ip as usize] {
                Char(_) | Ranges(_) => unreachable!(),
                Match | Bytes(_) => {}
                EmptyLook(ref inst) => {
                    match inst.look {
                        WordBoundary | NotWordBoundary => unreachable!(),
                        StartLine if flags.is_start_line() => {
                            self.stack.push(inst.goto as InstIdx);
                        }
                        EndLine if flags.is_end_line() => {
                            self.stack.push(inst.goto as InstIdx);
                        }
                        StartText if flags.is_start() => {
                            self.stack.push(inst.goto as InstIdx);
                        }
                        EndText if flags.is_end() => {
                            self.stack.push(inst.goto as InstIdx);
                        }
                        StartLine | EndLine | StartText | EndText => {}
                    }
                }
                Save(ref inst) => self.stack.push(inst.goto as InstIdx),
                Split(ref inst) => {
                    self.stack.push(inst.goto2 as InstIdx);
                    self.stack.push(inst.goto1 as InstIdx);
                }
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
        let mut inst_flags = Flags::new();
        let mut insts = vec![];
        for &ip in q {
            match self.prog.insts[ip as usize] {
                Char(_) | Ranges(_) => unreachable!(),
                Save(_) => {}
                Split(_) => {}
                Bytes(_) => insts.push(ip),
                EmptyLook(ref inst) => {
                    match inst.look {
                        WordBoundary | NotWordBoundary => unreachable!(),
                        StartLine => {
                            inst_flags.set_start_line(true);
                            insts.push(ip);
                        }
                        EndLine => {
                            inst_flags.set_end_line(true);
                            insts.push(ip);
                        }
                        StartText => {
                            inst_flags.set_start(true);
                            insts.push(ip);
                        }
                        EndText => {
                            inst_flags.set_end(true);
                            insts.push(ip);
                        }
                    }
                }
                Match => {
                    insts.push(ip);
                    // If this is a reverse program, then we want to continue
                    // executing to find the longest possible match. Otherwise,
                    // we only support leftmost-first semantics, so bail out.
                    if !self.prog.insts.is_reversed() {
                        break;
                    }
                }
            }
        }
        if inst_flags.is_empty() {
            // Wipe out the flags from the search text at the current byte,
            // except for whether this is a match state. We can do this
            // because we know the flags will never matter.
            given_flags = *Flags::new().set_match(given_flags.is_match());
        }
        if insts.len() == 0 && given_flags.is_empty() {
            return None;
        }
        let key = StateKey {
            insts: insts.clone(),
            given_flags: given_flags,
        };
        let num_byte_classes = self.num_byte_classes();
        Some(match self.compiled.entry(key) {
            Entry::Occupied(v) => *v.get(),
            Entry::Vacant(v) => {
                let next = vec![StatePtr::Unknown; num_byte_classes];
                self.states.push(State {
                    next: next,
                    insts: insts,
                    given_flags: given_flags,
                    inst_flags: inst_flags,
                });
                *v.insert(usize_to_u32(self.states.len() - 1))
            }
        })
    }

    fn next_state(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        si: StateIdx,
        b: Byte,
    ) -> Option<StateIdx> {
        let cls = self.byte_class(b);
        match self.states[si as usize].next[cls] {
            StatePtr::Some(nsi) => return Some(nsi),
            StatePtr::Dead => return None,
            StatePtr::Unknown => self.exec_byte(qcur, qnext, si, b)
        }
    }

    fn start_state(
        &mut self,
        q: &mut SparseSet<InstIdx>,
        start_flags: Flags,
    ) -> Option<StateIdx> {
        let flagi = start_flags.0 as usize;
        match self.start_states[flagi] {
            StatePtr::Some(si) => return Some(si),
            StatePtr::Dead => return None,
            StatePtr::Unknown => {}
        }
        q.clear();
        self.follow_epsilon_transitions(0, q, start_flags);
        let sp = self.cached_state(q, start_flags);
        self.start_states[flagi] = match sp {
            Some(si) => StatePtr::Some(si),
            None => StatePtr::Dead,
        };
        sp
    }

    fn start_flags(&self, text: &[u8], at: usize) -> Flags {
        let mut flags = Flags::new();
        flags.set_start(at == 0).set_end(text.len() == 0);
        flags.set_start_line(at == 0 || text[at - 1] == b'\n');
        flags.set_end_line(text.len() == 0);
        flags
    }

    fn start_flags_reverse(&self, text: &[u8], at: usize) -> Flags {
        let mut flags = Flags::new();
        flags.set_start(at == text.len()).set_end(text.len() == 0);
        flags.set_start_line(at == text.len() || text[at] == b'\n');
        flags.set_end_line(text.len() == 0);
        flags
    }

    fn prefix_at(&self, text: &[u8], at: usize) -> Option<usize> {
        self.prog.prefixes.find(&text[at..]).map(|(s, _)| at + s)
    }

    fn num_byte_classes(&self) -> usize {
        // We add 1 to account for the special EOF byte.
        (self.prog.insts.byte_classes()[255] + 1) + 1
    }

    fn byte_class(&self, b: Byte) -> usize {
        if b.is_eof() {
            self.num_byte_classes() - 1
        } else {
            self.prog.insts.byte_classes()[b.0 as usize]
        }
    }

    /// Approximate size returns the approximate heap space currently used
    /// by the DFA. It is used to determine whether the DFA's state cache
    /// needs to be flushed or even if the DFA needs to quit altogether.
    /// Namely, it is possible that for certain regexes on certain inputs,
    /// a new state could be created for every byte of input. (This is
    /// horrificly bad.)
    ///
    /// The approximation is guaranteed to be done in constant time (and
    /// indeed, this requirement is why it's approximate).
    fn approximate_size(&self) -> usize {
        use std::mem::size_of as size;
        // Estimate that there are about 32 = 256 / 8 instructions per state.
        // (This is a blatant overestimate.)
        let compiled =
            (self.compiled.len() * (size::<StateKey>() + 128))
            + (self.compiled.len() * size::<StateIdx>());
        let states =
            self.states.len()
            * (size::<State>()
               + 128
               + (self.num_byte_classes() * size::<StatePtr>()));
        let start_states = self.start_states.len() * size::<StatePtr>();
        self.prog.approximate_size() + compiled + states + start_states
    }
}

impl State {
    fn is_match(&self) -> bool {
        self.given_flags.is_match()
    }
}

impl StatePtr {
    fn is_unknown(self) -> bool {
        match self {
            StatePtr::Unknown => true,
            StatePtr::Some(_) | StatePtr::Dead => false,
        }
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
    fn has_non_match_flags(&self) -> bool {
        self.0 & 0b0_1111111 > 0
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

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut next = vec![];
        for (b, next_sp) in self.next.iter().enumerate() {
            match *next_sp {
                StatePtr::Some(si) => {
                    next.push((vb(b as usize), si.to_string()));
                }
                StatePtr::Dead => {
                    next.push((vb(b as usize), "DEAD".to_string()));
                }
                StatePtr::Unknown => {}
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

fn usize_to_u32(n: usize) -> u32 {
    if (n as u64) > (::std::u32::MAX as u64) {
        panic!("BUG: {} is too big to fit into u32", n)
    }
    n as u32
}

#[cfg(test)]
mod tests {
    use exec::{Executor, MatchEngine};
    use program::ProgramBuilder;
    use super::{Dfa, DfaResult};

    #[test]
    fn scratch() {
        // let re = ">.*\n|\n";
        // let text = " > abc\n";

        // let re = "(a|b)*a(a|b){10}";
        // let text = "ababbab";

        let re = ">[^\n]*\n|\n";
        let text = "\
>ONE Homo sapiens alu
GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGA
TCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACT
>TWO IUB ambiguity codes
cttBtatcatatgctaKggNcataaaSatgtaaaDcDRtBggDtctttataattcBgtcg
tactDtDagcctatttSVHtHttKtgtHMaSattgWaHKHttttagacatWatgtRgaaa
NtactMcSMtYtcMgRtacttctWBacgaaatatagScDtttgaagacacatagtVgYgt
>THREE Homo sapiens frequency
tgggcggctcaatcttgcctaatttctactattgtcagctgtacgactgtactaagtgta
tagccccaaataaaagaagtatcgatgcgtctttatgaccaaaggtcttataattgaagc
gcacttccgttcatcaaattaaatcctggcttacccgattctccggaagtctgacctaga
";

        let start = 144;
        let prog = ProgramBuilder::new(re).dfa(true).compile().unwrap();
        let matched = Dfa::exec(&prog, text.as_bytes(), start, false);
        println!("matched? {:?}", matched);

        println!("-------------------");

        if let DfaResult::Match(end) = matched {
            let prog = ProgramBuilder::new(re)
                                      .dfa(true)
                                      .reverse(true)
                                      .compile()
                                      .unwrap();
            let matched = Dfa::exec(
                &prog, &text.as_bytes()[start..], end - start, false);
            let match_start = match matched {
                DfaResult::Match(match_start) => start + match_start,
                s => panic!("{:?}", s),
            };
            println!("reverse matched? {:?}", match_start);
        }

        /*
        println!("####################");
        println!("####################");
        println!("####################");

        let exec = Executor::new(
            re, MatchEngine::Nfa, 10 * (1<<20), false).unwrap();
        let mut caps = [None; 4];
        assert!(exec.exec(&mut caps, text, 0));
        println!("NFA: {:?}", caps);

        let exec = Executor::new(
            re, MatchEngine::Automatic, 10 * (1<<20), false).unwrap();
        let mut caps = [None; 4];
        assert!(exec.exec(&mut caps, text, 0));
        println!("DFA? {:?}", caps);
        */
    }
}
