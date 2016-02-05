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
        // if let Inst::EmptyLook(_) = *inst {
            // return false;
        // }
        if let Inst::EmptyLook(ref inst) = *inst {
            match inst.look {
                WordBoundary | NotWordBoundary => return false,
                StartLine | EndLine => return false,
                _ => {}
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
    inst_flags: Flags,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct StateKey {
    insts: Vec<InstIdx>,
    given_flags: Flags,
    inst_flags: Flags,
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
        let r = dfa.exec_at(&mut cache.qcur, &mut cache.qnext, text, start);
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
        // println!("{:?}", self.prog.insts);
        let mut si = if !self.states.is_empty() {
            0
        } else {
            let flags = *Flags::new().set_start(at == 0)
                                     .set_end(text.len() == 0);
            self.nfa_to_dfa(0, qcur, flags);
            match self.cached_state(qcur, flags) {
                None => return None,
                Some(si) => si,
            }
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

    fn exec_byte(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        si: StateIdx,
        b: Byte,
    ) -> Option<StateIdx> {
        use inst::Inst::*;

        // println!("BYTE: {:?}, STATE: {:?}", b.0, si);

        let mut flag_before = Flags::new();
        let mut flag_after = Flags::new();

        flag_before.set_end(b.is_eof());

        self.states[si].add_to(qcur);
        qnext.clear();
        for &ip in &*qcur {
            self.nfa_to_dfa(ip, qnext, flag_before);
        }
        // println!("qcur: {:?}, qnext: {:?}", qcur, qnext);
        mem::swap(qcur, qnext);
        qnext.clear();
        for &ip in &*qcur {
            match self.prog.insts[ip] {
                Char(_) | Ranges(_) => unreachable!(),
                Save(_) | Split(_) | EmptyLook(_) => {}
                Match => {
                    flag_after.set_match(true);
                }
                Bytes(ref inst) => {
                    if b.as_byte().map_or(false, |b| inst.matches(b)) {
                        self.nfa_to_dfa(inst.goto, qnext, flag_after);
                    }
                }
            }
        }
        let next = self.cached_state(qnext, flag_after);
        // println!("next from cache: {:?}", next);
        self.states[si].next[b.0 as usize] = next;
        next
    }

    fn nfa_to_dfa(
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
                    StartLine | EndLine => unreachable!(),
                    StartText => {
                        if flags.is_start() {
                            self.nfa_to_dfa(inst.goto, q, flags);
                        }
                    }
                    EndText => {
                        if flags.is_end() {
                            self.nfa_to_dfa(inst.goto, q, flags);
                        }
                    }
                }
            }
            Save(ref inst) => self.nfa_to_dfa(inst.goto, q, flags),
            Split(ref inst) => {
                self.nfa_to_dfa(inst.goto1, q, flags);
                self.nfa_to_dfa(inst.goto2, q, flags);
            }
        }
    }

    fn cached_state(
        &mut self,
        q: &SparseSet<InstIdx>,
        mut given_flags: Flags,
    ) -> Option<StateIdx> {
        use std::collections::hash_map::Entry;
        use inst::Inst::*;
        use inst::EmptyLook::*;

        let mut insts = vec![];
        let mut inst_flags = Flags::new();
        for &ip in q {
            match self.prog.insts[ip] {
                Char(_) | Ranges(_) => unreachable!(),
                Save(_) => {}
                Split(_) => {}
                Bytes(_) => insts.push(ip),
                EmptyLook(ref inst) => {
                    match inst.look {
                        WordBoundary | NotWordBoundary => unreachable!(),
                        StartLine | EndLine => unreachable!(),
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
                    inst_flags.set_match(true);
                    insts.push(ip);
                    break;
                }
            }
        }
        if inst_flags.is_empty() {
            let is_match = given_flags.is_match();
            given_flags.clear();
            given_flags.set_match(is_match);
        }
        if insts.len() == 0 && given_flags.is_empty() {
            return None;
        }
        let key = StateKey {
            insts: insts.clone(),
            given_flags: given_flags,
            inst_flags: inst_flags,
        };
        Some(match self.compiled.entry(key) {
            Entry::Occupied(v) => *v.get(),
            Entry::Vacant(v) => {
                self.states.push(State {
                    next: [None; 257],
                    insts: insts,
                    given_flags: given_flags,
                    inst_flags: inst_flags,
                });
                *v.insert(self.states.len() - 1)
            }
        })
    }
}

impl State {
    fn add_to(&self, q: &mut SparseSet<InstIdx>) {
        q.clear();
        for &ip in &self.insts {
            q.add(ip);
        }
    }

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
         .field("inst_flags", &self.inst_flags)
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
    use program::ProgramBuilder;
    use super::Dfa;

    #[test]
    fn scratch() {
        // let re = ">.*\n|\n";
        // let text = " > abc\n";

        let re = "(^|a)+";
        let text = "a";

        // let re = "(a|b)*a(a|b){10}";
        // let text = "ababbababababbababbabbbbbbbbbbbbbbaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbaaaaaaaaabababbabababbababababbbbbbababab";

        // let re = r"\pL";
        // let text = format!("{}a", ::std::iter::repeat("☃5☃5").take(20).collect::<String>());

        let prog = ProgramBuilder::new(re).bytes(true).compile().unwrap();
        let matched = Dfa::exec(&prog, text.as_bytes(), 0);
        println!("matched? {:?}", matched);

        let re = ::Regex::new("(^|a)+").unwrap();
        re.is_match("a");
        println!("Regex::new: {:?}", re.find("a").unwrap());
    }
}
