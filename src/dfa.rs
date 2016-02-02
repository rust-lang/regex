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

use inst::{Insts, Inst, InstIdx};
use program::Program;
use sparse::SparseSet;

/// Return true if and only if the given program can be executed by a DFA.
///
/// Generally, a DFA is possible only when there are no zero-width assertions.
/// This restriction may be relaxed in the future.
pub fn can_exec(insts: &Insts) -> bool {
    // N.B. This will return true on Unicode programs, which isn't technically
    // true. The caller must currently know to translate prog to byte based
    // first.
    for inst in insts {
        if let Inst::EmptyLook(_) = *inst {
            return false;
        }
    }
    true
}

type StateIdx = usize;

#[derive(Debug)]
pub struct DfaCache {
    compiled: HashMap<Vec<InstIdx>, StateIdx>,
    states: Vec<State>,
    qcur: SparseSet<InstIdx>,
    qnext: SparseSet<InstIdx>,
}

#[derive(Debug)]
pub struct Dfa<'r, 'c, 's> {
    prog: &'r Program,
    compiled: &'c mut HashMap<Vec<InstIdx>, StateIdx>,
    states: &'s mut Vec<State>,
}

struct State {
    insts: Vec<InstIdx>,
    next: [Option<StateIdx>; 256],
    is_match: bool,
}

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
            self.nfa_to_dfa(0, qcur);
            match self.cached_state(qcur) {
                None => return None,
                Some(si) => si,
            }
        };
        let mut last_match = None;
        if self.states[si].is_match {
            last_match = Some(at);
        }
        for (i, &b) in text[at..].iter().enumerate() {
            si = if let Some(next_si) = self.states[si].next[b as usize] {
                next_si
            } else if let Some(next_si) = self.exec_byte(qcur, qnext, si, b) {
                next_si
            } else {
                break;
            };
            if self.states[si].is_match {
                last_match = Some(at + i + 1);
            }
        }
        last_match
    }

    fn exec_byte(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        si: StateIdx,
        b: u8,
    ) -> Option<StateIdx> {
        use inst::Inst::*;

        self.states[si].add_to(qcur);
        qnext.clear();
        for &ip in &*qcur {
            match self.prog.insts[ip] {
                EmptyLook(_) | Char(_) | Ranges(_) => unreachable!(),
                Match | Save(_) | Split(_) => {}
                Bytes(ref inst) => {
                    if inst.matches(b) {
                        self.nfa_to_dfa(inst.goto, qnext);
                    }
                }
            }
        }
        let next = self.cached_state(qnext);
        self.states[si].next[b as usize] = next;
        next
    }

    fn nfa_to_dfa(&self, ip: InstIdx, q: &mut SparseSet<InstIdx>) {
        use inst::Inst::*;

        if q.contains_sparse_index(ip) {
            return;
        }
        // TODO(ag): use an explicit stack.
        q.add(ip);
        match self.prog.insts[ip] {
            EmptyLook(_) | Char(_) | Ranges(_) => unreachable!(),
            Match | Bytes(_) => {}
            Save(ref inst) => self.nfa_to_dfa(inst.goto, q),
            Split(ref inst) => {
                self.nfa_to_dfa(inst.goto1, q);
                self.nfa_to_dfa(inst.goto2, q);
            }
        }
    }

    fn cached_state(&mut self, q: &SparseSet<InstIdx>) -> Option<StateIdx> {
        use std::collections::hash_map::Entry;
        use inst::Inst::*;

        let mut insts = vec![];
        let mut is_match = false;
        for &ip in q {
            match self.prog.insts[ip] {
                EmptyLook(_) | Char(_) | Ranges(_) => unreachable!(),
                Save(_) | Split(_) => {}
                Bytes(_) => insts.push(ip),
                Match => {
                    is_match = true;
                    insts.push(ip);
                    break;
                }
            }
        }
        if insts.len() == 0 {
            return None;
        }
        Some(match self.compiled.entry(insts.clone()) {
            Entry::Occupied(v) => *v.get(),
            Entry::Vacant(v) => {
                self.states.push(State {
                    insts: insts,
                    next: [None; 256],
                    is_match: is_match,
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
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut next = vec![];
        for (b, next_si) in self.next.iter().enumerate() {
            if let Some(si) = *next_si {
                next.push((vb(b as u8), si));
            }
        }
        f.debug_struct("State")
         .field("is_match", &self.is_match)
         .field("insts", &self.insts)
         .field("next", &next)
         .finish()
    }
}

fn vb(b: u8) -> String {
    use std::ascii::escape_default;
    let escaped = escape_default(b).collect::<Vec<u8>>();
    String::from_utf8_lossy(&escaped).into_owned()
}

#[cfg(test)]
mod tests {
    use program::ProgramBuilder;
    use super::Dfa;

    #[test]
    fn scratch() {
        // let re = ">.*\n|\n";
        // let text = " > abc\n";

        // let re = "(a|b)*a(a|b){10}";
        // let text = "ababbababababbababbabbbbbbbbbbbbbbaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbaaaaaaaaabababbabababbababababbbbbbababab";

        let re = r"\pL";
        let text = format!("{}a", ::std::iter::repeat("☃5☃5").take(20).collect::<String>());

        let prog = ProgramBuilder::new(re).dfa(true).compile().unwrap();
        let matched = Dfa::exec(&prog, text.as_bytes(), 0);
        println!("matched? {:?}", matched);
    }
}
