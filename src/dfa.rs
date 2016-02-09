// Copyright 2014-2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!
The DFA matching engine.

A DFA provides faster matching because the engine is in exactly one state at
any point in time. In the NFA, there may be multiple active states, and
considerable CPU cycles is spent shuffling them around. In finite automata
speak, the DFA follows epsilon transitions in the regex far less than the NFA.

A DFA is a classic trade off between time and space. The NFA is slower, but
its memory requirements are typically small and predictable. The DFA is faster,
but given the right regex and the right input, the number of states in the
DFA can grow exponentially. To mitigate this space problem, we do two things:

1. We implement an *online* DFA. That is, the DFA is constructed from the NFA
   during a search. When a new state is computed, it is stored in a cache so
   that it may be reused. An important consequence of this implementation
   is that states that are never reached for a particular input are never
   computed. (This is impossible in an "offline" DFA which needs to compute
   all possible states up front.)
2. If the cache gets too big, we wipe it and start again.

In pathological cases, a new state can be created for every byte of input.
(e.g., The regex `(a|b)*a(a|b){20}` on a long sequence of a's and b's.)
In this case, performance regresses to slightly slower than the full NFA
simulation. (We could investigate quitting the DFA and falling back to the
NFA.)

N.B. While this implementation is heavily commented, Russ Cox's series of
articles on regexes is strongly recommended: https://swtch.com/~rsc/regexp/
(As is the DFA implementation in RE2, which heavily influenced this
implementation.)
*/

use std::collections::HashMap;
use std::fmt;
use std::mem;

use inst::{Insts, Inst};
use program::Program;
use sparse::SparseSet;

/// The cache limit specifies approximately how much space we're willing to
/// give to the state cache. Once the state cache exceeds the size, it is wiped
/// and all states must be re-computed.
///
/// Note that this value does not impact correctness. It can be set to 0 and
/// the DFA will run just fine. (It will only ever store exactly one state
/// in the cache, and will likely run very slowly, but it will work.)
///
/// Also note that this limit is *per thread of execution*. That is, if the
/// same regex is used to search text across multiple threads simultaneously,
/// then the DFA cache is not shared. Instead, copies are made.
///
/// TODO(burntsushi): This feels like a knob that a caller ought to be able to
/// configure.
const CACHE_LIMIT: usize = 2 * (1<<50);

/// Return true if and only if the given program can be executed by a DFA.
///
/// Generally, a DFA is possible only when there are no word boundary
/// assertions. This is due to the difficulty (but likely not impossibility)
/// of tracking multi-byte assertions in the DFA.
pub fn can_exec(insts: &Insts) -> bool {
    use inst::EmptyLook::*;
    // If for some reason we manage to allocate a regex program with more
    // than 2^32-1 instructions, then we can't execute the DFA because we
    // use 32 bit pointers.
    if insts.len() > ::std::u32::MAX as usize {
        return false;
    }
    for inst in insts {
        match *inst {
            Inst::Char(_) | Inst::Ranges(_) => return false,
            Inst::EmptyLook(ref inst) => {
                match inst.look {
                    WordBoundary | NotWordBoundary => return false,
                    StartLine | EndLine | StartText | EndText => {}
                }
            }
            Inst::Match | Inst::Save(_) | Inst::Split(_) | Inst::Bytes(_) => {}
        }
    }
    true
}

/// The result of running the DFA.
///
/// Conceptually, this is essentially equivalent to an `Option<usize>`, where
/// the value indicates where the end of a match was found, if any. We split
/// this out into a third state called EarlyMatch, which indicates both that
/// the caller specified that they didn't care about *where* a match was found,
/// and that the position at which the earliest match occurred may not be the
/// correct leftmost-first ending match position.
///
/// NoMatch indicates that no match will ever be found and that processing can
/// quit immediately.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum DfaResult {
    Match(usize),
    EarlyMatch,
    NoMatch,
}

/// A reusable cache of DFA states.
///
/// This cache is reused between multiple invocations of the same regex
/// program. (It is not shared simultaneously between threads. If there is
/// contention, then new caches are created.)
#[derive(Debug)]
pub struct DfaCache {
    /// A cache of pre-compiled DFA states, keyed by the set of NFA states
    /// and the set of empty-width flags set at the byte in the input when the
    /// state was observed.
    ///
    /// A StatePtr is effectively a `*State`, but to avoid reference counting,
    /// we just pass indexes around manually. The performance impact of this
    /// isn't particularly clear, but this does enable us to use 32 bit
    /// indexes, which can significantly impact the number of states we can
    /// cram into our memory bounds.
    compiled: HashMap<StateKey, StatePtr>,
    /// Our heap of states. Both `DfaCache.compiled` and `State.next` point
    /// into this vec.
    states: Vec<State>,
    /// A set of cached start states, which are limited to the number of
    /// permutations of flags set just before the initial byte of input. (The
    /// index into this vec is a `Flags`.)
    ///
    /// N.B. A start state can be "dead" (i.e., no possible match), so we
    /// represent it with a StatePtr.
    start_states: Vec<StatePtr>,
    /// Stack scratch space used to follow epsilon transitions in the NFA.
    /// (This permits us to avoid recursion.)
    ///
    /// The maximum stack size is the number of NFA states.
    stack: Vec<InstIdx>,
    /// qcur and qnext are ordered sets with constant time
    /// addition/membership/clearing-whole-set and linear time iteration. They
    /// are used to manage the sets of NFA states in DFA states when computing
    /// cached DFA states. In particular, the order of the NFA states matters
    /// for leftmost-first style matching. Namely, when computing a cached
    /// state, the set of NFA states stops growing as soon as the first Match
    /// instruction is observed.
    qcur: SparseSet<InstIdx>,
    qnext: SparseSet<InstIdx>,
}

/// Dfa encapsulates the actual execution of the DFA.
///
/// Most of the fields in this struct are mutable pointers to locations in the
/// DFA cache.
///
/// Why don't we just store the DfaCache directly in this struct? Well, it
/// turns out that we often want to "iterate over {qcur,qnext} and perform
/// some possibly mutable operation." The borrow checker forbids this, even
/// if the callee doesn't mutate {qcur,qnext}. Therefore, we stick most of
/// DfaCache into Dfa directly as pointers and pass {qcur,qnext} around as
/// parameters.
#[derive(Debug)]
pub struct Dfa<'r, 'c, 's, 'ss, 't> {
    /// prog contains the NFA instruction opcodes. DFA execution uses either
    /// the `dfa` instructions or the `dfa_reverse` instructions. (It never
    /// uses `prog.prog`, which may have Unicode opcodes that cannot be
    /// executed by this DFA.)
    prog: &'r Program,
    /// The state state. We record it here because the pointer may change
    /// when the cache is wiped.
    start: StatePtr,
    /// When set, we can stop searching immediately after we enter a match
    /// state. (Normally we keep searching in order to provide leftmost-first
    /// semantics.)
    quit_on_first_match: bool,
    /// These are all from DfaCache. (Only {qcur,qnext} are missing.)
    compiled: &'c mut HashMap<StateKey, StatePtr>,
    states: &'s mut Vec<State>,
    start_states: &'ss mut Vec<StatePtr>,
    stack: &'t mut Vec<InstIdx>,
}

/// State is a DFA state. It contains transitions to next states (given an
/// input byte), an ordered set of NFA states (not necessarily complete) and
/// the set of empty-width flags set in the input when this state was created.
/// (It also contains the set of empty-width flags in all of its constituent
/// NFA states, which is useful for determining where and when to try and
/// follow epsilon transitions.)
#[derive(Clone)]
struct State {
    /// The set of transitions out of this state to other states.
    ///
    /// This is tricky and is NOT a simple vec with length 256. A vec with
    /// length 256 would work fine if not for the following:
    ///
    /// 1. Empty assertions can lead to matches "at the boundary" of the input,
    ///    so it is useful to have one extra transition that corresponds to
    ///    EOF. So why doesn't a vec with length 257 work?
    /// 2. If the vec has length 257 and each StatePtr is 4 bytes (even on 64
    ///    bit), then every state occupies at least 1KB on the heap. That's
    ///    ridiculous. As an optimization, we compute the set of all
    ///    equivalence classes of bytes in the regex. Each equivalence class
    ///    is defined to be the set of bytes that are indistinguishable when
    ///    searching for a match. For example, in the regex `[a-z]`, the byte
    ///    ranges `0..ord(a)-1`, `ord(a)-ord(z)` and `ord(z)+1..257` all
    ///    correspond to distinct classes. Therefore, we only need a vec of
    ///    length *3* for that particular regex, which is quite a bit better.
    next: Vec<StatePtr>,
    /// The set of NFA states in this DFA state, which are computed by
    /// following epsilon transitions from `insts[0]`. Note that not all
    /// epsilon transitions are necessarily followed! Namely, epsilon
    /// transitions that correspond to empty assertions are only followed if
    /// the flags set at the current byte satisfy the assertion.
    insts: Vec<InstIdx>,
    /// The set of flags (i.e., assertions) set immediately after the current
    /// byte in the input.
    ///
    /// Note that if inst_flags is empty, then given_flags is also empty (sans
    /// the match flag). This is because given_flags participate in this
    /// state's cache key, which influences the number of states created. If
    /// these flags are never discriminatory (which is the case if there are
    /// no assertions in this state's NFA states), then there's no sense in
    /// keeping them around.
    ///
    /// given_flags may have the match flag set, which indicates that this
    /// state is a match state.
    given_flags: Flags,
    /// The set of flags implied by the NFA states in `insts`.
    ///
    /// This never has the match flag set.
    inst_flags: Flags,
}

/// A state's key for identifying it in the cache. In particular, if two
/// state's cache keys are equivalent, then they cannot be discriminatory in
/// a match.
///
/// We capture two bits of information: an ordered set of NFA states for the
/// DFA state and the set of empty flags set immediately proceding the input
/// byte at which the corresponding state was created.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct StateKey {
    /// An ordered set of NFA states.
    insts: Vec<InstIdx>,
    /// A set of empty flags, which may also contain a match flag.
    given_flags: Flags,
}

/// InstIdx is a 32 bit pointer into a sequence of opcodes (i.e., it indexes
/// an NFA state).
///
/// Throughout this library, this is usually set to `usize`, but we force a
/// `u32` here to save on space.
type InstIdx = u32;

/// StatePtr is a 32 bit pointer into a sequence of states.
///
/// It has two special values: STATE_UNKNOWN and STATE_DEAD. All other values
/// are valid indexes into a cache's state sequence.
///
/// (We use sentinel values here to save on space. A more sensible
/// representation is a sum type.)
type StatePtr = u32;

/// An unknown state means that the state has not been computed yet, and that
/// the only way to progress is to compute it.
const STATE_UNKNOWN: StatePtr = 0;

/// A dead state means that the state has been computed and it is known that
/// once it is entered, no match can ever occur.
const STATE_DEAD: StatePtr = 1;

/// Byte is a u8 in spirit, but a u16 in practice so that we can represent the
/// special EOF sentinel value.
#[derive(Copy, Clone, Debug)]
struct Byte(u16);

/// A set of flags. All flags correspond to empty assertions (either in the
/// input or in the opcodes) except for one: the match flag.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Flags(u8);

impl DfaCache {
    /// Create new empty cache for the DFA engine.
    pub fn new() -> Self {
        DfaCache {
            compiled: HashMap::new(),
            states: vec![State::marker(), State::marker()],
            start_states: vec![STATE_UNKNOWN; 256],
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
        at: usize,
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
            start: 0, // filled in below
            stack: &mut cache.stack,
            quit_on_first_match: quit_on_first_match,
        };
        let r = if prog.insts.is_reversed() {
            let start_flags = dfa.start_flags_reverse(text, at);
            dfa.start = match dfa.start_state(&mut cache.qcur, start_flags) {
                None => return DfaResult::NoMatch,
                Some(si) => si,
            };
            dfa.exec_at_reverse(&mut cache.qcur, &mut cache.qnext, text, at)
        } else {
            let start_flags = dfa.start_flags(text, at);
            dfa.start = match dfa.start_state(&mut cache.qcur, start_flags) {
                None => return DfaResult::NoMatch,
                Some(si) => si,
            };
            dfa.exec_at(&mut cache.qcur, &mut cache.qnext, text, at)
        };
        // println!("{:?}", prog.insts);
        // println!("############################");
        // for (si, state) in dfa.states.iter().enumerate() {
            // println!("{:04} {:?}", si, state);
        // }
        // println!("size: {} bytes, states: {}",
                 // dfa.approximate_size(), dfa.states.len());
        // println!("############################");
        r
    }

    fn exec_at(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        text: &[u8],
        mut at: usize,
    ) -> DfaResult {
        let mut last_match = DfaResult::NoMatch;
        if self.states[self.start as usize].is_match() {
            if self.quit_on_first_match {
                return DfaResult::EarlyMatch;
            }
            last_match = DfaResult::Match(at);
        }
        let (mut si, mut i) = (self.start, at);
        while i < text.len() {
            if !self.prog.prefixes.is_empty() && si == self.start {
                i = match self.prefix_at(text, i) {
                    None => return DfaResult::NoMatch,
                    Some(i) => i,
                };
            }

            // Inline next_state to avoid an extra branch.
            let cls = self.prog.insts.byte_classes()[text[i] as usize];
            si = match self.states[si as usize].next[cls] {
                STATE_UNKNOWN => {
                    let b = Byte::input_byte(text[i]);
                    match self.exec_byte(qcur, qnext, si, b) {
                        Some(nsi) => nsi,
                        None => return last_match,
                    }
                }
                STATE_DEAD => return last_match,
                nsi => nsi,
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
        let mut last_match = DfaResult::NoMatch;
        if self.states[self.start as usize].is_match() {
            if self.quit_on_first_match {
                return DfaResult::EarlyMatch;
            }
            last_match = DfaResult::Match(at);
        }
        let (mut si, mut i) = (self.start, at);
        while i > 0 {
            i -= 1;

            // Inline next_state to avoid an extra branch.
            let cls = self.prog.insts.byte_classes()[text[i] as usize];
            si = match self.states[si as usize].next[cls] {
                STATE_UNKNOWN => {
                    let b = Byte::input_byte(text[i]);
                    match self.exec_byte(qcur, qnext, si, b) {
                        Some(nsi) => nsi,
                        None => return last_match,
                    }
                }
                STATE_DEAD => return last_match,
                nsi => nsi,
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
        mut si: StatePtr,
        b: Byte,
    ) -> Option<StatePtr> {
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
        let next = self.cached_state(qnext, flags, Some(&mut si));
        let cls = self.byte_class(b);
        self.states[si as usize].next[cls] = match next {
            Some(si) => si,
            None => STATE_DEAD,
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
    ///
    /// If the cache is full, then it is wiped before caching a new state.
    ///
    /// The current state should be specified if it exists, since it will need
    /// to be preserved if the cache clears itself. (Start states are
    /// always saved.) It takes a mutable pointer to the index because if
    /// the cache is cleared, the state's location may change.
    fn cached_state(
        &mut self,
        q: &SparseSet<InstIdx>,
        mut given_flags: Flags,
        current_state: Option<&mut StatePtr>,
    ) -> Option<StatePtr> {
        let (key, inst_flags) = match self.cached_state_key(q, given_flags) {
            None => return None,
            Some(v) => v,
        };
        if let Some(&si) = self.compiled.get(&key) {
            return Some(si);
        }

        // If the cache has gotten too big, wipe it.
        if self.approximate_size() > CACHE_LIMIT {
            self.clear_cache(current_state);
        }

        // OK, now there's enough room to push our new state.
        let num_byte_classes = self.num_byte_classes();
        let next = vec![STATE_UNKNOWN; num_byte_classes];
        self.states.push(State {
            next: next,
            insts: key.insts.clone(),
            given_flags: given_flags,
            inst_flags: inst_flags,
        });
        let si = usize_to_u32(self.states.len().checked_sub(1).unwrap());
        self.compiled.insert(key, si);
        Some(si)
    }

    fn clear_cache(&mut self, current_state: Option<&mut StatePtr>) {
        if self.states.len() <= 2 {
            // Why <= 2? Well, the states list always has its first two
            // positions filled by marker states for STATE_UNKNOWN and
            // STATE_DEAD. These states aren't actually used, but exist to
            // make sure no other state lives in those locations. Therefore,
            // a state vec with length <= 2 is actually "empty."
            return;
        }
        if let Some(si) = current_state {
            let cur = self.copy_state(*si);
            let start = self.copy_state(self.start);
            self.states.clear();
            self.compiled.clear();
            for start in self.start_states.iter_mut() {
                *start = STATE_UNKNOWN;
            }
            self.states.push(State::marker());
            self.states.push(State::marker());
            self.start = self.restore_state(start);
            *si = self.restore_state(cur);
        } else {
            let start = self.copy_state(self.start);
            self.states.clear();
            self.compiled.clear();
            for start in self.start_states.iter_mut() {
                *start = STATE_UNKNOWN;
            }
            self.states.push(State::marker());
            self.states.push(State::marker());
            self.start = self.restore_state(start);
        }
    }

    fn copy_state(&self, si: StatePtr) -> State {
        let mut state = self.states[si as usize].clone();
        // Make sure to erase any pointers from this state, so that
        // they are forced to be re-computed.
        state.next = vec![STATE_UNKNOWN; self.num_byte_classes()];
        state
    }

    fn restore_state(&mut self, state: State) -> StatePtr {
        let key = StateKey {
            insts: state.insts.clone(),
            given_flags: state.given_flags,
        };
        if let Some(&si) = self.compiled.get(&key) {
            return si;
        }
        let si = usize_to_u32(self.states.len());
        self.states.push(state);
        self.compiled.insert(key, si);
        si
    }

    fn cached_state_key(
        &mut self,
        q: &SparseSet<InstIdx>,
        mut given_flags: Flags,
    ) -> Option<(StateKey, Flags)> {
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
            None
        } else {
            let key = StateKey { insts: insts, given_flags: given_flags };
            Some((key, inst_flags))
        }
    }

    fn next_state(
        &mut self,
        qcur: &mut SparseSet<InstIdx>,
        qnext: &mut SparseSet<InstIdx>,
        si: StatePtr,
        b: Byte,
    ) -> Option<StatePtr> {
        let cls = self.byte_class(b);
        match self.states[si as usize].next[cls] {
            STATE_UNKNOWN => self.exec_byte(qcur, qnext, si, b),
            STATE_DEAD => return None,
            nsi => return Some(nsi),
        }
    }

    fn start_state(
        &mut self,
        q: &mut SparseSet<InstIdx>,
        start_flags: Flags,
    ) -> Option<StatePtr> {
        let flagi = start_flags.0 as usize;
        match self.start_states[flagi] {
            STATE_UNKNOWN => {}
            STATE_DEAD => return None,
            si => return Some(si),
        }
        q.clear();
        self.follow_epsilon_transitions(0, q, start_flags);
        let sp = self.cached_state(q, start_flags, None);
        self.start_states[flagi] = match sp {
            Some(si) => si,
            None => STATE_DEAD,
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
            + (self.compiled.len() * size::<StatePtr>());
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
    fn marker() -> State {
        State {
            next: vec![],
            insts: vec![],
            given_flags: Flags::new(),
            inst_flags: Flags::new(),
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
                STATE_UNKNOWN => {}
                STATE_DEAD => next.push((vb(b as usize), "DEAD".to_string())),
                si => next.push((vb(b as usize), si.to_string())),
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
        let text = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGA\n";
        // let text = "\
// >ONE Homo sapiens alu
// GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGA
// TCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACT
// >TWO IUB ambiguity codes
// cttBtatcatatgctaKggNcataaaSatgtaaaDcDRtBggDtctttataattcBgtcg
// tactDtDagcctatttSVHtHttKtgtHMaSattgWaHKHttttagacatWatgtRgaaa
// NtactMcSMtYtcMgRtacttctWBacgaaatatagScDtttgaagacacatagtVgYgt
// >THREE Homo sapiens frequency
// tgggcggctcaatcttgcctaatttctactattgtcagctgtacgactgtactaagtgta
// tagccccaaataaaagaagtatcgatgcgtctttatgaccaaaggtcttataattgaagc
// gcacttccgttcatcaaattaaatcctggcttacccgattctccggaagtctgacctaga
// ";

        let start = 0;
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
