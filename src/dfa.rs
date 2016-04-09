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
considerable CPU cycles are spent shuffling them around. In finite automata
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
2. If the cache gets too big, we wipe it and continue matching.

In pathological cases, a new state can be created for every byte of input.
(e.g., The regex `(a|b)*a(a|b){20}` on a long sequence of a's and b's.)
In this case, performance regresses to slightly slower than the full NFA
simulation, in large part because the cache becomes useless. (We could
investigate quitting the DFA and falling back to the NFA.)

N.B. While this implementation is heavily commented, Russ Cox's series of
articles on regexes is strongly recommended: https://swtch.com/~rsc/regexp/
(As is the DFA implementation in RE2, which heavily influenced this
implementation.)
*/

use std::collections::HashMap;
use std::fmt;
use std::mem;

use exec::ProgramCache;
use prog::{Inst, Program};
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
const CACHE_LIMIT: usize = 2 * (1<<20);

/// Return true if and only if the given program can be executed by a DFA.
///
/// Generally, a DFA is possible only when there are no word boundary
/// assertions. This is due to the difficulty (but likely not impossibility)
/// of tracking multi-byte assertions in the DFA.
pub fn can_exec(insts: &Program) -> bool {
    use prog::Inst::*;
    use prog::EmptyLook::*;
    // If for some reason we manage to allocate a regex program with more
    // than 2^32-1 instructions, then we can't execute the DFA because we
    // use 32 bit pointers.
    if insts.len() > ::std::u32::MAX as usize {
        return false;
    }
    for inst in insts {
        match *inst {
            Char(_) | Ranges(_) => return false,
            EmptyLook(ref inst) => {
                match inst.look {
                    WordBoundary | NotWordBoundary => return false,
                    WordBoundaryAscii | NotWordBoundaryAscii => {}
                    StartLine | EndLine | StartText | EndText => {}
                }
            }
            Match(_) | Save(_) | Split(_) | Bytes(_) => {}
        }
    }
    true
}

/// A reusable cache of DFA states.
///
/// This cache is reused between multiple invocations of the same regex
/// program. (It is not shared simultaneously between threads. If there is
/// contention, then new caches are created.)
#[derive(Clone, Debug)]
pub struct Cache {
    /// Group persistent DFA related cache state together. The sparse sets
    /// listed below are used as scratch space while computing uncached states.
    inner: CacheInner,
    /// qcur and qnext are ordered sets with constant time
    /// addition/membership/clearing-whole-set and linear time iteration. They
    /// are used to manage the sets of NFA states in DFA states when computing
    /// cached DFA states. In particular, the order of the NFA states matters
    /// for leftmost-first style matching. Namely, when computing a cached
    /// state, the set of NFA states stops growing as soon as the first Match
    /// instruction is observed.
    qcur: SparseSet,
    qnext: SparseSet,
}

#[derive(Clone, Debug)]
/// CacheInner is logically just a part of Cache, but groups together fields
/// that aren't passed as function parameters throughout search. (This split
/// is mostly an artifact of the borrow checker. It is happily paid.)
struct CacheInner {
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
    /// Our heap of states. Both `CacheInner.compiled` and `State.next` point
    /// into this vec.
    states: Vec<State>,
    /// A set of cached start states, which are limited to the number of
    /// permutations of flags set just before the initial byte of input. (The
    /// index into this vec is a `EmptyFlags`.)
    ///
    /// N.B. A start state can be "dead" (i.e., no possible match), so we
    /// represent it with a StatePtr.
    start_states: Vec<StatePtr>,
    /// Stack scratch space used to follow epsilon transitions in the NFA.
    /// (This permits us to avoid recursion.)
    ///
    /// The maximum stack size is the number of NFA states.
    stack: Vec<InstPtr>,
    /// The total number of times this cache has been flushed by the DFA
    /// because of space constraints.
    flush_count: u64,
}

/// Fsm encapsulates the actual execution of the DFA.
///
/// Most of the fields in this struct are mutable pointers to locations in the
/// DFA cache.
///
/// Why don't we just store the Cache directly in this struct? Well, it
/// turns out that we often want to "iterate over {qcur,qnext} and perform
/// some possibly mutable operation." The borrow checker forbids this, even
/// if the callee doesn't mutate {qcur,qnext}. Therefore, we stick most of
/// Cache into Fsm directly as pointers and pass {qcur,qnext} around as
/// parameters.
///
/// N.B. We only use a single lifetime here since all pointers are taken
/// from the same cache.
#[derive(Debug)]
pub struct Fsm<'a> {
    /// prog contains the NFA instruction opcodes. DFA execution uses either
    /// the `dfa` instructions or the `dfa_reverse` instructions from
    /// `exec::Executor`. (It never uses `Executor.prog`, which may have
    /// Unicode opcodes that cannot be executed by this DFA.)
    prog: &'a Program,
    /// The start state. We record it here because the pointer may change
    /// when the cache is wiped.
    start: StatePtr,
    /// The current position in the input.
    at: usize,
    /// Should we quit after seeing the first match?
    quit_after_match: bool,
    /// The last state that matched.
    ///
    /// When no match has occurred, this is set to STATE_UNKNOWN.
    last_match_si: StatePtr,
    /// The input position of the last cache flush. We use this to determine
    /// if we're thrashing in the cache too often. If so, the DFA quits so
    /// that we can fall back to the NFA algorithm.
    last_cache_flush: usize,
    /// All cached DFA information that is persisted between searches.
    cache: &'a mut CacheInner,
}

/// The result of running the DFA.
///
/// Generally, the result is either a match or not a match, but sometimes the
/// DFA runs too slow because the cache size is too small. In that case, it
/// gives up with the intent of falling back to the NFA algorithm.
#[derive(Clone, Debug)]
pub enum Result<T> {
    Match(T),
    NoMatch,
    Quit,
}

impl<T> Result<T> {
    pub fn is_match(&self) -> bool {
        match *self {
            Result::Match(_) => true,
            Result::NoMatch | Result::Quit => false,
        }
    }
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
    ///    (Equivalence classes are computed during compilation.)
    next: Box<[StatePtr]>,
    /// The set of NFA states in this DFA state, which are computed by
    /// following epsilon transitions from `insts[0]`. Note that not all
    /// epsilon transitions are necessarily followed! Namely, epsilon
    /// transitions that correspond to empty assertions are only followed if
    /// the flags set at the current byte satisfy the assertion.
    insts: Box<[InstPtr]>,
    /// Flags for this state, which indicate whether it is a match state,
    /// observed an ASCII word byte and whether any instruction in `insts`
    /// is a zero-width assertion.
    flags: StateFlags,
}

/// A state's key for identifying it in the cache. In particular, if two
/// state's cache keys are equivalent, then they cannot be discriminatory in
/// a match.
///
/// We capture two bits of information: an ordered set of NFA states for the
/// DFA state and whether it corresponds to a match state.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct StateKey {
    /// An ordered set of NFA states.
    insts: Box<[InstPtr]>,
    /// The state's flags.
    flags: StateFlags,
}

/// InstPtr is a 32 bit pointer into a sequence of opcodes (i.e., it indexes
/// an NFA state).
///
/// Throughout this library, this is usually set to `usize`, but we force a
/// `u32` here to save on space.
type InstPtr = u32;

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

/// A set of flags for zero-width assertions.
#[derive(Clone, Copy, Eq, Debug, Default, Hash, PartialEq)]
struct EmptyFlags {
    start: bool,
    end: bool,
    start_line: bool,
    end_line: bool,
    word_boundary: bool,
    not_word_boundary: bool,
}

/// A set of flags describing various configurations of a DFA state. This is
/// represented by a `u8` so that it is compact.
#[derive(Clone, Copy, Eq, Default, Hash, PartialEq)]
struct StateFlags(u8);

impl Cache {
    /// Create new empty cache for the DFA engine.
    pub fn new(prog: &Program) -> Self {
        Cache {
            inner: CacheInner {
                compiled: HashMap::new(),
                states: vec![State::invalid(), State::invalid()],
                start_states: vec![STATE_UNKNOWN; 256],
                stack: vec![],
                flush_count: 0,
            },
            qcur: SparseSet::new(prog.insts.len()),
            qnext: SparseSet::new(prog.insts.len()),
        }
    }
}

impl<'a> Fsm<'a> {
    #[inline(always)] // reduces constant overhead
    pub fn forward(
        prog: &'a Program,
        cache: &ProgramCache,
        quit_after_match: bool,
        text: &[u8],
        at: usize,
    ) -> Result<usize> {
        let mut cache = cache.borrow_mut();
        let mut cache = &mut cache.dfa;
        let mut dfa = Fsm {
            prog: prog,
            start: 0, // filled in below
            at: at,
            quit_after_match: quit_after_match,
            last_match_si: STATE_UNKNOWN,
            last_cache_flush: at,
            cache: &mut cache.inner,
        };
        let (empty_flags, state_flags) = dfa.start_flags(text, at);
        dfa.start = match dfa.start_state(
            &mut cache.qcur,
            empty_flags,
            state_flags,
        ) {
            None => return Result::Quit,
            Some(STATE_DEAD) => return Result::NoMatch,
            Some(si) => si,
        };
        debug_assert!(dfa.start != STATE_UNKNOWN);
        dfa.exec_at(&mut cache.qcur, &mut cache.qnext, text)
    }

    #[inline(always)] // reduces constant overhead
    pub fn reverse(
        prog: &'a Program,
        cache: &ProgramCache,
        quit_after_match: bool,
        text: &[u8],
        at: usize,
    ) -> Result<usize> {
        let mut cache = cache.borrow_mut();
        let mut cache = &mut cache.dfa_reverse;
        let mut dfa = Fsm {
            prog: prog,
            start: 0, // filled in below
            at: at,
            quit_after_match: quit_after_match,
            last_match_si: STATE_UNKNOWN,
            last_cache_flush: at,
            cache: &mut cache.inner,
        };
        let (empty_flags, state_flags) = dfa.start_flags_reverse(text, at);
        dfa.start = match dfa.start_state(
            &mut cache.qcur,
            empty_flags,
            state_flags,
        ) {
            None => return Result::Quit,
            Some(STATE_DEAD) => return Result::NoMatch,
            Some(si) => si,
        };
        debug_assert!(dfa.start != STATE_UNKNOWN);
        dfa.exec_at_reverse(&mut cache.qcur, &mut cache.qnext, text)
    }

    #[inline(always)] // reduces constant overhead
    pub fn forward_many(
        prog: &'a Program,
        cache: &ProgramCache,
        matches: &mut [bool],
        text: &[u8],
        at: usize,
    ) -> Result<usize> {
        debug_assert!(matches.len() == prog.matches.len());
        let mut cache = cache.borrow_mut();
        let mut cache = &mut cache.dfa;
        let mut dfa = Fsm {
            prog: prog,
            start: 0, // filled in below
            at: at,
            quit_after_match: false,
            last_match_si: STATE_UNKNOWN,
            last_cache_flush: at,
            cache: &mut cache.inner,
        };
        let (empty_flags, state_flags) = dfa.start_flags(text, at);
        dfa.start = match dfa.start_state(
            &mut cache.qcur,
            empty_flags,
            state_flags,
        ) {
            None => return Result::Quit,
            Some(STATE_DEAD) => return Result::NoMatch,
            Some(si) => si,
        };
        debug_assert!(dfa.start != STATE_UNKNOWN);
        let result = dfa.exec_at(&mut cache.qcur, &mut cache.qnext, text);
        if result.is_match() {
            if matches.len() == 1 {
                matches[0] = true;
            } else {
                debug_assert!(dfa.last_match_si != STATE_UNKNOWN);
                debug_assert!(dfa.last_match_si != STATE_DEAD);
                for &ip in dfa.cache.state(dfa.last_match_si).insts.iter() {
                    if let Inst::Match(slot) = dfa.prog[ip as usize] {
                        matches[slot] = true;
                    }
                }
            }
        }
        result
    }

    /// Executes the DFA on a forward NFA.
    ///
    /// {qcur,qnext} are scratch ordered sets which may be non-empty.
    #[inline(always)] // reduces constant overhead
    fn exec_at(
        &mut self,
        qcur: &mut SparseSet,
        qnext: &mut SparseSet,
        text: &[u8],
    ) -> Result<usize> {
        // For the most part, the DFA is basically:
        //
        //   last_match = null
        //   while current_byte != EOF:
        //     si = current_state.next[current_byte]
        //     if si is match
        //       last_match = si
        //   return last_match
        //
        // However, we need to deal with a few things:
        //
        //   1. This is an *online* DFA, so the current state's next list
        //      may not point to anywhere yet, so we must go out and compute
        //      them. (They are then cached into the current state's next list
        //      to avoid re-computation.)
        //   2. If we come across a state that is known to be dead (i.e., never
        //      leads to a match), then we can quit early.
        //   3. If the caller just wants to know if a match occurs, then we
        //      can quit as soon as we know we have a match. (Full leftmost
        //      first semantics require continuing on.)
        //   4. If we're in the start state, then we can use a pre-computed set
        //      of prefix literals to skip quickly along the input.
        //   5. After the input is exhausted, we run the DFA on one symbol
        //      that stands for EOF. This is useful for handling empty width
        //      assertions.
        //   6. We can't actually do state.next[byte]. Instead, we have to do
        //      state.next[byte_classes[byte]], which permits us to keep the
        //      'next' list very small.
        debug_assert!(!self.prog.is_reverse);

        // The last match is the currently known ending match position. It is
        // reported as an index to the most recent byte that resulted in a
        // transition to a match state and is always stored in capture slot `1`
        // when searching forwards. Its maximum value is `text.len()`,
        // which can only happen after the special EOF sentinel value is fed
        // to the DFA.
        let mut result = Result::NoMatch;
        let mut si = self.start;
        let mut next_si;
        let mut b: u8;
        while self.at < text.len() {
            // Our set of literal prefixes can itself be a DFA, but it is
            // offline and can generally be quite a bit faster. (For instance,
            // memchr is used if possible.)
            if !self.prog.prefixes.is_empty()
                && si == self.start
                && !self.prog.is_anchored_start {
                self.at = match self.prefix_at(text, self.at) {
                    None => return Result::NoMatch,
                    Some(i) => i,
                };
            }

            b = text[self.at];
            next_si = {
                let s = self.cache.state(si);
                if s.flags.is_match() {
                    result = Result::Match(self.at - 1);
                    if self.quit_after_match {
                        return result;
                    }
                    self.last_match_si = si;
                }
                s.next[self.u8_class(b)]
            };
            if next_si <= STATE_DEAD {
                // The next state may not have been cached, so re-compute it
                // (i.e., follow epsilon transitions).
                let byte = Byte::byte(b);
                next_si = match self.next_state(qcur, qnext, si, byte) {
                    None => return Result::Quit,
                    Some(STATE_DEAD) => return result,
                    Some(si) => si,
                };
                debug_assert!(next_si != STATE_UNKNOWN);
            }
            si = next_si;
            self.at += 1;
        }
        if self.cache.state(si).flags.is_match() {
            result = Result::Match(self.at - 1);
            if self.quit_after_match {
                return result;
            }
            self.last_match_si = si;
        }

        // Run the DFA once more on the special EOF senitnel value.
        si = match self.next_state(qcur, qnext, si, Byte::eof()) {
            None => return Result::Quit,
            Some(STATE_DEAD) => return result,
            Some(si) => si,
        };
        debug_assert!(si != STATE_UNKNOWN);
        if self.cache.state(si).flags.is_match() {
            self.last_match_si = si;
            result = Result::Match(text.len());
        }
        result
    }

    /// Executes the DFA on a reverse NFA.
    #[inline(always)] // reduces constant overhead
    fn exec_at_reverse(
        &mut self,
        qcur: &mut SparseSet,
        qnext: &mut SparseSet,
        text: &[u8],
    ) -> Result<usize> {
        // The comments in `exec_at` above mostly apply here too. The main
        // difference is that we move backwards over the input and we look for
        // the longest possible match instead of the leftmost-first match.
        //
        // N.B. The code duplication here is regrettable. Efforts to improve
        // it without sacrificing performance are welcome. ---AG
        debug_assert!(self.prog.is_reverse);
        let mut result = Result::NoMatch;
        let mut si = self.start;
        let mut next_si;
        let mut b: u8;
        while self.at > 0 {
            self.at -= 1;
            b = text[self.at];

            next_si = {
                let s = self.cache.state(si);
                if s.flags.is_match() {
                    result = Result::Match(self.at + 2);
                    if self.quit_after_match {
                        return result;
                    }
                    self.last_match_si = si;
                }
                s.next[self.u8_class(b)]
            };
            if next_si <= STATE_DEAD {
                // The next state may not have been cached, so re-compute it
                // (i.e., follow epsilon transitions).
                let byte = Byte::byte(b);
                next_si = match self.next_state(qcur, qnext, si, byte) {
                    None => return Result::Quit,
                    Some(STATE_DEAD) => return result,
                    Some(si) => si,
                };
                debug_assert!(next_si != STATE_UNKNOWN);
            }
            si = next_si;
        }
        if self.cache.state(si).flags.is_match() {
            result = Result::Match(self.at + 1);
            if self.quit_after_match {
                return result;
            }
            self.last_match_si = si;
        }

        // Run the DFA once more on the special EOF senitnel value.
        si = match self.next_state(qcur, qnext, si, Byte::eof()) {
            None => return Result::Quit,
            Some(STATE_DEAD) => return result,
            Some(si) => si,
        };
        debug_assert!(si != STATE_UNKNOWN);
        if self.cache.state(si).flags.is_match() {
            self.last_match_si = si;
            result = Result::Match(0);
        }
        result
    }

    /// Computes the next state given the current state and the current input
    /// byte (which may be EOF).
    ///
    /// If STATE_DEAD is returned, then there is no valid state transition.
    /// This implies that no permutation of future input can lead to a match
    /// state.
    ///
    /// STATE_UNKNOWN can never be returned.
    fn exec_byte(
        &mut self,
        qcur: &mut SparseSet,
        qnext: &mut SparseSet,
        mut si: StatePtr,
        b: Byte,
    ) -> Option<StatePtr> {
        use prog::Inst::*;

        // Initialize a queue with the current DFA state's NFA states.
        qcur.clear();
        for &ip in self.cache.state(si).insts.iter() {
            qcur.add(ip as usize);
        }

        // Before inspecting the current byte, we may need to also inspect
        // whether the position immediately preceding the current byte
        // satisfies the empty assertions found in the current state.
        //
        // We only need to do this step if there are any empty assertions in
        // the current state.
        let is_word_last = self.cache.state(si).flags.is_word();
        let is_word = b.is_ascii_word();
        if self.cache.state(si).flags.has_empty() {
            // Compute the flags immediately preceding the current byte.
            // This means we only care about the "end" or "end line" flags.
            // (The "start" flags are computed immediately proceding the
            // current byte and is handled below.)
            let mut flags = EmptyFlags::default();
            if b.is_eof() {
                flags.end = true;
                flags.end_line = true;
            } else if b.as_byte().map_or(false, |b| b == b'\n') {
                flags.end_line = true;
            }
            if is_word_last == is_word {
                flags.not_word_boundary = true;
            } else {
                flags.word_boundary = true;
            }
            // Now follow epsilon transitions from every NFA state, but make
            // sure we only follow transitions that satisfy our flags.
            qnext.clear();
            for &ip in &*qcur {
                self.follow_epsilons(usize_to_u32(ip), qnext, flags);
            }
            mem::swap(qcur, qnext);
        }

        // Now we set flags for immediately after the current byte. Since start
        // states are processed separately, and are the only states that can
        // have the StartText flag set, we therefore only need to worry about
        // the StartLine flag here.
        //
        // We do also keep track of whether this DFA state contains a NFA state
        // that is a matching state. This is precisely how we delay the DFA
        // matching by one byte in order to process the special EOF sentinel
        // byte. Namely, if this DFA state containing a matching NFA state,
        // then it is the *next* DFA state that is marked as a match.
        let mut empty_flags = EmptyFlags::default();
        let mut state_flags = StateFlags::default();
        empty_flags.start_line = b.as_byte().map_or(false, |b| b == b'\n');
        if b.is_ascii_word() {
            state_flags.set_word();
        }
        // Now follow all epsilon transitions again, but only after consuming
        // the current byte.
        qnext.clear();
        for &ip in &*qcur {
            match self.prog[ip as usize] {
                // These states never happen in a byte-based program.
                Char(_) | Ranges(_) => unreachable!(),
                // These states are handled when following epsilon transitions.
                Save(_) | Split(_) | EmptyLook(_) => {}
                Match(_) => {
                    state_flags.set_match();
                    if !self.continue_past_first_match() {
                        break;
                    } else if self.prog.matches.len() > 1
                            && !qnext.contains_ip(ip as usize) {
                        // If we are continuing on to find other matches,
                        // then keep a record of the match states we've seen.
                        qnext.add(ip);
                    }
                }
                Bytes(ref inst) => {
                    if b.as_byte().map_or(false, |b| inst.matches(b)) {
                        self.follow_epsilons(
                            inst.goto as InstPtr, qnext, empty_flags);
                    }
                }
            }
        }
        let mut cache = true;
        if b.is_eof() && self.prog.matches.len() > 1 {
            // If we're processing the last byte of the input and we're
            // matching a regex set, then make the next state contain the
            // previous states transitions. We do this so that the main
            // matching loop can extract all of the match instructions.
            mem::swap(qcur, qnext);
            // And don't cache this state because it's totally bunk.
            cache = false;
        }
        // We've now built up the set of NFA states that ought to comprise the
        // next DFA state, so try to find it in the cache, and if it doesn't
        // exist, cache it.
        //
        // N.B. We pass `&mut si` here because the cache may clear itself if
        // it has gotten too full. When that happens, the location of the
        // current state may change.
        let next = match self.cached_state(qnext, state_flags, Some(&mut si)) {
            None => return None,
            Some(next) => next,
        };
        debug_assert!(next != STATE_UNKNOWN);
        // And now store our state in the current state's next list.
        let cls = self.byte_class(b);
        if cache {
            self.cache.state_mut(si).next[cls] = next;
        }
        Some(next)
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
    /// be set in this case. (Even if the proceding byte is a `\n`, it will
    /// be handled in a subsequent DFA state.)
    fn follow_epsilons(
        &mut self,
        ip: InstPtr,
        q: &mut SparseSet,
        flags: EmptyFlags,
    ) {
        use prog::Inst::*;
        use prog::EmptyLook::*;

        // We need to traverse the NFA to follow epsilon transitions, so avoid
        // recursion with an explicit stack.
        self.cache.stack.push(ip);
        while let Some(ip) = self.cache.stack.pop() {
            // Don't visit states we've already added.
            if q.contains_ip(ip as usize) {
                continue;
            }
            q.add(ip as usize);
            match self.prog[ip as usize] {
                Char(_) | Ranges(_) => unreachable!(),
                Match(_) | Bytes(_) => {}
                EmptyLook(ref inst) => {
                    // Only follow empty assertion states if our flags satisfy
                    // the assertion.
                    match inst.look {
                        StartLine if flags.start_line => {
                            self.cache.stack.push(inst.goto as InstPtr);
                        }
                        EndLine if flags.end_line => {
                            self.cache.stack.push(inst.goto as InstPtr);
                        }
                        StartText if flags.start => {
                            self.cache.stack.push(inst.goto as InstPtr);
                        }
                        EndText if flags.end => {
                            self.cache.stack.push(inst.goto as InstPtr);
                        }
                        WordBoundaryAscii if flags.word_boundary => {
                            self.cache.stack.push(inst.goto as InstPtr);
                        }
                        NotWordBoundaryAscii if flags.not_word_boundary => {
                            self.cache.stack.push(inst.goto as InstPtr);
                        }
                        StartLine | EndLine | StartText | EndText => {}
                        WordBoundaryAscii | NotWordBoundaryAscii => {}
                        // The DFA doesn't support Unicode word boundaries. :-(
                        WordBoundary | NotWordBoundary => unreachable!(),
                    }
                }
                Save(ref inst) => self.cache.stack.push(inst.goto as InstPtr),
                Split(ref inst) => {
                    self.cache.stack.push(inst.goto2 as InstPtr);
                    self.cache.stack.push(inst.goto1 as InstPtr);
                }
            }
        }
    }

    /// Find a previously computed state matching the given set of instructions
    /// and is_match bool.
    ///
    /// The given set of instructions should represent a single state in the
    /// NFA along with all states reachable without consuming any input.
    ///
    /// The is_match bool should be true if and only if the preceding DFA state
    /// contains an NFA matching state. The cached state produced here will
    /// then signify a match. (This enables us to delay a match by one byte,
    /// in order to account for the EOF sentinel byte.)
    ///
    /// If the cache is full, then it is wiped before caching a new state.
    ///
    /// The current state should be specified if it exists, since it will need
    /// to be preserved if the cache clears itself. (Start states are
    /// always saved, so they should not be passed here.) It takes a mutable
    /// pointer to the index because if the cache is cleared, the state's
    /// location may change.
    fn cached_state(
        &mut self,
        q: &SparseSet,
        mut state_flags: StateFlags,
        current_state: Option<&mut StatePtr>,
    ) -> Option<StatePtr> {
        // If we couldn't come up with a non-empty key to represent this state,
        // then it is dead and can never lead to a match.
        //
        // Note that inst_flags represent the set of empty width assertions
        // in q. We use this as an optimization in exec_byte to determine when
        // we should follow epsilon transitions at the empty string preceding
        // the current byte.
        let key = match self.cached_state_key(q, &mut state_flags) {
            None => return Some(STATE_DEAD),
            Some(v) => v,
        };
        // In the cache? Cool. Done.
        if let Some(&si) = self.cache.compiled.get(&key) {
            return Some(si);
        }

        // If the cache has gotten too big, wipe it.
        if self.approximate_size() > CACHE_LIMIT {
            if !self.clear_cache_and_save(current_state) {
                // Ooops. DFA is giving up.
                return None;
            }
        }

        // OK, now there's enough room to push our new state.
        // We do this even if the cache size is set to 0!
        let next = vec![STATE_UNKNOWN; self.num_byte_classes()];
        self.cache.states.push(State {
            next: next.into_boxed_slice(),
            insts: key.insts.clone(),
            flags: state_flags,
        });
        let si = usize_to_u32(self.cache.states.len().checked_sub(1).unwrap());
        self.cache.compiled.insert(key, si);
        Some(si)
    }

    /// Produces a key suitable for describing a state in the DFA cache.
    ///
    /// The key invariant here is that equivalent keys are produced for any two
    /// sets of ordered NFA states (and toggling of whether the previous NFA
    /// states contain a match state) that do not discriminate a match for any
    /// input.
    ///
    /// Specifically, q should be an ordered set of NFA states and is_match
    /// should be true if and only if the previous NFA states contained a match
    /// state.
    fn cached_state_key(
        &mut self,
        q: &SparseSet,
        state_flags: &mut StateFlags,
    ) -> Option<StateKey> {
        use prog::Inst::*;
        use prog::EmptyLook::*;

        // We need to build up enough information to recognize pre-built states
        // in the DFA. Generally speaking, this includes every instruction
        // except for those which are purely epsilon transitions, e.g., the
        // Save and Split instructions.
        //
        // Empty width assertions are also epsilon transitions, but since they
        // are conditional, we need to make them part of a state's key in the
        // cache.
        let mut insts = vec![];
        for &ip in q {
            let ip = usize_to_u32(ip);
            match self.prog[ip as usize] {
                Char(_) | Ranges(_) => unreachable!(),
                Save(_) => {}
                Split(_) => {}
                Bytes(_) => insts.push(ip),
                EmptyLook(ref inst) => {
                    match inst.look {
                        StartLine => {
                            state_flags.set_empty();
                            insts.push(ip);
                        }
                        EndLine => {
                            state_flags.set_empty();
                            insts.push(ip);
                        }
                        StartText => {
                            state_flags.set_empty();
                            insts.push(ip);
                        }
                        EndText => {
                            state_flags.set_empty();
                            insts.push(ip);
                        }
                        WordBoundaryAscii => {
                            state_flags.set_empty();
                            insts.push(ip);
                        }
                        NotWordBoundaryAscii => {
                            state_flags.set_empty();
                            insts.push(ip);
                        }
                        WordBoundary | NotWordBoundary => unreachable!(),
                    }
                }
                Match(_) => {
                    insts.push(ip);
                    if !self.continue_past_first_match() {
                        break;
                    }
                }
            }
        }
        // If we couldn't transition to any other instructions and we didn't
        // see a match when expanding NFA states previously, then this is a
        // dead state and no amount of additional input can transition out
        // of this state.
        if insts.len() == 0 && !state_flags.is_match() {
            None
        } else {
            Some(StateKey {
                insts: insts.into_boxed_slice(),
                flags: *state_flags,
            })
        }
    }

    /// Clears the cache, but saves and restores current_state if it is not
    /// none.
    ///
    /// The current state must be provided here in case its location in the
    /// cache changes.
    ///
    /// This returns false if the cache is not cleared and the DFA should
    /// give up.
    fn clear_cache_and_save(
        &mut self,
        current_state: Option<&mut StatePtr>,
    ) -> bool {
        if self.cache.states.len() <= 2 {
            // Why <= 2? Well, the states list always has its first two
            // positions filled by marker states for STATE_UNKNOWN and
            // STATE_DEAD. These states aren't actually used, but exist to
            // make sure no other state lives in those locations. Therefore,
            // a state vec with length <= 2 is actually "empty."
            return true;
        }
        match current_state {
            None => self.clear_cache(),
            Some(si) => {
                let cur = self.copy_state(*si);
                if !self.clear_cache() {
                    return false;
                }
                *si = self.restore_state(cur);
                true
            }
        }
    }

    /// Wipes the state cache, but saves and restores the current start state.
    ///
    /// This returns false if the cache is not cleared and the DFA should
    /// give up.
    fn clear_cache(&mut self) -> bool {
        // Bail out of the DFA if we're moving too "slowly."
        // A heuristic from RE2: assume the DFA is too slow if it is processing
        // 10 or fewer bytes per state.
        // Additionally, we permit the cache to be flushed a few times before
        // caling it quits.
        let nstates = self.cache.states.len();
        if self.cache.flush_count >= 3
            && self.at >= self.last_cache_flush
            && (self.at - self.last_cache_flush) <= 10 * nstates {
            return false;
        }
        // Update statistics tracking cache flushes.
        self.last_cache_flush = self.at;
        self.cache.flush_count += 1;

        // OK, actually flush the cache.
        let start = self.copy_state(self.start);
        self.cache.states.clear();
        self.cache.compiled.clear();
        for start in self.cache.start_states.iter_mut() {
            *start = STATE_UNKNOWN;
        }
        self.cache.states.push(State::invalid());
        self.cache.states.push(State::invalid());
        self.start = self.restore_state(start);
        true
    }

    /// Returns a fresh copy of state si with all of its next pointers set to
    /// unknown.
    fn copy_state(&self, si: StatePtr) -> State {
        let mut state = self.cache.state(si).clone();
        // Make sure to erase any pointers from this state, so that
        // they are forced to be re-computed.
        let unknowns = vec![STATE_UNKNOWN; self.num_byte_classes()];
        state.next = unknowns.into_boxed_slice();
        state
    }

    /// Restores the given state back into the cache, and returns a pointer
    /// to it.
    fn restore_state(&mut self, state: State) -> StatePtr {
        let key = StateKey {
            insts: state.insts.clone(),
            flags: state.flags,
        };
        if let Some(&si) = self.cache.compiled.get(&key) {
            return si;
        }
        let si = usize_to_u32(self.cache.states.len());
        self.cache.states.push(state);
        self.cache.compiled.insert(key, si);
        si
    }

    /// Returns the next state given the current state si and current byte
    /// b. {qcur,qnext} are used as scratch space for storing ordered NFA
    /// states.
    ///
    /// This tries to fetch the next state from the cache, but if that fails,
    /// it computes the next state, caches it and returns a pointer to it.
    ///
    /// The pointer can be to a real state, or it can be STATE_DEAD.
    /// STATE_UNKNOWN cannot be returned.
    ///
    /// None is returned if a new state could not be allocated (i.e., the DFA
    /// ran out of space and thinks it's running too slowly).
    fn next_state(
        &mut self,
        qcur: &mut SparseSet,
        qnext: &mut SparseSet,
        si: StatePtr,
        b: Byte,
    ) -> Option<StatePtr> {
        if si == STATE_DEAD {
            return Some(STATE_DEAD);
        }
        let cls = self.byte_class(b);
        match self.cache.state(si).next[cls] {
            STATE_UNKNOWN => self.exec_byte(qcur, qnext, si, b),
            STATE_DEAD => return Some(STATE_DEAD),
            nsi => return Some(nsi),
        }
    }

    /// Computes and returns the start state, where searching begins at
    /// position `at` in `text`. If the state has already been computed,
    /// then it is pulled from the cache. If the state hasn't been cached,
    /// then it is computed, cached and a pointer to it is returned.
    ///
    /// This may return STATE_DEAD but never STATE_UNKNOWN.
    #[inline(always)] // reduces constant overhead
    fn start_state(
        &mut self,
        q: &mut SparseSet,
        empty_flags: EmptyFlags,
        state_flags: StateFlags,
    ) -> Option<StatePtr> {
        let flagi = empty_flags.as_index();
        match self.cache.start_states[flagi] {
            STATE_UNKNOWN => {}
            STATE_DEAD => return Some(STATE_DEAD),
            si => return Some(si),
        }
        q.clear();
        let start = usize_to_u32(self.prog.start);
        self.follow_epsilons(start, q, empty_flags);
        // Start states can never be match states because we delay every match
        // by one byte. Given an empty string and an empty match, the match
        // won't actually occur until the DFA processes the special EOF
        // sentinel byte.
        let sp = match self.cached_state(q, state_flags, None) {
            None => return None,
            Some(sp) => sp,
        };
        self.cache.start_states[flagi] = sp;
        Some(sp)
    }

    /// Computes the set of starting flags for the given position in text.
    ///
    /// This should only be used when executing the DFA forwards over the
    /// input.
    fn start_flags(&self, text: &[u8], at: usize) -> (EmptyFlags, StateFlags) {
        let mut empty_flags = EmptyFlags::default();
        let mut state_flags = StateFlags::default();
        empty_flags.start = at == 0;
        empty_flags.end = text.len() == 0;
        empty_flags.start_line = at == 0 || text[at - 1] == b'\n';
        empty_flags.end_line = text.len() == 0;
        if at > 0 && Byte::byte(text[at - 1]).is_ascii_word() {
            state_flags.set_word();
        }
        (empty_flags, state_flags)
    }

    /// Computes the set of starting flags for the given position in text.
    ///
    /// This should only be used when executing the DFA in reverse over the
    /// input.
    fn start_flags_reverse(
        &self,
        text: &[u8],
        at: usize,
    ) -> (EmptyFlags, StateFlags) {
        let mut empty_flags = EmptyFlags::default();
        let mut state_flags = StateFlags::default();
        empty_flags.start = at == text.len();
        empty_flags.end = text.len() == 0;
        empty_flags.start_line = at == text.len() || text[at] == b'\n';
        empty_flags.end_line = text.len() == 0;
        if at < text.len() && Byte::byte(text[at]).is_ascii_word() {
            state_flags.set_word();
        }
        (empty_flags, state_flags)
    }

    /// Quickly finds the next occurrence of any literal prefixes in the regex.
    /// If there are no literal prefixes, then the current position is
    /// returned. If there are literal prefixes and one could not be found,
    /// then None is returned.
    ///
    /// This should only be called when the DFA is in a start state.
    fn prefix_at(&self, text: &[u8], at: usize) -> Option<usize> {
        self.prog.prefixes.find(&text[at..]).map(|(s, _)| at + s)
    }

    /// Returns the number of byte classes required to discriminate transitions
    /// in each state.
    ///
    /// invariant: num_byte_classes() == len(State.next)
    fn num_byte_classes(&self) -> usize {
        // We add 1 to account for the special EOF byte.
        (self.prog.byte_classes[255] as usize + 1) + 1
    }

    /// Given an input byte or the special EOF sentinel, return its
    /// corresponding byte class.
    fn byte_class(&self, b: Byte) -> usize {
        if b.is_eof() {
            self.num_byte_classes() - 1
        } else {
            self.prog.byte_classes[b.0 as usize] as usize
        }
    }

    /// Like byte_class, but explicitly for u8s.
    fn u8_class(&self, b: u8) -> usize {
        self.prog.byte_classes[b as usize] as usize
    }

    /// Returns true if the DFA should continue searching past the first match.
    ///
    /// Leftmost first semantics in the DFA are preserved by not following NFA
    /// transitions after the first match is seen.
    ///
    /// On occasion, we want to avoid leftmost first semantics to find either
    /// the longest match (for reverse search) or all possible matches (for
    /// regex sets).
    fn continue_past_first_match(&self) -> bool {
        self.prog.is_reverse || self.prog.matches.len() > 1
    }

    /// Approximate size returns the approximate heap space currently used by
    /// the DFA. It is used to determine whether the DFA's state cache needs to
    /// be wiped. Namely, it is possible that for certain regexes on certain
    /// inputs, a new state could be created for every byte of input. (This is
    /// bad for memory use, so we bound it with a cache.)
    ///
    /// The approximation is guaranteed to be done in constant time (and
    /// indeed, this requirement is why it's approximate).
    fn approximate_size(&self) -> usize {
        use std::mem::size_of as size;
        // Estimate that there are about 16 instructions per state consuming
        // 64 = 16 * 4 bytes of space.
        let compiled =
            (self.cache.compiled.len() * (size::<StateKey>() + 64))
            + (self.cache.compiled.len() * size::<StatePtr>());
        let states =
            self.cache.states.len()
            * (size::<State>()
               + 64
               + (self.num_byte_classes() * size::<StatePtr>()));
        let start_states = self.cache.start_states.len() * size::<StatePtr>();
        self.prog.approximate_size() + compiled + states + start_states
    }

    /// Returns the actual heap space of the DFA. This visits every state in
    /// the DFA.
    #[allow(dead_code)] // useful for debugging
    fn actual_size(&self) -> usize {
        let mut compiled = 0;
        for k in self.cache.compiled.keys() {
            compiled += mem::size_of::<StateKey>();
            compiled += mem::size_of::<StatePtr>();
            compiled += k.insts.len() * mem::size_of::<InstPtr>();
        }
        let mut states = 0;
        for s in &self.cache.states {
            states += mem::size_of::<State>();
            states += s.next.len() * mem::size_of::<StatePtr>();
            states += s.insts.len() * mem::size_of::<InstPtr>();
        }
        compiled
        + states
        + (self.cache.start_states.len() * mem::size_of::<StatePtr>())
        + (self.cache.stack.len() * mem::size_of::<InstPtr>())
        + mem::size_of::<Fsm>()
        + mem::size_of::<CacheInner>()
        + self.prog.approximate_size() // OK, not actual, but close enough
    }
}

impl State {
    /// Return an invalid state. This is only used to "pad" the state cache so
    /// that the special sentinel values (STATE_UNKNOWN and STATE_DEAD) are
    /// never used.
    fn invalid() -> State {
        State {
            next: vec![].into_boxed_slice(),
            insts: vec![].into_boxed_slice(),
            flags: StateFlags::default(),
        }
    }
}

impl CacheInner {
    /// Get a state at the given pointer.
    fn state(&self, si: StatePtr) -> &State {
        &self.states[si as usize]
    }

    /// Get a mutable state at the given pointer.
    fn state_mut(&mut self, si: StatePtr) -> &mut State {
        &mut self.states[si as usize]
    }
}

impl EmptyFlags {
    fn as_index(&self) -> usize {
        (((self.start as u8) << 0) |
         ((self.end as u8) << 1) |
         ((self.start_line as u8) << 2) |
         ((self.end_line as u8) << 3) |
         ((self.word_boundary as u8) << 4) |
         ((self.not_word_boundary as u8) << 5))
        as usize
    }
}

impl StateFlags {
    fn is_match(&self) -> bool {
        self.0 & 0b0000000_1 > 0
    }

    fn set_match(&mut self) {
        self.0 |= 0b0000000_1;
    }

    fn is_word(&self) -> bool {
        self.0 & 0b000000_1_0 > 0
    }

    fn set_word(&mut self) {
        self.0 |= 0b000000_1_0;
    }

    fn has_empty(&self) -> bool {
        self.0 & 0b00000_1_00 > 0
    }

    fn set_empty(&mut self) {
        self.0 |= 0b00000_1_00;
    }
}

impl Byte {
    fn byte(b: u8) -> Self { Byte(b as u16) }
    fn eof() -> Self { Byte(256) }
    fn is_eof(&self) -> bool { self.0 == 256 }

    fn is_ascii_word(&self) -> bool {
        let b = match self.as_byte() {
            None => return false,
            Some(b) => b,
        };
        match b {
            b'A'...b'Z' | b'a'...b'z' | b'0'...b'9' | b'_' => true,
            _ => false,
        }
    }

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
         .field("flags", &self.flags)
         .field("insts", &self.insts)
         .field("next", &next)
         .finish()
    }
}

impl fmt::Debug for StateFlags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("StateFlags")
         .field("is_match", &self.is_match())
         .field("is_word", &self.is_word())
         .field("has_empty", &self.has_empty())
         .finish()
    }
}

/// Helper function for formatting a byte as a nice-to-read escaped string.
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
