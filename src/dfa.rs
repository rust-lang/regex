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

use exec::Search;
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
                    // We have a hope of supporting at least these some day.
                    WordBoundaryAscii | NotWordBoundaryAscii => return false,
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
    stack: Vec<InstPtr>,
    /// The total number of times this cache has been flushed by the DFA
    /// because of space constraints.
    cache_flush_count: u64,
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
///
/// N.B. We only use a single lifetime here since all pointers are taken
/// from the same cache.
#[derive(Debug)]
pub struct Dfa<'a, 'b, 'c: 'b, 'm: 'b> {
    /// prog contains the NFA instruction opcodes. DFA execution uses either
    /// the `dfa` instructions or the `dfa_reverse` instructions from
    /// `exec::Executor`. (It never uses `Executor.prog`, which may have
    /// Unicode opcodes that cannot be executed by this DFA.)
    prog: &'a Program,
    /// The start state. We record it here because the pointer may change
    /// when the cache is wiped.
    start: StatePtr,
    /// The search configuration, which includes capture groups. It also
    /// includes space for indicating which regex matched if executing a
    /// regex set.
    search: &'b mut Search<'c, 'm>,
    /// The current position in the input.
    at: usize,
    /// The last state that matched.
    ///
    /// When no match has occurred, this is set to STATE_UNKNOWN.
    last_match_si: StatePtr,
    /// The input position of the last cache flush. We use this to determine
    /// if we're thrashing in the cache too often. If so, the DFA quits so
    /// that we can fall back to the NFA algorithm.
    last_cache_flush: usize,
    /// These are all from DfaCache. (Only {qcur,qnext} are missing.)
    compiled: &'a mut HashMap<StateKey, StatePtr>,
    states: &'a mut Vec<State>,
    start_states: &'a mut Vec<StatePtr>,
    stack: &'a mut Vec<InstPtr>,
    cache_flush_count: &'a mut u64,
}

/// The result of running the DFA.
///
/// Generally, the result is either a match or not a match, but sometimes the
/// DFA runs too slow because the cache size is too small. In that case, it
/// gives up with the intent of falling back to the NFA algorithm.
pub enum DfaResult {
    Match,
    NoMatch,
    Quit,
}

impl DfaResult {
    pub fn is_match(&self) -> bool {
        match *self {
            DfaResult::Match => true,
            DfaResult::NoMatch | DfaResult::Quit => false,
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
    next: Vec<StatePtr>,
    /// The set of NFA states in this DFA state, which are computed by
    /// following epsilon transitions from `insts[0]`. Note that not all
    /// epsilon transitions are necessarily followed! Namely, epsilon
    /// transitions that correspond to empty assertions are only followed if
    /// the flags set at the current byte satisfy the assertion.
    insts: Vec<InstPtr>,
    /// Whether this is a match state or not.
    is_match: bool,
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
/// DFA state and whether it corresponds to a match state.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct StateKey {
    /// An ordered set of NFA states.
    insts: Vec<InstPtr>,
    /// Whether this is a matching state or not.
    is_match: bool,
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

/// A set of flags. All flags correspond to empty assertions (either in the
/// input or in the opcodes) except for one: the match flag.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Flags(u8);

impl DfaCache {
    /// Create new empty cache for the DFA engine.
    pub fn new() -> Self {
        DfaCache {
            compiled: HashMap::new(),
            states: vec![State::invalid(), State::invalid()],
            start_states: vec![STATE_UNKNOWN; 256],
            stack: vec![],
            cache_flush_count: 0,
            qcur: SparseSet::new(0),
            qnext: SparseSet::new(0),
        }
    }

    /// Resizes ensures that the cache is the right size for the given program.
    ///
    /// N.B. This exists because it is inconvenient (i.e., my failing) to tie
    /// the initial creation of the cache with knowledge about the program, so
    /// we resize it once.
    fn resize(&mut self, num_insts: usize) {
        if num_insts == self.qcur.capacity() {
            return;
        }
        self.qcur = SparseSet::new(num_insts);
        self.qnext = SparseSet::new(num_insts);
    }
}

impl<'a, 'b, 'c, 'm> Dfa<'a, 'b, 'c, 'm> {
    /// The main entry point to executing a DFA, which returns the *end* of
    /// a match if one exists, using Perl's "leftmost-first" semantics.
    ///
    /// text can safely be arbitrary bytes, and the location returned is still
    /// guaranteed to correspond to a valid UTF-8 sequence boundary. (Note
    /// though that whether arbitrary bytes are actually consumed depends on
    /// the given program.)
    ///
    /// at is the position in text at which to start looking for a match. While
    /// it may seem like we should omit `at` and just rely on the caller to
    /// slice `text` appropriately, it is necessary to tell whether `at` is
    /// at the beginning of `text` or not (i.e., for empty assertions).
    pub fn exec(
        prog: &'a Program,
        search: &'b mut Search<'c, 'm>,
        text: &[u8],
        at: usize,
    ) -> DfaResult {
        // Retrieve our DFA cache from the program. If another thread tries to
        // execute this DFA *simultaneously*, then a new independent cache is
        // created.
        let mut _cache = prog.cache_dfa();
        let mut cache = &mut **_cache;
        cache.resize(prog.len());

        let mut dfa = Dfa {
            prog: prog,
            start: 0, // filled in below
            search: search,
            at: at,
            last_match_si: STATE_UNKNOWN,
            last_cache_flush: at,
            compiled: &mut cache.compiled,
            states: &mut cache.states,
            start_states: &mut cache.start_states,
            stack: &mut cache.stack,
            cache_flush_count: &mut cache.cache_flush_count,
        };
        dfa.start = match dfa.start_state(&mut cache.qcur, text) {
            None => return DfaResult::Quit,
            Some(STATE_DEAD) => return DfaResult::NoMatch,
            Some(si) => si,
        };
        debug_assert!(dfa.start != STATE_UNKNOWN);
        let result = if prog.is_reverse {
            dfa.exec_at_reverse(&mut cache.qcur, &mut cache.qnext, text)
        } else {
            dfa.exec_at(&mut cache.qcur, &mut cache.qnext, text)
        };
        if result.is_match() {
            if dfa.search.find_one_match() {
                dfa.search.set_match(0);
            } else {
                debug_assert!(dfa.last_match_si != STATE_UNKNOWN);
                debug_assert!(dfa.last_match_si != STATE_DEAD);
                for &ip in &dfa.states[dfa.last_match_si as usize].insts {
                    if let Inst::Match(slot) = dfa.prog[ip as usize] {
                        dfa.search.set_match(slot);
                    }
                }
            }
        }
        result
    }

    /// Executes the DFA on a forward NFA.
    ///
    /// {qcur,qnext} are scratch ordered sets which may be non-empty.
    fn exec_at(
        &mut self,
        qcur: &mut SparseSet,
        qnext: &mut SparseSet,
        text: &[u8],
    ) -> DfaResult {
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
        let (mut si, mut result) = (self.start, DfaResult::NoMatch);
        while self.at < text.len() {
            // Our set of literal prefixes can itself be a DFA, but it is
            // offline and can generally be quite a bit faster. (For instance,
            // memchr is used if possible.)
            if !self.prog.prefixes.is_empty() && si == self.start {
                self.at = match self.prefix_at(text, self.at) {
                    None => return DfaResult::NoMatch,
                    Some(i) => i,
                };
            }

            // The following logic is essentially what `self.next_state` does,
            // but we inline it manually here to avoid the extra branch and
            // also because we know we have a real `u8` (not a `Byte`, which
            // may be the special EOF sentinel value).
            let cls = self.prog.byte_classes[text[self.at] as usize];
            let mut next_si = self.states[si as usize].next[cls as usize];
            if next_si <= STATE_DEAD {
                if next_si == STATE_DEAD {
                    return result;
                }
                // The next state may not have been cached, so re-compute it
                // (i.e., follow epsilon transitions).
                let b = Byte::byte(text[self.at]);
                next_si = match self.exec_byte(qcur, qnext, si, b) {
                    None => return DfaResult::Quit,
                    Some(next_si) => next_si,
                };
                debug_assert!(next_si != STATE_UNKNOWN);
                if next_si == STATE_DEAD {
                    return result;
                }
            }
            si = next_si;
            if self.states[si as usize].is_match {
                self.last_match_si = si;
                if self.search.quit_after_first_match() {
                    return DfaResult::Match;
                }
                result = DfaResult::Match;
                self.search.set_end(Some(self.at));
            }
            self.at += 1;
        }
        // Run the DFA once more on the special EOF senitnel value.
        si = match self.next_state(qcur, qnext, si, Byte::eof()) {
            None => return DfaResult::Quit,
            Some(si) => si,
        };
        debug_assert!(si != STATE_UNKNOWN);
        if si == STATE_DEAD {
            return result;
        }
        if self.states[si as usize].is_match {
            self.last_match_si = si;
            if self.search.quit_after_first_match() {
                return DfaResult::Match;
            }
            result = DfaResult::Match;
            self.search.set_end(Some(text.len()));
        }
        result
    }

    /// Executes the DFA on a reverse NFA.
    fn exec_at_reverse(
        &mut self,
        qcur: &mut SparseSet,
        qnext: &mut SparseSet,
        text: &[u8],
    ) -> DfaResult {
        // The comments in `exec_at` above mostly apply here too. The main
        // difference is that we move backwards over the input and we look for
        // the longest possible match instead of the leftmost-first match.
        //
        // N.B. The code duplication here is regrettable. Efforts to improve
        // it without sacrificing performance are welcome. ---AG
        debug_assert!(self.prog.is_reverse);
        let (mut si, mut result) = (self.start, DfaResult::NoMatch);
        while self.at > 0 {
            self.at -= 1;

            let cls = self.prog.byte_classes[text[self.at] as usize];
            let mut next_si = self.states[si as usize].next[cls as usize];
            if next_si <= STATE_DEAD {
                if next_si == STATE_DEAD {
                    return result;
                }
                // The next state may not have been cached, so re-compute it
                // (i.e., follow epsilon transitions).
                let b = Byte::byte(text[self.at]);
                next_si = match self.exec_byte(qcur, qnext, si, b) {
                    None => return DfaResult::Quit,
                    Some(next_si) => next_si,
                };
                debug_assert!(next_si != STATE_UNKNOWN);
                if next_si == STATE_DEAD {
                    return result;
                }
            }
            si = next_si;
            if self.states[si as usize].is_match {
                self.last_match_si = si;
                if self.search.quit_after_first_match() {
                    return DfaResult::NoMatch;
                }
                result = DfaResult::Match;
                self.search.set_start(Some(self.at+1));
            }
        }
        // Run the DFA once more on the special EOF senitnel value.
        si = match self.next_state(qcur, qnext, si, Byte::eof()) {
            None => return DfaResult::Quit,
            Some(si) => si,
        };
        debug_assert!(si != STATE_UNKNOWN);
        if si == STATE_DEAD {
            return result;
        }
        if self.states[si as usize].is_match {
            self.last_match_si = si;
            if self.search.quit_after_first_match() {
                return DfaResult::Match;
            }
            result = DfaResult::Match;
            self.search.set_start(Some(0));
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
        for &ip in &self.states[si as usize].insts {
            qcur.add(ip as usize);
        }

        // Before inspecting the current byte, we may need to also inspect
        // whether the position immediately preceding the current byte
        // satisfies the empty assertions found in the current state.
        //
        // We only need to do this step if there are any empty assertions in
        // the current state.
        if self.states[si as usize].inst_flags.has_non_match_flags() {
            // Compute the flags immediately preceding the current byte.
            // This means we only care about the "end" or "end line" flags.
            // (The "start" flags are computed immediately proceding the
            // current byte and is handled below.)
            let mut flags = Flags::new();
            if b.is_eof() {
                flags.set_end(true).set_end_line(true);
            } else if b.as_byte().map_or(false, |b| b == b'\n') {
                flags.set_end_line(true);
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
        let mut flags = Flags::new();
        let mut is_match = false;
        if b.as_byte().map_or(false, |b| b == b'\n') {
            flags.set_start_line(true);
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
                    is_match = true;
                    if !self.continue_past_first_match() {
                        break;
                    } else if !self.search.find_one_match()
                            && !qnext.contains_ip(ip as usize) {
                        // If we are continuing on to find other matches,
                        // then keep a record of the match states we've seen.
                        qnext.add(ip);
                        // BREADCRUMBS:
                        // Perhaps we need another sparse set here and track
                        // these "recorded" matches separately. They should
                        // still make their way into cached states, but perhaps
                        // they shouldn't prevent a DEAD state from
                        // occurring.
                    }
                }
                Bytes(ref inst) => {
                    if b.as_byte().map_or(false, |b| inst.matches(b)) {
                        self.follow_epsilons(
                            inst.goto as InstPtr, qnext, flags);
                    }
                }
            }
        }
        let mut cache = true;
        if b.is_eof() && !self.search.find_one_match() {
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
        let next = match self.cached_state(qnext, is_match, Some(&mut si)) {
            None => return None,
            Some(next) => next,
        };
        debug_assert!(next != STATE_UNKNOWN);
        // And now store our state in the current state's next list.
        let cls = self.byte_class(b);
        if cache {
            self.states[si as usize].next[cls] = next;
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
        flags: Flags,
    ) {
        use prog::Inst::*;
        use prog::EmptyLook::*;

        // We need to traverse the NFA to follow epsilon transitions, so avoid
        // recursion with an explicit stack.
        self.stack.push(ip);
        while let Some(ip) = self.stack.pop() {
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
                        StartLine if flags.is_start_line() => {
                            self.stack.push(inst.goto as InstPtr);
                        }
                        EndLine if flags.is_end_line() => {
                            self.stack.push(inst.goto as InstPtr);
                        }
                        StartText if flags.is_start() => {
                            self.stack.push(inst.goto as InstPtr);
                        }
                        EndText if flags.is_end() => {
                            self.stack.push(inst.goto as InstPtr);
                        }
                        StartLine | EndLine | StartText | EndText => {}
                        // The DFA doesn't support word boundaries. :-(
                        WordBoundary
                        | NotWordBoundary
                        | WordBoundaryAscii
                        | NotWordBoundaryAscii => unreachable!(),
                    }
                }
                Save(ref inst) => self.stack.push(inst.goto as InstPtr),
                Split(ref inst) => {
                    self.stack.push(inst.goto2 as InstPtr);
                    self.stack.push(inst.goto1 as InstPtr);
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
        is_match: bool,
        current_state: Option<&mut StatePtr>,
    ) -> Option<StatePtr> {
        // If we couldn't come up with a non-empty key to represent this state,
        // then it is dead and can never lead to a match.
        //
        // Note that inst_flags represent the set of empty width assertions
        // in q. We use this as an optimization in exec_byte to determine when
        // we should follow epsilon transitions at the empty string preceding
        // the current byte.
        let (key, inst_flags) = match self.cached_state_key(q, is_match) {
            None => return Some(STATE_DEAD),
            Some(v) => v,
        };
        // In the cache? Cool. Done.
        if let Some(&si) = self.compiled.get(&key) {
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
        self.states.push(State {
            next: next,
            insts: key.insts.clone(),
            is_match: is_match,
            inst_flags: inst_flags,
        });
        let si = usize_to_u32(self.states.len().checked_sub(1).unwrap());
        self.compiled.insert(key, si);
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
        is_match: bool,
    ) -> Option<(StateKey, Flags)> {
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
        let mut inst_flags = Flags::new();
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
                        WordBoundary
                        | NotWordBoundary
                        | WordBoundaryAscii
                        | NotWordBoundaryAscii => unreachable!(),
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
        if insts.len() == 0 && !is_match {
            None
        } else {
            let key = StateKey { insts: insts, is_match: is_match };
            Some((key, inst_flags))
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
        if self.states.len() <= 2 {
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
        if *self.cache_flush_count >= 3
            && self.at >= self.last_cache_flush
            && (self.at - self.last_cache_flush) <= 10 * self.states.len() {
            return false;
        }
        // Update statistics tracking cache flushes.
        self.last_cache_flush = self.at;
        *self.cache_flush_count += 1;

        // OK, actually flush the cache.
        let start = self.copy_state(self.start);
        self.states.clear();
        self.compiled.clear();
        for start in self.start_states.iter_mut() {
            *start = STATE_UNKNOWN;
        }
        self.states.push(State::invalid());
        self.states.push(State::invalid());
        self.start = self.restore_state(start);
        true
    }

    /// Returns a fresh copy of state si with all of its next pointers set to
    /// unknown.
    fn copy_state(&self, si: StatePtr) -> State {
        let mut state = self.states[si as usize].clone();
        // Make sure to erase any pointers from this state, so that
        // they are forced to be re-computed.
        state.next = vec![STATE_UNKNOWN; self.num_byte_classes()];
        state
    }

    /// Restores the given state back into the cache, and returns a pointer
    /// to it.
    fn restore_state(&mut self, state: State) -> StatePtr {
        let key = StateKey {
            insts: state.insts.clone(),
            is_match: state.is_match,
        };
        if let Some(&si) = self.compiled.get(&key) {
            return si;
        }
        let si = usize_to_u32(self.states.len());
        self.states.push(state);
        self.compiled.insert(key, si);
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
        let cls = self.byte_class(b);
        match self.states[si as usize].next[cls] {
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
    fn start_state(
        &mut self,
        q: &mut SparseSet,
        text: &[u8],
    ) -> Option<StatePtr> {
        let start_flags = if self.prog.is_reverse {
            self.start_flags_reverse(text, self.at)
        } else {
            self.start_flags(text, self.at)
        };
        let flagi = start_flags.0 as usize;
        match self.start_states[flagi] {
            STATE_UNKNOWN => {}
            STATE_DEAD => return Some(STATE_DEAD),
            si => return Some(si),
        }
        q.clear();
        self.follow_epsilons(0, q, start_flags);
        // Start states can never be match states because we delay every match
        // by one byte. Given an empty string and an empty match, the match
        // won't actually occur until the DFA processes the special EOF
        // sentinel byte.
        let sp = match self.cached_state(q, false, None) {
            None => return None,
            Some(sp) => sp,
        };
        self.start_states[flagi] = sp;
        Some(sp)
    }

    /// Computes the set of starting flags for the given position in text.
    ///
    /// This should only be used when executing the DFA forwards over the
    /// input.
    fn start_flags(&self, text: &[u8], at: usize) -> Flags {
        let mut flags = Flags::new();
        flags.set_start(at == 0).set_end(text.len() == 0);
        flags.set_start_line(at == 0 || text[at - 1] == b'\n');
        flags.set_end_line(text.len() == 0);
        flags
    }

    /// Computes the set of starting flags for the given position in text.
    ///
    /// This should only be used when executing the DFA in reverse over the
    /// input.
    fn start_flags_reverse(&self, text: &[u8], at: usize) -> Flags {
        let mut flags = Flags::new();
        flags.set_start(at == text.len()).set_end(text.len() == 0);
        flags.set_start_line(at == text.len() || text[at] == b'\n');
        flags.set_end_line(text.len() == 0);
        flags
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

    /// Returns true if the DFA should continue searching past the first match.
    ///
    /// Leftmost first semantics in the DFA are preserved by not following NFA
    /// transitions after the first match is seen.
    ///
    /// On occasion, we want to avoid leftmost first semantics to find either
    /// the longest match (for reverse search) or all possible matches (for
    /// regex sets).
    fn continue_past_first_match(&self) -> bool {
        self.prog.is_reverse || !self.search.find_one_match()
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
        // Estimate that there are about 32 instructions per state consuming
        // 128 = 32 * 4 bytes of space. (This is hopefully a blatant
        // overestimate.)
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
    /// Return an invalid state. This is only used to "pad" the state cache so
    /// that the special sentinel values (STATE_UNKNOWN and STATE_DEAD) are
    /// never used.
    fn invalid() -> State {
        State {
            next: vec![],
            insts: vec![],
            is_match: false,
            inst_flags: Flags::new(),
        }
    }
}

impl Flags {
    #[inline]
    fn new() -> Self {
        Flags(0)
    }

    #[inline]
    fn has_non_match_flags(&self) -> bool {
        self.0 & 0b0_1111111 > 0
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
    fn is_start_line(&self) -> bool { self.0 & 0b000_1_0000 > 0 }

    #[inline]
    fn set_start_line(&mut self, yes: bool) -> &mut Self {
        self.set(yes, 0b000_1_0000);
        self
    }

    #[inline]
    fn is_end_line(&self) -> bool { self.0 & 0b0000_1_000 > 0 }

    #[inline]
    fn set_end_line(&mut self, yes: bool) -> &mut Self {
        self.set(yes, 0b0000_1_000);
        self
    }
}

impl Byte {
    #[inline] fn byte(b: u8) -> Self { Byte(b as u16) }
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
         .field("is_match", &self.is_match)
         .field("insts", &self.insts)
         .field("next", &next)
         .finish()
    }
}

impl fmt::Debug for Flags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Flags")
         .field("start", &if self.is_start() { 1 } else { 0 })
         .field("end", &if self.is_end() { 1 } else { 0 })
         .field("start_line", &if self.is_start_line() { 1 } else { 0 })
         .field("end_line", &if self.is_end_line() { 1 } else { 0 })
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
