/*!
Routines for proving when reverse meta strategies may return after the first
confirmed candidate match.

The reverse suffix and reverse inner strategies first search for a literal
candidate and then confirm that candidate with another regex search. Returning
after the first confirmed candidate is only correct when no earlier match can
be skipped over by the literal scan. The checks in this module are conservative
proofs for that condition. If any check gets confused, grows too large or sees
a construct it doesn't model, it reports failure and the corresponding reverse
strategy is not used.

The analysis here is not part of matching. It only runs while building the
meta regex, and it operates on small Thompson NFAs compiled only for the part
of a pattern relevant to the proof.
*/

use alloc::{collections::BTreeSet, vec, vec::Vec};

use regex_syntax::hir::literal::Literal;

use crate::{
    nfa::thompson::{State, NFA},
    util::{primitives::StateID, sparse_set::SparseSet},
};

/// A canonical, owned representation of an epsilon-closed set of NFA states.
type StateSet = Vec<StateID>;

/// Return true when the reverse suffix strategy can return after the first
/// confirmed suffix candidate.
///
/// If this returns false, then some proper suffix of a non-matching proper
/// prefix may itself be a complete match. In that case, the first confirmed
/// suffix could be an interior match instead of the leftmost match.
pub(crate) fn reverse_suffix_is_safe(nfa: &NFA) -> bool {
    ReverseSuffix::new(nfa).map_or(false, |mut x| x.is_safe())
}

/// Return true when `nfa` can match a string containing `literal`.
///
/// This is used to prove that the prefix before a reverse suffix literal
/// cannot itself contain the suffix literal. If it can, then a suffix scan may
/// see an interior occurrence before it sees the suffix that ends the leftmost
/// match.
pub(crate) fn prefix_contains_literal(nfa: &NFA, literal: &[u8]) -> bool {
    PrefixLiteral::new(nfa, literal).map_or(true, |mut x| x.contains())
}

/// Return true when the reverse inner strategy can return after the first
/// confirmed inner literal candidate.
///
/// If this returns false, then an inner literal could begin before an earlier
/// match reaches the extracted inner position, while a proper suffix has
/// already matched the prefix. In that case, the first confirmed inner literal
/// can belong to a later match instead of the leftmost match.
pub(crate) fn reverse_inner_is_safe(
    prefix: &NFA,
    literals: &[Literal],
) -> bool {
    ReverseInner::new(prefix, literals).map_or(false, |mut x| x.is_safe())
}

#[derive(Debug)]
struct ReverseSuffix<'a> {
    full: NFAStateSets<'a>,
}

impl<'a> ReverseSuffix<'a> {
    const LIMIT: usize = 10_000;

    fn new(nfa: &'a NFA) -> Option<ReverseSuffix<'a>> {
        Some(ReverseSuffix { full: NFAStateSets::new(nfa, false)? })
    }

    fn is_safe(&mut self) -> bool {
        let mut seen = BTreeSet::new();
        let mut queue = vec![ReverseSuffixState {
            main: self.full.start.clone(),
            suffix: vec![],
            consumed: false,
        }];
        let mut i = 0;
        while let Some(state) = queue.get(i).cloned() {
            i += 1;
            for byte in 0..=u8::MAX {
                let main = match self.full.step(&state.main, byte) {
                    None => return false,
                    Some(main) => main,
                };
                let mut suffix_base = state.suffix.clone();
                if state.consumed {
                    suffix_base.extend(self.full.start.iter().copied());
                    suffix_base.sort_unstable();
                    suffix_base.dedup();
                }
                let suffix = match self.full.step(&suffix_base, byte) {
                    None => return false,
                    Some(suffix) => suffix,
                };
                if self.full.is_match(&suffix)
                    && !self.full.is_match(&main)
                    && self.full.can_extend_nonempty(&main)
                {
                    return false;
                }
                let next = ReverseSuffixState { main, suffix, consumed: true };
                if seen.insert(next.clone()) {
                    if seen.len() > Self::LIMIT {
                        return false;
                    }
                    queue.push(next);
                }
            }
        }
        true
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct ReverseSuffixState {
    main: StateSet,
    suffix: StateSet,
    consumed: bool,
}

#[derive(Debug)]
struct PrefixLiteral<'a> {
    prefix: NFAStateSets<'a>,
    literal: &'a [u8],
    literal_len: StateID,
    fallback: Vec<StateID>,
}

impl<'a> PrefixLiteral<'a> {
    const LIMIT: usize = 10_000;

    fn new(nfa: &'a NFA, literal: &'a [u8]) -> Option<PrefixLiteral<'a>> {
        if literal.is_empty() {
            return None;
        }
        let literal_len = StateID::new(literal.len()).ok()?;
        Some(PrefixLiteral {
            prefix: NFAStateSets::new(nfa, true)?,
            literal,
            literal_len,
            fallback: PrefixLiteral::kmp_fallback(literal),
        })
    }

    fn contains(&mut self) -> bool {
        let start = PrefixLiteralState {
            prefix: self.prefix.start.clone(),
            matched: StateID::ZERO,
        };
        let mut seen = BTreeSet::new();
        seen.insert(start.clone());
        let mut queue = vec![start];
        let mut i = 0;
        while let Some(state) = queue.get(i).cloned() {
            i += 1;
            for byte in 0..=u8::MAX {
                let prefix = match self.prefix.step(&state.prefix, byte) {
                    None => return true,
                    Some(prefix) => prefix,
                };
                if !self.prefix.can_reach_match(&prefix) {
                    continue;
                }
                let matched = self.next_match_len(state.matched, byte);
                if matched == self.literal_len {
                    return true;
                }
                let next = PrefixLiteralState { prefix, matched };
                if seen.insert(next.clone()) {
                    if seen.len() > Self::LIMIT {
                        return true;
                    }
                    queue.push(next);
                }
            }
        }
        false
    }

    fn next_match_len(&self, mut len: StateID, byte: u8) -> StateID {
        while len != StateID::ZERO && self.literal[len.as_usize()] != byte {
            len = self.fallback[len.as_usize() - 1];
        }
        if self.literal[len.as_usize()] == byte {
            len = StateID::new(len.as_usize() + 1)
                .expect("matched length should fit in StateID");
        }
        len
    }

    fn kmp_fallback(literal: &[u8]) -> Vec<StateID> {
        let mut fallback = vec![StateID::ZERO; literal.len()];
        let mut len = StateID::ZERO;
        for i in 1..literal.len() {
            while len != StateID::ZERO && literal[i] != literal[len.as_usize()]
            {
                len = fallback[len.as_usize() - 1];
            }
            if literal[i] == literal[len.as_usize()] {
                len = StateID::new(len.as_usize() + 1)
                    .expect("fallback length should fit in StateID");
                fallback[i] = len;
            }
        }
        fallback
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct PrefixLiteralState {
    prefix: StateSet,
    matched: StateID,
}

#[derive(Debug)]
struct ReverseInner<'a> {
    prefix: NFAStateSets<'a>,
    literals: &'a [Literal],
}

impl<'a> ReverseInner<'a> {
    const LIMIT: usize = 10_000;

    fn new(
        prefix: &'a NFA,
        literals: &'a [Literal],
    ) -> Option<ReverseInner<'a>> {
        if literals.iter().any(|lit| lit.is_empty()) {
            return None;
        }
        Some(ReverseInner {
            prefix: NFAStateSets::new(prefix, true)?,
            literals,
        })
    }

    fn is_safe(&mut self) -> bool {
        let mut seen = BTreeSet::new();
        let start = ReverseInnerState {
            main_prefix: self.prefix.start.clone(),
            suffixes: vec![],
        };
        seen.insert(start.clone());
        let mut queue = vec![start];
        let mut i = 0;
        while let Some(state) = queue.get(i).cloned() {
            i += 1;
            if self.is_unsafe_boundary(&state) {
                return false;
            }
            for byte in 0..=u8::MAX {
                let main_prefix =
                    match self.prefix.step(&state.main_prefix, byte) {
                        None => return false,
                        Some(main_prefix) => main_prefix,
                    };
                if !self.prefix.can_reach_match(&main_prefix) {
                    continue;
                }

                let mut suffixes = vec![];
                for suffix in state.suffixes.iter() {
                    let prefix = match self.prefix.step(suffix, byte) {
                        None => return false,
                        Some(prefix) => prefix,
                    };
                    if self.prefix.can_reach_match(&prefix) {
                        suffixes.push(prefix);
                    }
                }
                suffixes.push(self.prefix.start.clone());
                suffixes.sort_unstable();
                suffixes.dedup();

                let next = ReverseInnerState { main_prefix, suffixes };
                if seen.insert(next.clone()) {
                    if seen.len() > Self::LIMIT {
                        return false;
                    }
                    queue.push(next);
                }
            }
        }
        true
    }

    fn is_unsafe_boundary(&mut self, state: &ReverseInnerState) -> bool {
        if self.prefix.is_match(&state.main_prefix) {
            return false;
        }
        state.suffixes.iter().any(|suffix| {
            self.prefix.is_match(suffix)
                && self.literals.iter().any(|lit| {
                    self.prefix_can_extend_over_literal(
                        &state.main_prefix,
                        lit.as_bytes(),
                    )
                })
        })
    }

    fn prefix_can_extend_over_literal(
        &mut self,
        states: &[StateID],
        literal: &[u8],
    ) -> bool {
        let mut states = states.to_vec();
        for &byte in literal {
            states = match self.prefix.step(&states, byte) {
                None => return true,
                Some(states) => states,
            };
            if self.prefix.can_reach_match(&states) {
                return true;
            }
        }
        false
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct ReverseInnerState {
    main_prefix: StateSet,
    suffixes: Vec<StateSet>,
}

/// A small helper for stepping a Thompson NFA through all possible strings.
///
/// The sets of NFA states returned here are always epsilon closures. Look
/// states and empty matches are deliberately rejected by the constructor when
/// they would make the proof unsound or too hard to model.
#[derive(Debug)]
struct NFAStateSets<'a> {
    nfa: &'a NFA,
    start: StateSet,
    can_reach_match: Vec<bool>,
    stack: Vec<StateID>,
    closure: SparseSet,
    next: SparseSet,
}

impl<'a> NFAStateSets<'a> {
    fn new(nfa: &'a NFA, allow_empty: bool) -> Option<NFAStateSets<'a>> {
        if !nfa.look_set_any().is_empty() || (!allow_empty && nfa.has_empty())
        {
            return None;
        }
        let mut sets = NFAStateSets {
            nfa,
            start: vec![],
            can_reach_match: NFAStateSets::compute_can_reach_match(nfa),
            stack: vec![],
            closure: SparseSet::new(nfa.states().len()),
            next: SparseSet::new(nfa.states().len()),
        };
        sets.start = sets.epsilon_closure(nfa.start_anchored())?;
        Some(sets)
    }

    fn step(&mut self, set: &[StateID], byte: u8) -> Option<StateSet> {
        self.next.clear();
        for &sid in set {
            match *self.nfa.state(sid) {
                State::ByteRange { trans } => {
                    if trans.matches_byte(byte) {
                        NFAStateSets::epsilon_closure_into(
                            self.nfa,
                            &mut self.stack,
                            &mut self.next,
                            trans.next,
                        )?;
                    }
                }
                State::Sparse(ref sparse) => {
                    if let Some(next_sid) = sparse.matches_byte(byte) {
                        NFAStateSets::epsilon_closure_into(
                            self.nfa,
                            &mut self.stack,
                            &mut self.next,
                            next_sid,
                        )?;
                    }
                }
                State::Dense(ref dense) => {
                    if let Some(next_sid) = dense.matches_byte(byte) {
                        NFAStateSets::epsilon_closure_into(
                            self.nfa,
                            &mut self.stack,
                            &mut self.next,
                            next_sid,
                        )?;
                    }
                }
                State::Look { .. } => return None,
                State::Union { .. }
                | State::BinaryUnion { .. }
                | State::Capture { .. }
                | State::Fail
                | State::Match { .. } => {}
            }
        }
        Some(NFAStateSets::canonical(&self.next))
    }

    fn epsilon_closure(&mut self, sid: StateID) -> Option<StateSet> {
        self.closure.clear();
        NFAStateSets::epsilon_closure_into(
            self.nfa,
            &mut self.stack,
            &mut self.closure,
            sid,
        )?;
        Some(NFAStateSets::canonical(&self.closure))
    }

    /// Compute the epsilon closure of `sid`, writing it into `set`.
    ///
    /// This mirrors the PikeVM epsilon closure routine, but drops all of the
    /// slot handling since this analysis does not report captures. The sparse
    /// set handles de-duplication and preserves insertion order while we
    /// traverse epsilon transitions with an explicit stack.
    fn epsilon_closure_into(
        nfa: &NFA,
        stack: &mut Vec<StateID>,
        set: &mut SparseSet,
        mut sid: StateID,
    ) -> Option<()> {
        stack.clear();
        loop {
            if !set.insert(sid) {
                sid = match stack.pop() {
                    None => return Some(()),
                    Some(sid) => sid,
                };
                continue;
            }
            match *nfa.state(sid) {
                State::Look { .. } => return None,
                State::Union { ref alternates } => {
                    sid = match alternates.get(0) {
                        None => return Some(()),
                        Some(&sid) => sid,
                    };
                    stack.extend(alternates[1..].iter().copied().rev());
                }
                State::BinaryUnion { alt1, alt2 } => {
                    sid = alt1;
                    stack.push(alt2);
                }
                State::Capture { next, .. } => {
                    sid = next;
                }
                State::ByteRange { .. }
                | State::Sparse(_)
                | State::Dense(_)
                | State::Fail
                | State::Match { .. } => match stack.pop() {
                    None => return Some(()),
                    Some(next) => sid = next,
                },
            }
        }
    }

    fn canonical(set: &SparseSet) -> StateSet {
        let mut canonical = set.iter().collect::<Vec<_>>();
        canonical.sort_unstable();
        canonical
    }

    fn compute_can_reach_match(nfa: &NFA) -> Vec<bool> {
        let mut can = vec![false; nfa.states().len()];
        let mut predecessors = vec![vec![]; nfa.states().len()];
        let mut stack = vec![];
        for (sid, state) in nfa.states().iter().enumerate() {
            let sid = StateID::new(sid).unwrap();
            match *state {
                State::Match { .. } => {
                    can[sid.as_usize()] = true;
                    stack.push(sid);
                }
                State::Look { .. } => {}
                State::Union { ref alternates } => {
                    for &next in alternates {
                        predecessors[next.as_usize()].push(sid);
                    }
                }
                State::BinaryUnion { alt1, alt2 } => {
                    predecessors[alt1.as_usize()].push(sid);
                    predecessors[alt2.as_usize()].push(sid);
                }
                State::Capture { next, .. } => {
                    predecessors[next.as_usize()].push(sid);
                }
                State::ByteRange { trans } => {
                    predecessors[trans.next.as_usize()].push(sid);
                }
                State::Sparse(ref sparse) => {
                    for t in sparse.transitions.iter() {
                        predecessors[t.next.as_usize()].push(sid);
                    }
                }
                State::Dense(ref dense) => {
                    for t in dense.iter() {
                        predecessors[t.next.as_usize()].push(sid);
                    }
                }
                State::Fail => {}
            }
        }
        while let Some(sid) = stack.pop() {
            for &pred in predecessors[sid.as_usize()].iter() {
                if can[pred.as_usize()] {
                    continue;
                }
                can[pred.as_usize()] = true;
                stack.push(pred);
            }
        }
        can
    }

    fn is_match(&self, set: &[StateID]) -> bool {
        set.iter()
            .any(|&sid| matches!(*self.nfa.state(sid), State::Match { .. }))
    }

    fn can_reach_match(&self, set: &[StateID]) -> bool {
        set.iter().any(|&sid| self.can_reach_match[sid.as_usize()])
    }

    fn can_extend_nonempty(&mut self, set: &[StateID]) -> bool {
        (0..=u8::MAX).any(|byte| {
            let next = match self.step(set, byte) {
                None => return true,
                Some(next) => next,
            };
            self.can_reach_match(&next)
        })
    }
}
