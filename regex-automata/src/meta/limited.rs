/*!
This module defines two bespoke reverse DFA searching routines. (One for the
lazy DFA and one for the fully compiled DFA.) These routines differ from the
usual ones by permitting the caller to specify a minimum starting position.
That is, the search will begin at `input.end()` and will usually stop at
`input.start()`, unless `min_start > input.start()`, in which case, the search
will stop at `min_start`.

In other words, this lets you say, "no, the search must not extend past this
point, even if it's within the bounds of the given `Input`." And if the search
*does* want to go past that point, it stops and returns a "may be quadratic"
error, which indicates that the caller should retry using some other technique.

These routines specifically exist to protect against quadratic behavior when
employing the "reverse suffix" and "reverse inner" optimizations. Without the
backstop these routines provide, it is possible for parts of the haystack to
get re-scanned over and over again. The backstop not only prevents this, but
*tells you when it is happening* so that you can change the strategy.

Why can't we just use the normal search routines? We could use the normal
search routines and just set the start bound on the provided `Input` to our
`min_start` position. The problem here is that it's impossible to distinguish
between "no match because we reached the end of input" and "determined there
was no match well before the end of input." The former case is what we care
about with respect to quadratic behavior. The latter case is totally fine.

Why don't we modify the normal search routines to report the position at which
the search stops? I considered this, and I still wonder if it is indeed the
right thing to do. However, I think the straight-forward thing to do there
would be to complicate the return type signature of almost every search routine
in this crate, which I really do not want to do. It therefore might make more
sense to provide a richer way for search routines to report meta data, but that
was beyond my bandwidth to work on at the time of writing.

See the 'opt/reverse-inner' and 'opt/reverse-suffix' benchmarks in rebar for a
real demonstration of how quadratic behavior is mitigated.
*/

use crate::{
    meta::error::{RetryError, RetryQuadraticError},
    HalfMatch, Input, MatchError,
};

#[cfg(feature = "dfa-build")]
pub(crate) fn dfa_try_search_half_rev(
    dfa: &crate::dfa::dense::DFA<alloc::vec::Vec<u32>>,
    input: &Input<'_>,
    min_start: usize,
) -> Result<Option<HalfMatch>, RetryError> {
    use crate::dfa::Automaton;

    let mut mat = None;
    let mut sid = dfa.start_state_reverse(input)?;
    if input.start() == input.end() {
        dfa_eoi_rev(dfa, input, &mut sid, &mut mat)?;
        return Ok(mat);
    }
    let mut at = input.end() - 1;
    loop {
        sid = dfa.next_state(sid, input.haystack()[at]);
        if dfa.is_special_state(sid) {
            if dfa.is_match_state(sid) {
                let pattern = dfa.match_pattern(sid, 0);
                // Since reverse searches report the beginning of a
                // match and the beginning is inclusive (not exclusive
                // like the end of a match), we add 1 to make it
                // inclusive.
                mat = Some(HalfMatch::new(pattern, at + 1));
            } else if dfa.is_dead_state(sid) {
                return Ok(mat);
            } else if dfa.is_quit_state(sid) {
                if mat.is_some() {
                    return Ok(mat);
                }
                return Err(MatchError::quit(input.haystack()[at], at).into());
            }
        }
        if at == input.start() {
            break;
        }
        at -= 1;
        if at < min_start {
            trace!(
                "reached position {} which is before the previous literal \
				 match, quitting to avoid quadratic behavior",
                at,
            );
            return Err(RetryError::Quadratic(RetryQuadraticError::new()));
        }
    }
    dfa_eoi_rev(dfa, input, &mut sid, &mut mat)?;
    Ok(mat)
}

#[cfg(feature = "hybrid")]
pub(crate) fn hybrid_try_search_half_rev(
    dfa: &crate::hybrid::dfa::DFA,
    cache: &mut crate::hybrid::dfa::Cache,
    input: &Input<'_>,
    min_start: usize,
) -> Result<Option<HalfMatch>, RetryError> {
    let mut mat = None;
    let mut sid = dfa.start_state_reverse(cache, input)?;
    if input.start() == input.end() {
        hybrid_eoi_rev(dfa, cache, input, &mut sid, &mut mat)?;
        return Ok(mat);
    }
    let mut at = input.end() - 1;
    loop {
        sid = dfa
            .next_state(cache, sid, input.haystack()[at])
            .map_err(|_| MatchError::gave_up(at))?;
        if sid.is_tagged() {
            if sid.is_match() {
                let pattern = dfa.match_pattern(cache, sid, 0);
                // Since reverse searches report the beginning of a
                // match and the beginning is inclusive (not exclusive
                // like the end of a match), we add 1 to make it
                // inclusive.
                mat = Some(HalfMatch::new(pattern, at + 1));
            } else if sid.is_dead() {
                return Ok(mat);
            } else if sid.is_quit() {
                if mat.is_some() {
                    return Ok(mat);
                }
                return Err(MatchError::quit(input.haystack()[at], at).into());
            }
        }
        if at == input.start() {
            break;
        }
        at -= 1;
        if at < min_start {
            trace!(
                "reached position {} which is before the previous literal \
				 match, quitting to avoid quadratic behavior",
                at,
            );
            return Err(RetryError::Quadratic(RetryQuadraticError::new()));
        }
    }
    hybrid_eoi_rev(dfa, cache, input, &mut sid, &mut mat)?;
    Ok(mat)
}

#[cfg(feature = "dfa-build")]
#[cfg_attr(feature = "perf-inline", inline(always))]
fn dfa_eoi_rev(
    dfa: &crate::dfa::dense::DFA<alloc::vec::Vec<u32>>,
    input: &Input<'_>,
    sid: &mut crate::util::primitives::StateID,
    mat: &mut Option<HalfMatch>,
) -> Result<(), MatchError> {
    use crate::dfa::Automaton;

    let sp = input.get_span();
    if sp.start > 0 {
        let byte = input.haystack()[sp.start - 1];
        *sid = dfa.next_state(*sid, byte);
        if dfa.is_match_state(*sid) {
            let pattern = dfa.match_pattern(*sid, 0);
            *mat = Some(HalfMatch::new(pattern, sp.start));
        } else if dfa.is_quit_state(*sid) {
            if mat.is_some() {
                return Ok(());
            }
            return Err(MatchError::quit(byte, sp.start - 1));
        }
    } else {
        *sid = dfa.next_eoi_state(*sid);
        if dfa.is_match_state(*sid) {
            let pattern = dfa.match_pattern(*sid, 0);
            *mat = Some(HalfMatch::new(pattern, 0));
        }
        // N.B. We don't have to check 'is_quit' here because the EOI
        // transition can never lead to a quit state.
        debug_assert!(!dfa.is_quit_state(*sid));
    }
    Ok(())
}

#[cfg(feature = "hybrid")]
#[cfg_attr(feature = "perf-inline", inline(always))]
fn hybrid_eoi_rev(
    dfa: &crate::hybrid::dfa::DFA,
    cache: &mut crate::hybrid::dfa::Cache,
    input: &Input<'_>,
    sid: &mut crate::hybrid::LazyStateID,
    mat: &mut Option<HalfMatch>,
) -> Result<(), MatchError> {
    let sp = input.get_span();
    if sp.start > 0 {
        let byte = input.haystack()[sp.start - 1];
        *sid = dfa
            .next_state(cache, *sid, byte)
            .map_err(|_| MatchError::gave_up(sp.start))?;
        if sid.is_match() {
            let pattern = dfa.match_pattern(cache, *sid, 0);
            *mat = Some(HalfMatch::new(pattern, sp.start));
        } else if sid.is_quit() {
            if mat.is_some() {
                return Ok(());
            }
            return Err(MatchError::quit(byte, sp.start - 1));
        }
    } else {
        *sid = dfa
            .next_eoi_state(cache, *sid)
            .map_err(|_| MatchError::gave_up(sp.start))?;
        if sid.is_match() {
            let pattern = dfa.match_pattern(cache, *sid, 0);
            *mat = Some(HalfMatch::new(pattern, 0));
        }
        // N.B. We don't have to check 'is_quit' here because the EOI
        // transition can never lead to a quit state.
        debug_assert!(!sid.is_quit());
    }
    Ok(())
}
