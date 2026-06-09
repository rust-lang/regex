use core::{
    fmt::Debug,
    panic::{RefUnwindSafe, UnwindSafe},
};

use alloc::{collections::BTreeSet, sync::Arc, vec, vec::Vec};

use regex_syntax::hir::{literal, Class, Hir, HirKind, Look as HirLook};

use crate::{
    meta::{
        error::{BuildError, RetryError, RetryFailError, RetryQuadraticError},
        regex::{Cache, RegexInfo},
        reverse_inner, wrappers,
    },
    nfa::thompson::{self, State, WhichCaptures, NFA},
    util::{
        captures::{Captures, GroupInfo},
        look::LookMatcher,
        prefilter::{self, Prefilter, PrefilterI},
        primitives::{NonMaxUsize, PatternID, StateID},
        search::{Anchored, HalfMatch, Input, Match, MatchKind, PatternSet},
    },
};

/// A trait that represents a single meta strategy. Its main utility is in
/// providing a way to do dynamic dispatch over a few choices.
///
/// Why dynamic dispatch? I actually don't have a super compelling reason, and
/// importantly, I have not benchmarked it with the main alternative: an enum.
/// I went with dynamic dispatch initially because the regex engine search code
/// really can't be inlined into caller code in most cases because it's just
/// too big. In other words, it is already expected that every regex search
/// will entail at least the cost of a function call.
///
/// I do wonder whether using enums would result in better codegen overall
/// though. It's a worthwhile experiment to try. Probably the most interesting
/// benchmark to run in such a case would be one with a high match count. That
/// is, a benchmark to test the overall latency of a search call.
pub(super) trait Strategy:
    Debug + Send + Sync + RefUnwindSafe + UnwindSafe + 'static
{
    fn group_info(&self) -> &GroupInfo;

    fn create_cache(&self) -> Cache;

    fn reset_cache(&self, cache: &mut Cache);

    fn is_accelerated(&self) -> bool;

    fn memory_usage(&self) -> usize;

    fn search(&self, cache: &mut Cache, input: &Input<'_>) -> Option<Match>;

    fn search_half(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<HalfMatch>;

    fn is_match(&self, cache: &mut Cache, input: &Input<'_>) -> bool;

    fn search_slots(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        slots: &mut [Option<NonMaxUsize>],
    ) -> Option<PatternID>;

    fn which_overlapping_matches(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        patset: &mut PatternSet,
    );
}

pub(super) fn new(
    info: &RegexInfo,
    hirs: &[&Hir],
) -> Result<Arc<dyn Strategy>, BuildError> {
    // At this point, we're committed to a regex engine of some kind. So pull
    // out a prefilter if we can, which will feed to each of the constituent
    // regex engines.
    let pre = if info.is_always_anchored_start() {
        // PERF: I'm not sure we necessarily want to do this... We may want to
        // run a prefilter for quickly rejecting in some cases. The problem
        // is that anchored searches overlap quite a bit with the use case
        // of "run a regex on every line to extract data." In that case, the
        // regex always matches, so running a prefilter doesn't really help us
        // there. The main place where a prefilter helps in an anchored search
        // is if the anchored search is not expected to match frequently. That
        // is, the prefilter gives us a way to possibly reject a haystack very
        // quickly.
        //
        // Maybe we should do use a prefilter, but only for longer haystacks?
        // Or maybe we should only use a prefilter when we think it's "fast"?
        //
        // Interestingly, I think we currently lack the infrastructure for
        // disabling a prefilter based on haystack length. That would probably
        // need to be a new 'Input' option. (Interestingly, an 'Input' used to
        // carry a 'Prefilter' with it, but I moved away from that.)
        debug!("skipping literal extraction since regex is anchored");
        None
    } else if let Some(pre) = info.config().get_prefilter() {
        debug!(
            "skipping literal extraction since the caller provided a prefilter"
        );
        Some(pre.clone())
    } else if info.config().get_auto_prefilter() {
        let kind = info.config().get_match_kind();
        let prefixes = crate::util::prefilter::prefixes(kind, hirs);
        // If we can build a full `Strategy` from just the extracted prefixes,
        // then we can short-circuit and avoid building a regex engine at all.
        if let Some(pre) = Pre::from_prefixes(info, &prefixes) {
            debug!(
                "found that the regex can be broken down to a literal \
                 search, avoiding the regex engine entirely",
            );
            return Ok(pre);
        }
        // This now attempts another short-circuit of the regex engine: if we
        // have a huge alternation of just plain literals, then we can just use
        // Aho-Corasick for that and avoid the regex engine entirely.
        //
        // You might think this case would just be handled by
        // `Pre::from_prefixes`, but that technique relies on heuristic literal
        // extraction from the corresponding `Hir`. That works, but part of
        // heuristics limit the size and number of literals returned. This case
        // will specifically handle patterns with very large alternations.
        //
        // One wonders if we should just roll this our heuristic literal
        // extraction, and then I think this case could disappear entirely.
        if let Some(pre) = Pre::from_alternation_literals(info, hirs) {
            debug!(
                "found plain alternation of literals, \
                 avoiding regex engine entirely and using Aho-Corasick"
            );
            return Ok(pre);
        }
        prefixes.literals().and_then(|strings| {
            debug!(
                "creating prefilter from {} literals: {:?}",
                strings.len(),
                strings,
            );
            Prefilter::new(kind, strings)
        })
    } else {
        debug!("skipping literal extraction since prefilters were disabled");
        None
    };
    let mut core = Core::new(info.clone(), pre.clone(), hirs)?;
    // Now that we have our core regex engines built, there are a few cases
    // where we can do a little bit better than just a normal "search forward
    // and maybe use a prefilter when in a start state." However, these cases
    // may not always work or otherwise build on top of the Core searcher.
    // For example, the reverse anchored optimization seems like it might
    // always work, but only the DFAs support reverse searching and the DFAs
    // might give up or quit for reasons. If we had, e.g., a PikeVM that
    // supported reverse searching, then we could avoid building a full Core
    // engine for this case.
    core = match ReverseAnchored::new(core) {
        Err(core) => core,
        Ok(ra) => {
            debug!("using reverse anchored strategy");
            return Ok(Arc::new(ra));
        }
    };
    core = match ReverseSuffix::new(core, hirs) {
        Err(core) => core,
        Ok(rs) => {
            debug!("using reverse suffix strategy");
            return Ok(Arc::new(rs));
        }
    };
    core = match ReverseInner::new(core, hirs) {
        Err(core) => core,
        Ok(ri) => {
            debug!("using reverse inner strategy");
            return Ok(Arc::new(ri));
        }
    };
    debug!("using core strategy");
    Ok(Arc::new(core))
}

#[derive(Clone, Debug)]
struct Pre<P> {
    pre: P,
    group_info: GroupInfo,
}

impl<P: PrefilterI> Pre<P> {
    fn new(pre: P) -> Arc<dyn Strategy> {
        // The only thing we support when we use prefilters directly as a
        // strategy is the start and end of the overall match for a single
        // pattern. In other words, exactly one implicit capturing group. Which
        // is exactly what we use here for a GroupInfo.
        let group_info = GroupInfo::new([[None::<&str>]]).unwrap();
        Arc::new(Pre { pre, group_info })
    }
}

// This is a little weird, but we don't actually care about the type parameter
// here because we're selecting which underlying prefilter to use. So we just
// define it on an arbitrary type.
impl Pre<()> {
    /// Given a sequence of prefixes, attempt to return a full `Strategy` using
    /// just the prefixes.
    ///
    /// Basically, this occurs when the prefixes given not just prefixes,
    /// but an enumeration of the entire language matched by the regular
    /// expression.
    ///
    /// A number of other conditions need to be true too. For example, there
    /// can be only one pattern, the number of explicit capture groups is 0, no
    /// look-around assertions and so on.
    ///
    /// Note that this ignores `Config::get_auto_prefilter` because if this
    /// returns something, then it isn't a prefilter but a matcher itself.
    /// Therefore, it shouldn't suffer from the problems typical to prefilters
    /// (such as a high false positive rate).
    fn from_prefixes(
        info: &RegexInfo,
        prefixes: &literal::Seq,
    ) -> Option<Arc<dyn Strategy>> {
        let kind = info.config().get_match_kind();
        // Check to see if our prefixes are exact, which means we might be
        // able to bypass the regex engine entirely and just rely on literal
        // searches.
        if !prefixes.is_exact() {
            return None;
        }
        // We also require that we have a single regex pattern. Namely,
        // we reuse the prefilter infrastructure to implement search and
        // prefilters only report spans. Prefilters don't know about pattern
        // IDs. The multi-regex case isn't a lost cause, we might still use
        // Aho-Corasick and we might still just use a regular prefilter, but
        // that's done below.
        if info.pattern_len() != 1 {
            return None;
        }
        // We can't have any capture groups either. The literal engines don't
        // know how to deal with things like '(foo)(bar)'. In that case, a
        // prefilter will just be used and then the regex engine will resolve
        // the capture groups.
        if info.props()[0].explicit_captures_len() != 0 {
            return None;
        }
        // We also require that it has zero look-around assertions. Namely,
        // literal extraction treats look-around assertions as if they match
        // *every* empty string. But of course, that isn't true. So for
        // example, 'foo\bquux' never matches anything, but 'fooquux' is
        // extracted from that as an exact literal. Such cases should just run
        // the regex engine. 'fooquux' will be used as a normal prefilter, and
        // then the regex engine will try to look for an actual match.
        if !info.props()[0].look_set().is_empty() {
            return None;
        }
        // Finally, currently, our prefilters are all oriented around
        // leftmost-first match semantics, so don't try to use them if the
        // caller asked for anything else.
        if kind != MatchKind::LeftmostFirst {
            return None;
        }
        // The above seems like a lot of requirements to meet, but it applies
        // to a lot of cases. 'foo', '[abc][123]' and 'foo|bar|quux' all meet
        // the above criteria, for example.
        //
        // Note that this is effectively a latency optimization. If we didn't
        // do this, then the extracted literals would still get bundled into
        // a prefilter, and every regex engine capable of running unanchored
        // searches supports prefilters. So this optimization merely sidesteps
        // having to run the regex engine at all to confirm the match. Thus, it
        // decreases the latency of a match.

        // OK because we know the set is exact and thus finite.
        let prefixes = prefixes.literals().unwrap();
        debug!(
            "trying to bypass regex engine by creating \
             prefilter from {} literals: {:?}",
            prefixes.len(),
            prefixes,
        );
        let choice = match prefilter::Choice::new(kind, prefixes) {
            Some(choice) => choice,
            None => {
                debug!(
                    "regex bypass failed because no prefilter could be built"
                );
                return None;
            }
        };
        let strat: Arc<dyn Strategy> = match choice {
            prefilter::Choice::Memchr(pre) => Pre::new(pre),
            prefilter::Choice::Memchr2(pre) => Pre::new(pre),
            prefilter::Choice::Memchr3(pre) => Pre::new(pre),
            prefilter::Choice::Memmem(pre) => Pre::new(pre),
            prefilter::Choice::Teddy(pre) => Pre::new(pre),
            prefilter::Choice::ByteSet(pre) => Pre::new(pre),
            prefilter::Choice::AhoCorasick(pre) => Pre::new(pre),
        };
        Some(strat)
    }

    /// Attempts to extract an alternation of literals, and if it's deemed
    /// worth doing, returns an Aho-Corasick prefilter as a strategy.
    ///
    /// And currently, this only returns something when 'hirs.len() == 1'. This
    /// could in theory do something if there are multiple HIRs where all of
    /// them are alternation of literals, but I haven't had the time to go down
    /// that path yet.
    fn from_alternation_literals(
        info: &RegexInfo,
        hirs: &[&Hir],
    ) -> Option<Arc<dyn Strategy>> {
        use crate::util::prefilter::AhoCorasick;

        let lits = crate::meta::literal::alternation_literals(info, hirs)?;
        let ac = AhoCorasick::new(MatchKind::LeftmostFirst, &lits)?;
        Some(Pre::new(ac))
    }
}

// This implements Strategy for anything that implements PrefilterI.
//
// Note that this must only be used for regexes of length 1. Multi-regexes
// don't work here. The prefilter interface only provides the span of a match
// and not the pattern ID. (I did consider making it more expressive, but I
// couldn't figure out how to tie everything together elegantly.) Thus, so long
// as the regex only contains one pattern, we can simply assume that a match
// corresponds to PatternID::ZERO. And indeed, that's what we do here.
//
// In practice, since this impl is used to report matches directly and thus
// completely bypasses the regex engine, we only wind up using this under the
// following restrictions:
//
// * There must be only one pattern. As explained above.
// * The literal sequence must be finite and only contain exact literals.
// * There must not be any look-around assertions. If there are, the literals
// extracted might be exact, but a match doesn't necessarily imply an overall
// match. As a trivial example, 'foo\bbar' does not match 'foobar'.
// * The pattern must not have any explicit capturing groups. If it does, the
// caller might expect them to be resolved. e.g., 'foo(bar)'.
//
// So when all of those things are true, we use a prefilter directly as a
// strategy.
//
// In the case where the number of patterns is more than 1, we don't use this
// but do use a special Aho-Corasick strategy if all of the regexes are just
// simple literals or alternations of literals. (We also use the Aho-Corasick
// strategy when len(patterns)==1 if the number of literals is large. In that
// case, literal extraction gives up and will return an infinite set.)
impl<P: PrefilterI> Strategy for Pre<P> {
    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn group_info(&self) -> &GroupInfo {
        &self.group_info
    }

    fn create_cache(&self) -> Cache {
        Cache {
            capmatches: Captures::all(self.group_info().clone()),
            pikevm: wrappers::PikeVMCache::none(),
            backtrack: wrappers::BoundedBacktrackerCache::none(),
            onepass: wrappers::OnePassCache::none(),
            hybrid: wrappers::HybridCache::none(),
            revhybrid: wrappers::ReverseHybridCache::none(),
        }
    }

    fn reset_cache(&self, _cache: &mut Cache) {}

    fn is_accelerated(&self) -> bool {
        self.pre.is_fast()
    }

    fn memory_usage(&self) -> usize {
        self.pre.memory_usage()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search(&self, _cache: &mut Cache, input: &Input<'_>) -> Option<Match> {
        if input.is_done() {
            return None;
        }
        if input.get_anchored().is_anchored() {
            return self
                .pre
                .prefix(input.haystack(), input.get_span())
                .map(|sp| Match::new(PatternID::ZERO, sp));
        }
        self.pre
            .find(input.haystack(), input.get_span())
            .map(|sp| Match::new(PatternID::ZERO, sp))
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_half(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<HalfMatch> {
        self.search(cache, input).map(|m| HalfMatch::new(m.pattern(), m.end()))
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn is_match(&self, cache: &mut Cache, input: &Input<'_>) -> bool {
        self.search(cache, input).is_some()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_slots(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        slots: &mut [Option<NonMaxUsize>],
    ) -> Option<PatternID> {
        let m = self.search(cache, input)?;
        if let Some(slot) = slots.get_mut(0) {
            *slot = NonMaxUsize::new(m.start());
        }
        if let Some(slot) = slots.get_mut(1) {
            *slot = NonMaxUsize::new(m.end());
        }
        Some(m.pattern())
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn which_overlapping_matches(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        patset: &mut PatternSet,
    ) {
        if self.search(cache, input).is_some() {
            patset.insert(PatternID::ZERO);
        }
    }
}

#[derive(Debug)]
struct Core {
    info: RegexInfo,
    pre: Option<Prefilter>,
    nfa: NFA,
    nfarev: Option<NFA>,
    pikevm: wrappers::PikeVM,
    backtrack: wrappers::BoundedBacktracker,
    onepass: wrappers::OnePass,
    hybrid: wrappers::Hybrid,
    dfa: wrappers::DFA,
}

impl Core {
    fn new(
        info: RegexInfo,
        pre: Option<Prefilter>,
        hirs: &[&Hir],
    ) -> Result<Core, BuildError> {
        let mut lookm = LookMatcher::new();
        lookm.set_line_terminator(info.config().get_line_terminator());
        let thompson_config = thompson::Config::new()
            .utf8(info.config().get_utf8_empty())
            .nfa_size_limit(info.config().get_nfa_size_limit())
            .shrink(false)
            .which_captures(info.config().get_which_captures())
            .look_matcher(lookm);
        let nfa = thompson::Compiler::new()
            .configure(thompson_config.clone())
            .build_many_from_hir(hirs)
            .map_err(BuildError::nfa)?;
        // It's possible for the PikeVM or the BB to fail to build, even though
        // at this point, we already have a full NFA in hand. They can fail
        // when a Unicode word boundary is used but where Unicode word boundary
        // support is disabled at compile time, thus making it impossible to
        // match. (Construction can also fail if the NFA was compiled without
        // captures, but we always enable that above.)
        let pikevm = wrappers::PikeVM::new(&info, pre.clone(), &nfa)?;
        let backtrack =
            wrappers::BoundedBacktracker::new(&info, pre.clone(), &nfa)?;
        // The onepass engine can of course fail to build, but we expect it to
        // fail in many cases because it is an optimization that doesn't apply
        // to all regexes. The 'OnePass' wrapper encapsulates this failure (and
        // logs a message if it occurs).
        let onepass = wrappers::OnePass::new(&info, &nfa);
        // We try to encapsulate whether a particular regex engine should be
        // used within each respective wrapper, but the DFAs need a reverse NFA
        // to build itself, and we really do not want to build a reverse NFA if
        // we know we aren't going to use the lazy DFA. So we do a config check
        // up front, which is in practice the only way we won't try to use the
        // DFA.
        let (nfarev, hybrid, dfa) =
            if !info.config().get_hybrid() && !info.config().get_dfa() {
                (None, wrappers::Hybrid::none(), wrappers::DFA::none())
            } else {
                // FIXME: Technically, we don't quite yet KNOW that we need
                // a reverse NFA. It's possible for the DFAs below to both
                // fail to build just based on the forward NFA. In which case,
                // building the reverse NFA was totally wasted work. But...
                // fixing this requires breaking DFA construction apart into
                // two pieces: one for the forward part and another for the
                // reverse part. Quite annoying. Making it worse, when building
                // both DFAs fails, it's quite likely that the NFA is large and
                // that it will take quite some time to build the reverse NFA
                // too. So... it's really probably worth it to do this!
                let nfarev = thompson::Compiler::new()
                    // Currently, reverse NFAs don't support capturing groups,
                    // so we MUST disable them. But even if we didn't have to,
                    // we would, because nothing in this crate does anything
                    // useful with capturing groups in reverse. And of course,
                    // the lazy DFA ignores capturing groups in all cases.
                    .configure(
                        thompson_config
                            .clone()
                            .which_captures(WhichCaptures::None)
                            .reverse(true),
                    )
                    .build_many_from_hir(hirs)
                    .map_err(BuildError::nfa)?;
                let dfa = if !info.config().get_dfa() {
                    wrappers::DFA::none()
                } else {
                    wrappers::DFA::new(&info, pre.clone(), &nfa, &nfarev)
                };
                let hybrid = if !info.config().get_hybrid() {
                    wrappers::Hybrid::none()
                } else if dfa.is_some() {
                    debug!("skipping lazy DFA because we have a full DFA");
                    wrappers::Hybrid::none()
                } else {
                    wrappers::Hybrid::new(&info, pre.clone(), &nfa, &nfarev)
                };
                (Some(nfarev), hybrid, dfa)
            };
        Ok(Core {
            info,
            pre,
            nfa,
            nfarev,
            pikevm,
            backtrack,
            onepass,
            hybrid,
            dfa,
        })
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn try_search_mayfail(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<Result<Option<Match>, RetryFailError>> {
        if let Some(e) = self.dfa.get(input) {
            trace!("using full DFA for search at {:?}", input.get_span());
            Some(e.try_search(input))
        } else if let Some(e) = self.hybrid.get(input) {
            trace!("using lazy DFA for search at {:?}", input.get_span());
            Some(e.try_search(&mut cache.hybrid, input))
        } else {
            None
        }
    }

    fn search_nofail(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<Match> {
        let caps = &mut cache.capmatches;
        caps.set_pattern(None);
        // We manually inline 'try_search_slots_nofail' here because we need to
        // borrow from 'cache.capmatches' in this method, but if we do, then
        // we can't pass 'cache' wholesale to to 'try_slots_no_hybrid'. It's a
        // classic example of how the borrow checker inhibits decomposition.
        // There are of course work-arounds (more types and/or interior
        // mutability), but that's more annoying than this IMO.
        let pid = if let Some(ref e) = self.onepass.get(input) {
            trace!("using OnePass for search at {:?}", input.get_span());
            e.search_slots(&mut cache.onepass, input, caps.slots_mut())
        } else if let Some(ref e) = self.backtrack.get(input) {
            trace!(
                "using BoundedBacktracker for search at {:?}",
                input.get_span()
            );
            e.search_slots(&mut cache.backtrack, input, caps.slots_mut())
        } else {
            trace!("using PikeVM for search at {:?}", input.get_span());
            let e = self.pikevm.get();
            e.search_slots(&mut cache.pikevm, input, caps.slots_mut())
        };
        caps.set_pattern(pid);
        caps.get_match()
    }

    fn search_half_nofail(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<HalfMatch> {
        // Only the lazy/full DFA returns half-matches, since the DFA requires
        // a reverse scan to find the start position. These fallback regex
        // engines can find the start and end in a single pass, so we just do
        // that and throw away the start offset to conform to the API.
        let m = self.search_nofail(cache, input)?;
        Some(HalfMatch::new(m.pattern(), m.end()))
    }

    fn search_slots_nofail(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        slots: &mut [Option<NonMaxUsize>],
    ) -> Option<PatternID> {
        if let Some(ref e) = self.onepass.get(input) {
            trace!(
                "using OnePass for capture search at {:?}",
                input.get_span()
            );
            e.search_slots(&mut cache.onepass, input, slots)
        } else if let Some(ref e) = self.backtrack.get(input) {
            trace!(
                "using BoundedBacktracker for capture search at {:?}",
                input.get_span()
            );
            e.search_slots(&mut cache.backtrack, input, slots)
        } else {
            trace!(
                "using PikeVM for capture search at {:?}",
                input.get_span()
            );
            let e = self.pikevm.get();
            e.search_slots(&mut cache.pikevm, input, slots)
        }
    }

    fn is_match_nofail(&self, cache: &mut Cache, input: &Input<'_>) -> bool {
        if let Some(ref e) = self.onepass.get(input) {
            trace!(
                "using OnePass for is-match search at {:?}",
                input.get_span()
            );
            e.search_slots(&mut cache.onepass, input, &mut []).is_some()
        } else if let Some(ref e) = self.backtrack.get(input) {
            trace!(
                "using BoundedBacktracker for is-match search at {:?}",
                input.get_span()
            );
            e.is_match(&mut cache.backtrack, input)
        } else {
            trace!(
                "using PikeVM for is-match search at {:?}",
                input.get_span()
            );
            let e = self.pikevm.get();
            e.is_match(&mut cache.pikevm, input)
        }
    }

    fn is_capture_search_needed(&self, slots_len: usize) -> bool {
        slots_len > self.nfa.group_info().implicit_slot_len()
    }
}

impl Strategy for Core {
    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn group_info(&self) -> &GroupInfo {
        self.nfa.group_info()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn create_cache(&self) -> Cache {
        Cache {
            capmatches: Captures::all(self.group_info().clone()),
            pikevm: self.pikevm.create_cache(),
            backtrack: self.backtrack.create_cache(),
            onepass: self.onepass.create_cache(),
            hybrid: self.hybrid.create_cache(),
            revhybrid: wrappers::ReverseHybridCache::none(),
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn reset_cache(&self, cache: &mut Cache) {
        cache.pikevm.reset(&self.pikevm);
        cache.backtrack.reset(&self.backtrack);
        cache.onepass.reset(&self.onepass);
        cache.hybrid.reset(&self.hybrid);
    }

    fn is_accelerated(&self) -> bool {
        self.pre.as_ref().map_or(false, |pre| pre.is_fast())
    }

    fn memory_usage(&self) -> usize {
        self.info.memory_usage()
            + self.pre.as_ref().map_or(0, |pre| pre.memory_usage())
            + self.nfa.memory_usage()
            + self.nfarev.as_ref().map_or(0, |nfa| nfa.memory_usage())
            + self.onepass.memory_usage()
            + self.dfa.memory_usage()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search(&self, cache: &mut Cache, input: &Input<'_>) -> Option<Match> {
        // We manually inline try_search_mayfail here because letting the
        // compiler do it seems to produce pretty crappy codegen.
        return if let Some(e) = self.dfa.get(input) {
            trace!("using full DFA for full search at {:?}", input.get_span());
            match e.try_search(input) {
                Ok(x) => x,
                Err(_err) => {
                    trace!("full DFA search failed: {_err}");
                    self.search_nofail(cache, input)
                }
            }
        } else if let Some(e) = self.hybrid.get(input) {
            trace!("using lazy DFA for full search at {:?}", input.get_span());
            match e.try_search(&mut cache.hybrid, input) {
                Ok(x) => x,
                Err(_err) => {
                    trace!("lazy DFA search failed: {_err}");
                    self.search_nofail(cache, input)
                }
            }
        } else {
            self.search_nofail(cache, input)
        };
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_half(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<HalfMatch> {
        // The main difference with 'search' is that if we're using a DFA, we
        // can use a single forward scan without needing to run the reverse
        // DFA.
        if let Some(e) = self.dfa.get(input) {
            trace!("using full DFA for half search at {:?}", input.get_span());
            match e.try_search_half_fwd(input) {
                Ok(x) => x,
                Err(_err) => {
                    trace!("full DFA half search failed: {_err}");
                    self.search_half_nofail(cache, input)
                }
            }
        } else if let Some(e) = self.hybrid.get(input) {
            trace!("using lazy DFA for half search at {:?}", input.get_span());
            match e.try_search_half_fwd(&mut cache.hybrid, input) {
                Ok(x) => x,
                Err(_err) => {
                    trace!("lazy DFA half search failed: {_err}");
                    self.search_half_nofail(cache, input)
                }
            }
        } else {
            self.search_half_nofail(cache, input)
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn is_match(&self, cache: &mut Cache, input: &Input<'_>) -> bool {
        if let Some(e) = self.dfa.get(input) {
            trace!(
                "using full DFA for is-match search at {:?}",
                input.get_span()
            );
            match e.try_search_half_fwd(input) {
                Ok(x) => x.is_some(),
                Err(_err) => {
                    trace!("full DFA half search failed: {_err}");
                    self.is_match_nofail(cache, input)
                }
            }
        } else if let Some(e) = self.hybrid.get(input) {
            trace!(
                "using lazy DFA for is-match search at {:?}",
                input.get_span()
            );
            match e.try_search_half_fwd(&mut cache.hybrid, input) {
                Ok(x) => x.is_some(),
                Err(_err) => {
                    trace!("lazy DFA half search failed: {_err}");
                    self.is_match_nofail(cache, input)
                }
            }
        } else {
            self.is_match_nofail(cache, input)
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_slots(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        slots: &mut [Option<NonMaxUsize>],
    ) -> Option<PatternID> {
        // Even if the regex has explicit capture groups, if the caller didn't
        // provide any explicit slots, then it doesn't make sense to try and do
        // extra work to get offsets for those slots. Ideally the caller should
        // realize this and not call this routine in the first place, but alas,
        // we try to save the caller from themselves if they do.
        if !self.is_capture_search_needed(slots.len()) {
            trace!("asked for slots unnecessarily, trying fast path");
            let m = self.search(cache, input)?;
            copy_match_to_slots(m, slots);
            return Some(m.pattern());
        }
        // If the onepass DFA is available for this search (which only happens
        // when it's anchored), then skip running a fallible DFA. The onepass
        // DFA isn't as fast as a full or lazy DFA, but it is typically quite
        // a bit faster than the backtracker or the PikeVM. So it isn't as
        // advantageous to try and do a full/lazy DFA scan first.
        //
        // We still theorize that it's better to do a full/lazy DFA scan, even
        // when it's anchored, because it's usually much faster and permits us
        // to say "no match" much more quickly. This does hurt the case of,
        // say, parsing each line in a log file into capture groups, because
        // in that case, the line always matches. So the lazy DFA scan is
        // usually just wasted work. But, the lazy DFA is usually quite fast
        // and doesn't cost too much here.
        if self.onepass.get(&input).is_some() {
            return self.search_slots_nofail(cache, &input, slots);
        }
        let m = match self.try_search_mayfail(cache, input) {
            Some(Ok(Some(m))) => m,
            Some(Ok(None)) => return None,
            Some(Err(_err)) => {
                trace!("fast capture search failed: {_err}");
                return self.search_slots_nofail(cache, input, slots);
            }
            None => {
                return self.search_slots_nofail(cache, input, slots);
            }
        };
        // At this point, now that we've found the bounds of the
        // match, we need to re-run something that can resolve
        // capturing groups. But we only need to run on it on the
        // match bounds and not the entire haystack.
        trace!(
            "match found at {}..{} in capture search, \
		  	 using another engine to find captures",
            m.start(),
            m.end(),
        );
        let input = input
            .clone()
            .span(m.start()..m.end())
            .anchored(Anchored::Pattern(m.pattern()));
        Some(
            self.search_slots_nofail(cache, &input, slots)
                .expect("should find a match"),
        )
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn which_overlapping_matches(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        patset: &mut PatternSet,
    ) {
        if let Some(e) = self.dfa.get(input) {
            trace!(
                "using full DFA for overlapping search at {:?}",
                input.get_span()
            );
            let _err = match e.try_which_overlapping_matches(input, patset) {
                Ok(()) => return,
                Err(err) => err,
            };
            trace!("fast overlapping search failed: {_err}");
        } else if let Some(e) = self.hybrid.get(input) {
            trace!(
                "using lazy DFA for overlapping search at {:?}",
                input.get_span()
            );
            let _err = match e.try_which_overlapping_matches(
                &mut cache.hybrid,
                input,
                patset,
            ) {
                Ok(()) => {
                    return;
                }
                Err(err) => err,
            };
            trace!("fast overlapping search failed: {_err}");
        }
        trace!(
            "using PikeVM for overlapping search at {:?}",
            input.get_span()
        );
        let e = self.pikevm.get();
        e.which_overlapping_matches(&mut cache.pikevm, input, patset)
    }
}

#[derive(Debug)]
struct ReverseAnchored {
    core: Core,
}

impl ReverseAnchored {
    fn new(core: Core) -> Result<ReverseAnchored, Core> {
        if !core.info.is_always_anchored_end() {
            debug!(
                "skipping reverse anchored optimization because \
				 the regex is not always anchored at the end"
            );
            return Err(core);
        }
        // Note that the caller can still request an anchored search even when
        // the regex isn't anchored at the start. We detect that case in the
        // search routines below and just fallback to the core engine. This
        // is fine because both searches are anchored. It's just a matter of
        // picking one. Falling back to the core engine is a little simpler,
        // since if we used the reverse anchored approach, we'd have to add an
        // extra check to ensure the match reported starts at the place where
        // the caller requested the search to start.
        if core.info.is_always_anchored_start() {
            debug!(
                "skipping reverse anchored optimization because \
				 the regex is also anchored at the start"
            );
            return Err(core);
        }
        // Only DFAs can do reverse searches (currently), so we need one of
        // them in order to do this optimization. It's possible (although
        // pretty unlikely) that we have neither and need to give up.
        if !core.hybrid.is_some() && !core.dfa.is_some() {
            debug!(
                "skipping reverse anchored optimization because \
				 we don't have a lazy DFA or a full DFA"
            );
            return Err(core);
        }
        Ok(ReverseAnchored { core })
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn try_search_half_anchored_rev(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Result<Option<HalfMatch>, RetryFailError> {
        // We of course always want an anchored search. In theory, the
        // underlying regex engines should automatically enable anchored
        // searches since the regex is itself anchored, but this more clearly
        // expresses intent and is always correct.
        let input = input.clone().anchored(Anchored::Yes);
        if let Some(e) = self.core.dfa.get(&input) {
            trace!(
                "using full DFA for reverse anchored search at {:?}",
                input.get_span()
            );
            e.try_search_half_rev(&input)
        } else if let Some(e) = self.core.hybrid.get(&input) {
            trace!(
                "using lazy DFA for reverse anchored search at {:?}",
                input.get_span()
            );
            e.try_search_half_rev(&mut cache.hybrid, &input)
        } else {
            unreachable!("ReverseAnchored always has a DFA")
        }
    }
}

// Note that in this impl, we don't check that 'input.end() ==
// input.haystack().len()'. In particular, when that condition is false, a
// match is always impossible because we know that the regex is always anchored
// at the end (or else 'ReverseAnchored' won't be built). We don't check that
// here because the 'Regex' wrapper actually does that for us in all cases.
// Thus, in this impl, we can actually assume that the end position in 'input'
// is equivalent to the length of the haystack.
impl Strategy for ReverseAnchored {
    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn group_info(&self) -> &GroupInfo {
        self.core.group_info()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn create_cache(&self) -> Cache {
        self.core.create_cache()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn reset_cache(&self, cache: &mut Cache) {
        self.core.reset_cache(cache);
    }

    fn is_accelerated(&self) -> bool {
        // Since this is anchored at the end, a reverse anchored search is
        // almost certainly guaranteed to result in a much faster search than
        // a standard forward search.
        true
    }

    fn memory_usage(&self) -> usize {
        self.core.memory_usage()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search(&self, cache: &mut Cache, input: &Input<'_>) -> Option<Match> {
        if input.get_anchored().is_anchored() {
            return self.core.search(cache, input);
        }
        match self.try_search_half_anchored_rev(cache, input) {
            Err(_err) => {
                trace!("fast reverse anchored search failed: {_err}");
                self.core.search_nofail(cache, input)
            }
            Ok(None) => None,
            Ok(Some(hm)) => {
                Some(Match::new(hm.pattern(), hm.offset()..input.end()))
            }
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_half(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<HalfMatch> {
        if input.get_anchored().is_anchored() {
            return self.core.search_half(cache, input);
        }
        match self.try_search_half_anchored_rev(cache, input) {
            Err(_err) => {
                trace!("fast reverse anchored search failed: {_err}");
                self.core.search_half_nofail(cache, input)
            }
            Ok(None) => None,
            Ok(Some(hm)) => {
                // Careful here! 'try_search_half' is a *forward* search that
                // only cares about the *end* position of a match. But
                // 'hm.offset()' is actually the start of the match. So we
                // actually just throw that away here and, since we know we
                // have a match, return the only possible position at which a
                // match can occur: input.end().
                Some(HalfMatch::new(hm.pattern(), input.end()))
            }
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn is_match(&self, cache: &mut Cache, input: &Input<'_>) -> bool {
        if input.get_anchored().is_anchored() {
            return self.core.is_match(cache, input);
        }
        match self.try_search_half_anchored_rev(cache, input) {
            Err(_err) => {
                trace!("fast reverse anchored search failed: {_err}");
                self.core.is_match_nofail(cache, input)
            }
            Ok(None) => false,
            Ok(Some(_)) => true,
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_slots(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        slots: &mut [Option<NonMaxUsize>],
    ) -> Option<PatternID> {
        if input.get_anchored().is_anchored() {
            return self.core.search_slots(cache, input, slots);
        }
        match self.try_search_half_anchored_rev(cache, input) {
            Err(_err) => {
                trace!("fast reverse anchored search failed: {_err}");
                self.core.search_slots_nofail(cache, input, slots)
            }
            Ok(None) => None,
            Ok(Some(hm)) => {
                if !self.core.is_capture_search_needed(slots.len()) {
                    trace!("asked for slots unnecessarily, skipping captures");
                    let m = Match::new(hm.pattern(), hm.offset()..input.end());
                    copy_match_to_slots(m, slots);
                    return Some(m.pattern());
                }
                let start = hm.offset();
                let input = input
                    .clone()
                    .span(start..input.end())
                    .anchored(Anchored::Pattern(hm.pattern()));
                self.core.search_slots_nofail(cache, &input, slots)
            }
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn which_overlapping_matches(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        patset: &mut PatternSet,
    ) {
        // It seems like this could probably benefit from a reverse anchored
        // optimization, perhaps by doing an overlapping reverse search (which
        // the DFAs do support). I haven't given it much thought though, and
        // I'm currently focus more on the single pattern case.
        self.core.which_overlapping_matches(cache, input, patset)
    }
}

#[derive(Debug)]
struct ReverseSuffix {
    core: Core,
    pre: Prefilter,
}

impl ReverseSuffix {
    fn new(core: Core, hirs: &[&Hir]) -> Result<ReverseSuffix, Core> {
        if !core.info.config().get_auto_prefilter() {
            debug!(
                "skipping reverse suffix optimization because \
                 automatic prefilters are disabled"
            );
            return Err(core);
        }
        // Also like the reverse inner optimization, a reverse suffix encodes
        // leftmost-first match semantics.
        if core.info.config().get_match_kind() != MatchKind::LeftmostFirst {
            debug!(
                "skipping reverse suffix optimization because \
				 match kind is {:?} but this only supports leftmost-first",
                core.info.config().get_match_kind(),
            );
            return Err(core);
        }
        // Like the reverse inner optimization, we don't do this for regexes
        // that are always anchored. It could lead to scanning too much, but
        // could say "no match" much more quickly than running the regex
        // engine if the initial literal scan doesn't match. With that said,
        // the reverse suffix optimization has lower overhead, since it only
        // requires a reverse scan after a literal match to confirm or reject
        // the match. (Although, in the case of confirmation, it then needs to
        // do another forward scan to find the end position.)
        //
        // Note that the caller can still request an anchored search even
        // when the regex isn't anchored. We detect that case in the search
        // routines below and just fallback to the core engine. Currently this
        // optimization assumes all searches are unanchored, so if we do want
        // to enable this optimization for anchored searches, it will need a
        // little work to support it.
        if core.info.is_always_anchored_start() {
            debug!(
                "skipping reverse suffix optimization because \
				 the regex is always anchored at the start",
            );
            return Err(core);
        }
        // Only DFAs can do reverse searches (currently), so we need one of
        // them in order to do this optimization. It's possible (although
        // pretty unlikely) that we have neither and need to give up.
        if !core.hybrid.is_some() && !core.dfa.is_some() {
            debug!(
                "skipping reverse suffix optimization because \
				 we don't have a lazy DFA or a full DFA"
            );
            return Err(core);
        }
        if core.pre.as_ref().map_or(false, |p| p.is_fast()) {
            debug!(
                "skipping reverse suffix optimization because \
				 we already have a prefilter that we think is fast"
            );
            return Err(core);
        }
        let kind = core.info.config().get_match_kind();
        let suffixes = crate::util::prefilter::suffixes(kind, hirs);
        let lcs = match suffixes.longest_common_suffix() {
            None => {
                debug!(
                    "skipping reverse suffix optimization because \
                     a longest common suffix could not be found",
                );
                return Err(core);
            }
            Some(lcs) if lcs.is_empty() => {
                debug!(
                    "skipping reverse suffix optimization because \
                     the longest common suffix is the empty string",
                );
                return Err(core);
            }
            Some(lcs) => lcs,
        };
        let no_internal_suffix =
            ReverseSuffix::has_no_internal_suffix(&core.info, hirs, &lcs);
        let guarded_internal_suffix =
            ReverseSuffix::has_guarded_internal_suffix(hirs, &lcs);
        let pre = match Prefilter::new(kind, core::slice::from_ref(&lcs)) {
            Some(pre) => pre,
            None => {
                debug!(
                    "skipping reverse suffix optimization because \
                     a prefilter could not be constructed from the \
                     longest common suffix",
                );
                return Err(core);
            }
        };
        if !pre.is_fast() {
            debug!(
                "skipping reverse suffix optimization because \
				 while we have a suffix prefilter, it is not \
				 believed to be 'fast'"
            );
            return Err(core);
        }
        if !no_internal_suffix
            && !guarded_internal_suffix
            && !ReverseSuffix::is_early_return_safe(&core.nfa)
        {
            debug!(
                "skipping reverse suffix optimization because \
                 an earlier suffix match could be a complete match \
                 inside of a larger match"
            );
            return Err(core);
        }
        Ok(ReverseSuffix { core, pre })
    }

    fn is_early_return_safe(nfa: &NFA) -> bool {
        ReverseSuffixOverlap::new(nfa).map_or(false, |mut x| x.is_safe())
    }

    fn has_no_internal_suffix(
        info: &RegexInfo,
        hirs: &[&Hir],
        suffix: &[u8],
    ) -> bool {
        if hirs.len() != 1 || suffix.is_empty() {
            return false;
        }
        let prefix = match ReverseSuffix::strip_literal_suffix(hirs[0], suffix)
        {
            None => return false,
            Some(prefix) => prefix,
        };
        let mut lookm = LookMatcher::new();
        lookm.set_line_terminator(info.config().get_line_terminator());
        let thompson_config = thompson::Config::new()
            .reverse(false)
            .utf8(info.config().get_utf8_empty())
            .nfa_size_limit(info.config().get_nfa_size_limit())
            .shrink(false)
            .which_captures(WhichCaptures::None)
            .look_matcher(lookm);
        let prefix_nfa = match thompson::Compiler::new()
            .configure(thompson_config)
            .build_from_hir(&prefix)
        {
            Err(_) => return false,
            Ok(prefix_nfa) => prefix_nfa,
        };
        PrefixLiteralOverlap::new(&prefix_nfa, suffix)
            .map_or(false, |mut x| !x.contains())
    }

    fn has_guarded_internal_suffix(hirs: &[&Hir], suffix: &[u8]) -> bool {
        if hirs.len() != 1 || suffix.is_empty() {
            return false;
        }
        let (core, looks) = match ReverseSuffix::strip_trailing_looks(hirs[0])
        {
            None => return false,
            Some(x) => x,
        };
        let word = match looks
            .iter()
            .find_map(|&look| ReverseSuffix::look_word_kind(look))
        {
            None => return false,
            Some(word) => word,
        };
        if !ReverseSuffix::literal_is_word(suffix, word) {
            return false;
        }
        let prefix = match ReverseSuffix::strip_literal_suffix(&core, suffix) {
            None => return false,
            Some(prefix) => prefix,
        };
        ReverseSuffix::hir_consumes_only_word(&prefix, word)
    }

    fn strip_trailing_looks(hir: &Hir) -> Option<(Hir, Vec<HirLook>)> {
        match hir.kind() {
            HirKind::Capture(capture) => {
                ReverseSuffix::strip_trailing_looks(&capture.sub)
            }
            HirKind::Concat(hirs) => {
                let prefix_len = hirs
                    .iter()
                    .rposition(|hir| !matches!(hir.kind(), HirKind::Look(_)))
                    .map_or(0, |i| i + 1);
                let looks = hirs[prefix_len..]
                    .iter()
                    .filter_map(|hir| match hir.kind() {
                        HirKind::Look(look) => Some(*look),
                        _ => None,
                    })
                    .collect::<Vec<_>>();
                if looks.is_empty() {
                    return None;
                }
                Some((Hir::concat(hirs[..prefix_len].to_vec()), looks))
            }
            HirKind::Look(look) => Some((Hir::empty(), vec![*look])),
            _ => None,
        }
    }

    fn hir_consumes_only_word(hir: &Hir, word: WordKind) -> bool {
        match hir.kind() {
            HirKind::Empty | HirKind::Look(_) => true,
            HirKind::Literal(lit) => {
                ReverseSuffix::literal_is_word(&lit.0, word)
            }
            HirKind::Class(Class::Bytes(cls)) => {
                for range in cls.ranges() {
                    for byte in range.start()..=range.end() {
                        if !ReverseSuffix::byte_is_word(byte, word) {
                            return false;
                        }
                    }
                }
                true
            }
            HirKind::Class(Class::Unicode(cls)) => {
                for range in cls.ranges() {
                    let start = u32::from(range.start());
                    let end = u32::from(range.end());
                    for cp in start..=end {
                        let ch = match char::from_u32(cp) {
                            None => continue,
                            Some(ch) => ch,
                        };
                        if !ReverseSuffix::char_is_word(ch, word) {
                            return false;
                        }
                    }
                }
                true
            }
            HirKind::Repetition(rep) => {
                ReverseSuffix::hir_consumes_only_word(&rep.sub, word)
            }
            HirKind::Capture(capture) => {
                ReverseSuffix::hir_consumes_only_word(&capture.sub, word)
            }
            HirKind::Concat(hirs) | HirKind::Alternation(hirs) => hirs
                .iter()
                .all(|hir| ReverseSuffix::hir_consumes_only_word(hir, word)),
        }
    }

    fn look_word_kind(look: HirLook) -> Option<WordKind> {
        match look {
            HirLook::WordUnicode
            | HirLook::WordEndUnicode
            | HirLook::WordEndHalfUnicode
            | HirLook::WordStartUnicode
            | HirLook::WordStartHalfUnicode => Some(WordKind::Unicode),
            HirLook::WordAscii
            | HirLook::WordEndAscii
            | HirLook::WordEndHalfAscii
            | HirLook::WordStartAscii
            | HirLook::WordStartHalfAscii => Some(WordKind::Ascii),
            _ => None,
        }
    }

    fn literal_is_word(lit: &[u8], word: WordKind) -> bool {
        match word {
            WordKind::Ascii => {
                lit.iter().all(|&byte| regex_syntax::is_word_byte(byte))
            }
            WordKind::Unicode => core::str::from_utf8(lit)
                .map_or(false, |s| {
                    s.chars().all(|ch| ReverseSuffix::char_is_word(ch, word))
                }),
        }
    }

    fn byte_is_word(byte: u8, word: WordKind) -> bool {
        match word {
            WordKind::Ascii => regex_syntax::is_word_byte(byte),
            WordKind::Unicode => {
                byte.is_ascii() && regex_syntax::is_word_byte(byte)
            }
        }
    }

    fn char_is_word(ch: char, word: WordKind) -> bool {
        match word {
            WordKind::Ascii => {
                ch.is_ascii()
                    && regex_syntax::is_word_byte(u8::try_from(ch).unwrap())
            }
            WordKind::Unicode => {
                regex_syntax::try_is_word_character(ch).unwrap_or(false)
            }
        }
    }

    fn strip_literal_suffix(hir: &Hir, suffix: &[u8]) -> Option<Hir> {
        match hir.kind() {
            HirKind::Literal(lit) => {
                let bytes = &lit.0;
                let prefix_len = bytes.len().checked_sub(suffix.len())?;
                if !bytes[prefix_len..].eq(suffix) {
                    return None;
                }
                Some(Hir::literal(bytes[..prefix_len].to_vec()))
            }
            HirKind::Capture(capture) => {
                ReverseSuffix::strip_literal_suffix(&capture.sub, suffix)
            }
            HirKind::Concat(hirs) => {
                let (last, init) = hirs.split_last()?;
                let last = ReverseSuffix::strip_literal_suffix(last, suffix)?;
                let mut prefix = init.to_vec();
                prefix.push(last);
                Some(Hir::concat(prefix))
            }
            _ => None,
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn try_search_half_start(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Result<Option<HalfMatch>, RetryError> {
        let mut span = input.get_span();
        let mut min_start = 0;
        loop {
            let litmatch = match self.pre.find(input.haystack(), span) {
                None => break,
                Some(span) => span,
            };
            trace!("reverse suffix scan found suffix match at {litmatch:?}");
            let revinput = input
                .clone()
                .anchored(Anchored::Yes)
                .span(input.start()..litmatch.end);
            if let Some(hm) =
                self.try_search_half_rev_limited(cache, &revinput, min_start)?
            {
                return Ok(Some(hm));
            }

            if span.start >= span.end {
                break;
            }
            span.start = litmatch.start.checked_add(1).unwrap();
            min_start = litmatch.end;
        }
        Ok(None)
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn try_search_half_fwd(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Result<Option<HalfMatch>, RetryFailError> {
        if let Some(e) = self.core.dfa.get(&input) {
            trace!(
                "using full DFA for forward reverse suffix search at {:?}",
                input.get_span()
            );
            e.try_search_half_fwd(&input)
        } else if let Some(e) = self.core.hybrid.get(&input) {
            trace!(
                "using lazy DFA for forward reverse suffix search at {:?}",
                input.get_span()
            );
            e.try_search_half_fwd(&mut cache.hybrid, &input)
        } else {
            unreachable!("ReverseSuffix always has a DFA")
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn try_search_half_rev_limited(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        min_start: usize,
    ) -> Result<Option<HalfMatch>, RetryError> {
        if let Some(e) = self.core.dfa.get(&input) {
            trace!(
                "using full DFA for reverse suffix search at {:?}, \
                 but will be stopped at {} to avoid quadratic behavior",
                input.get_span(),
                min_start,
            );
            e.try_search_half_rev_limited(&input, min_start)
        } else if let Some(e) = self.core.hybrid.get(&input) {
            trace!(
                "using lazy DFA for reverse suffix search at {:?}, \
                 but will be stopped at {} to avoid quadratic behavior",
                input.get_span(),
                min_start,
            );
            e.try_search_half_rev_limited(&mut cache.hybrid, &input, min_start)
        } else {
            unreachable!("ReverseSuffix always has a DFA")
        }
    }
}

#[derive(Debug)]
struct ReverseSuffixOverlap<'a> {
    full: NFAStateSets<'a>,
}

impl<'a> ReverseSuffixOverlap<'a> {
    const LIMIT: usize = 10_000;

    fn new(nfa: &'a NFA) -> Option<ReverseSuffixOverlap<'a>> {
        Some(ReverseSuffixOverlap { full: NFAStateSets::new(nfa, false)? })
    }

    // Returns false if a non-matching proper prefix of some match can have a
    // proper suffix that is itself a complete match. In that case, returning
    // after the first confirmed suffix can report the interior match instead
    // of the larger leftmost match.
    fn is_safe(&mut self) -> bool {
        let mut seen = BTreeSet::new();
        let mut queue = vec![ReverseSuffixOverlapState {
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
                let next =
                    ReverseSuffixOverlapState { main, suffix, consumed: true };
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
struct ReverseSuffixOverlapState {
    main: Vec<usize>,
    suffix: Vec<usize>,
    consumed: bool,
}

#[derive(Debug)]
struct PrefixLiteralOverlap<'a> {
    prefix: NFAStateSets<'a>,
    literal: &'a [u8],
    fallback: Vec<usize>,
}

impl<'a> PrefixLiteralOverlap<'a> {
    const LIMIT: usize = 10_000;

    fn new(
        nfa: &'a NFA,
        literal: &'a [u8],
    ) -> Option<PrefixLiteralOverlap<'a>> {
        if literal.is_empty() {
            return None;
        }
        Some(PrefixLiteralOverlap {
            prefix: NFAStateSets::new(nfa, true)?,
            literal,
            fallback: PrefixLiteralOverlap::kmp_fallback(literal),
        })
    }

    fn contains(&mut self) -> bool {
        let start = PrefixLiteralOverlapState {
            prefix: self.prefix.start.clone(),
            matched: 0,
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
                if matched == self.literal.len() {
                    return true;
                }
                let next = PrefixLiteralOverlapState { prefix, matched };
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

    fn next_match_len(&self, mut len: usize, byte: u8) -> usize {
        while len > 0 && self.literal[len] != byte {
            len = self.fallback[len - 1];
        }
        if self.literal[len] == byte {
            len += 1;
        }
        len
    }

    fn kmp_fallback(literal: &[u8]) -> Vec<usize> {
        let mut fallback = vec![0; literal.len()];
        let mut len = 0;
        for i in 1..literal.len() {
            while len > 0 && literal[i] != literal[len] {
                len = fallback[len - 1];
            }
            if literal[i] == literal[len] {
                len += 1;
                fallback[i] = len;
            }
        }
        fallback
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct PrefixLiteralOverlapState {
    prefix: Vec<usize>,
    matched: usize,
}

#[derive(Clone, Copy, Debug)]
enum WordKind {
    Ascii,
    Unicode,
}

#[derive(Debug)]
struct NFAStateSets<'a> {
    nfa: &'a NFA,
    start: Vec<usize>,
    can_reach_match: Vec<bool>,
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
            can_reach_match: vec![false; nfa.states().len()],
        };
        sets.start = sets.epsilon_closure(&[nfa.start_anchored()])?;
        sets.can_reach_match = sets.compute_can_reach_match();
        Some(sets)
    }

    fn step(&self, set: &[usize], byte: u8) -> Option<Vec<usize>> {
        let mut next = vec![];
        for &sid in set {
            let sid = StateID::new(sid).ok()?;
            match *self.nfa.state(sid) {
                State::ByteRange { trans } => {
                    if trans.matches_byte(byte) {
                        next.push(trans.next);
                    }
                }
                State::Sparse(ref sparse) => {
                    if let Some(next_sid) = sparse.matches_byte(byte) {
                        next.push(next_sid);
                    }
                }
                State::Dense(ref dense) => {
                    if let Some(next_sid) = dense.matches_byte(byte) {
                        next.push(next_sid);
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
        self.epsilon_closure(&next)
    }

    fn epsilon_closure(&self, set: &[StateID]) -> Option<Vec<usize>> {
        let mut stack = set.to_vec();
        let mut closure = vec![];
        let mut seen = vec![false; self.nfa.states().len()];
        while let Some(sid) = stack.pop() {
            let index = sid.as_usize();
            if seen[index] {
                continue;
            }
            seen[index] = true;
            closure.push(index);
            match *self.nfa.state(sid) {
                State::Look { .. } => return None,
                State::Union { ref alternates } => {
                    stack.extend(alternates.iter().copied());
                }
                State::BinaryUnion { alt1, alt2 } => {
                    stack.push(alt2);
                    stack.push(alt1);
                }
                State::Capture { next, .. } => stack.push(next),
                State::ByteRange { .. }
                | State::Sparse(_)
                | State::Dense(_)
                | State::Fail
                | State::Match { .. } => {}
            }
        }
        closure.sort_unstable();
        Some(closure)
    }

    fn compute_can_reach_match(&self) -> Vec<bool> {
        let mut can = vec![false; self.nfa.states().len()];
        loop {
            let mut changed = false;
            for (sid, state) in self.nfa.states().iter().enumerate() {
                if can[sid] {
                    continue;
                }
                let reaches = match *state {
                    State::Match { .. } => true,
                    State::Look { .. } => false,
                    State::Union { ref alternates } => {
                        alternates.iter().any(|&sid| can[sid.as_usize()])
                    }
                    State::BinaryUnion { alt1, alt2 } => {
                        can[alt1.as_usize()] || can[alt2.as_usize()]
                    }
                    State::Capture { next, .. } => can[next.as_usize()],
                    State::ByteRange { trans } => can[trans.next.as_usize()],
                    State::Sparse(ref sparse) => sparse
                        .transitions
                        .iter()
                        .any(|t| can[t.next.as_usize()]),
                    State::Dense(ref dense) => {
                        dense.iter().any(|t| can[t.next.as_usize()])
                    }
                    State::Fail => false,
                };
                if reaches {
                    can[sid] = true;
                    changed = true;
                }
            }
            if !changed {
                return can;
            }
        }
    }

    fn is_match(&self, set: &[usize]) -> bool {
        set.iter().any(|&sid| {
            matches!(
                *self.nfa.state(StateID::new(sid).unwrap()),
                State::Match { .. }
            )
        })
    }

    fn can_reach_match(&self, set: &[usize]) -> bool {
        set.iter().any(|&sid| self.can_reach_match[sid])
    }

    fn can_extend_nonempty(&self, set: &[usize]) -> bool {
        for &sid in set {
            let sid = StateID::new(sid).unwrap();
            match *self.nfa.state(sid) {
                State::ByteRange { trans } => {
                    if self.can_reach_match[trans.next.as_usize()] {
                        return true;
                    }
                }
                State::Sparse(ref sparse) => {
                    if sparse
                        .transitions
                        .iter()
                        .any(|t| self.can_reach_match[t.next.as_usize()])
                    {
                        return true;
                    }
                }
                State::Dense(ref dense) => {
                    if dense
                        .iter()
                        .any(|t| self.can_reach_match[t.next.as_usize()])
                    {
                        return true;
                    }
                }
                State::Look { .. }
                | State::Union { .. }
                | State::BinaryUnion { .. }
                | State::Capture { .. }
                | State::Fail
                | State::Match { .. } => {}
            }
        }
        false
    }
}

impl Strategy for ReverseSuffix {
    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn group_info(&self) -> &GroupInfo {
        self.core.group_info()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn create_cache(&self) -> Cache {
        self.core.create_cache()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn reset_cache(&self, cache: &mut Cache) {
        self.core.reset_cache(cache);
    }

    fn is_accelerated(&self) -> bool {
        self.pre.is_fast()
    }

    fn memory_usage(&self) -> usize {
        self.core.memory_usage() + self.pre.memory_usage()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search(&self, cache: &mut Cache, input: &Input<'_>) -> Option<Match> {
        if input.get_anchored().is_anchored() {
            return self.core.search(cache, input);
        }
        match self.try_search_half_start(cache, input) {
            Err(RetryError::Quadratic(_err)) => {
                trace!("reverse suffix optimization failed: {_err}");
                self.core.search(cache, input)
            }
            Err(RetryError::Fail(_err)) => {
                trace!("reverse suffix reverse fast search failed: {_err}");
                self.core.search_nofail(cache, input)
            }
            Ok(None) => None,
            Ok(Some(hm_start)) => {
                let fwdinput = input
                    .clone()
                    .anchored(Anchored::Pattern(hm_start.pattern()))
                    .span(hm_start.offset()..input.end());
                match self.try_search_half_fwd(cache, &fwdinput) {
                    Err(_err) => {
                        trace!(
                            "reverse suffix forward fast search failed: {_err}"
                        );
                        self.core.search_nofail(cache, input)
                    }
                    Ok(None) => {
                        unreachable!(
                            "suffix match plus reverse match implies \
						     there must be a match",
                        )
                    }
                    Ok(Some(hm_end)) => Some(Match::new(
                        hm_start.pattern(),
                        hm_start.offset()..hm_end.offset(),
                    )),
                }
            }
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_half(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<HalfMatch> {
        if input.get_anchored().is_anchored() {
            return self.core.search_half(cache, input);
        }
        match self.try_search_half_start(cache, input) {
            Err(RetryError::Quadratic(_err)) => {
                trace!("reverse suffix half optimization failed: {_err}");
                self.core.search_half(cache, input)
            }
            Err(RetryError::Fail(_err)) => {
                trace!(
                    "reverse suffix reverse fast half search failed: {_err}"
                );
                self.core.search_half_nofail(cache, input)
            }
            Ok(None) => None,
            Ok(Some(hm_start)) => {
                // This is a bit subtle. It is tempting to just stop searching
                // at this point and return a half-match with an offset
                // corresponding to where the suffix was found. But the suffix
                // match does not necessarily correspond to the end of the
                // proper leftmost-first match. Consider /[a-z]+ing/ against
                // 'tingling'. The first suffix match is the first 'ing', and
                // the /[a-z]+/ matches the 't'. So if we stopped here, then
                // we'd report 'ting' as the match. But 'tingling' is the
                // correct match because of greediness.
                let fwdinput = input
                    .clone()
                    .anchored(Anchored::Pattern(hm_start.pattern()))
                    .span(hm_start.offset()..input.end());
                match self.try_search_half_fwd(cache, &fwdinput) {
                    Err(_err) => {
                        trace!(
                            "reverse suffix forward fast search failed: {_err}"
                        );
                        self.core.search_half_nofail(cache, input)
                    }
                    Ok(None) => {
                        unreachable!(
                            "suffix match plus reverse match implies \
						     there must be a match",
                        )
                    }
                    Ok(Some(hm_end)) => Some(hm_end),
                }
            }
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn is_match(&self, cache: &mut Cache, input: &Input<'_>) -> bool {
        if input.get_anchored().is_anchored() {
            return self.core.is_match(cache, input);
        }
        match self.try_search_half_start(cache, input) {
            Err(RetryError::Quadratic(_err)) => {
                trace!("reverse suffix half optimization failed: {_err}");
                self.core.is_match_nofail(cache, input)
            }
            Err(RetryError::Fail(_err)) => {
                trace!(
                    "reverse suffix reverse fast half search failed: {_err}"
                );
                self.core.is_match_nofail(cache, input)
            }
            Ok(None) => false,
            Ok(Some(_)) => true,
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_slots(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        slots: &mut [Option<NonMaxUsize>],
    ) -> Option<PatternID> {
        if input.get_anchored().is_anchored() {
            return self.core.search_slots(cache, input, slots);
        }
        if !self.core.is_capture_search_needed(slots.len()) {
            trace!("asked for slots unnecessarily, trying fast path");
            let m = self.search(cache, input)?;
            copy_match_to_slots(m, slots);
            return Some(m.pattern());
        }
        let hm_start = match self.try_search_half_start(cache, input) {
            Err(RetryError::Quadratic(_err)) => {
                trace!("reverse suffix captures optimization failed: {_err}");
                return self.core.search_slots(cache, input, slots);
            }
            Err(RetryError::Fail(_err)) => {
                trace!(
                    "reverse suffix reverse fast captures search failed: \
                        {_err}"
                );
                return self.core.search_slots_nofail(cache, input, slots);
            }
            Ok(None) => return None,
            Ok(Some(hm_start)) => hm_start,
        };
        trace!(
            "match found at {}..{} in capture search, \
		  	 using another engine to find captures",
            hm_start.offset(),
            input.end(),
        );
        let start = hm_start.offset();
        let input = input
            .clone()
            .span(start..input.end())
            .anchored(Anchored::Pattern(hm_start.pattern()));
        self.core.search_slots_nofail(cache, &input, slots)
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn which_overlapping_matches(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        patset: &mut PatternSet,
    ) {
        self.core.which_overlapping_matches(cache, input, patset)
    }
}

#[derive(Debug)]
struct ReverseInner {
    core: Core,
    preinner: Prefilter,
    nfarev: NFA,
    hybrid: wrappers::ReverseHybrid,
    dfa: wrappers::ReverseDFA,
}

#[derive(Debug)]
struct ReverseInnerOverlap<'a> {
    prefix: NFAStateSets<'a>,
    literals: &'a [Vec<u8>],
}

impl<'a> ReverseInnerOverlap<'a> {
    const LIMIT: usize = 10_000;

    fn new(
        prefix: &'a NFA,
        literals: &'a [Vec<u8>],
    ) -> Option<ReverseInnerOverlap<'a>> {
        if literals.iter().any(|lit| lit.is_empty()) {
            return None;
        }
        Some(ReverseInnerOverlap {
            prefix: NFAStateSets::new(prefix, true)?,
            literals,
        })
    }

    // Returns false if an inner literal could begin before an earlier match's
    // prefix reaches the extracted inner position, while a proper suffix has
    // already matched the prefix. In that case, the first confirmed inner
    // literal can belong to a later match instead of the leftmost match.
    fn is_safe(&mut self) -> bool {
        let mut seen = BTreeSet::new();
        let start = ReverseInnerOverlapState {
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

                let next = ReverseInnerOverlapState { main_prefix, suffixes };
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

    fn is_unsafe_boundary(&self, state: &ReverseInnerOverlapState) -> bool {
        if self.prefix.is_match(&state.main_prefix) {
            return false;
        }
        state.suffixes.iter().any(|suffix| {
            self.prefix.is_match(suffix)
                && self.literals.iter().any(|lit| {
                    self.prefix_can_extend_over_literal(
                        &state.main_prefix,
                        lit,
                    )
                })
        })
    }

    fn prefix_can_extend_over_literal(
        &self,
        states: &[usize],
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
struct ReverseInnerOverlapState {
    main_prefix: Vec<usize>,
    suffixes: Vec<Vec<usize>>,
}

impl ReverseInner {
    // The reverse inner early-return proof can get expensive when it explores
    // prefixes with large bounded repeats over broad classes. Small bounded
    // repeats are cheap enough to leave to the precise NFA product analysis.
    const ABSORBING_BOUNDED_REPEAT_LIMIT: u32 = 32;

    fn new(core: Core, hirs: &[&Hir]) -> Result<ReverseInner, Core> {
        if !core.info.config().get_auto_prefilter() {
            debug!(
                "skipping reverse inner optimization because \
                 automatic prefilters are disabled"
            );
            return Err(core);
        }
        // Currently we hard-code the assumption of leftmost-first match
        // semantics. This isn't a huge deal because 'all' semantics tend to
        // only be used for forward overlapping searches with multiple regexes,
        // and this optimization only supports a single pattern at the moment.
        if core.info.config().get_match_kind() != MatchKind::LeftmostFirst {
            debug!(
                "skipping reverse inner optimization because \
				 match kind is {:?} but this only supports leftmost-first",
                core.info.config().get_match_kind(),
            );
            return Err(core);
        }
        // It's likely that a reverse inner scan has too much overhead for it
        // to be worth it when the regex is anchored at the start. It is
        // possible for it to be quite a bit faster if the initial literal
        // scan fails to detect a match, in which case, we can say "no match"
        // very quickly. But this could be undesirable, e.g., scanning too far
        // or when the literal scan matches. If it matches, then confirming the
        // match requires a reverse scan followed by a forward scan to confirm
        // or reject, which is a fair bit of work.
        //
        // Note that the caller can still request an anchored search even
        // when the regex isn't anchored. We detect that case in the search
        // routines below and just fallback to the core engine. Currently this
        // optimization assumes all searches are unanchored, so if we do want
        // to enable this optimization for anchored searches, it will need a
        // little work to support it.
        if core.info.is_always_anchored_start() {
            debug!(
                "skipping reverse inner optimization because \
				 the regex is always anchored at the start",
            );
            return Err(core);
        }
        // Only DFAs can do reverse searches (currently), so we need one of
        // them in order to do this optimization. It's possible (although
        // pretty unlikely) that we have neither and need to give up.
        if !core.hybrid.is_some() && !core.dfa.is_some() {
            debug!(
                "skipping reverse inner optimization because \
				 we don't have a lazy DFA or a full DFA"
            );
            return Err(core);
        }
        if core.pre.as_ref().map_or(false, |p| p.is_fast()) {
            debug!(
                "skipping reverse inner optimization because \
				 we already have a prefilter that we think is fast"
            );
            return Err(core);
        } else if core.pre.is_some() {
            debug!(
                "core engine has a prefix prefilter, but it is \
                 probably not fast, so continuing with attempt to \
                 use reverse inner prefilter"
            );
        }
        let (concat_prefix, preinner, inner_literals) =
            match reverse_inner::extract(hirs) {
                Some(x) => x,
                // N.B. the 'extract' function emits debug messages explaining
                // why we bailed out here.
                None => return Err(core),
            };
        if !ReverseInner::is_early_return_safe(
            &core.info,
            &concat_prefix,
            &inner_literals,
        ) {
            debug!(
                "skipping reverse inner optimization because an inner \
                 literal match could be confirmed before an earlier match"
            );
            return Err(core);
        }
        debug!("building reverse NFA for prefix before inner literal");
        let mut lookm = LookMatcher::new();
        lookm.set_line_terminator(core.info.config().get_line_terminator());
        let thompson_config = thompson::Config::new()
            .reverse(true)
            .utf8(core.info.config().get_utf8_empty())
            .nfa_size_limit(core.info.config().get_nfa_size_limit())
            .shrink(false)
            .which_captures(WhichCaptures::None)
            .look_matcher(lookm);
        let result = thompson::Compiler::new()
            .configure(thompson_config)
            .build_from_hir(&concat_prefix);
        let nfarev = match result {
            Ok(nfarev) => nfarev,
            Err(_err) => {
                debug!(
                    "skipping reverse inner optimization because the \
					 reverse NFA failed to build: {}",
                    _err,
                );
                return Err(core);
            }
        };
        debug!("building reverse DFA for prefix before inner literal");
        let dfa = if !core.info.config().get_dfa() {
            wrappers::ReverseDFA::none()
        } else {
            wrappers::ReverseDFA::new(&core.info, &nfarev)
        };
        let hybrid = if !core.info.config().get_hybrid() {
            wrappers::ReverseHybrid::none()
        } else if dfa.is_some() {
            debug!(
                "skipping lazy DFA for reverse inner optimization \
				 because we have a full DFA"
            );
            wrappers::ReverseHybrid::none()
        } else {
            wrappers::ReverseHybrid::new(&core.info, &nfarev)
        };
        Ok(ReverseInner { core, preinner, nfarev, hybrid, dfa })
    }

    fn is_early_return_safe(
        info: &RegexInfo,
        concat_prefix: &Hir,
        inner_literals: &[Vec<u8>],
    ) -> bool {
        if ReverseInner::prefix_cannot_consume_inner_literals(
            concat_prefix,
            inner_literals,
        ) {
            return true;
        }
        if ReverseInner::has_absorbing_bounded_repeat(
            concat_prefix,
            inner_literals,
        ) {
            return false;
        }
        let mut lookm = LookMatcher::new();
        lookm.set_line_terminator(info.config().get_line_terminator());
        // The proof below starts every prefix run at a candidate match start.
        // For an earlier real match, leading look-around assertions have
        // already been satisfied. For later suffix candidates, dropping them
        // can only add candidates to check, which is conservative.
        let concat_prefix = ReverseInner::strip_leading_looks(concat_prefix);
        let thompson_config = thompson::Config::new()
            .reverse(false)
            .utf8(info.config().get_utf8_empty())
            .nfa_size_limit(info.config().get_nfa_size_limit())
            .shrink(false)
            .which_captures(WhichCaptures::None)
            .look_matcher(lookm);
        let prefix_nfa = match thompson::Compiler::new()
            .configure(thompson_config)
            .build_from_hir(&concat_prefix)
        {
            Err(_) => return false,
            Ok(prefix_nfa) => prefix_nfa,
        };
        ReverseInnerOverlap::new(&prefix_nfa, inner_literals)
            .map_or(false, |mut x| x.is_safe())
    }

    fn has_absorbing_bounded_repeat(hir: &Hir, literals: &[Vec<u8>]) -> bool {
        match hir.kind() {
            HirKind::Repetition(rep) => {
                if let Some(max) = rep.max.filter(|&max| {
                    max >= ReverseInner::ABSORBING_BOUNDED_REPEAT_LIMIT
                }) {
                    if let Some(cls) = ReverseInner::class(&rep.sub) {
                        if literals.iter().any(|lit| {
                            ReverseInner::class_can_consume_literal(
                                cls, max, lit,
                            )
                        }) {
                            return true;
                        }
                    }
                }
                ReverseInner::has_absorbing_bounded_repeat(&rep.sub, literals)
            }
            HirKind::Capture(capture) => {
                ReverseInner::has_absorbing_bounded_repeat(
                    &capture.sub,
                    literals,
                )
            }
            HirKind::Concat(hirs) | HirKind::Alternation(hirs) => {
                hirs.iter().any(|hir| {
                    ReverseInner::has_absorbing_bounded_repeat(hir, literals)
                })
            }
            HirKind::Empty
            | HirKind::Literal(_)
            | HirKind::Class(_)
            | HirKind::Look(_) => false,
        }
    }

    fn prefix_cannot_consume_inner_literals(
        hir: &Hir,
        literals: &[Vec<u8>],
    ) -> bool {
        literals.iter().all(|lit| {
            !lit.is_empty()
                && lit.iter().any(|&byte| {
                    !ReverseInner::hir_can_consume_byte(hir, byte)
                })
        })
    }

    fn hir_can_consume_byte(hir: &Hir, byte: u8) -> bool {
        match hir.kind() {
            HirKind::Empty | HirKind::Look(_) => false,
            HirKind::Literal(lit) => lit.0.contains(&byte),
            HirKind::Class(Class::Bytes(cls)) => {
                ReverseInner::byte_class_contains(cls, byte)
            }
            HirKind::Class(Class::Unicode(cls)) => {
                if byte > 0x7F {
                    return true;
                }
                let ch = char::from(byte);
                ReverseInner::unicode_class_contains(cls, ch)
            }
            HirKind::Repetition(rep) => {
                ReverseInner::hir_can_consume_byte(&rep.sub, byte)
            }
            HirKind::Capture(capture) => {
                ReverseInner::hir_can_consume_byte(&capture.sub, byte)
            }
            HirKind::Concat(hirs) | HirKind::Alternation(hirs) => hirs
                .iter()
                .any(|hir| ReverseInner::hir_can_consume_byte(hir, byte)),
        }
    }

    fn class(hir: &Hir) -> Option<&Class> {
        match hir.kind() {
            HirKind::Class(cls) => Some(cls),
            HirKind::Capture(capture) => ReverseInner::class(&capture.sub),
            _ => None,
        }
    }

    fn class_can_consume_literal(cls: &Class, max: u32, lit: &[u8]) -> bool {
        if lit.is_empty() {
            return false;
        }
        match *cls {
            Class::Bytes(ref cls) => {
                let len = match u32::try_from(lit.len()) {
                    Err(_) => return false,
                    Ok(len) => len,
                };
                len <= max
                    && lit.iter().all(|&byte| {
                        ReverseInner::byte_class_contains(cls, byte)
                    })
            }
            Class::Unicode(ref cls) => {
                let lit = match core::str::from_utf8(lit) {
                    Err(_) => return false,
                    Ok(lit) => lit,
                };
                let len = match u32::try_from(lit.chars().count()) {
                    Err(_) => return false,
                    Ok(len) => len,
                };
                len <= max
                    && lit.chars().all(|ch| {
                        ReverseInner::unicode_class_contains(cls, ch)
                    })
            }
        }
    }

    fn byte_class_contains(
        cls: &regex_syntax::hir::ClassBytes,
        byte: u8,
    ) -> bool {
        cls.ranges()
            .iter()
            .any(|range| range.start() <= byte && byte <= range.end())
    }

    fn unicode_class_contains(
        cls: &regex_syntax::hir::ClassUnicode,
        ch: char,
    ) -> bool {
        cls.ranges()
            .iter()
            .any(|range| range.start() <= ch && ch <= range.end())
    }

    fn strip_leading_looks(hir: &Hir) -> Hir {
        match hir.kind() {
            HirKind::Concat(hirs) => {
                let hirs = hirs
                    .iter()
                    .skip_while(|hir| {
                        matches!(hir.kind(), HirKind::Empty | HirKind::Look(_))
                    })
                    .cloned()
                    .collect();
                Hir::concat(hirs)
            }
            HirKind::Empty | HirKind::Look(_) => Hir::empty(),
            _ => hir.clone(),
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn try_search_full(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Result<Option<Match>, RetryError> {
        let mut span = input.get_span();
        let mut min_match_start = 0;
        let mut min_pre_start = 0;
        loop {
            let litmatch = match self.preinner.find(input.haystack(), span) {
                None => break,
                Some(span) => span,
            };
            if litmatch.start < min_pre_start {
                trace!(
                    "found inner prefilter match at {litmatch:?}, which starts \
					 before the end of the last forward scan at {min_pre_start}, \
					 quitting to avoid quadratic behavior",
                );
                return Err(RetryError::Quadratic(RetryQuadraticError::new()));
            }
            trace!("reverse inner scan found inner match at {litmatch:?}");
            let revinput = input
                .clone()
                .anchored(Anchored::Yes)
                .span(input.start()..litmatch.start);
            // Note that in addition to the literal search above scanning past
            // our minimum start point, this routine can also return an error
            // as a result of detecting possible quadratic behavior if the
            // reverse scan goes past the minimum start point. That is, the
            // literal search might not, but the reverse regex search for the
            // prefix might!
            if let Some(hm_start) = self.try_search_half_rev_limited(
                cache,
                &revinput,
                min_match_start,
            )? {
                let fwdinput = input
                    .clone()
                    .anchored(Anchored::Pattern(hm_start.pattern()))
                    .span(hm_start.offset()..input.end());
                match self.try_search_half_fwd_stopat(cache, &fwdinput)? {
                    Err(stopat) => {
                        min_pre_start = stopat;
                        span.start = litmatch.start.checked_add(1).unwrap();
                    }
                    Ok(hm_end) => {
                        return Ok(Some(Match::new(
                            hm_start.pattern(),
                            hm_start.offset()..hm_end.offset(),
                        )));
                    }
                }
            }

            if span.start >= span.end {
                break;
            }
            span.start = litmatch.start.checked_add(1).unwrap();
            min_match_start = litmatch.end;
        }
        Ok(None)
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn try_search_half_fwd_stopat(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Result<Result<HalfMatch, usize>, RetryFailError> {
        if let Some(e) = self.core.dfa.get(&input) {
            trace!(
                "using full DFA for forward reverse inner search at {:?}",
                input.get_span()
            );
            e.try_search_half_fwd_stopat(&input)
        } else if let Some(e) = self.core.hybrid.get(&input) {
            trace!(
                "using lazy DFA for forward reverse inner search at {:?}",
                input.get_span()
            );
            e.try_search_half_fwd_stopat(&mut cache.hybrid, &input)
        } else {
            unreachable!("ReverseInner always has a DFA")
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn try_search_half_rev_limited(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        min_start: usize,
    ) -> Result<Option<HalfMatch>, RetryError> {
        if let Some(e) = self.dfa.get(&input) {
            trace!(
                "using full DFA for reverse inner search at {:?}, \
                 but will be stopped at {} to avoid quadratic behavior",
                input.get_span(),
                min_start,
            );
            e.try_search_half_rev_limited(&input, min_start)
        } else if let Some(e) = self.hybrid.get(&input) {
            trace!(
                "using lazy DFA for reverse inner search at {:?}, \
                 but will be stopped at {} to avoid quadratic behavior",
                input.get_span(),
                min_start,
            );
            e.try_search_half_rev_limited(
                &mut cache.revhybrid,
                &input,
                min_start,
            )
        } else {
            unreachable!("ReverseInner always has a DFA")
        }
    }
}

impl Strategy for ReverseInner {
    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn group_info(&self) -> &GroupInfo {
        self.core.group_info()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn create_cache(&self) -> Cache {
        let mut cache = self.core.create_cache();
        cache.revhybrid = self.hybrid.create_cache();
        cache
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn reset_cache(&self, cache: &mut Cache) {
        self.core.reset_cache(cache);
        cache.revhybrid.reset(&self.hybrid);
    }

    fn is_accelerated(&self) -> bool {
        self.preinner.is_fast()
    }

    fn memory_usage(&self) -> usize {
        self.core.memory_usage()
            + self.preinner.memory_usage()
            + self.nfarev.memory_usage()
            + self.dfa.memory_usage()
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search(&self, cache: &mut Cache, input: &Input<'_>) -> Option<Match> {
        if input.get_anchored().is_anchored() {
            return self.core.search(cache, input);
        }
        match self.try_search_full(cache, input) {
            Err(RetryError::Quadratic(_err)) => {
                trace!("reverse inner optimization failed: {_err}");
                self.core.search(cache, input)
            }
            Err(RetryError::Fail(_err)) => {
                trace!("reverse inner fast search failed: {_err}");
                self.core.search_nofail(cache, input)
            }
            Ok(matornot) => matornot,
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_half(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
    ) -> Option<HalfMatch> {
        if input.get_anchored().is_anchored() {
            return self.core.search_half(cache, input);
        }
        match self.try_search_full(cache, input) {
            Err(RetryError::Quadratic(_err)) => {
                trace!("reverse inner half optimization failed: {_err}");
                self.core.search_half(cache, input)
            }
            Err(RetryError::Fail(_err)) => {
                trace!("reverse inner fast half search failed: {_err}");
                self.core.search_half_nofail(cache, input)
            }
            Ok(None) => None,
            Ok(Some(m)) => Some(HalfMatch::new(m.pattern(), m.end())),
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn is_match(&self, cache: &mut Cache, input: &Input<'_>) -> bool {
        if input.get_anchored().is_anchored() {
            return self.core.is_match(cache, input);
        }
        match self.try_search_full(cache, input) {
            Err(RetryError::Quadratic(_err)) => {
                trace!("reverse inner half optimization failed: {_err}");
                self.core.is_match_nofail(cache, input)
            }
            Err(RetryError::Fail(_err)) => {
                trace!("reverse inner fast half search failed: {_err}");
                self.core.is_match_nofail(cache, input)
            }
            Ok(None) => false,
            Ok(Some(_)) => true,
        }
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn search_slots(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        slots: &mut [Option<NonMaxUsize>],
    ) -> Option<PatternID> {
        if input.get_anchored().is_anchored() {
            return self.core.search_slots(cache, input, slots);
        }
        if !self.core.is_capture_search_needed(slots.len()) {
            trace!("asked for slots unnecessarily, trying fast path");
            let m = self.search(cache, input)?;
            copy_match_to_slots(m, slots);
            return Some(m.pattern());
        }
        let m = match self.try_search_full(cache, input) {
            Err(RetryError::Quadratic(_err)) => {
                trace!("reverse inner captures optimization failed: {_err}");
                return self.core.search_slots(cache, input, slots);
            }
            Err(RetryError::Fail(_err)) => {
                trace!("reverse inner fast captures search failed: {_err}");
                return self.core.search_slots_nofail(cache, input, slots);
            }
            Ok(None) => return None,
            Ok(Some(m)) => m,
        };
        trace!(
            "match found at {}..{} in capture search, \
		  	 using another engine to find captures",
            m.start(),
            m.end(),
        );
        let input = input
            .clone()
            .span(m.start()..m.end())
            .anchored(Anchored::Pattern(m.pattern()));
        self.core.search_slots_nofail(cache, &input, slots)
    }

    #[cfg_attr(feature = "perf-inline", inline(always))]
    fn which_overlapping_matches(
        &self,
        cache: &mut Cache,
        input: &Input<'_>,
        patset: &mut PatternSet,
    ) {
        self.core.which_overlapping_matches(cache, input, patset)
    }
}

/// Copies the offsets in the given match to the corresponding positions in
/// `slots`.
///
/// In effect, this sets the slots corresponding to the implicit group for the
/// pattern in the given match. If the indices for the corresponding slots do
/// not exist, then no slots are set.
///
/// This is useful when the caller provides slots (or captures), but you use a
/// regex engine that doesn't operate on slots (like a lazy DFA). This function
/// lets you map the match you get back to the slots provided by the caller.
#[cfg_attr(feature = "perf-inline", inline(always))]
fn copy_match_to_slots(m: Match, slots: &mut [Option<NonMaxUsize>]) {
    let slot_start = m.pattern().as_usize() * 2;
    let slot_end = slot_start + 1;
    if let Some(slot) = slots.get_mut(slot_start) {
        *slot = NonMaxUsize::new(m.start());
    }
    if let Some(slot) = slots.get_mut(slot_end) {
        *slot = NonMaxUsize::new(m.end());
    }
}
