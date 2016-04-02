// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

use mempool::Pool;
use syntax::{Expr, ExprBuilder, Literals};

use backtrack::{self, Backtrack, BacktrackCache};
use compile::Compiler;
use dfa::{self, Dfa, DfaCache, DfaResult};
use error::Error;
use input::{ByteInput, CharInput};
use literals::LiteralSearcher;
use nfa::{Nfa, NfaCache};
use params::Params;
use prog::{Program, InstPtr};
use re_bytes;
use re_unicode;
use set;

/// Exec manages the execution of a regular expression.
///
/// In particular, this manages the various compiled forms of a single regular
/// expression and the choice of which matching engine to use to execute a
/// regular expression.
#[derive(Clone, Debug)]
pub struct Exec {
    /// The original regular expressions given by the caller to compile.
    res: Vec<String>,
    /// A compiled program that is used in the NFA simulation and backtracking.
    /// It can be byte-based or Unicode codepoint based.
    ///
    /// N.B. It is not possibly to make this byte-based from the public API.
    /// It is only used for testing byte based programs in the NFA simulations.
    nfa: Program,
    /// A compiled byte based program for DFA execution. This is only used
    /// if a DFA can be executed. (Currently, only word boundary assertions are
    /// not supported.) Note that this program contains an embedded `.*?`
    /// preceding the first capture group, unless the regex is anchored at the
    /// beginning.
    dfa: Program,
    /// The same as above, except the program is reversed (and there is no
    /// preceding `.*?`). This is used by the DFA to find the starting location
    /// of matches.
    dfa_reverse: Program,
    /// Set to true if and only if the DFA can be executed.
    can_dfa: bool,
    /// A set of suffix literals extracted from the regex.
    ///
    /// Prefix literals are stored on the `Program`, since they are used inside
    /// the matching engines.
    suffixes: LiteralSearcher,
    /// A preference for matching engine selection.
    ///
    /// This defaults to Automatic, which means the matching engine is selected
    /// based on heuristics (such as the nature and size of the compiled
    /// program, in addition to the size of the search text).
    ///
    /// If either Nfa or Backtrack is set, then it is always used because
    /// either is capable of executing every compiled program on any input
    /// size.
    match_engine: MatchEngine,
    /// Caches for the various matching engines.
    cache: ProgramPool,
}

/// Facilitates the construction of an executor by exposing various knobs
/// to control how a regex is executed and what kinds of resources it's
/// permitted to use.
pub struct ExecBuilder {
    res: Vec<String>,
    match_engine: MatchEngine,
    size_limit: usize,
    bytes: bool,
    only_utf8: bool,
}

impl ExecBuilder {
    /// Create a regex execution builder.
    ///
    /// This uses default settings for everything except the regex itself,
    /// which must be provided. Further knobs can be set by calling methods,
    /// and then finally, `build` to actually create the executor.
    pub fn new(re: &str) -> Self {
        Self::new_many(&[re])
    }

    /// Like new, but compiles the union of the given regular expressions.
    ///
    /// Note that when compiling 2 or more regular expressions, capture groups
    /// are completely unsupported. (This means both `find` and `captures`
    /// wont work.)
    pub fn new_many<I, S>(res: I) -> Self
            where S: AsRef<str>, I: IntoIterator<Item=S> {
        ExecBuilder {
            res: res.into_iter().map(|s| s.as_ref().to_owned()).collect(),
            match_engine: MatchEngine::Automatic,
            size_limit: 10 * (1 << 20),
            bytes: false,
            only_utf8: true,
        }
    }

    /// Set the matching engine to be automatically determined.
    ///
    /// This is the default state and will apply whatever optimizations are
    /// possible, such as running a DFA.
    ///
    /// This overrides whatever was previously set via the `nfa` or
    /// `bounded_backtracking` methods.
    pub fn automatic(mut self) -> Self {
        self.match_engine = MatchEngine::Automatic;
        self
    }

    /// Sets the matching engine to use the NFA algorithm no matter what
    /// optimizations are possible.
    ///
    /// This overrides whatever was previously set via the `automatic` or
    /// `bounded_backtracking` methods.
    pub fn nfa(mut self) -> Self {
        self.match_engine = MatchEngine::Nfa;
        self
    }

    /// Sets the matching engine to use a bounded backtracking engine no
    /// matter what optimizations are possible.
    ///
    /// One must use this with care, since the bounded backtracking engine
    /// uses memory proportion to `len(regex) * len(text)`.
    ///
    /// This overrides whatever was previously set via the `automatic` or
    /// `nfa` methods.
    pub fn bounded_backtracking(mut self) -> Self {
        self.match_engine = MatchEngine::Backtrack;
        self
    }

    /// Sets the size limit on a single compiled regular expression program.
    ///
    /// The default is ~10MB.
    ///
    /// N.B. Typically, multiple programs are compiled for every regular
    /// expression and this limit applies to *each* of them.
    pub fn size_limit(mut self, bytes: usize) -> Self {
        self.size_limit = bytes;
        self
    }

    /// Compiles byte based programs for use with the NFA matching engines.
    ///
    /// By default, the NFA engines match on Unicode scalar values. They can
    /// be made to use byte based programs instead. In general, the byte based
    /// programs are slower because of a less efficient encoding of character
    /// classes.
    ///
    /// Note that this does not impact DFA matching engines, which always
    /// execute on bytes.
    pub fn bytes(mut self, yes: bool) -> Self {
        self.bytes = yes;
        self
    }

    /// When disabled, the program compiled may match arbitrary bytes.
    ///
    /// When enabled (the default), all compiled programs exclusively match
    /// valid UTF-8 bytes.
    pub fn only_utf8(mut self, yes: bool) -> Self {
        self.only_utf8 = yes;
        self
    }

    /// Build an executor that can run a regular expression.
    pub fn build(self) -> Result<Exec, Error> {
        if self.res.is_empty() {
            return Ok(Exec {
                res: vec![],
                nfa: Program::new(),
                dfa: Program::new(),
                dfa_reverse: Program::new(),
                can_dfa: false,
                suffixes: LiteralSearcher::empty(),
                match_engine: MatchEngine::Automatic,
                cache: ProgramPool::new(),
            });
        }
        let parsed = try!(parse(&self.res, self.only_utf8));
        let mut nfa = try!(
            Compiler::new()
                     .size_limit(self.size_limit)
                     .bytes(self.bytes)
                     .only_utf8(self.only_utf8)
                     .compile(&parsed.exprs));
        let mut dfa = try!(
            Compiler::new()
                     .size_limit(self.size_limit)
                     .dfa(true)
                     .only_utf8(self.only_utf8)
                     .compile(&parsed.exprs));
        let dfa_reverse = try!(
            Compiler::new()
                     .size_limit(self.size_limit)
                     .dfa(true)
                     .only_utf8(self.only_utf8)
                     .reverse(true)
                     .compile(&parsed.exprs));

        let prefixes = parsed.prefixes.unambiguous_prefixes();
        let suffixes = parsed.suffixes.unambiguous_suffixes();
        nfa.prefixes = LiteralSearcher::prefixes(prefixes);
        dfa.prefixes = nfa.prefixes.clone();
        let can_dfa = dfa::can_exec(&dfa);
        Ok(Exec {
            res: self.res,
            nfa: nfa,
            dfa: dfa,
            dfa_reverse: dfa_reverse,
            can_dfa: can_dfa,
            suffixes: LiteralSearcher::suffixes(suffixes),
            match_engine: self.match_engine,
            cache: ProgramPool::new(),
        })
    }
}

impl Exec {
    /// The main entry point for execution of a regular expression on text.
    ///
    /// caps represents the capture locations that the caller wants. Generally,
    /// there are three varieties: no captures requested (e.g., `is_match`),
    /// one capture requested (e.g., `find` or `find_iter`) or multiple
    /// captures requested (e.g., `captures` or `captures_iter` along with
    /// at least one capturing group in the regex). Each of these three cases
    /// provokes different behavior from the matching engines, where fewer
    /// captures generally means faster matching.
    ///
    /// text should be the search text and start should be the position in
    /// the text to start searching. Note that passing a simple slice here
    /// isn't sufficient, since look-behind assertions sometimes need to
    /// inspect the character immediately preceding the start location.
    ///
    /// Note that this method takes self.match_engine into account when
    /// choosing the engine to use. If self.match_engine is Nfa or Backtrack,
    /// then that engine is always used. Otherwise, one is selected
    /// automatically.
    pub fn exec(
        &self,
        params: &mut Params,
        text: &[u8],
        start: usize,
    ) -> bool {
        // An empty regular expression never matches.
        if self.nfa.insts.is_empty() {
            return false;
        }
        // If we have prefix/suffix literals and the regex is anchored, then
        // we should be able to detect certain classes of failed matches
        // very quickly.
        //
        // But don't do this on very short haystacks, since detecting a
        // non-match on short haystack should be fast anyway.
        if text.len() > 256 && !self.is_anchor_match(text, start) {
            return false;
        }
        // Why isn't the DFA or literal engine checked for here? Well, it's
        // only possible to execute those engines in exec_auto. See comment on
        // MatchEngine below for more details.
        match self.match_engine {
            MatchEngine::Automatic => self.exec_auto(params, text, start),
            MatchEngine::Backtrack => {
                let mut cache = &mut *self.cache.get().borrow_mut();
                self.exec_backtrack(cache, params, text, start)
            }
            MatchEngine::Nfa => {
                let mut cache = &mut *self.cache.get().borrow_mut();
                self.exec_nfa(cache, params, text, start)
            }
        }
    }

    /// Like exec, but always selects the engine automatically.
    fn exec_auto(
        &self,
        params: &mut Params,
        text: &[u8],
        start: usize,
    ) -> bool {
        if params.captures().len() <= 2 && self.nfa.prefixes.complete() {
            // We should be able to execute the literal engine even if there
            // are more captures by falling back to the NFA engine after a
            // match. However, that's effectively what the NFA engine does
            // already (since it will use the literal engine if it exists).
            self.exec_literals(&self.nfa.prefixes, params, text, start)
        } else if self.can_dfa {
            self.exec_dfa(params, text, start)
        } else {
            let mut cache = &mut *self.cache.get().borrow_mut();
            self.exec_auto_nfa(cache, params, text, start)
        }
    }

    /// Like exec, but always tries to execute the lazy DFA.
    ///
    /// Note that self.can_dfa must be true. This will panic otherwise.
    fn exec_dfa<'a>(
        &self,
        params: &mut Params,
        text: &[u8],
        start: usize,
    ) -> bool {
        debug_assert!(self.can_dfa);
        if self.should_suffix_scan() {
            return self.exec_dfa_reverse_first(params, text, start);
        }
        let mut cache = &mut *self.cache.get().borrow_mut();
        match Dfa::exec(&self.dfa, &mut cache.dfa, params, text, start) {
            DfaResult::Match => {} // fallthrough
            DfaResult::NoMatch => return false,
            DfaResult::Quit => {
                params.reset();
                return self.exec_auto_nfa(cache, params, text, start);
            }
        }
        if params.style().match_only() {
            return true;
        }
        let match_end = match params.captures().get(1) {
            Some(&Some(i)) => i,
            // The DFA returned true for a match, but did not set any capture
            // location because the caller didn't ask for them. Therefore, we
            // can quit immediately.
            _ => return true,
        };
        // invariant: caps.len() >= 2 && caps.len() % 2 == 0
        // If the reported end of the match is the same as the start, then we
        // have an empty match and we can quit now.
        if start == match_end {
            // Be careful... If the caller wants sub-captures, than we are
            // obliged to run the NFA to get them.
            if params.captures().len() == 2 {
                // The caller only needs the start/end, so we can avoid the
                // NFA here.
                params.set_start(Some(start));
                params.set_end(Some(start));
                return true;
            }
            return self.exec_auto_nfa(cache, params, text, start);
        }
        // OK, now we find the start of the match by running the DFA backwards
        // on the text. We *start* the search at the end of the match.
        let result = Dfa::exec(
            &self.dfa_reverse,
            &mut cache.dfa_reverse,
            params,
            &text[start..],
            match_end - start);
        match result {
            DfaResult::Match => {} // fallthrough
            DfaResult::NoMatch => {
                panic!("BUG: forward match implies reverse match");
            }
            DfaResult::Quit => {
                params.reset();
                return self.exec_auto_nfa(cache, params, text, start);
            }
        }
        let match_start = match params.captures().get(0) {
            Some(&Some(i)) => start + i,
            _ => panic!("BUG: early match can't happen on reverse search"),
        };
        if params.captures().len() == 2 {
            // If the caller doesn't care about capture locations, then we can
            // avoid running the NFA to fill them in.
            params.set_start(Some(match_start));
            params.set_end(Some(match_end));
            return true;
        }
        self.exec_auto_nfa(cache, params, text, match_start)
    }

    /// Like exec_dfa, but tries executing the DFA in reverse from suffix
    /// literal matches.
    ///
    /// Note that self.can_dfa must be true. This will panic otherwise.
    fn exec_dfa_reverse_first(
        &self,
        params: &mut Params,
        text: &[u8],
        start: usize,
    ) -> bool {
        let mut cache = &mut *self.cache.get().borrow_mut();
        let lcs = self.suffixes.lcs();

        let mut end = start;
        while end <= text.len() {
            end = end + match lcs.find(&text[end..]) {
                None => return false,
                Some(e) => e + lcs.len(),
            };
            params.set_end(Some(end)); // in case we quit early

            // Search in reverse from the end of the suffix match.
            let result = Dfa::exec(
                &self.dfa_reverse,
                &mut cache.dfa_reverse,
                params,
                &text[start..end],
                end - start);
            let match_start = match result {
                DfaResult::Match => match params.captures().get(0) {
                    Some(&Some(i)) => start + i,
                    // We know we have a match, but the caller didn't ask
                    // for any captures, so we can quit now.
                    _ => return true,
                },
                DfaResult::NoMatch => continue,
                DfaResult::Quit => {
                    params.reset();
                    return self.exec_auto_nfa(cache, params, text, start);
                }
            };
            if params.style().match_only() {
                return true;
            }

            // Now search forwards from the start of the reverse match.
            let result = Dfa::exec(
                &self.dfa,
                &mut cache.dfa,
                params,
                text,
                match_start);
            let match_end = match result {
                DfaResult::Match => match params.captures().get(1) {
                    Some(&Some(i)) => i,
                    _ => panic!("BUG: early match can't happen"),
                },
                DfaResult::NoMatch => {
                    panic!("BUG: reverse match implies forward match");
                }
                DfaResult::Quit => {
                    params.reset();
                    return self.exec_auto_nfa(cache, params, text, start);
                }
            };

            // If the caller only requested the start/end of a match, then we
            // can quit now.
            if params.captures().len() == 2 {
                params.set_start(Some(match_start));
                params.set_end(Some(match_end));
                return true;
            }
            // Otherwise, we have to fall back to NFA to fill in captures.
            return self.exec_auto_nfa(cache, params, text, match_start);
        }
        false
    }

    /// This is like exec_auto, except it always chooses between either the
    /// full NFA simulation or the bounded backtracking engine.
    fn exec_auto_nfa(
        &self,
        cache: &mut ProgramCache,
        params: &mut Params,
        text: &[u8],
        start: usize,
    ) -> bool {
        if backtrack::should_exec(self.nfa.len(), text.len()) {
            self.exec_backtrack(cache, params, text, start)
        } else {
            self.exec_nfa(cache, params, text, start)
        }
    }

    /// Always run the NFA algorithm.
    fn exec_nfa(
        &self,
        cache: &mut ProgramCache,
        params: &mut Params,
        text: &[u8],
        start: usize,
    ) -> bool {
        if self.nfa.uses_bytes() {
            Nfa::exec(
                &self.nfa,
                &mut cache.nfa,
                params,
                ByteInput::new(text),
                start)
        } else {
            Nfa::exec(
                &self.nfa,
                &mut cache.nfa,
                params,
                CharInput::new(text),
                start)
        }
    }

    /// Always runs the NFA using bounded backtracking.
    fn exec_backtrack(
        &self,
        cache: &mut ProgramCache,
        params: &mut Params,
        text: &[u8],
        start: usize,
    ) -> bool {
        if self.nfa.uses_bytes() {
            Backtrack::exec(
                &self.nfa,
                &mut cache.backtrack,
                params,
                ByteInput::new(text),
                start)
        } else {
            Backtrack::exec(
                &self.nfa,
                &mut cache.backtrack,
                params,
                CharInput::new(text),
                start)
        }
    }

    /// Executes the special literal matching engine.
    ///
    /// When a regular expression is small and can be expanded to a finite set
    /// of literals that all result in matches, then we can avoid all of the
    /// regex machinery and use specialized DFAs.
    ///
    /// This panics if the set of literals do not correspond to matches.
    fn exec_literals(
        &self,
        literals: &LiteralSearcher,
        params: &mut Params,
        text: &[u8],
        start: usize,
    ) -> bool {
        debug_assert!(literals.complete());
        debug_assert!(self.res.len() == 1);
        match literals.find(&text[start..]) {
            None => false,
            Some((s, e)) => {
                if s > 0 && self.nfa.is_anchored_start
                    || e < text.len() && self.nfa.is_anchored_end {
                    // It seem inefficient to reject the match here, but in
                    // fact, for large strings this would have been rejected
                    // earlier. To avoid overhead, we skip that check for
                    // smaller strings but need to make sure we don't
                    // accidentally report an errant match.
                    return false;
                }
                if params.captures().len() == 2 {
                    params.set_start(Some(start + s));
                    params.set_end(Some(start + e));
                }
                params.set_match(0);
                true
            }
        }
    }

    /// Returns false if the regex has a start/end anchor, but none of the
    /// prefix/suffix literals match.
    ///
    /// Returns true if there are no anchors, no prefix/suffix literals or if
    /// the literals match.
    pub fn is_anchor_match(&self, text: &[u8], start: usize) -> bool {
        self.is_anchor_start_match(text, start)
        && self.is_anchor_end_match(text, start)
    }

    fn is_anchor_start_match(&self, text: &[u8], _start: usize) -> bool {
        if !self.nfa.is_anchored_start || self.nfa.prefixes.is_empty() {
            return true;
        }
        self.nfa.prefixes.find_start(text).is_some()
    }

    fn is_anchor_end_match(&self, text: &[u8], _start: usize) -> bool {
        if !self.nfa.is_anchored_end || self.suffixes.is_empty() {
            return true;
        }
        self.suffixes.find_end(text).is_some()
    }

    /// Returns true if the program is amenable to suffix scanning.
    ///
    /// When this is true, as a heuristic, we assume it is OK to quickly scan
    /// for suffix literals and then do a *reverse* DFA match from any matches
    /// produced by the literal scan. (And then followed by a forward DFA
    /// search, since the previously found suffix literal maybe not actually be
    /// the end of a match.)
    ///
    /// This is a bit of a specialized optimization, but can result in pretty
    /// big performance wins if 1) there are no prefix literals and 2) the
    /// suffix literals are pretty rare in the text. (1) is obviously easy to
    /// account for but (2) is harder. As a proxy, we assume that longer
    /// strings are generally rarer, so we only enable this optimization when
    /// we have a meaty suffix.
    fn should_suffix_scan(&self) -> bool {
        if self.suffixes.is_empty() {
            return false;
        }
        let lcs_len = self.suffixes.lcs().char_len();
        lcs_len >= 3 && lcs_len > self.dfa.prefixes.lcp().char_len()
    }

    /// Build a Regex from this executor.
    pub fn into_regex(self) -> re_unicode::Regex {
        re_unicode::Regex::from(self)
    }

    /// Build a RegexSet from this executor.
    pub fn into_regex_set(self) -> set::RegexSet {
        set::RegexSet::from(self)
    }

    /// Build a Regex from this executor that can match arbitrary bytes.
    pub fn into_byte_regex(self) -> re_bytes::Regex {
        re_bytes::Regex::from(self)
    }

    /// Build a RegexSet from this executor that can match arbitrary bytes.
    pub fn into_byte_regex_set(self) -> re_bytes::RegexSet {
        re_bytes::RegexSet::from(self)
    }

    /// The original regular expressions given by the caller that were
    /// compiled.
    pub fn regex_strings(&self) -> &[String] {
        &self.res
    }

    /// Return a slice of instruction pointers to match slots.
    ///
    /// There is a match slot for every regular expression in this executor.
    pub fn matches(&self) -> &[InstPtr] {
        &self.nfa.matches
    }

    /// Return a slice of capture names.
    ///
    /// Any capture that isn't named is None.
    pub fn captures(&self) -> &[Option<String>] {
        &self.nfa.captures
    }

    /// Return a reference to named groups mapping (from group name to
    /// group position).
    pub fn capture_name_idx(&self) -> &Arc<HashMap<String, usize>> {
        &self.nfa.capture_name_idx
    }
}

/// Some of the matching engines offered by this regex implementation.
///
/// This is exported for use in testing.
///
/// Note that only engines that can be used on *every* regex are exposed here.
/// For example, it is useful for testing purposes to say, "always execute
/// the backtracking engine" or "always execute the full NFA simulation."
/// However, we cannot say things like, "always execute the pure literals
/// engine" or "always execute the DFA" because they only work on a subset of
/// regexes supported by this crate. Specifically, the only way to run the
/// DFA or literal engines is to use Automatic.
#[doc(hidden)]
#[derive(Clone, Copy, Debug)]
enum MatchEngine {
    /// Automatically choose the best matching engine based on heuristics.
    Automatic,
    /// A bounded backtracking implementation. About twice as fast as the
    /// NFA, but can only work on small regexes and small input.
    Backtrack,
    /// A full NFA simulation. Can always be employed but almost always the
    /// slowest choice.
    Nfa,
}

/// ProgramPool is a proxy for mempool::Pool that knows how to impl Clone.
#[derive(Debug)]
struct ProgramPool(Pool<RefCell<ProgramCache>>);

impl ProgramPool {
    fn new() -> Self {
        let create = || RefCell::new(ProgramCache::new());
        ProgramPool(Pool::new(Box::new(create)))
    }
}

impl Deref for ProgramPool {
    type Target = Pool<RefCell<ProgramCache>>;
    fn deref(&self) -> &Self::Target { &self.0 }
}

impl Clone for ProgramPool {
    fn clone(&self) -> ProgramPool { ProgramPool::new() }
}

/// ProgramCache maintains reusable allocations for each matching engine
/// available to a particular program.
///
/// The allocations are created lazily, so we don't pay for caches that
/// aren't used.
///
/// N.B. These are all behind a pointer because it's fewer bytes to memcpy.
/// These caches are pushed/popped from the pool a lot, and a smaller
/// footprint can have an impact on matching small inputs. See, for example,
/// the hard_32 benchmark.
#[derive(Debug)]
struct ProgramCache {
    nfa: NfaCache,
    backtrack: BacktrackCache,
    dfa: DfaCache,
    dfa_reverse: DfaCache,
}

impl ProgramCache {
    fn new() -> Self {
        ProgramCache {
            nfa: NfaCache::new(),
            backtrack: BacktrackCache::new(),
            dfa: DfaCache::new(),
            dfa_reverse: DfaCache::new(),
        }
    }
}

impl Clone for ProgramCache {
    fn clone(&self) -> ProgramCache {
        ProgramCache::new()
    }
}

struct Parsed {
    exprs: Vec<Expr>,
    prefixes: Literals,
    suffixes: Literals,
}

fn parse(res: &[String], only_utf8: bool) -> Result<Parsed, Error> {
    let mut exprs = Vec::with_capacity(res.len());
    let mut prefixes = Some(Literals::empty());
    let mut suffixes = Some(Literals::empty());
    for re in res {
        let parser =
            ExprBuilder::new()
                .allow_bytes(!only_utf8)
                .unicode(only_utf8);
        let expr = try!(parser.parse(re));
        prefixes = prefixes.and_then(|mut prefixes| {
            if !prefixes.union_prefixes(&expr) {
                None
            } else {
                Some(prefixes)
            }
        });
        suffixes = suffixes.and_then(|mut suffixes| {
            if !suffixes.union_suffixes(&expr) {
                None
            } else {
                Some(suffixes)
            }
        });
        exprs.push(expr);
    }
    // If this is a set, then we have to force our prefixes/suffixes to all be
    // cut so that they don't trigger the literal engine (which doesn't work
    // with sets... yet).
    if res.len() != 1 {
        if let Some(ref mut prefixes) = prefixes {
            prefixes.cut();
        }
        if let Some(ref mut suffixes) = suffixes {
            suffixes.cut();
        }
    }
    Ok(Parsed {
        exprs: exprs,
        prefixes: prefixes.unwrap_or(Literals::empty()),
        suffixes: suffixes.unwrap_or(Literals::empty()),
    })
}
