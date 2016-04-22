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
use std::sync::Arc;

use mempool::Pool;
use syntax::{Expr, ExprBuilder, Literals};

use backtrack;
use compile::Compiler;
use dfa;
use error::Error;
use input::{ByteInput, CharInput};
use literals::LiteralSearcher;
use pikevm;
use prog::Program;
use re_bytes;
use re_trait::{RegularExpression, Slot};
use re_unicode;
use set;

/// Exec manages the execution of a regular expression.
///
/// In particular, this manages the various compiled forms of a single regular
/// expression and the choice of which matching engine to use to execute a
/// regular expression.
#[derive(Debug)]
pub struct Exec {
    /// All read only state.
    ro: Arc<ExecReadOnly>,
    /// Caches for the various matching engines.
    cache: Pool<ProgramCache>,
}

/// ExecNoSync is like Exec, except it embeds a reference to a cache. This
/// means it is no longer Sync, but we can now avoid the overhead of
/// synchronization to fetch the cache.
#[derive(Debug)]
pub struct ExecNoSync<'c> {
    /// All read only state.
    ro: &'c Arc<ExecReadOnly>,
    /// Caches for the various matching engines.
    cache: &'c ProgramCache,
}

/// ExecNoSyncStr is like ExecNoSync, but matches on &str instead of &[u8].
pub struct ExecNoSyncStr<'c>(ExecNoSync<'c>);

/// ExecReadOnly comprises all read only state for a regex. Namely, all such
/// state is determined at compile time and never changes during search.
#[derive(Debug)]
struct ExecReadOnly {
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
    /// A set of suffix literals extracted from the regex.
    ///
    /// Prefix literals are stored on the `Program`, since they are used inside
    /// the matching engines.
    suffixes: LiteralSearcher,
    /// match_type encodes as much upfront knowledge about how we're going to
    /// execute a search as possible.
    match_type: MatchType,
}

/// Facilitates the construction of an executor by exposing various knobs
/// to control how a regex is executed and what kinds of resources it's
/// permitted to use.
pub struct ExecBuilder {
    res: Vec<String>,
    match_type: Option<MatchType>,
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
            match_type: None,
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
        self.match_type = None;
        self
    }

    /// Sets the matching engine to use the NFA algorithm no matter what
    /// optimizations are possible.
    ///
    /// This overrides whatever was previously set via the `automatic` or
    /// `bounded_backtracking` methods.
    pub fn nfa(mut self) -> Self {
        self.match_type = Some(MatchType::Nfa(MatchNfaType::PikeVM));
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
        self.match_type = Some(MatchType::Nfa(MatchNfaType::Backtrack));
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
            let ro = Arc::new(ExecReadOnly {
                res: vec![],
                nfa: Program::new(),
                dfa: Program::new(),
                dfa_reverse: Program::new(),
                suffixes: LiteralSearcher::empty(),
                match_type: MatchType::Nothing,
            });
            let ro_ = ro.clone();
            return Ok(Exec { ro: ro, cache: create_pool(ro_) });
        }
        let parsed = try!(Parsed::parse(&self.res, self.only_utf8));
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

        let mut ro = ExecReadOnly {
            res: self.res,
            nfa: nfa,
            dfa: dfa,
            dfa_reverse: dfa_reverse,
            suffixes: LiteralSearcher::suffixes(suffixes),
            match_type: MatchType::Nothing,
        };
        ro.match_type = ro.choose_match_type(self.match_type);
        // println!("MATCH TYPE for '{:?}': {:?}", ro.res, ro.match_type);

        let ro = Arc::new(ro);
        let ro_ = ro.clone();
        Ok(Exec { ro: ro, cache: create_pool(ro_) })
    }
}

impl<'c> RegularExpression for ExecNoSyncStr<'c> {
    type Text = str;

    fn slots_len(&self) -> usize { self.0.slots_len() }

    fn next_after_empty(&self, text: &str, i: usize) -> usize {
        i + text[i..].chars().next().unwrap().len_utf8()
    }

    #[inline(always)] // reduces constant overhead
    fn shortest_match_at(&self, text: &str, start: usize) -> Option<usize> {
        self.0.shortest_match_at(text.as_bytes(), start)
    }

    #[inline(always)] // reduces constant overhead
    fn is_match_at(&self, text: &str, start: usize) -> bool {
        self.0.is_match_at(text.as_bytes(), start)
    }

    #[inline(always)] // reduces constant overhead
    fn find_at(&self, text: &str, start: usize) -> Option<(usize, usize)> {
        self.0.find_at(text.as_bytes(), start)
    }

    #[inline(always)] // reduces constant overhead
    fn captures_at(
        &self,
        slots: &mut [Slot],
        text: &str,
        start: usize,
    ) -> Option<(usize, usize)> {
        self.0.captures_at(slots, text.as_bytes(), start)
    }
}

impl<'c> RegularExpression for ExecNoSync<'c> {
    type Text = [u8];

    /// Returns the number of capture slots in the regular expression. (There
    /// are two slots for every capture group, corresponding to possibly empty
    /// start and end locations of the capture.)
    fn slots_len(&self) -> usize {
        self.ro.nfa.captures.len() * 2
    }

    fn next_after_empty(&self, _text: &[u8], i: usize) -> usize {
        i + 1
    }

    /// Returns the end of a match location, possibly occurring before the
    /// end location of the correct leftmost-first match.
    #[inline(always)] // reduces constant overhead
    fn shortest_match_at(&self, text: &[u8], start: usize) -> Option<usize> {
        if !self.is_anchor_end_match(text) {
            return None;
        }
        match self.ro.match_type {
            MatchType::Literal(ty) => {
                self.exec_literals(ty, text, start).map(|(_, e)| e)
            }
            MatchType::Dfa | MatchType::DfaMany => {
                match dfa::Fsm::forward(
                    &self.ro.dfa,
                    &self.cache,
                    true,
                    text,
                    start,
                ) {
                    dfa::Result::Match(e) => Some(start + e),
                    dfa::Result::NoMatch => None,
                    dfa::Result::Quit => {
                        return self.shortest_match_nfa(
                            MatchNfaType::Auto, text, start);
                    }
                }
            }
            MatchType::DfaAnchoredReverse => {
                match dfa::Fsm::reverse(
                    &self.ro.dfa_reverse,
                    &self.cache,
                    true,
                    &text[start..],
                    text.len(),
                ) {
                    dfa::Result::Match(_) => Some(text.len()),
                    dfa::Result::NoMatch => None,
                    dfa::Result::Quit => {
                        return self.shortest_match_nfa(
                            MatchNfaType::Auto, text, start);
                    }
                }
            }
            MatchType::Nfa(ty) => self.shortest_match_nfa(ty, text, start),
            MatchType::Nothing => None,
        }
    }

    /// Returns true if and only if the regex matches text.
    ///
    /// For single regular expressions, this is equivalent to calling
    /// shortest_match(...).is_some().
    #[inline(always)] // reduces constant overhead
    fn is_match_at(&self, text: &[u8], start: usize) -> bool {
        use self::MatchType::*;
        if !self.is_anchor_end_match(text) {
            return false;
        }
        // We need to do this dance because shortest_match relies on the NFA
        // filling in captures[1], but a RegexSet has no captures. In other
        // words, a RegexSet can't (currently) use shortest_match. ---AG
        match self.ro.match_type {
            Literal(_) | Dfa | DfaAnchoredReverse | DfaMany | Nothing => {
                self.shortest_match_at(text, start).is_some()
            }
            Nfa(ty) => {
                self.exec_nfa(ty, &mut [false], &mut [], true, text, start)
            }
        }
    }

    /// Finds the start and end location of the leftmost-first match, starting
    /// at the given location.
    #[inline(always)] // reduces constant overhead
    fn find_at(&self, text: &[u8], start: usize) -> Option<(usize, usize)> {
        if !self.is_anchor_end_match(text) {
            return None;
        }
        match self.ro.match_type {
            MatchType::Literal(ty) => {
                self.exec_literals(ty, text, start)
            }
            MatchType::Dfa => {
                match self.find_dfa_forward(text, start) {
                    dfa::Result::Match((s, e)) => Some((s, e)),
                    dfa::Result::NoMatch => None,
                    dfa::Result::Quit => {
                        self.find_nfa(MatchNfaType::Auto, text, start)
                    }
                }
            }
            MatchType::DfaAnchoredReverse => {
                match self.find_dfa_anchored_reverse(text, start) {
                    dfa::Result::Match((s, e)) => Some((s, e)),
                    dfa::Result::NoMatch => None,
                    dfa::Result::Quit => {
                        self.find_nfa(MatchNfaType::Auto, text, start)
                    }
                }
            }
            MatchType::Nfa(ty) => self.find_nfa(ty, text, start),
            MatchType::Nothing => None,
            MatchType::DfaMany => {
                unreachable!("BUG: RegexSet cannot be used with find")
            }
        }
    }

    /// Finds the start and end location of the leftmost-first match and also
    /// fills in all matching capture groups.
    ///
    /// The number of capture slots given should be equal to the total number
    /// of capture slots in the compiled program.
    ///
    /// Note that the first two slots always correspond to the start and end
    /// locations of the overall match.
    fn captures_at(
        &self,
        slots: &mut [Slot],
        text: &[u8],
        start: usize,
    ) -> Option<(usize, usize)> {
        if !self.is_anchor_end_match(text) {
            return None;
        }
        match self.ro.match_type {
            MatchType::Literal(ty) => {
                self.exec_literals(ty, text, start).and_then(|(s, _)| {
                    self.captures_nfa(MatchNfaType::Auto, slots, text, s)
                })
            }
            MatchType::Dfa => {
                match self.find_dfa_forward(text, start) {
                    dfa::Result::Match((s, _)) => {
                        self.captures_nfa(
                            MatchNfaType::Auto, slots, text, s)
                    }
                    dfa::Result::NoMatch => None,
                    dfa::Result::Quit => {
                        self.captures_nfa(
                            MatchNfaType::Auto, slots, text, start)
                    }
                }
            }
            MatchType::DfaAnchoredReverse => {
                match self.find_dfa_anchored_reverse(text, start) {
                    dfa::Result::Match((s, _)) => {
                        self.captures_nfa(
                            MatchNfaType::Auto, slots, text, s)
                    }
                    dfa::Result::NoMatch => None,
                    dfa::Result::Quit => {
                        self.captures_nfa(
                            MatchNfaType::Auto, slots, text, start)
                    }
                }
            }
            MatchType::Nfa(ty) => {
                self.captures_nfa(ty, slots, text, start)
            }
            MatchType::Nothing => None,
            MatchType::DfaMany => {
                unreachable!("BUG: RegexSet cannot be used with captures")
            }
        }
    }
}

impl<'c> ExecNoSync<'c> {
    /// Finds which regular expressions match the given text.
    ///
    /// `matches` should have length equal to the number of regexes being
    /// searched.
    ///
    /// This is only useful when one wants to know which regexes in a set
    /// match some text.
    pub fn many_matches_at(
        &self,
        matches: &mut [bool],
        text: &[u8],
        start: usize,
    ) -> bool {
        use self::MatchType::*;
        if !self.is_anchor_end_match(text) {
            return false;
        }
        match self.ro.match_type {
            Literal(ty) => {
                debug_assert!(matches.len() == 1);
                matches[0] = self.exec_literals(ty, text, start).is_some();
                matches[0]
            }
            Dfa | DfaAnchoredReverse | DfaMany => {
                match dfa::Fsm::forward_many(
                    &self.ro.dfa,
                    &self.cache,
                    matches,
                    text,
                    start,
                ) {
                    dfa::Result::Match(_) => true,
                    dfa::Result::NoMatch => false,
                    dfa::Result::Quit => {
                        self.exec_nfa(
                            MatchNfaType::Auto,
                            matches,
                            &mut [],
                            false,
                            text,
                            start)
                    }
                }
            }
            Nfa(ty) => self.exec_nfa(ty, matches, &mut [], false, text, start),
            Nothing => false,
        }
    }

    /// Like shortest_match, but executes an NFA engine.
    fn shortest_match_nfa(
        &self,
        ty: MatchNfaType,
        text: &[u8],
        start: usize,
    ) -> Option<usize> {
        let mut slots = [None, None];
        if self.exec_nfa(ty, &mut [false], &mut slots, true, text, start) {
            slots[1]
        } else {
            None
        }
    }

    /// Finds the leftmost-first match (start and end) using only the DFA.
    ///
    /// If the result returned indicates that the DFA quit, then another
    /// matching engine should be used.
    #[inline(always)] // reduces constant overhead
    fn find_dfa_forward(
        &self,
        text: &[u8],
        start: usize,
    ) -> dfa::Result<(usize, usize)> {
        use dfa::Result::*;
        let end = match dfa::Fsm::forward(
            &self.ro.dfa,
            &self.cache,
            false,
            text,
            start,
        ) {
            NoMatch => return NoMatch,
            Quit => return Quit,
            Match(end) if start == end => return Match((start, start)),
            Match(end) => end,
        };
        // Now run the DFA in reverse to find the start of the match.
        match dfa::Fsm::reverse(
            &self.ro.dfa_reverse,
            &self.cache,
            false,
            &text[start..],
            end - start,
        ) {
            Match(s) => Match((start + s, end)),
            NoMatch => NoMatch,
            Quit => Quit,
        }
    }

    /// Finds the leftmost-first match (start and end) using only the DFA,
    /// but assumes the regex is anchored at the end and therefore starts at
    /// the end of the regex and matches in reverse.
    ///
    /// If the result returned indicates that the DFA quit, then another
    /// matching engine should be used.
    #[inline(always)] // reduces constant overhead
    fn find_dfa_anchored_reverse(
        &self,
        text: &[u8],
        start: usize,
    ) -> dfa::Result<(usize, usize)> {
        use dfa::Result::*;
        match dfa::Fsm::reverse(
            &self.ro.dfa_reverse,
            &self.cache,
            false,
            &text[start..],
            text.len() - start,
        ) {
            Match(s) => Match((start + s, text.len())),
            NoMatch => NoMatch,
            Quit => Quit,
        }
    }

    /// Like find, but executes an NFA engine.
    fn find_nfa(
        &self,
        ty: MatchNfaType,
        text: &[u8],
        start: usize,
    ) -> Option<(usize, usize)> {
        let mut slots = [None, None];
        if self.exec_nfa(ty, &mut [false], &mut slots, false, text, start) {
            match (slots[0], slots[1]) {
                (Some(s), Some(e)) => Some((s, e)),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Like find_nfa, but fills in captures.
    ///
    /// `slots` should have length equal to `2 * nfa.captures.len()`.
    fn captures_nfa(
        &self,
        ty: MatchNfaType,
        slots: &mut [Slot],
        text: &[u8],
        start: usize,
    ) -> Option<(usize, usize)> {
        if self.exec_nfa(ty, &mut [false], slots, false, text, start) {
            match (slots[0], slots[1]) {
                (Some(s), Some(e)) => Some((s, e)),
                _ => None,
            }
        } else {
            None
        }
    }

    #[inline(always)] // reduces constant overhead
    fn exec_literals(
        &self,
        ty: MatchLiteralType,
        text: &[u8],
        start: usize,
    ) -> Option<(usize, usize)> {
        use self::MatchLiteralType::*;
        match ty {
            Unanchored => {
                let lits = &self.ro.nfa.prefixes;
                lits.find(&text[start..])
                    .map(|(s, e)| (start + s, start + e))
            }
            AnchoredStart => {
                let lits = &self.ro.nfa.prefixes;
                lits.find_start(&text[start..])
                    .map(|(s, e)| (start + s, start + e))
            }
            AnchoredEnd => self.ro.suffixes.find_end(&text),
        }
    }

    fn exec_nfa(
        &self,
        mut ty: MatchNfaType,
        matches: &mut [bool],
        slots: &mut [Slot],
        quit_after_match: bool,
        text: &[u8],
        start: usize,
    ) -> bool {
        use self::MatchNfaType::*;
        if let Auto = ty {
            if backtrack::should_exec(self.ro.nfa.len(), text.len()) {
                ty = Backtrack;
            } else {
                ty = PikeVM;
            }
        }
        match ty {
            Auto => unreachable!(),
            Backtrack => self.exec_backtrack(matches, slots, text, start),
            PikeVM => {
                self.exec_pikevm(
                    matches, slots, quit_after_match, text, start)
            }
        }
    }

    /// Always run the NFA algorithm.
    fn exec_pikevm(
        &self,
        matches: &mut [bool],
        slots: &mut [Slot],
        quit_after_match: bool,
        text: &[u8],
        start: usize,
    ) -> bool {
        if self.ro.nfa.uses_bytes() {
            pikevm::Fsm::exec(
                &self.ro.nfa,
                &self.cache,
                matches,
                slots,
                quit_after_match,
                ByteInput::new(text),
                start)
        } else {
            pikevm::Fsm::exec(
                &self.ro.nfa,
                &self.cache,
                matches,
                slots,
                quit_after_match,
                CharInput::new(text),
                start)
        }
    }

    /// Always runs the NFA using bounded backtracking.
    fn exec_backtrack(
        &self,
        matches: &mut [bool],
        slots: &mut [Slot],
        text: &[u8],
        start: usize,
    ) -> bool {
        if self.ro.nfa.uses_bytes() {
            backtrack::Bounded::exec(
                &self.ro.nfa,
                &self.cache,
                matches,
                slots,
                ByteInput::new(text),
                start)
        } else {
            backtrack::Bounded::exec(
                &self.ro.nfa,
                &self.cache,
                matches,
                slots,
                CharInput::new(text),
                start)
        }
    }

    #[inline(always)] // reduces constant overhead
    fn is_anchor_end_match(&self, text: &[u8]) -> bool {
        // Only do this check if the haystack is big (>1MB).
        if text.len() > (1<<20) && self.ro.nfa.is_anchored_end {
            let lcs = self.ro.suffixes.lcs();
            if lcs.len() >= 1 && !lcs.is_suffix(text) {
                return false;
            }
        }
        true
    }

    pub fn capture_name_idx(&self) -> &Arc<HashMap<String, usize>> {
        &self.ro.nfa.capture_name_idx
    }
}

impl<'c> ExecNoSyncStr<'c> {
    pub fn capture_name_idx(&self) -> &Arc<HashMap<String, usize>> {
        self.0.capture_name_idx()
    }
}

impl Exec {
    /// Get a searcher that isn't Sync.
    #[inline(always)] // reduces constant overhead
    pub fn searcher(&self) -> ExecNoSync {
        ExecNoSync {
            ro: &self.ro, // a clone is too expensive here! (and not needed)
            cache: self.cache.get(),
        }
    }

    /// Get a searcher that isn't Sync and can match on &str.
    #[inline(always)] // reduces constant overhead
    pub fn searcher_str(&self) -> ExecNoSyncStr {
        ExecNoSyncStr(self.searcher())
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
        &self.ro.res
    }

    /// Return a slice of capture names.
    ///
    /// Any capture that isn't named is None.
    pub fn capture_names(&self) -> &[Option<String>] {
        &self.ro.nfa.captures
    }

    /// Return a reference to named groups mapping (from group name to
    /// group position).
    pub fn capture_name_idx(&self) -> &Arc<HashMap<String, usize>> {
        &self.ro.nfa.capture_name_idx
    }
}

impl Clone for Exec {
    fn clone(&self) -> Exec {
        Exec {
            ro: self.ro.clone(),
            cache: create_pool(self.ro.clone()),
        }
    }
}

impl ExecReadOnly {
    fn choose_match_type(&self, hint: Option<MatchType>) -> MatchType {
        use self::MatchType::*;
        if let Some(Nfa(_)) = hint {
            return hint.unwrap();
        }
        // If the NFA is empty, then we'll never match anything.
        if self.nfa.insts.is_empty() {
            return Nothing;
        }
        // If our set of prefixes is complete, then we can use it to find
        // a match in lieu of a regex engine. This doesn't quit work well in
        // the presence of multiple regexes, so only do it when there's one.
        if self.res.len() == 1 {
            if self.nfa.prefixes.complete() {
                return if self.nfa.is_anchored_start {
                    Literal(MatchLiteralType::AnchoredStart)
                } else {
                    Literal(MatchLiteralType::Unanchored)
                };
            }
            if self.suffixes.complete() {
                return if self.nfa.is_anchored_end {
                    Literal(MatchLiteralType::AnchoredEnd)
                } else {
                    // This case shouldn't happen. When the regex isn't
                    // anchored, then complete prefixes should imply complete
                    // suffixes.
                    Literal(MatchLiteralType::Unanchored)
                };
            }
        }
        // If we can execute the DFA, then we totally should.
        if dfa::can_exec(&self.dfa) {
            // Regex sets require a slightly specialized path.
            if self.res.len() >= 2 {
                return DfaMany;
            }
            // If the regex is anchored at the end but not the start, then
            // just match in reverse from the end of the haystack.
            if !self.nfa.is_anchored_start && self.nfa.is_anchored_end {
                return DfaAnchoredReverse;
            }
            // Fall back to your garden variety forward searching lazy DFA.
            return Dfa;
        }
        // We're so totally hosed.
        Nfa(MatchNfaType::Auto)
    }
}

#[derive(Clone, Copy, Debug)]
enum MatchType {
    /// A single or multiple literal search. This is only used when the regex
    /// can be decomposed into unambiguous literal search.
    Literal(MatchLiteralType),
    /// A normal DFA search.
    Dfa,
    /// A reverse DFA search starting from the end of a haystack.
    DfaAnchoredReverse,
    /// Use the DFA on two or more regular expressions.
    DfaMany,
    /// An NFA variant.
    Nfa(MatchNfaType),
    /// No match is ever possible, so don't ever try to search.
    Nothing,
}

#[derive(Clone, Copy, Debug)]
enum MatchLiteralType {
    /// Match literals anywhere in text.
    Unanchored,
    /// Match literals only at the start of text.
    AnchoredStart,
    /// Match literals only at the end of text.
    AnchoredEnd,
}

#[derive(Clone, Copy, Debug)]
enum MatchNfaType {
    /// Choose between Backtrack and PikeVM.
    Auto,
    /// NFA bounded backtracking.
    ///
    /// (This is only set by tests, since it never makes sense to always want
    /// backtracking.)
    Backtrack,
    /// The Pike VM.
    ///
    /// (This is only set by tests, since it never makes sense to always want
    /// the Pike VM.)
    PikeVM,
}

/// ProgramCache maintains reusable allocations for each matching engine
/// available to a particular program.
pub type ProgramCache = RefCell<ProgramCacheInner>;

#[derive(Clone, Debug)]
pub struct ProgramCacheInner {
    pub pikevm: pikevm::Cache,
    pub backtrack: backtrack::Cache,
    pub dfa: dfa::Cache,
    pub dfa_reverse: dfa::Cache,
}

/// Creates a fresh pool.
fn create_pool(ro: Arc<ExecReadOnly>) -> Pool<ProgramCache> {
    Pool::new(Box::new(move || RefCell::new(ProgramCacheInner::new(&ro))))
}

impl ProgramCacheInner {
    fn new(ro: &ExecReadOnly) -> Self {
        ProgramCacheInner {
            pikevm: pikevm::Cache::new(&ro.nfa),
            backtrack: backtrack::Cache::new(&ro.nfa),
            dfa: dfa::Cache::new(&ro.dfa),
            dfa_reverse: dfa::Cache::new(&ro.dfa_reverse),
        }
    }
}

/// An intermediate data structure for parsing a bunch of expressions and
/// correctly extracting the prefixes and suffixes of all expressions.
struct Parsed {
    exprs: Vec<Expr>,
    prefixes: Literals,
    suffixes: Literals,
}

impl Parsed {
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
        Ok(Parsed {
            exprs: exprs,
            prefixes: prefixes.unwrap_or(Literals::empty()),
            suffixes: suffixes.unwrap_or(Literals::empty()),
        })
    }
}
