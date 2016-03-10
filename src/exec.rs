// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::collections::HashMap;
use std::sync::Arc;

use syntax;

use backtrack::{self, Backtrack};
use compile::Compiler;
use dfa::{self, Dfa, DfaResult};
use error::Error;
use input::{ByteInput, CharInput};
use literals::Literals;
use nfa::Nfa;
use prog::{Program, InstPtr};
use re_bytes;
use re_unicode;

pub type CaptureSlots<'a> = &'a mut [CaptureSlot];

pub type CaptureSlot = Option<usize>;

/// The parameters to running a regex search over some text.
#[derive(Debug)]
pub struct Search<'caps, 'matches> {
    /// The matching engine writes capture locations to this slice.
    ///
    /// Note that some matching engines, like the DFA, have limited support
    /// for this. The DFA can only fill in one capture location (the end
    /// location of the match).
    pub captures: CaptureSlots<'caps>,
    /// The matching engine indicates which match instructions were executed
    /// when searching stopped.
    ///
    /// In standard searches, there is exactly one value in this slice and it
    /// should be initialized to `false`. When executing sets of regexes,
    /// there should be a location for each regex.
    matches: &'matches mut [bool],
    /// Whether the matching engine has recorded any match.
    matched_any: bool,
}

impl<'caps, 'matches> Search<'caps, 'matches> {
    pub fn new(
        captures: CaptureSlots<'caps>,
        matches: &'matches mut [bool],
    ) -> Search<'caps, 'matches> {
        Search {
            captures: captures,
            matches: matches,
            matched_any: false,
        }
    }

    pub fn quit_after_first_match(&self) -> bool {
        self.captures.is_empty() && self.matches.len() <= 1
    }

    pub fn find_many_matches(&self) -> bool {
        self.matches.len() > 1
    }

    pub fn find_one_match(&self) -> bool {
        self.matches.len() == 1
    }

    pub fn matched_all(&self) -> bool {
        self.matches.iter().all(|m| *m)
    }

    pub fn set_match(&mut self, match_slot: usize) {
        self.matched_any = true;
        if let Some(old) = self.matches.get_mut(match_slot) {
            *old = true;
        }
    }

    pub fn capture(&self, i: usize) -> Option<CaptureSlot> {
        self.captures.get(i).map(|&slot| slot)
    }

    pub fn set_start(&mut self, slot: CaptureSlot) {
        self.set_capture(0, slot);
    }

    pub fn set_end(&mut self, slot: CaptureSlot) {
        self.set_capture(1, slot);
    }

    pub fn set_capture(&mut self, i: usize, slot: CaptureSlot) {
        if let Some(old_slot) = self.captures.get_mut(i) {
            *old_slot = slot;
        }
    }

    pub fn copy_captures_from(&mut self, caps: &[CaptureSlot]) {
        for (slot, val) in self.captures.iter_mut().zip(caps.iter()) {
            *slot = *val;
        }
    }

    pub fn reset(&mut self) {
        for slot in self.captures.iter_mut() {
            *slot = None;
        }
        for m in self.matches.iter_mut() {
            *m = false;
        }
    }
}

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
    prog: Program,
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
            return Err(Error::InvalidSet);
        }
        let mut exprs = vec![];
        for re in &self.res {
            let parser =
                syntax::ExprBuilder::new()
                    .allow_bytes(!self.only_utf8)
                    .unicode(self.only_utf8);
            exprs.push(try!(parser.parse(re)));
        }
        let mut prog = try!(
            Compiler::new()
                     .size_limit(self.size_limit)
                     .bytes(self.bytes)
                     .only_utf8(self.only_utf8)
                     .compile(&exprs));
        let mut dfa = try!(
            Compiler::new()
                     .size_limit(self.size_limit)
                     .dfa(true)
                     .only_utf8(self.only_utf8)
                     .compile(&exprs));
        let dfa_reverse = try!(
            Compiler::new()
                     .size_limit(self.size_limit)
                     .dfa(true)
                     .only_utf8(self.only_utf8)
                     .reverse(true)
                     .compile(&exprs));

        // Compute literal prefixes for only `prog`, which is likely a Unicode
        // based program. Literal prefix extract currently works better on
        // Unicode programs.
        prog.prefixes = Literals::prefixes(&prog);
        // And give it to the DFA too, which can use Unicode prefixes even
        // though the program itself is byte based.
        dfa.prefixes = prog.prefixes.clone();
        let can_dfa = dfa::can_exec(&dfa);
        Ok(Exec {
            res: self.res,
            prog: prog,
            dfa: dfa,
            dfa_reverse: dfa_reverse,
            can_dfa: can_dfa,
            match_engine: self.match_engine,
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
    pub fn exec<'c, 'm>(
        &self,
        search: &mut Search<'c, 'm>,
        text: &[u8],
        start: usize,
    ) -> bool {
        // Why isn't the DFA or literal engine checked for here? Well, it's
        // only possible to execute those engines in exec_auto. See comment on
        // MatchEngine below for more details.
        match self.match_engine {
            MatchEngine::Automatic => self.exec_auto(search, text, start),
            MatchEngine::Backtrack => self.exec_backtrack(search, text, start),
            MatchEngine::Nfa => self.exec_nfa(search, text, start),
        }
    }

    /// Like exec, but always selects the engine automatically.
    fn exec_auto<'c, 'm>(
        &self,
        search: &mut Search<'c, 'm>,
        text: &[u8],
        start: usize,
    ) -> bool {
        if search.captures.len() <= 2 && self.prog.prefixes.at_match() {
            // We should be able to execute the literal engine even if there
            // are more captures by falling back to the NFA engine after a
            // match. However, that's effectively what the NFA engine does
            // already (since it will use the literal engine if it exists).
            self.exec_literals(search, text, start)
        } else if self.can_dfa {
            self.exec_dfa(search, text, start)
        } else {
            self.exec_auto_nfa(search, text, start)
        }
    }

    /// Like exec, but always tries to execute the lazy DFA.
    ///
    /// Note that self.can_dfa must be true. This will panic otherwise.
    fn exec_dfa<'a, 'c, 'm>(
        &self,
        search: &'a mut Search<'c, 'm>,
        text: &[u8],
        start: usize,
    ) -> bool {
        debug_assert!(self.can_dfa);
        match Dfa::exec(&self.dfa, search, text, start) {
            DfaResult::Match => {} // fallthrough
            DfaResult::NoMatch => return false,
            DfaResult::Quit => {
                search.reset();
                return self.exec_auto_nfa(search, text, start);
            }
        }
        let match_end = match search.captures.get(1) {
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
            if search.captures.len() == 2 {
                // The caller only needs the start/end, so we can avoid the
                // NFA here.
                search.captures[0] = Some(start);
                search.captures[1] = Some(start);
                return true;
            }
            return self.exec_auto_nfa(search, text, start);
        }
        // OK, now we find the start of the match by running the DFA backwards
        // on the text. We *start* the search at the end of the match.
        let result = Dfa::exec(
            &self.dfa_reverse, search, &text[start..], match_end - start);
        match result {
            DfaResult::Match => {} // fallthrough
            DfaResult::NoMatch => {
                panic!("BUG: forward match implies backward match");
            }
            DfaResult::Quit => {
                search.reset();
                return self.exec_auto_nfa(search, text, start);
            }
        }
        let match_start = match search.captures.get(0) {
            Some(&Some(i)) => start + i,
            _ => panic!("BUG: early match can't happen on reverse search"),
        };
        if search.captures.len() == 2 {
            // If the caller doesn't care about capture locations, then we can
            // avoid running the NFA to fill them in.
            search.captures[0] = Some(match_start);
            search.captures[1] = Some(match_end);
            return true;
        }
        self.exec_auto_nfa(search, text, match_start)
    }

    /// This is like exec_auto, except it always chooses between either the
    /// full NFA simulation or the bounded backtracking engine.
    fn exec_auto_nfa<'c, 'm>(
        &self,
        search: &mut Search<'c, 'm>,
        text: &[u8],
        start: usize,
    ) -> bool {
        if backtrack::should_exec(self.prog.len(), text.len()) {
            self.exec_backtrack(search, text, start)
        } else {
            self.exec_nfa(search, text, start)
        }
    }

    /// Always run the NFA algorithm.
    fn exec_nfa<'c, 'm>(
        &self,
        search: &mut Search<'c, 'm>,
        text: &[u8],
        start: usize,
    ) -> bool {
        if self.prog.uses_bytes() {
            Nfa::exec(&self.prog, search, ByteInput::new(text), start)
        } else {
            Nfa::exec(&self.prog, search, CharInput::new(text), start)
        }
    }

    /// Always runs the NFA using bounded backtracking.
    fn exec_backtrack<'c, 'm>(
        &self,
        search: &mut Search<'c, 'm>,
        text: &[u8],
        start: usize,
    ) -> bool {
        if self.prog.uses_bytes() {
            Backtrack::exec(&self.prog, search, ByteInput::new(text), start)
        } else {
            Backtrack::exec(&self.prog, search, CharInput::new(text), start)
        }
    }

    /// Executes the special literal matching engine.
    ///
    /// When a regular expression is small and can be expanded to a finite set
    /// of literals that all result in matches, then we can avoid all of the
    /// regex machinery and use specialized DFAs.
    ///
    /// This panics if the set of literals do not correspond to matches.
    fn exec_literals<'c, 'm>(
        &self,
        search: &mut Search<'c, 'm>,
        text: &[u8],
        start: usize,
    ) -> bool {
        debug_assert!(self.prog.prefixes.at_match());
        match self.prog.prefixes.find(&text[start..]) {
            None => false,
            Some((s, e)) => {
                if search.captures.len() == 2 {
                    search.captures[0] = Some(start + s);
                    search.captures[1] = Some(start + e);
                }
                true
            }
        }
    }

    /// Build a dynamic Regex from this executor.
    pub fn into_regex(self) -> re_unicode::Regex {
        re_unicode::Regex::from(self)
    }

    /// Build a dynamic Regex from this executor that can match arbitrary
    /// bytes.
    pub fn into_byte_regex(self) -> re_bytes::Regex {
        re_bytes::Regex::from(self)
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
        &self.prog.matches
    }

    /// Return a slice of capture names.
    ///
    /// Any capture that isn't named is None.
    pub fn captures(&self) -> &[Option<String>] {
        &self.prog.captures
    }

    /// Return a reference to named groups mapping (from group name to
    /// group position).
    pub fn capture_name_idx(&self) -> &Arc<HashMap<String, usize>> {
        &self.prog.capture_name_idx
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
