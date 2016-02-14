// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use backtrack::{self, Backtrack};
use dfa::{self, Dfa, DfaResult};
use input::{ByteInput, CharInput};
use nfa::Nfa;
use program::{Program, ProgramBuilder};
use re::CaptureIdxs;

use {Regex, Error};

/// Exec manages the execution of a regular expression.
///
/// In particular, this manages the various compiled forms of a single regular
/// expression and the choice of which matching engine to use to execute a
/// regular expression.
#[derive(Clone, Debug)]
pub struct Exec {
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
pub struct ExecBuilder<'r> {
    re: &'r str,
    match_engine: MatchEngine,
    size_limit: usize,
    bytes: bool,
}

impl<'r> ExecBuilder<'r> {
    /// Create a regex execution builder.
    ///
    /// This uses default settings for everything except the regex itself,
    /// which must be provided. Further knobs can be set by calling methods,
    /// and then finally, `build` to actually create the executor.
    pub fn new(re: &'r str) -> Self {
        ExecBuilder {
            re: re,
            match_engine: MatchEngine::Automatic,
            size_limit: 10 * (1 << 20),
            bytes: false,
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
    /// classes. However, it may be useful (some day) for matching on raw
    /// bytes that may not be UTF-8.
    ///
    /// Note that this does not impact DFA matching engines, which always
    /// execute on bytes.
    pub fn bytes(mut self, yes: bool) -> Self {
        self.bytes = yes;
        self
    }

    /// Build an executor that can run a regular expression.
    pub fn build(self) -> Result<Exec, Error> {
        let prog = try!(
            ProgramBuilder::new(self.re)
                           .size_limit(self.size_limit)
                           .bytes(self.bytes)
                           .compile());
        let dfa = try!(
            ProgramBuilder::new(self.re)
                           .size_limit(self.size_limit)
                           .dfa(true)
                           .compile());
        let dfa_reverse = try!(
            ProgramBuilder::new(self.re)
                           .size_limit(self.size_limit)
                           .dfa(true)
                           .reverse(true)
                           .compile());
        let can_dfa = dfa::can_exec(&dfa.insts);
        Ok(Exec {
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
    pub fn exec(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        // Why isn't the DFA or literal engine checked for here? Well, it's
        // only possible to execute those engines in exec_auto. See comment on
        // MatchEngine below for more details.
        match self.match_engine {
            MatchEngine::Automatic => self.exec_auto(caps, text, start),
            MatchEngine::Backtrack => self.exec_backtrack(caps, text, start),
            MatchEngine::Nfa => self.exec_nfa(caps, text, start),
        }
    }

    /// Like exec, but always selects the engine automatically.
    pub fn exec_auto(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        if caps.len() <= 2 && self.prog.is_prefix_match() {
            // We should be able to execute the literal engine even if there
            // are more captures by falling back to the NFA engine after a
            // match. However, that's effectively what the NFA engine does
            // already (since it will use the literal engine if it exists).
            self.exec_literals(caps, text, start)
        } else if self.can_dfa {
            self.exec_dfa(caps, text, start)
        } else {
            self.exec_auto_nfa(caps, text, start)
        }
    }

    /// Like exec, but always tries to execute the lazy DFA.
    ///
    /// Note that self.can_dfa must be true. This will panic otherwise.
    fn exec_dfa(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        debug_assert!(self.can_dfa);
        let btext = text.as_bytes();
        let search = Dfa::exec(&self.dfa, btext, start, caps.is_empty());
        let match_end = match search {
            DfaResult::Match(match_end) => match_end,
            DfaResult::EarlyMatch => return true,
            DfaResult::NoMatch => return false,
        };
        // If caller has not requested any captures, then we don't need to
        // find the start position.
        if caps.is_empty() {
            return true;
        }
        // invariant: caps.len() >= 2 && caps.len() % 2 == 0
        // If the reported end of the match is the same as the start, then we
        // have an empty match and we can quit now.
        if start == match_end {
            // Be careful... If the caller wants sub-captures, than we are
            // obliged to run the NFA to get them.
            if caps.len() == 2 {
                // The caller only needs the start/end, so we can avoid the
                // NFA here.
                caps[0] = Some(start);
                caps[1] = Some(start);
                return true;
            }
            return self.exec_auto_nfa(caps, text, start);
        }
        // OK, now we find the start of the match by running the DFA backwards
        // on the text. We *start* the search at the end of the match.
        let search = Dfa::exec(
            &self.dfa_reverse, &btext[start..], match_end - start, false);
        let match_start = match search {
            DfaResult::Match(match_start) => start + match_start,
            DfaResult::EarlyMatch => {
                panic!("BUG: early matches can't happen on reverse search")
            }
            DfaResult::NoMatch => {
                panic!("BUG: forward match implies backward match")
            }
        };
        if caps.len() == 2 {
            // If the caller doesn't care about capture locations, then we can
            // avoid running the NFA to fill them in.
            caps[0] = Some(match_start);
            caps[1] = Some(match_end);
            return true;
        }
        self.exec_auto_nfa(caps, text, match_start)
    }

    /// This is like exec_auto, except it always chooses between either the
    /// full NFA simulation or the bounded backtracking engine.
    fn exec_auto_nfa(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        if backtrack::should_exec(self.prog.insts.len(), text.len()) {
            self.exec_backtrack(caps, text, start)
        } else {
            self.exec_nfa(caps, text, start)
        }
    }

    /// Always run the NFA algorithm.
    fn exec_nfa(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        if self.prog.insts.is_bytes() {
            Nfa::exec(&self.prog, caps, ByteInput::new(text), start)
        } else {
            Nfa::exec(&self.prog, caps, CharInput::new(text), start)
        }
    }

    /// Always runs the NFA using bounded backtracking.
    fn exec_backtrack(
        &self,
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        if self.prog.insts.is_bytes() {
            Backtrack::exec(&self.prog, caps, ByteInput::new(text), start)
        } else {
            Backtrack::exec(&self.prog, caps, CharInput::new(text), start)
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
        caps: &mut CaptureIdxs,
        text: &str,
        start: usize,
    ) -> bool {
        debug_assert!(self.prog.is_prefix_match());
        match self.prog.prefixes.find(&text.as_bytes()[start..]) {
            None => false,
            Some((s, e)) => {
                if caps.len() == 2 {
                    caps[0] = Some(start + s);
                    caps[1] = Some(start + e);
                }
                true
            }
        }
    }

    /// Build a dynamic Regex from this executor.
    pub fn into_regex(self) -> Regex {
        Regex::Dynamic(self)
    }

    /// Return the original regular expression string.
    pub fn regex_str(&self) -> &str {
        &self.prog.original
    }

    /// Return a slice of capture names.
    ///
    /// Any capture that isn't named is None.
    pub fn capture_names(&self) -> &[Option<String>] {
        &self.prog.cap_names
    }

    /// Return a fresh allocation for storing all possible captures in the
    /// underlying regular expression.
    pub fn alloc_captures(&self) -> Vec<Option<usize>> {
        self.prog.alloc_captures()
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
