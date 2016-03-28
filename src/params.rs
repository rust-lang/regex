// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// This module defines the internal parameter interface to matching engines.
// This doesn't actually contain any searching code.

/// Search is a set of parameters that influence the behavior of matching
/// engines. In general, a given set of search parameters influence all the
/// matching engines in the same way.
#[derive(Debug)]
pub struct Params<'c, 'm> {
    /// Storage for capture slots.
    captures: &'c mut [Slot],
    /// Which regular expression matched. For a `Regex`, this always has
    /// length 1, but a `RegexSet` has length equal to the number of regexes
    /// in the set.
    matches: &'m mut [bool],
    /// Whether `true` exists in `matches`.
    matched_any: bool,
    /// When true, always use "match-short" semantics.
    match_short: bool,
}

/// Slot is a single saved capture location. Note that there are two slots for
/// every capture in a regular expression (one slot each for the start and end
/// of the capture).
pub type Slot = Option<usize>;

/// The matching semantics implied by a given set of search parameters.
///
/// In general, each semantics is more expensive than the previous one.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MatchStyle {
    /// "match-short" semantics means that the matching engine will do only
    /// as much work as needed to discover whether an expression matches
    /// the given text or not. Generally, match-short semantics means that
    /// capture locations are not tracked. However, if memory is given to track
    /// captures, then the ending location of the *shortest match* will be
    /// returned. The starting location may not be returned.
    Short,
    /// "all-match" semantics means that more than one regular expression is
    /// being executed and that the matching engine should answer whether each
    /// regular expression matches or not. (This is only applicable for regex
    /// sets.)
    All,
    /// "find" semantics means that the matching engine will find the proper
    /// leftmost-first match and return the start and end location of that
    /// match. Other capture groups are not tracked.
    Find,
    /// "capture" semantics means that the locations of all captures in the
    /// regular expression are tracked.
    Capture,
}

impl MatchStyle {
    pub fn match_only(&self) -> bool {
        match *self {
            MatchStyle::Short | MatchStyle::All => true,
            _ => false,
        }
    }

    pub fn quit_after_first_match(&self) -> bool {
        *self == MatchStyle::Short
    }
}

impl<'c, 'm> Params<'c, 'm> {
    /// Creates a new set of search parameters.
    ///
    /// A default set of search parameters results in match-short semantics.
    pub fn new(caps: &'c mut [Slot], mats: &'m mut [bool]) -> Params<'c, 'm> {
        Params {
            captures: caps,
            matches: mats,
            matched_any: false,
            match_short: false,
        }
    }

    /// Resets the parameters as if no search has happened.
    ///
    /// This is useful for reusing the same set of parameters for multiple
    /// independent searches.
    ///
    /// This method never changes the match semantics implied by the
    /// parameters.
    pub fn reset(&mut self) {
        for slot in self.captures.iter_mut() {
            *slot = None;
        }
        for m in self.matches.iter_mut() {
            *m = false;
        }
    }

    /// Force match-short semantics.
    ///
    /// When this is enabled (it's disabled by default), match-short semantics
    /// will always be used.
    ///
    /// Note that if backing storage for captures is provided then some of them
    /// may be filled in after a search (but may not represent leftmost-first
    /// match locations).
    pub fn set_match_short(mut self, yes: bool) -> Params<'c, 'm> {
        self.match_short = yes;
        self
    }

    pub fn alloc_captures(n: usize) -> Vec<Slot> {
        vec![None; 2 * n]
    }

    pub fn alloc_matches(n: usize) -> Vec<bool> {
        vec![false; n]
    }

    /// Returns all capture slots. (There are 2 slots for every capture group.)
    pub fn captures(&self) -> &[Slot] {
        &self.captures
    }

    /// Returns mutable capture slots. (There are 2 slots for every capture
    /// group.)
    pub fn captures_mut(&mut self) -> &mut [Slot] {
        &mut self.captures
    }

    /// Returns true if one or more regular expressions matched.
    pub fn is_match(&self) -> bool {
        self.matched_any
    }

    /// Returns all matches.
    pub fn matches(&self) -> &[bool] {
        &self.matches
    }

    /// Sets the capture slot at index `i` to the slot given.
    ///
    /// If `i` does not point to a valid capture slot, then this is a no-op.
    #[doc(hidden)]
    pub fn set_capture(&mut self, i: usize, slot: Slot) {
        if let Some(old_slot) = self.captures.get_mut(i) {
            *old_slot = slot;
        }
    }

    /// Sets the start location of the match.
    #[doc(hidden)]
    pub fn set_start(&mut self, slot: Slot) {
        self.set_capture(0, slot)
    }

    /// Set the end location of the match.
    #[doc(hidden)]
    pub fn set_end(&mut self, slot: Slot) {
        self.set_capture(1, slot)
    }

    /// Copies the given capture slots into self's capture slots.
    #[doc(hidden)]
    pub fn copy_captures_from(&mut self, caps: &[Slot]) {
        for (slot, val) in self.captures.iter_mut().zip(caps.iter()) {
            *slot = *val;
        }
    }

    /// Indicates that the regular expression at index `i` matches.
    #[doc(hidden)]
    pub fn set_match(&mut self, i: usize) {
        self.matched_any = true;
        if let Some(old) = self.matches.get_mut(i) {
            *old = true;
        }
    }

    /// Returns the style of matching implied by these parameters.
    #[doc(hidden)]
    pub fn style(&self) -> MatchStyle {
        if self.match_short {
            MatchStyle::Short
        } else if self.captures.is_empty() {
            if self.matches.len() > 1 {
                MatchStyle::All
            } else {
                MatchStyle::Short
            }
        } else if self.captures.len() > 2 {
            MatchStyle::Capture
        } else {
            MatchStyle::Find
        }
    }
}
