// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/// Slot is a single saved capture location. Note that there are two slots for
/// every capture in a regular expression (one slot each for the start and end
/// of the capture).
pub type Slot = Option<usize>;

/// RegularExpression describes types that can implement regex searching.
///
/// This trait is my attempt at reducing code duplication and to standardize
/// the internal API. Specific duplication that is avoided are the `find`
/// and `capture` iterators, which are slightly tricky.
///
/// It's not clear whether this trait is worth it, and it also isn't
/// clear whether it's useful as a public trait or not. Methods like
/// `next_after_empty` reak of bad design, but the rest of the methods seem
/// somewhat reasonable. One particular thing this trait would expose would be
/// the ability to start the search of a regex anywhere in a haystack, which
/// isn't possible in the current public API.
pub trait RegularExpression: Sized {
    /// The type of the haystack.
    type Text: ?Sized;

    /// The number of capture slots in the compiled regular expression. This is
    /// always two times the number of capture groups (two slots per group).
    fn slots_len(&self) -> usize;

    /// Returns the position of the next character after `i`.
    ///
    /// For example, a haystack with type `&[u8]` probably returns `i+1`,
    /// whereas a haystack with type `&str` probably returns `i` plus the
    /// length of the next UTF-8 sequence.
    fn next_after_empty(&self, text: &Self::Text, i: usize) -> usize;

    /// Returns the location of the shortest match.
    fn shortest_match_at(
        &self,
        text: &Self::Text,
        start: usize,
    ) -> Option<usize>;

    /// Returns whether the regex matches the text given.
    fn is_match_at(
        &self,
        text: &Self::Text,
        start: usize,
    ) -> bool;

    /// Returns the leftmost-first match location if one exists.
    fn find_at(
        &self,
        text: &Self::Text,
        start: usize,
    ) -> Option<(usize, usize)>;

    /// Returns the leftmost-first match location if one exists, and also
    /// fills in any matching capture slot locations.
    fn read_captures_at(
        &self,
        slots: &mut [Slot],
        text: &Self::Text,
        start: usize,
    ) -> Option<(usize, usize)>;

    /// Returns an iterator over all non-overlapping successive leftmost-first
    /// matches.
    fn find_iter<'t>(
        self,
        text: &'t Self::Text,
    ) -> FindMatches<'t, Self> {
        FindMatches {
            re: self,
            text: text,
            last_end: 0,
            last_match: None,
        }
    }

    /// Returns an iterator over all non-overlapping successive leftmost-first
    /// matches with captures.
    fn captures_iter<'t>(
        self,
        text: &'t Self::Text,
    ) -> FindCaptures<'t, Self> {
        FindCaptures(self.find_iter(text))
    }
}

/// An iterator over all non-overlapping successive leftmost-first matches.
pub struct FindMatches<'t, R> where R: RegularExpression, R::Text: 't {
    re: R,
    text: &'t R::Text,
    last_end: usize,
    last_match: Option<usize>,
}

impl<'t, R> FindMatches<'t, R> where R: RegularExpression, R::Text: 't {
    /// Return the text being searched.
    pub fn text(&self) -> &'t R::Text {
        self.text
    }

    /// Return the underlying regex.
    pub fn regex(&self) -> &R {
        &self.re
    }
}

impl<'t, R> Iterator for FindMatches<'t, R>
        where R: RegularExpression, R::Text: 't + AsRef<[u8]> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        if self.last_end > self.text.as_ref().len() {
            return None;
        }
        let (s, e) = match self.re.find_at(self.text, self.last_end) {
            None => return None,
            Some((s, e)) => (s, e),
        };
        if s == e {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            self.last_end = self.re.next_after_empty(&self.text, e);
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(e) == self.last_match {
                return self.next();
            }
        } else {
            self.last_end = e;
        }
        self.last_match = Some(e);
        Some((s, e))
    }
}

/// An iterator over all non-overlapping successive leftmost-first matches with
/// captures.
pub struct FindCaptures<'t, R>(FindMatches<'t, R>)
    where R: RegularExpression, R::Text: 't;

impl<'t, R> FindCaptures<'t, R> where R: RegularExpression, R::Text: 't {
    /// Return the text being searched.
    pub fn text(&self) -> &'t R::Text {
        self.0.text()
    }

    /// Return the underlying regex.
    pub fn regex(&self) -> &R {
        self.0.regex()
    }
}

impl<'t, R> Iterator for FindCaptures<'t, R>
        where R: RegularExpression, R::Text: 't + AsRef<[u8]> {
    type Item = Vec<Slot>;

    fn next(&mut self) -> Option<Vec<Slot>> {
        if self.0.last_end > self.0.text.as_ref().len() {
            return None
        }
        let mut slots = vec![None; self.0.re.slots_len()];
        let (s, e) = match self.0.re.read_captures_at(
            &mut slots,
            self.0.text,
            self.0.last_end,
        ) {
            None => return None,
            Some((s, e)) => (s, e),
        };
        if s == e {
            self.0.last_end = self.0.re.next_after_empty(&self.0.text, e);
            if Some(e) == self.0.last_match {
                return self.next();
            }
        } else {
            self.0.last_end = e;
        }
        self.0.last_match = Some(e);
        Some(slots)
    }
}
