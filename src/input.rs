// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::ops;

use char::Char;
use prefix::Prefix;

/// Represents a location in the input.
#[derive(Clone, Copy, Debug)]
pub struct InputAt {
    pos: usize,
    c: Char,
    len: usize,
}

impl InputAt {
    /// Returns true iff this position is at the beginning of the input.
    pub fn is_beginning(&self) -> bool {
        self.pos == 0
    }

    /// Returns the character at this position.
    ///
    /// If this position is just before or after the input, then an absent
    /// character is returned.
    pub fn char(&self) -> Char {
        self.c
    }

    /// Returns the UTF-8 width of the character at this position.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns the byte offset of this position.
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// Returns the byte offset of the next position in the input.
    pub fn next_pos(&self) -> usize {
        self.pos + self.len
    }
}

/// An abstraction over input used in the matching engines.
pub trait Input {
    /// Return an encoding of the position at byte offset `i`.
    fn at(&self, i: usize) -> InputAt;
    /// Return an encoding of the char position just prior to byte offset `i`.
    fn previous_at(&self, i: usize) -> InputAt;
    /// Scan the input for a matching prefix.
    fn prefix_at(&self, prefixes: &Prefix, at: InputAt) -> Option<InputAt>;
}

/// An input reader over characters.
///
/// (This is the only implementation of `Input` at the moment.)
#[derive(Debug)]
pub struct CharInput<'t>(&'t str);

impl<'t> CharInput<'t> {
    /// Return a new character input reader for the given string.
    pub fn new(s: &'t str) -> CharInput<'t> {
        CharInput(s)
    }
}

impl<'t> ops::Deref for CharInput<'t> {
    type Target = str;

    fn deref(&self) -> &str {
        self.0
    }
}

impl<'t> Input for CharInput<'t> {
    // This `inline(always)` increases throughput by almost 25% on the `hard`
    // benchmarks over a normal `inline` annotation.
    //
    // I'm not sure why `#[inline]` isn't enough to convince LLVM, but it is
    // used *a lot* in the guts of the matching engines.
    #[inline(always)]
    fn at(&self, i: usize) -> InputAt {
        let c = self[i..].chars().next().into();
        InputAt {
            pos: i,
            c: c,
            len: c.len_utf8(),
        }
    }

    fn previous_at(&self, i: usize) -> InputAt {
        let c: Char = self[..i].chars().rev().next().into();
        let len = c.len_utf8();
        InputAt {
            pos: i - len,
            c: c,
            len: len,
        }
    }

    fn prefix_at(&self, prefixes: &Prefix, at: InputAt) -> Option<InputAt> {
        prefixes.find(&self[at.pos()..]).map(|(s, _)| self.at(at.pos() + s))
    }
}
