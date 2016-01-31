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
use literals::Literals;

/// Represents a location in the input.
#[derive(Clone, Copy, Debug)]
pub struct InputAt {
    pos: usize,
    c: Char,
    byte: Option<u8>,
    len: usize,
}

impl InputAt {
    /// Returns true iff this position is at the beginning of the input.
    pub fn is_beginning(&self) -> bool {
        self.pos == 0
    }

    /// Returns true iff this position is past the end of the input.
    pub fn is_end(&self) -> bool {
        self.c.is_none() && self.byte.is_none()
    }

    /// Returns the character at this position.
    ///
    /// If this position is just before or after the input, then an absent
    /// character is returned.
    pub fn char(&self) -> Char {
        self.c
    }

    /// Returns the byte at this position.
    pub fn byte(&self) -> Option<u8> {
        self.byte
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

    /// Return the Unicode character occurring next to `at`.
    ///
    /// If no such character could be decoded, then `Char` is absent.
    fn next_char(&self, at: InputAt) -> Char;

    /// Return the Unicode character occurring previous to `at`.
    ///
    /// If no such character could be decoded, then `Char` is absent.
    fn previous_char(&self, at: InputAt) -> Char;

    /// Scan the input for a matching prefix.
    fn prefix_at(&self, prefixes: &Literals, at: InputAt) -> Option<InputAt>;

    /// The number of bytes in the input.
    fn len(&self) -> usize;

    fn as_bytes(&self) -> &[u8];
}

impl<'a, T: Input> Input for &'a T {
    fn at(&self, i: usize) -> InputAt { (**self).at(i) }
    fn next_char(&self, at: InputAt) -> Char { (**self).next_char(at) }
    fn previous_char(&self, at: InputAt) -> Char { (**self).previous_char(at) }
    fn prefix_at(&self, prefixes: &Literals, at: InputAt) -> Option<InputAt> {
        (**self).prefix_at(prefixes, at)
    }
    fn len(&self) -> usize { (**self).len() }
    fn as_bytes(&self) -> &[u8] { (**self).as_bytes() }
}

/// An input reader over characters.
#[derive(Clone, Copy, Debug)]
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
            byte: None,
            len: c.len_utf8(),
        }
    }

    fn next_char(&self, at: InputAt) -> Char {
        at.char()
    }

    fn previous_char(&self, at: InputAt) -> Char {
        self[..at.pos()].chars().rev().next().into()
    }

    fn prefix_at(&self, prefixes: &Literals, at: InputAt) -> Option<InputAt> {
        prefixes
            .find(&self.as_bytes()[at.pos()..])
            .map(|(s, _)| self.at(at.pos() + s))
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

/// An input reader over bytes.
///
/// N.B. We represent the reader with a string for now, since that gives us
/// easy access to necessary Unicode decoding (used for word boundary look
/// ahead/look behind).
#[derive(Clone, Copy, Debug)]
pub struct ByteInput<'t>(&'t str);

impl<'t> ByteInput<'t> {
    /// Return a new byte-based input reader for the given string.
    pub fn new(s: &'t str) -> ByteInput<'t> {
        ByteInput(s)
    }
}

impl<'t> ops::Deref for ByteInput<'t> {
    type Target = str;

    fn deref(&self) -> &str {
        self.0
    }
}

impl<'t> Input for ByteInput<'t> {
    #[inline(always)]
    fn at(&self, i: usize) -> InputAt {
        InputAt {
            pos: i,
            c: None.into(),
            byte: self.as_bytes().get(i).map(|&b| b),
            len: 1,
        }
    }

    fn next_char(&self, at: InputAt) -> Char {
        self[at.pos()..].chars().next().into()
    }

    fn previous_char(&self, at: InputAt) -> Char {
        self[..at.pos()].chars().rev().next().into()
    }

    fn prefix_at(&self, prefixes: &Literals, at: InputAt) -> Option<InputAt> {
        prefixes
            .find(&self.as_bytes()[at.pos()..])
            .map(|(s, _)| self.at(at.pos() + s))
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}
