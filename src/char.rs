// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::char;
use std::cmp::Ordering;
use std::fmt;
use std::u32;

use syntax;

/// An inline representation of `Option<char>`.
///
/// This eliminates the need to do case analysis on `Option<char>` to determine
/// ordinality with other characters.
///
/// (The `Option<char>` is not related to encoding. Instead, it is used in the
/// matching engines to represent the beginning and ending boundaries of the
/// search text.)
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Char(u32);

impl fmt::Debug for Char {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match char::from_u32(self.0) {
            None => write!(f, "Empty"),
            Some(c) => write!(f, "{:?}", c),
        }
    }
}

impl Char {
    /// Returns true iff the character is absent.
    #[inline]
    pub fn is_none(self) -> bool { self.0 == u32::MAX }

    /// Returns the length of the character's UTF-8 encoding.
    ///
    /// If the character is absent, then `0` is returned.
    #[inline]
    pub fn len_utf8(self) -> usize {
        char::from_u32(self.0).map_or(0, |c| c.len_utf8())
    }

    /// Returns true iff the character is a word character.
    ///
    /// If the character is absent, then false is returned.
    pub fn is_word_char(self) -> bool {
        char::from_u32(self.0).map_or(false, syntax::is_word_char)
    }

    /// Converts the character to a real primitive `char`.
    ///
    /// If the character is absent, then `None` is returned.
    pub fn as_char(self) -> Option<char> {
        // This is only used in the `regex!` macro because it expands char
        // classes into `match` expressions (instead of binary search).
        char::from_u32(self.0)
    }
}

impl From<char> for Char {
    fn from(c: char) -> Char { Char(c as u32) }
}

impl From<Option<char>> for Char {
    fn from(c: Option<char>) -> Char {
        c.map_or(Char(u32::MAX), |c| c.into())
    }
}

impl PartialEq<char> for Char {
    #[inline]
    fn eq(&self, other: &char) -> bool { self.0 == *other as u32 }
}

impl PartialEq<Char> for char {
    #[inline]
    fn eq(&self, other: &Char) -> bool { *self as u32 == other.0 }
}

impl PartialOrd<char> for Char {
    #[inline]
    fn partial_cmp(&self, other: &char) -> Option<Ordering> {
        self.0.partial_cmp(&(*other as u32))
    }
}

impl PartialOrd<Char> for char {
    #[inline]
    fn partial_cmp(&self, other: &Char) -> Option<Ordering> {
        (*self as u32).partial_cmp(&other.0)
    }
}
