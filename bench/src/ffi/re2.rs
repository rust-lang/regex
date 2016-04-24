// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(non_camel_case_types)]

use libc::{c_uchar, c_int, c_void};

/// Regex wraps an RE2 regular expression.
///
/// It cannot be used safely from multiple threads simultaneously.
pub struct Regex {
    re: *mut re2_regexp,
}

unsafe impl Send for Regex {}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe { re2_regexp_free(self.re); }
    }
}

#[derive(Debug)]
pub struct Error(());

impl Regex {
    pub fn new(pattern: &str) -> Result<Regex, Error> {
        unsafe { Ok(Regex { re: re2_regexp_new(pattern.into()) }) }
    }

    pub fn is_match(&self, text: &str) -> bool {
        unsafe {
            re2_regexp_match(self.re, text.into(), 0, text.len() as c_int)
        }
    }

    pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> FindMatches<'r, 't> {
        FindMatches {
            re: self,
            text: text,
            last_end: 0,
            last_match: None,
        }
    }

    fn find_at(&self, text: &str, start: usize) -> Option<(usize, usize)> {
        let (mut s, mut e): (c_int, c_int) = (0, 0);
        let matched = unsafe {
            re2_regexp_find(
                self.re,
                text.into(),
                start as c_int,
                text.len() as c_int,
                &mut s,
                &mut e,
            )
        };
        if matched {
            Some((s as usize, e as usize))
        } else {
            None
        }
    }
}

pub struct FindMatches<'r, 't> {
    re: &'r Regex,
    text: &'t str,
    last_end: usize,
    last_match: Option<usize>,
}

// This implementation is identical to the one Rust uses, since both Rust's
// regex engine and RE2 handle empty matches in the same way.
impl<'r, 't> Iterator for FindMatches<'r, 't> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        fn next_after_empty(text: &str, i: usize) -> usize {
            let b = match text.as_bytes().get(i) {
                None => return text.len() + 1,
                Some(&b) => b,
            };
            let inc = if b <= 0x7F {
                1
            } else if b <= 0b110_11111 {
                2
            } else if b <= 0b1110_1111 {
                3
            } else {
                4
            };
            i + inc
        }

        if self.last_end > self.text.len() {
            return None;
        }
        let (s, e) = match self.re.find_at(self.text, self.last_end) {
            None => return None,
            Some((s, e)) => (s, e),
        };
        assert!(s >= self.last_end);
        if s == e {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            self.last_end = next_after_empty(&self.text, e);
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(e) == self.last_match {
                return self.next();
            }
        } else {
            self.last_end = e;
        }
        self.last_match = Some(self.last_end);
        Some((s, e))
    }
}

// RE2 FFI is below. Note that this uses a hand-rolled C API that is defined
// in re2.cpp.

type re2_regexp = c_void;

#[repr(C)]
struct re2_string {
    text: *const c_uchar,
    len: c_int,
}

impl<'a> From<&'a str> for re2_string {
    fn from(s: &'a str) -> re2_string {
        re2_string { text: s.as_ptr(), len: s.len() as c_int }
    }
}

extern {
    fn re2_regexp_new(pat: re2_string) -> *mut re2_regexp;
    fn re2_regexp_free(re: *mut re2_regexp);
    fn re2_regexp_match(
        re: *mut re2_regexp,
        text: re2_string,
        startpos: c_int,
        endpos: c_int,
    ) -> bool;
    fn re2_regexp_find(
        re: *mut re2_regexp,
        text: re2_string,
        startpos: c_int,
        endpos: c_int,
        match_start: *mut c_int,
        match_end: *mut c_int,
    ) -> bool;
}
