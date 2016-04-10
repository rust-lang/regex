// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// These benchmarks use PCRE to reproduce some of the benchmarks used to track
// performance of regexes in this crate. I'm not an experienced user of PCRE,
// so it's possible that usage here is not optimal. If it isn't, then
// improvements are welcome. (I'm aware that PCRE has a DFA, but it doesn't
// seem to actually preserve leftmost-first semantics, like the DFA in this
// crate does.)
//
// Note that for these benchmarks, all we need is to detect whether there is
// a match or not.

#![feature(test)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]

#[macro_use]
extern crate lazy_static;
extern crate libc;
extern crate test;

use std::fmt;
use std::ptr;
use std::str;

use libc::{c_int, c_void, size_t, uint8_t, uint32_t};

const PCRE2_UCP: uint32_t = 0x00020000;
const PCRE2_UTF: uint32_t = 0x00080000;
const PCRE2_NO_UTF_CHECK: uint32_t = 0x40000000;
const PCRE2_JIT_COMPLETE: uint32_t = 0x00000001;
const PCRE2_ERROR_NOMATCH: c_int = -1;

type code = c_void;

type match_data = c_void;

type compile_context = c_void; // unused

type general_context = c_void; // unused

type match_context = c_void; // unused

extern {
    fn pcre2_compile_8(
        pattern: *const uint8_t,
        len: size_t,
        options: uint32_t,
        error_code: *mut c_int,
        error_offset: *mut size_t,
        context: *mut compile_context,
    ) -> *mut code;

    fn pcre2_code_free_8(
        code: *mut code,
    );

    fn pcre2_match_data_create_from_pattern_8(
        code: *const code,
        context: *mut general_context,
    ) -> *mut match_data;

    fn pcre2_match_data_free_8(
        match_data: *mut match_data,
    );

    fn pcre2_get_ovector_pointer_8(
        match_data: *mut match_data,
    ) -> *mut size_t;

    fn pcre2_match_8(
        code: *const code,
        subject: *const uint8_t,
        length: size_t,
        startoffset: size_t,
        options: uint32_t,
        match_data: *mut match_data,
        match_context: *mut match_context,
    ) -> c_int;

    fn pcre2_jit_compile_8(
        code: *const code,
        options: uint32_t,
    ) -> c_int;

    fn pcre2_jit_match_8(
        code: *const code,
        subject: *const uint8_t,
        length: size_t,
        startoffset: size_t,
        options: uint32_t,
        match_data: *mut match_data,
        match_context: *mut match_context,
    ) -> c_int;

    fn pcre2_get_error_message_8(
        error_code: c_int,
        buf: *mut uint8_t,
        buflen: size_t,
    ) -> c_int;
}

struct Regex {
    code: *mut code,
    match_data: *mut match_data,
    ovector: *mut size_t,
}

// Regex can't be used safely from multiple threads simultaneously, so this is
// a lie and therefore unsafe. It is, however, convenient and fine for the
// purposes of benchmarking where a Regex is only ever used in one thread.
unsafe impl Send for Regex {}
unsafe impl Sync for Regex {}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe { pcre2_code_free_8(self.code); }
    }
}

struct Error {
    code: c_int,
    offset: size_t,
}

struct FindMatches<'r, 't> {
    re: &'r Regex,
    text: &'t str,
    last_match_end: usize,
}

impl Regex {
    fn new(pattern: &str) -> Result<Regex, Error> {
        let mut error_code: c_int = 0;
        let mut error_offset: size_t = 0;
        let code = unsafe { pcre2_compile_8(
            pattern.as_ptr(),
            pattern.len(),
            PCRE2_UCP | PCRE2_UTF,
            &mut error_code,
            &mut error_offset,
            ptr::null_mut(),
        ) };
        if code.is_null() {
            return Err(Error {
                code: error_code,
                offset: error_offset,
            });
        }
        let err = unsafe { pcre2_jit_compile_8(code, PCRE2_JIT_COMPLETE) };
        if err < 0 {
            panic!("pcre2_jit_compile_8 failed with error: {:?}", err);
        }
        let match_data = unsafe { pcre2_match_data_create_from_pattern_8(
            code,
            ptr::null_mut(),
        ) };
        if match_data.is_null() {
            panic!("could not allocate match_data");
        }
        let ovector = unsafe { pcre2_get_ovector_pointer_8(match_data) };
        if ovector.is_null() {
            panic!("could not get ovector");
        }
        Ok(Regex { code: code, match_data: match_data, ovector: ovector })
    }

    fn _match(&self, text: &str, start: usize) -> Option<(usize, usize)> {
        let err = unsafe { pcre2_jit_match_8(
            self.code,
            text.as_ptr(),
            text.len(),
            start,
            PCRE2_NO_UTF_CHECK,
            self.match_data,
            ptr::null_mut(),
        ) };
        if err == PCRE2_ERROR_NOMATCH {
            None
        } else if err < 0 {
            panic!("unknown error code: {:?}", err)
        } else {
            let s = unsafe { *self.ovector };
            let e = unsafe { *self.ovector.offset(1) };
            Some((s, e))
        }
    }

    fn is_match(&self, text: &str) -> bool {
        self._match(text, 0).is_some()
    }

    fn find_iter<'r, 't>(&'r self, text: &'t str) -> FindMatches<'r, 't> {
        FindMatches {
            re: self,
            text: text,
            last_match_end: 0,
        }
    }
}

impl<'r, 't> Iterator for FindMatches<'r, 't> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        match self.re._match(self.text, self.last_match_end) {
            None => None,
            Some((s, e)) => {
                self.last_match_end = e;
                Some((s, e))
            }
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const BUF_LEN: size_t = 256;
        let mut buf = [0; BUF_LEN];
        let len = unsafe { pcre2_get_error_message_8(
            self.code,
            buf.as_mut_ptr(),
            BUF_LEN,
        ) };
        if len < 0 {
            write!(f, "Unknown PCRE error. (code: {:?}, offset: {:?})",
                   self.code, self.offset)
        } else {
            let msg = str::from_utf8(&buf[..len as usize]).unwrap();
            write!(f, "error at {:?}: {}", self.offset, msg)
        }
    }
}

macro_rules! regex(
    ($re:expr) => { ::Regex::new($re).unwrap() }
);

mod misc;
mod sherlock;
