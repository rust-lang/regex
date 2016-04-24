// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(non_snake_case)]

use std::ffi::{CString, CStr};
use std::fmt;
use std::ptr;

use libc::{c_char, c_int, c_void};
use libpcre_sys::{
    PCRE_UTF8, PCRE_NO_UTF8_CHECK, PCRE_ERROR_NOMATCH,
    pcre, pcre_extra,
    pcre_compile, pcre_free, pcre_study, pcre_free_study, pcre_exec,
};

const PCRE_UCP: c_int = 0x20000000;
const PCRE_STUDY_JIT_COMPLETE: c_int = 0x0001;

// We use libpcre-sys directly because the pcre crate has unavoidable
// performance problems in its core matching routines. (e.g., It always
// allocates an ovector.)
pub struct Regex {
    code: *mut pcre,
    extra: *mut pcre_extra,
}

unsafe impl Send for Regex {}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe {
            pcre_free_study(self.extra);
            pcre_free(self.code as *mut c_void);
        }
    }
}

pub struct Error {
    msg: String,
    offset: c_int,
}

impl Regex {
    pub fn new(pattern: &str) -> Result<Regex, Error> {
        let pattern = CString::new(pattern.to_owned()).unwrap();
        let mut errptr: *const c_char = ptr::null();
        let mut erroffset: c_int = 0;
        let code = unsafe { pcre_compile(
            pattern.as_ptr(),
            PCRE_UCP | PCRE_UTF8,
            &mut errptr,
            &mut erroffset,
            ptr::null(),
        ) };
        if code.is_null() {
            let msg = unsafe {
                CStr::from_ptr(errptr).to_str().unwrap().to_owned()
            };
            return Err(Error { msg: msg, offset: erroffset });
        }

        let extra = unsafe { pcre_study(
            code,
            PCRE_STUDY_JIT_COMPLETE,
            &mut errptr,
        ) };
        if extra.is_null() {
            if errptr.is_null() {
                panic!("unexpected error. Maybe JIT support isn't enabled?");
            }
            let msg = unsafe {
                CStr::from_ptr(errptr).to_str().unwrap().to_owned()
            };
            return Err(Error { msg: msg, offset: 0 });
        }
        Ok(Regex { code: code, extra: extra })
    }

    pub fn is_match(&self, text: &str) -> bool {
        self.find_at(text, 0).is_some()
    }

    pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> FindMatches<'r, 't> {
        FindMatches {
            re: self,
            text: text,
            last_match_end: 0,
        }
    }

    fn find_at(&self, text: &str, start: usize) -> Option<(usize, usize)> {
        const OVEC_SIZE: usize = 15 * 3; // hopefully enough for benchmarks?
        let mut ovec: [c_int; OVEC_SIZE] = [0; OVEC_SIZE];
        let err = unsafe { pcre_exec(
            self.code,
            self.extra,
            text.as_ptr() as *const i8,
            text.len() as c_int,
            start as c_int,
            PCRE_NO_UTF8_CHECK,
            ovec.as_mut_ptr(),
            OVEC_SIZE as c_int,
        ) };
        if err == PCRE_ERROR_NOMATCH {
            None
        } else if err < 0 {
            panic!("unknown error code: {:?}", err)
        } else {
            Some((ovec[0] as usize, ovec[1] as usize))
        }
    }
}

pub struct FindMatches<'r, 't> {
    re: &'r Regex,
    text: &'t str,
    last_match_end: usize,
}

impl<'r, 't> Iterator for FindMatches<'r, 't> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        match self.re.find_at(self.text, self.last_match_end) {
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
        write!(f, "PCRE error at {:?}: {}", self.offset, self.msg)
    }
}
