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

use std::mem;
use std::ptr;
use std::sync::{Once, ONCE_INIT};

use libc::{c_char, c_int, c_long, c_void};

// Used to initialize the TCL interpreter exactly once.
static ONCE: Once = ONCE_INIT;

/// Text is a TCL string object backed by a Rust string.
///
/// This is a special type that is created once per benchmark and is not
/// included in timings. In particular, all regex searches execute on values
/// of this type, so we're careful to avoid the overhead of creating such
/// objects on every search.
pub struct Text {
    s: String,
    obj: *mut tcl_obj,
}

// TCL's objects are ref-counted in a thread-unsafe manner, which would
// normally disqualify a Send bound. However, we don't permit Text to be used
// in a way that can lead to unsafety. In particular, the ref count is always
// 1, until it is dropped, in which the ref count is decreased to zero and
// the underlying memory is freed.
unsafe impl Send for Text {}

impl Drop for Text {
    fn drop(&mut self) {
        unsafe {
            assert_eq!((*self.obj).ref_count, 1);
            // This will drop the ref count to 0 and cause it to be freed.
            (*self.obj).decr_ref_count();
        }
    }
}

impl Text {
    pub fn new(text: String) -> Text {
        let ptr = text.as_ptr() as *const c_char;
        let len = text.len() as c_int;
        let obj = unsafe { Tcl_NewStringObj(ptr, len) };
        unsafe {
            (*obj).incr_ref_count();
        }
        Text { s: text, obj: obj }
    }

    pub fn len(&self) -> usize {
        self.s.len()
    }
}

/// Regex wraps a TCL regex. It owns a TCL string object and a pointer to a
/// regexp object. The two share storage.
///
/// There's no Drop impl for Regex because the memory for the regex will be
/// freed when `pat` is dropped.
pub struct Regex {
    pat: Text,
    re: *mut tcl_regexp,
}

unsafe impl Send for Regex {}

#[derive(Debug)]
pub struct Error(());

impl Regex {
    pub fn new(pattern: &str) -> Result<Regex, Error> {
        ONCE.call_once(|| {
            unsafe { Tcl_CreateInterp(); }
        });

        let pat = Text::new(pattern.to_owned());
        let re = unsafe {
            Tcl_GetRegExpFromObj(ptr::null_mut(), pat.obj, TCL_REG_ADVANCED)
        };
        if re.is_null() {
            return Err(Error(()));
        }
        Ok(Regex {
            pat: pat,
            re: re,
        })
    }

    pub fn is_match(&self, text: &Text) -> bool {
        let result = unsafe { Tcl_RegExpExecObj(
            ptr::null_mut(),
            self.re,
            text.obj,
            0,
            1,
            0,
        ) };
        if result == -1 {
            panic!("Tcl_RegExpExecObj failed");
        }
        result > 0
    }

    pub fn find_iter<'r, 't>(&'r self, text: &'t Text) -> FindMatches<'r, 't> {
        FindMatches {
            re: self,
            text: text,
            last_match: 0,
        }
    }

    fn find_at(&self, text: &Text, start: usize) -> Option<(usize, usize)> {
        let result = unsafe { Tcl_RegExpExecObj(
            ptr::null_mut(),
            self.re,
            text.obj,
            start as c_int,
            1,
            0,
        ) };
        if result == -1 {
            panic!("Tcl_RegExpExecObj failed");
        } else if result == 0 {
            return None;
        }
        let mut info: tcl_regexp_info = unsafe { mem::zeroed() };
        unsafe {
            Tcl_RegExpGetInfo(self.re, &mut info);
            let s = start as c_long + (*info.matches).start;
            let e = start as c_long + (*info.matches).end;
            Some((s as usize, e as usize))
        }
    }
}

pub struct FindMatches<'r, 't> {
    re: &'r Regex,
    text: &'t Text,
    last_match: usize,
}

impl<'r, 't> Iterator for FindMatches<'r, 't> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        match self.re.find_at(self.text, self.last_match) {
            None => None,
            Some((s, e)) => {
                self.last_match = e;
                Some((s, e))
            }
        }
    }
}

// TCL's FFI. We only wrap the bits we need.

const TCL_REG_ADVANCED: c_int = 3;

type tcl_interp = c_void;
type tcl_regexp = c_void;

#[repr(C)]
struct tcl_obj {
    ref_count: c_int,
    // There are more fields, but we don't care about them.
    // We're careful to only access ref_count so we can increment/decrement it.
    // This is necessary because Tcl_IncRefCount and Tcl_DecrRefCount are
    // macros.
}

impl tcl_obj {
    unsafe fn incr_ref_count(&mut self) {
        self.ref_count += 1;
    }

    unsafe fn decr_ref_count(&mut self) {
        self.ref_count -= 1;
        if self.ref_count <= 0 {
            TclFreeObj(self);
        }
    }
}

#[repr(C)]
struct tcl_regexp_info {
    nsubs: c_int,
    matches: *mut tcl_regexp_indices,
    extend_start: c_long,
    reserved: c_long,
}

#[repr(C)]
struct tcl_regexp_indices {
    start: c_long,
    end: c_long,
}

extern {
    fn Tcl_CreateInterp() -> *mut tcl_interp;

    fn Tcl_NewStringObj(
        pat: *const c_char,
        len: c_int,
    ) -> *mut tcl_obj;

    fn TclFreeObj(
        obj: *mut tcl_obj,
    );

    fn Tcl_GetRegExpFromObj(
        int: *mut tcl_interp,
        pat: *mut tcl_obj,
        flags: c_int,
    ) -> *mut tcl_regexp;

    fn Tcl_RegExpExecObj(
        int: *mut tcl_interp,
        re: *mut tcl_regexp,
        text: *mut tcl_obj,
        offset: c_int,
        nmatches: c_int,
        flags: c_int,
    ) -> c_int;

    fn Tcl_RegExpGetInfo(
        re: *mut tcl_regexp,
        info: *mut tcl_regexp_info,
    );
}
