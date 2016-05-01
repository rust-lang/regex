extern crate libc;
extern crate regex;

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::fmt;
use std::io::{self, Write};
use std::ops::Deref;
use std::panic::{self, AssertUnwindSafe};
use std::ptr;
use std::slice;
use std::str;

use libc::{abort, c_char, size_t};
use regex::bytes;

macro_rules! ffi_fn {
    (fn $name:ident($($arg:ident: $arg_ty:ty),*,) -> $ret:ty $body:block) => {
        ffi_fn!(fn $name($($arg: $arg_ty),*) -> $ret $body);
    };
    (fn $name:ident($($arg:ident: $arg_ty:ty),*) -> $ret:ty $body:block) => {
        #[no_mangle]
        pub extern fn $name($($arg: $arg_ty),*) -> $ret {
            match panic::catch_unwind(AssertUnwindSafe(move || $body)) {
                Ok(v) => v,
                Err(err) => {
                    let msg = if let Some(&s) = err.downcast_ref::<&str>() {
                        s.to_owned()
                    } else if let Some(s) = err.downcast_ref::<String>() {
                        s.to_owned()
                    } else {
                        "UNABLE TO SHOW RESULT OF PANIC.".to_owned()
                    };
                    let _ = writeln!(
                        &mut io::stderr(),
                        "panic unwind caught, aborting: {:?}",
                        msg);
                    unsafe { abort() }
                }
            }
        }
    };
    (fn $name:ident($($arg:ident: $arg_ty:ty),*,) $body:block) => {
        ffi_fn!(fn $name($($arg: $arg_ty),*) -> () $body);
    };
    (fn $name:ident($($arg:ident: $arg_ty:ty),*) $body:block) => {
        ffi_fn!(fn $name($($arg: $arg_ty),*) -> () $body);
    };
}

pub struct Regex {
    re: bytes::Regex,
    capture_names: HashMap<String, i32>,
}

pub struct Options {
    size_limit: usize,
    dfa_size_limit: usize,
}

const RURE_FLAG_CASEI: u32 = 1 << 0;
const RURE_FLAG_MULTI: u32 = 1 << 1;
const RURE_FLAG_DOTNL: u32 = 1 << 2;
const RURE_FLAG_SWAP_GREED: u32 = 1 << 3;
const RURE_FLAG_SPACE: u32 = 1 << 4;
const RURE_FLAG_UNICODE: u32 = 1 << 5;
const RURE_DEFAULT_FLAGS: u32 = RURE_FLAG_UNICODE;

#[repr(C)]
pub struct rure_match {
    pub start: size_t,
    pub end: size_t,
}

pub struct Captures(Vec<Option<usize>>);

pub struct Iter {
    re: *const Regex,
    last_end: usize,
    last_match: Option<usize>,
}

#[derive(Debug)]
pub struct Error {
    message: Option<CString>,
    kind: ErrorKind,
}

#[derive(Debug)]
enum ErrorKind {
    None,
    Str(str::Utf8Error),
    Regex(regex::Error),
}

impl Deref for Regex {
    type Target = bytes::Regex;
    fn deref(&self) -> &bytes::Regex { &self.re }
}

impl Default for Options {
    fn default() -> Options {
        Options {
            size_limit: 10 * (1<<20),
            dfa_size_limit: 2 * (1<<20),
        }
    }
}

impl Error {
    fn new(kind: ErrorKind) -> Error {
        Error {
            message: None,
            kind: kind,
        }
    }

    fn is_err(&self) -> bool {
        match self.kind {
            ErrorKind::None => false,
            ErrorKind::Str(_) | ErrorKind::Regex(_) => true,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::None => write!(f, "no error"),
            ErrorKind::Str(ref e) => e.fmt(f),
            ErrorKind::Regex(ref e) => e.fmt(f),
        }
    }
}

ffi_fn! {
    fn rure_compile_must(pattern: *const c_char) -> *const Regex {
        let len = unsafe { CStr::from_ptr(pattern).to_bytes().len() };
        let pat = pattern as *const u8;
        let mut err = Error::new(ErrorKind::None);
        let re = rure_compile(
            pat, len, RURE_DEFAULT_FLAGS, ptr::null(), &mut err);
        if err.is_err() {
            let _ = writeln!(&mut io::stderr(), "{}", err);
            let _ = writeln!(
                &mut io::stderr(), "aborting from rure_compile_must");
            unsafe { abort() }
        }
        re
    }
}

ffi_fn! {
    fn rure_compile(
        pattern: *const u8,
        length: size_t,
        flags: u32,
        options: *const Options,
        error: *mut Error,
    ) -> *const Regex {
        let pat = unsafe { slice::from_raw_parts(pattern, length) };
        let pat = match str::from_utf8(pat) {
            Ok(pat) => pat,
            Err(err) => {
                unsafe {
                    if !error.is_null() {
                        *error = Error::new(ErrorKind::Str(err));
                    }
                    return ptr::null();
                }
            }
        };
        let mut builder = bytes::RegexBuilder::new(pat);
        if !options.is_null() {
            let options = unsafe { &*options };
            builder = builder.size_limit(options.size_limit);
            builder = builder.dfa_size_limit(options.dfa_size_limit);
        }
        builder = builder.case_insensitive(flags & RURE_FLAG_CASEI > 0);
        builder = builder.multi_line(flags & RURE_FLAG_MULTI > 0);
        builder = builder.dot_matches_new_line(flags & RURE_FLAG_DOTNL > 0);
        builder = builder.swap_greed(flags & RURE_FLAG_SWAP_GREED > 0);
        builder = builder.ignore_whitespace(flags & RURE_FLAG_SPACE > 0);
        builder = builder.unicode(flags & RURE_FLAG_UNICODE > 0);
        match builder.compile() {
            Ok(re) => {
                let mut capture_names = HashMap::new();
                for (i, name) in re.capture_names().enumerate() {
                    if let Some(name) = name {
                        capture_names.insert(name.to_owned(), i as i32);
                    }
                }
                let re = Regex {
                    re: re,
                    capture_names: capture_names,
                };
                Box::into_raw(Box::new(re))
            }
            Err(err) => {
                unsafe {
                    if !error.is_null() {
                        *error = Error::new(ErrorKind::Regex(err));
                    }
                    ptr::null()
                }
            }
        }
    }
}

ffi_fn! {
    fn rure_free(re: *const Regex) {
        unsafe { Box::from_raw(re as *mut Regex); }
    }
}

ffi_fn! {
    fn rure_is_match(
        re: *const Regex,
        haystack: *const u8,
        len: size_t,
        start: size_t,
    ) -> bool {
        let re = unsafe { &*re };
        let haystack = unsafe { slice::from_raw_parts(haystack, len) };
        re.is_match_at(haystack, start)
    }
}

ffi_fn! {
    fn rure_find(
        re: *const Regex,
        haystack: *const u8,
        len: size_t,
        start: size_t,
        match_info: *mut rure_match,
    ) -> bool {
        let re = unsafe { &*re };
        let haystack = unsafe { slice::from_raw_parts(haystack, len) };
        re.find_at(haystack, start).map(|(s, e)| unsafe {
            if !match_info.is_null() {
                (*match_info).start = s;
                (*match_info).end = e;
            }
        }).is_some()
    }
}

ffi_fn! {
    fn rure_find_captures(
        re: *const Regex,
        haystack: *const u8,
        len: size_t,
        start: size_t,
        captures: *mut Captures,
    ) -> bool {
        let re = unsafe { &*re };
        let haystack = unsafe { slice::from_raw_parts(haystack, len) };
        let slots = unsafe { &mut (*captures).0 };
        re.read_captures_at(slots, haystack, start).is_some()
    }
}

ffi_fn! {
    fn rure_shortest_match(
        re: *const Regex,
        haystack: *const u8,
        len: size_t,
        start: size_t,
        end: *mut usize,
    ) -> bool {
        let re = unsafe { &*re };
        let haystack = unsafe { slice::from_raw_parts(haystack, len) };
        match re.shortest_match_at(haystack, start) {
            None => false,
            Some(i) => {
                if !end.is_null() {
                    unsafe {
                        *end = i;
                    }
                }
                true
            }
        }
    }
}

ffi_fn! {
    fn rure_capture_name_index(
        re: *const Regex,
        name: *const c_char,
    ) -> i32 {
        let re = unsafe { &*re };
        let name = unsafe { CStr::from_ptr(name) };
        let name = match name.to_str() {
            Err(_) => return -1,
            Ok(name) => name,
        };
        re.capture_names.get(name).map(|&i|i).unwrap_or(-1)
    }
}

ffi_fn! {
    fn rure_iter_new(
        re: *const Regex,
    ) -> *mut Iter {
        Box::into_raw(Box::new(Iter {
            re: re,
            last_end: 0,
            last_match: None,
        }))
    }
}

ffi_fn! {
    fn rure_iter_free(it: *mut Iter) {
        unsafe { Box::from_raw(it); }
    }
}

ffi_fn! {
    fn rure_iter_next(
        it: *mut Iter,
        haystack: *const u8,
        len: size_t,
        match_info: *mut rure_match,
    ) -> bool {
        let it = unsafe { &mut *it };
        let re = unsafe { &*it.re };
        let text = unsafe { slice::from_raw_parts(haystack, len) };
        if it.last_end > text.len() {
            return false;
        }
        let (s, e) = match re.find_at(text, it.last_end) {
            None => return false,
            Some((s, e)) => (s, e),
        };
        if s == e {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            it.last_end += 1;
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(e) == it.last_match {
                return rure_iter_next(it, haystack, len, match_info);
            }
        } else {
            it.last_end = e;
        }
        it.last_match = Some(e);
        if !match_info.is_null() {
            unsafe {
                (*match_info).start = s;
                (*match_info).end = e;
            }
        }
        true
    }
}

ffi_fn! {
    fn rure_iter_next_captures(
        it: *mut Iter,
        haystack: *const u8,
        len: size_t,
        captures: *mut Captures,
    ) -> bool {
        let it = unsafe { &mut *it };
        let re = unsafe { &*it.re };
        let slots = unsafe { &mut (*captures).0 };
        let text = unsafe { slice::from_raw_parts(haystack, len) };
        if it.last_end > text.len() {
            return false;
        }
        let (s, e) = match re.read_captures_at(slots, text, it.last_end) {
            None => return false,
            Some((s, e)) => (s, e),
        };
        if s == e {
            // This is an empty match. To ensure we make progress, start
            // the next search at the smallest possible starting position
            // of the next match following this one.
            it.last_end += 1;
            // Don't accept empty matches immediately following a match.
            // Just move on to the next match.
            if Some(e) == it.last_match {
                return rure_iter_next_captures(it, haystack, len, captures);
            }
        } else {
            it.last_end = e;
        }
        it.last_match = Some(e);
        true
    }
}

ffi_fn! {
    fn rure_captures_new(re: *const Regex) -> *mut Captures {
        let re = unsafe { &*re };
        let captures = Captures(vec![None; 2 * re.captures_len()]);
        Box::into_raw(Box::new(captures))
    }
}

ffi_fn! {
    fn rure_captures_free(captures: *const Captures) {
        unsafe { Box::from_raw(captures as *mut Captures); }
    }
}

ffi_fn! {
    fn rure_captures_at(
        captures: *const Captures,
        i: size_t,
        match_info: *mut rure_match,
    ) -> bool {
        let captures = unsafe { &(*captures).0 };
        match (captures[i * 2], captures[i * 2 + 1]) {
            (Some(start), Some(end)) => {
                if !match_info.is_null() {
                    unsafe {
                        (*match_info).start = start;
                        (*match_info).end = end;
                    }
                }
                true
            }
            _ => false
        }
    }
}

ffi_fn! {
    fn rure_captures_len(captures: *const Captures) -> size_t {
        unsafe { (*captures).0.len() / 2 }
    }
}

ffi_fn! {
    fn rure_options_new() -> *mut Options {
        Box::into_raw(Box::new(Options::default()))
    }
}

ffi_fn! {
    fn rure_options_free(options: *mut Options) {
        unsafe { Box::from_raw(options); }
    }
}

ffi_fn! {
    fn rure_options_size_limit(options: *mut Options, limit: size_t) {
        let options = unsafe { &mut *options };
        options.size_limit = limit;
    }
}

ffi_fn! {
    fn rure_options_dfa_size_limit(options: *mut Options, limit: size_t) {
        let options = unsafe { &mut *options };
        options.dfa_size_limit = limit;
    }
}

ffi_fn! {
    fn rure_error_new() -> *mut Error {
        Box::into_raw(Box::new(Error::new(ErrorKind::None)))
    }
}

ffi_fn! {
    fn rure_error_free(err: *mut Error) {
        unsafe { Box::from_raw(err); }
    }
}

ffi_fn! {
    fn rure_error_message(err: *mut Error) -> *const c_char {
        let err = unsafe { &mut *err };
        let cmsg = match CString::new(format!("{}", err)) {
            Ok(msg) => msg,
            Err(err) => {
                // I guess this can probably happen if the regex itself has a
                // NUL, and that NUL re-occurs in the context presented by the
                // error message. In this case, just show as much as we can.
                let nul = err.nul_position();
                let msg = err.into_vec();
                CString::new(msg[0..nul].to_owned()).unwrap()
            }
        };
        let p = cmsg.as_ptr();
        err.message = Some(cmsg);
        p
    }
}
