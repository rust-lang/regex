#![allow(non_camel_case_types)]

use libc::{c_uchar, c_void};

/// Regex wraps a D regular expression
pub struct Regex {
    re: *mut d_regex,
}

unsafe impl Send for Regex {}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe {
            d_phobos_regex_free(self.re);
            rt_term();
        }
    }
}

#[derive(Debug)]
pub struct Error(());

impl Regex {
    pub fn new(pattern: &str) -> Result<Regex, Error> {
        unsafe {
            rt_init();

            Ok(Regex { re: d_phobos_regex_new(pattern.into()) })
        }
    }

    pub fn is_match(&self, text: &str) -> bool {
        unsafe { d_phobos_regex_is_match(self.re, text.into()) }
    }

    pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> FindMatches<'r, 't> {
        FindMatches { re: self, text: text, last_end: 0, last_match: None }
    }

    fn find_at(&self, text: &str, start: usize) -> Option<(usize, usize)> {
        let (mut s, mut e): (usize, usize) = (0, 0);
        let matched = unsafe {
            d_phobos_regex_find_at(self.re, text.into(), start, &mut s, &mut e)
        };
        if matched {
            Some((s, e))
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

impl<'r, 't> Iterator for FindMatches<'r, 't> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        match self.re.find_at(self.text, self.last_end) {
            None => None,
            Some((s, e)) => {
                self.last_end = e;
                Some((s, e))
            }
        }
    }
}

type d_regex = c_void;

#[repr(C)]
struct d_string {
    len: usize,
    text: *const c_uchar,
}

impl<'a> From<&'a str> for d_string {
    fn from(s: &'a str) -> d_string {
        d_string { len: s.len(), text: s.as_ptr() }
    }
}

extern "C" {
    fn rt_init() -> i32;
    fn rt_term() -> i32;
    fn d_phobos_regex_new(s: d_string) -> *mut d_regex;
    fn d_phobos_regex_free(r: *mut d_regex);
    fn d_phobos_regex_is_match(r: *mut d_regex, s: d_string) -> bool;
    fn d_phobos_regex_find_at(
        r: *mut d_regex,
        s: d_string,
        start: usize,
        match_start: *mut usize,
        match_end: *mut usize,
    ) -> bool;
}
