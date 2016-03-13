// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(test)]

#[macro_use] extern crate lazy_static;
extern crate onig;
extern crate rand;
extern crate test;

use std::ops::Deref;

pub struct Regex(onig::Regex);

unsafe impl Send for Regex {}
unsafe impl Sync for Regex {}

impl Deref for Regex {
    type Target = onig::Regex;

    fn deref(&self) -> &onig::Regex {
        &self.0
    }
}

impl Regex {
    fn new(pattern: &str) -> Result<Self, onig::Error> {
        onig::Regex::new(pattern).map(Regex)
    }

    // Gah. onig's match function is anchored, but find is not.
    fn is_match(&self, text: &str) -> bool {
        self.search_with_options(
            text, 0, text.len(), onig::SEARCH_OPTION_NONE, None).is_some()
    }
}

macro_rules! regex(
    ($re:expr) => {{
        ::Regex::new($re).unwrap()
    }}
);

mod misc;
mod sherlock;
