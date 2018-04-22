// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg_attr(feature = "pattern", feature(pattern))]

extern crate rand;
extern crate regex;

macro_rules! regex_new {
    ($re:expr) => {{
        use regex::internal::ExecBuilder;
        ExecBuilder::new($re).onepass().build().map(|e| e.into_regex())
    }}
}

macro_rules! regex {
    ($re:expr) => {
        regex_new!($re).unwrap()
    }
}

// Even though we don't support regex sets, we should still provide a
// constructor for them that sets the onepass flag in order to make
// sure that we properly fall back to a different impl.
macro_rules! regex_set_new {
    ($re:expr) => {{
        use regex::internal::ExecBuilder;
        ExecBuilder::new_many($re).onepass().build().map(|e| e.into_regex_set())
    }}
}

macro_rules! regex_set {
    ($res:expr) => {
        regex_set_new!($res).unwrap()
    }
}

// Must come before other module definitions.
include!("macros_str.rs");
include!("macros.rs");

mod api;
mod api_str;
mod crazy;
mod flags;
mod fowler;
mod multiline;
mod noparse;
mod regression;
mod replace;
mod searcher;
mod set;
mod suffix_reverse;
mod unicode;
mod word_boundary;
mod word_boundary_unicode;
mod onepass_unit;
