// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(phase, macro_rules)]

#[phase(plugin)]
extern crate regex_macros;
extern crate regex;
extern crate "test" as stdtest;

#[path = "bench.rs"]
mod native_bench;
#[path = "tests.rs"]
mod native_tests;

mod native_static;

// Due to macro scoping rules, this definition only applies for the modules
// defined below. Effectively, it allows us to use the same tests for both
// native and dynamic regexes.
macro_rules! regex(
    ($re:expr) => (
        match ::regex::Regex::new($re) {
            Ok(re) => re,
            Err(err) => panic!("{}", err),
        }
    );
)

#[path = "bench.rs"]
mod dynamic_bench;
#[path = "tests.rs"]
mod dynamic_tests;

