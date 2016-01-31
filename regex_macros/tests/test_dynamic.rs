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

extern crate regex;

// Due to macro scoping rules, this definition only applies for the modules
// defined below. Effectively, it allows us to use the same tests for both
// native and dynamic regexes.
//
// This is also used to test the various matching engines. This one exercises
// the normal code path which automatically chooses the engine based on the
// regex and the input. Other dynamic tests explicitly set the engine to use.
macro_rules! regex {
    ($re:expr) => {{
        let e = ::regex::internal::MatchEngine::Automatic;
        ::regex::Regex::with_engine($re, e, 10 * (1 << 20), false).unwrap()
    }}
}

#[cfg(feature = "pattern")]
macro_rules! searcher_expr { ($e:expr) => ($e) }
#[cfg(not(feature = "pattern"))]
macro_rules! searcher_expr { ($e:expr) => ({}) }

mod tests;

// Regression test for https://github.com/rust-lang/regex/issues/98
//
// N.B. This test is here because it wreaks havoc with code generation via
// the `regex!` plugin.
#[test]
fn regression_many_repeat_stack_overflow() {
    let re = regex!("^.{1,2500}");
    assert_eq!(re.find("a"), Some((0, 1)));
}
