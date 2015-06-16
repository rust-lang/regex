// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg_attr(feature = "pattern", feature(core))]

extern crate regex;

macro_rules! regex {
    ($re:expr) => {{
        let e = Some(::regex::internal::MatchEngine::Nfa);
        ::regex::Regex::with_engine(e, 10 * (1 << 20), $re).unwrap()
    }}
}

#[cfg(feature = "pattern")]
macro_rules! searcher_expr { ($e:expr) => ($e) }
#[cfg(not(feature = "pattern"))]
macro_rules! searcher_expr { ($e:expr) => ({}) }

mod tests;
