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
#![allow(non_snake_case)]

extern crate enum_set;
#[macro_use] extern crate lazy_static;
extern crate pcre;
extern crate rand;
extern crate test;

/// A nominal wrapper around pcre::Pcre to expose an interface similar to
/// regex::Regex.
struct Regex(pcre::Pcre);

/// lazy_static wants this. No reason not to provide it.
/// It's unsafe, but we don't really care when benchmarking.
unsafe impl Send for Regex {}
unsafe impl Sync for Regex {}

impl Regex {
    fn new(pattern: &str) -> Result<Regex, pcre::CompilationError> {
        use enum_set::EnumSet;
        use pcre::{Pcre, CompileOption, StudyOption};

        let mut comp_opts = EnumSet::new();
        // Rust's regex library exclusively uses Unicode-aware character
        // classes.
        comp_opts.insert(CompileOption::Ucp);
        let mut re = try!(Pcre::compile_with_options(pattern, &comp_opts));

        // Make it go as fast as possible?
        let mut study_opts = EnumSet::new();
        study_opts.insert(StudyOption::StudyJitCompile);
        re.study_with_options(&study_opts);

        Ok(Regex(re))
    }

    fn is_match(&mut self, text: &str) -> bool {
        self.0.exec(text).is_some()
    }

    fn find_iter<'a, 'p>(
        &'p mut self,
        text: &'a str,
    ) -> pcre::MatchIterator<'a, 'p> {
        self.0.matches(text)
    }
}

macro_rules! regex(
    ($re:expr) => { ::Regex::new($re).unwrap() }
);

mod misc;
mod sherlock;
