// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Enable the benchmarking harness.
#![feature(test)]

// If we're benchmarking the Rust regex plugin, then pull that in.
// This will bring a `regex!` macro into scope.
#![cfg_attr(feature = "re-rust-plugin", feature(plugin))]
#![cfg_attr(feature = "re-rust-plugin", plugin(regex_macros))]

#[macro_use]
extern crate lazy_static;
#[cfg(not(any(feature = "re-rust", feature = "re-rust-bytes")))]
extern crate libc;
#[cfg(feature = "re-pcre1")]
extern crate libpcre_sys;
#[cfg(feature = "re-onig")]
extern crate onig;
#[cfg(any(
    feature = "re-rust",
    feature = "re-rust-bytes",
    feature = "re-rust-plugin",
  ))]
extern crate regex;
#[cfg(feature = "re-rust")]
extern crate regex_syntax;
extern crate test;


#[cfg(feature = "re-onig")]
pub use ffi::onig::Regex;
#[cfg(feature = "re-pcre1")]
pub use ffi::pcre1::Regex;
#[cfg(feature = "re-pcre2")]
pub use ffi::pcre2::Regex;
#[cfg(feature = "re-re2")]
pub use ffi::re2::Regex;
#[cfg(any(feature = "re-rust", feature = "re-rust-plugin"))]
pub use regex::Regex;
#[cfg(feature = "re-rust-bytes")]
pub use regex::bytes::Regex;
#[cfg(feature = "re-tcl")]
pub use ffi::tcl::Regex;

// Usage: regex!(pattern)
//
// Builds a ::Regex from a borrowed string. This is used in every regex
// engine except for the Rust plugin, because the plugin itself defines the
// same macro.
//
// Due to macro scoping rules, this definition only applies for the modules
// defined below. Effectively, it allows us to use the same tests for both
// native and dynamic regexes.
#[cfg(not(feature = "re-rust-bytes"))]
#[cfg(not(feature = "re-rust-plugin"))]
macro_rules! regex {
    ($re:expr) => { ::Regex::new(&$re.to_owned()).unwrap() }
}

#[cfg(feature = "re-rust-bytes")]
#[cfg(not(feature = "re-rust-plugin"))]
macro_rules! regex {
    ($re:expr) => {{
        // Always enable the Unicode flag for byte based regexes.
        // Really, this should have been enabled by default. *sigh*
        use regex::bytes::RegexBuilder;
        RegexBuilder::new(&$re.to_owned()).unicode(true).compile().unwrap()
    }}
}

// Usage: text!(haystack)
//
// Builds a ::Text from an owned string.
//
// This macro is called on every input searched in every benchmark. It is
// called exactly once per benchmark and its time is not included in the
// benchmark timing.
//
// The text given to the macro is always a String, which is guaranteed to be
// valid UTF-8.
//
// The return type should be an owned value that can deref to whatever the
// regex accepts in its `is_match` and `find_iter` methods.
#[cfg(feature = "re-tcl")]
macro_rules! text {
    ($text:expr) => {{
        use ffi::tcl::Text;
        Text::new($text)
    }}
}

#[cfg(feature = "re-rust-bytes")]
macro_rules! text {
    ($text:expr) => {{
        let text: String = $text;
        text.into_bytes()
    }}
}

#[cfg(any(
    feature = "re-onig",
    feature = "re-pcre1",
    feature = "re-pcre2",
    feature = "re-re2",
    feature = "re-rust",
    feature = "re-rust-plugin",
  ))]
macro_rules! text {
    ($text:expr) => { $text }
}

// The type of the value yielded by the `text!` macro defined above.
#[cfg(feature = "re-tcl")]
type Text = ffi::tcl::Text;
#[cfg(feature = "re-rust-bytes")]
type Text = Vec<u8>;
#[cfg(any(
    feature = "re-onig",
    feature = "re-pcre1",
    feature = "re-pcre2",
    feature = "re-re2",
    feature = "re-rust",
    feature = "re-rust-plugin",
  ))]
type Text = String;

// Macros for writing benchmarks easily. We provide macros for benchmarking
// matches, non-matches and for finding all successive non-overlapping matches
// in a string (including a check that the count is correct).

// USAGE: bench_match!(name, pattern, haystack)
//
// This benchmarks how fast a regular expression can report whether it matches
// a particular haystack. If the regex doesn't match, then the benchmark fails.
// Regexes are compiled exactly once.
//
// name is an identifier for the benchmark.
//
// pattern should be a &'static str representing the regular expression.
//
// haystack should be a String.
macro_rules! bench_match {
    ($name:ident, $pattern:expr, $haystack:expr) => {
        bench_is_match!($name, true, regex!($pattern), $haystack);
    }
}

// USAGE: bench_not_match!(name, pattern, haystack)
//
// This benchmarks how fast a regular expression can report whether it matches
// a particular haystack. If the regex matches, then the benchmark fails.
// Regexes are compiled exactly once.
//
// name is an identifier for the benchmark.
//
// pattern should be a &'static str representing the regular expression.
//
// haystack should be a String.
macro_rules! bench_not_match {
    ($name:ident, $pattern:expr, $haystack:expr) => {
        bench_is_match!($name, false, regex!($pattern), $haystack);
    }
}

// USAGE: bench_is_match!(name, is_match, regex, haystack)
//
// This benchmarks how fast a regular expression can report whether it matches
// a particular haystack. If the regex match status doesn't match is_match,
// then the benchmark fails. Regexes are compiled exactly once.
//
// name is an identifier for the benchmark.
//
// is_match reports whether the regex is expected to match the haystack or not.
//
// regex should be a ::Regex.
//
// haystack should be a String.
macro_rules! bench_is_match {
    ($name:ident, $is_match:expr, $re:expr, $haystack:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            use std::sync::Mutex;

            // Why do we use lazy_static here? It seems sensible to just
            // compile a regex outside of the b.iter() call and be done with
            // it. However, it seems like Rust's benchmark harness actually
            // calls the entire benchmark function multiple times. This doesn't
            // factor into the timings reported in the benchmarks, but it does
            // make the benchmarks take substantially longer to run because
            // they're spending a lot of time recompiling regexes.
            lazy_static! {
                static ref RE: Mutex<Regex> = Mutex::new($re);
                static ref TEXT: Mutex<Text> = Mutex::new(text!($haystack));
            };
            let re = RE.lock().unwrap();
            let text = TEXT.lock().unwrap();
            b.bytes = text.len() as u64;
            b.iter(|| {
                if re.is_match(&text) != $is_match {
                    if $is_match {
                        panic!("expected match, got not match");
                    } else {
                        panic!("expected no match, got match");
                    }
                }
            });
        }
    }
}

// USAGE: bench_find!(name, pattern, count, haystack)
//
// This benchmarks how fast a regular expression can count all successive
// non-overlapping matches in haystack. If the count reported does not match
// the count given, then the benchmark fails.
//
// name is an identifier for the benchmark.
//
// pattern should be a &'static str representing the regular expression.
//
// haystack should be a String.
macro_rules! bench_find {
    ($name:ident, $pattern:expr, $count:expr, $haystack:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            use std::sync::Mutex;

            lazy_static! {
                static ref RE: Mutex<Regex> = Mutex::new(regex!($pattern));
                static ref TEXT: Mutex<Text> = Mutex::new(text!($haystack));
            };
            let re = RE.lock().unwrap();
            let text = TEXT.lock().unwrap();
            b.bytes = text.len() as u64;
            b.iter(|| {
                let count = re.find_iter(&text).count();
                assert_eq!($count, count)
            });
        }
    }
}

mod ffi;
mod misc;
mod sherlock;
