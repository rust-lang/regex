#![cfg_attr(feature = "pattern", feature(pattern))]

use regex;

// Due to macro scoping rules, this definition only applies for the modules
// defined below. Effectively, it allows us to use the same tests for both
// native and dynamic regexes.
//
// This is also used to test the various matching engines. This one exercises
// the normal code path which automatically chooses the engine based on the
// regex and the input. Other dynamic tests explicitly set the engine to use.
macro_rules! regex_new {
    ($re:expr) => {{
        use regex::Regex;
        Regex::new($re)
    }};
}

macro_rules! regex {
    ($re:expr) => {
        regex_new!($re).unwrap()
    };
}

macro_rules! regex_set_new {
    ($re:expr) => {{
        use regex::RegexSet;
        RegexSet::new($re)
    }};
}

macro_rules! regex_set {
    ($res:expr) => {
        regex_set_new!($res).unwrap()
    };
}

// Must come before other module definitions.
include!("macros_str.rs");
include!("macros.rs");

mod api;
mod api_str;
mod crazy;
mod flags;
mod fowler;
mod marker_traits;
mod misc;
mod multiline;
mod noparse;
mod regression;
mod regression_fuzz;
mod replace;
mod searcher;
mod set;
mod shortest_match;
mod suffix_reverse;
#[cfg(feature = "unicode")]
mod unicode;
#[cfg(feature = "unicode-perl")]
mod word_boundary;
#[cfg(feature = "unicode-perl")]
mod word_boundary_unicode;

#[test]
fn disallow_non_utf8() {
    assert!(regex::Regex::new(r"(?-u)\xFF").is_err());
    assert!(regex::Regex::new(r"(?-u).").is_err());
    assert!(regex::Regex::new(r"(?-u)[\xFF]").is_err());
    assert!(regex::Regex::new(r"(?-u)☃").is_err());
}

#[test]
fn disallow_octal() {
    assert!(regex::Regex::new(r"\0").is_err());
}

#[test]
fn allow_octal() {
    assert!(regex::RegexBuilder::new(r"\0").octal(true).build().is_ok());
}

// See: https://github.com/rust-lang/regex/issues/568
#[test]
fn oibits_regression() {
    use regex::Regex;
    use std::panic;

    let _ = panic::catch_unwind(|| Regex::new("a").unwrap());
}

// See: https://github.com/rust-lang/regex/issues/750
#[test]
#[cfg(target_pointer_width = "64")]
fn regex_is_reasonably_small() {
    use std::mem::size_of;

    use regex::bytes;
    use regex::{Regex, RegexSet};

    assert_eq!(16, size_of::<Regex>());
    assert_eq!(16, size_of::<RegexSet>());
    assert_eq!(16, size_of::<bytes::Regex>());
    assert_eq!(16, size_of::<bytes::RegexSet>());
}
