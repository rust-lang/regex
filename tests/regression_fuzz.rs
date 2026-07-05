// These tests are only run for the "default" test target because some of them
// can take quite a long time. Some of them take long enough that it's not
// practical to run them in debug mode. :-/

use regex::Regex;

macro_rules! regex {
    ($pattern:expr) => {
        regex::Regex::new($pattern).unwrap()
    };
}

// See: https://oss-fuzz.com/testcase-detail/5673225499181056
//
// Ignored by default since it takes too long in debug mode (almost a minute).
#[test]
#[ignore]
fn fuzz1() {
    regex!(r"1}{55}{0}*{1}{55}{55}{5}*{1}{55}+{56}|;**");
}

// See: https://bugs.chromium.org/p/oss-fuzz/issues/detail?id=26505
// See: https://github.com/rust-lang/regex/issues/722
#[test]
#[cfg(feature = "unicode")]
fn empty_any_errors_no_panic() {
    assert!(Regex::new(r"\P{any}").is_ok());
}

// This tests that a very large regex errors during compilation instead of
// using gratuitous amounts of memory. The specific problem is that the
// compiler wasn't accounting for the memory used by Unicode character classes
// correctly.
//
// See: https://bugs.chromium.org/p/oss-fuzz/issues/detail?id=33579
#[test]
fn big_regex_fails_to_compile() {
    let pat = "[\u{0}\u{e}\u{2}\\w~~>[l\t\u{0}]p?<]{971158}";
    assert!(Regex::new(pat).is_err());
}

// This was caught while on master but before a release went out(!).
//
// See: https://bugs.chromium.org/p/oss-fuzz/issues/detail?id=58173
#[test]
fn todo() {
    let pat = "(?:z|xx)@|xx";
    assert!(Regex::new(pat).is_ok());
}

// This was caused by the fuzzer, and then minimized by hand.
//
// This was caused by a bug in DFA determinization that mishandled NFA fail
// states.
#[test]
fn fail_branch_prevents_match() {
    let pat = r".*[a&&b]A|B";
    let hay = "B";
    let re = Regex::new(pat).unwrap();
    assert!(re.is_match(hay));
}

// This was caught by a differential fuzzer (REAL-regex vs this crate), and
// then minimized by hand.
//
// The reverse suffix optimization processes suffix-prefilter candidates in
// order of match *end*. When one alternation branch's entire match (an exact
// suffix literal) is a proper suffix of another branch's suffix, a match can
// be nested inside the tail of an overlapping match that ends later but
// starts *earlier*. Yielding the first confirmed candidate then skips the
// true leftmost match, and the anti-quadratic clamp makes the miss permanent.
#[test]
fn reverse_suffix_nested_overlap_misses_leftmost() {
    let re = Regex::new("a|.aa").unwrap();
    let spans: Vec<(usize, usize)> =
        re.find_iter("-#aa").map(|m| (m.start(), m.end())).collect();
    assert_eq!(spans, vec![(1, 4)]);
}
