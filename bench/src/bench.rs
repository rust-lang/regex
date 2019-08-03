// Enable the benchmarking harness.
#![feature(test)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate cfg_if;

#[cfg(not(any(feature = "re-rust", feature = "re-rust-bytes")))]
extern crate libc;

extern crate regex_syntax;
extern crate test;

cfg_if! {
    if #[cfg(feature = "re-pcre1")] {
        extern crate libpcre_sys;
        pub use ffi::pcre1::Regex;
    } else if #[cfg(feature = "re-onig")] {
        extern crate onig;
        pub use ffi::onig::Regex;
    } else if #[cfg(any(feature = "re-rust"))] {
        extern crate regex;
        pub use regex::{Regex, RegexSet};
    } else if #[cfg(feature = "re-rust-bytes")] {
        extern crate regex;
        pub use regex::bytes::{Regex, RegexSet};
    } else if #[cfg(feature = "re-re2")] {
        pub use ffi::re2::Regex;
    } else if #[cfg(feature = "re-dphobos")] {
        pub use ffi::d_phobos::Regex;
    } else if #[cfg(feature = "re-pcre2")] {
        pub use ffi::pcre2::Regex;
    } else if #[cfg(any(feature = "re-stdcpp", feature = "re-boost"))] {
        pub use ffi::stdcpp::Regex;
    } else if #[cfg(feature = "re-tcl")] {
        pub use ffi::tcl::Regex;
    } else {
        compile_error!(
            "To run the benchmarks, see `./run -h` or the HACKING.md document"
        );
    }
}

// Usage: regex!(pattern)
//
// Builds a ::Regex from a borrowed string.
//
// Due to macro scoping rules, this definition only applies for the modules
// defined below. Effectively, it allows us to use the same tests for both
// native and dynamic regexes.
macro_rules! regex {
    ($re:expr) => {
        ::Regex::new(&$re.to_owned()).unwrap()
    };
}

cfg_if! {
    if #[cfg(feature = "re-tcl")] {
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
        macro_rules! text {
            ($text:expr) => {{
                use ffi::tcl::Text;
                Text::new($text)
            }}
        }
        type Text = ffi::tcl::Text;
    } else if #[cfg(feature = "re-rust-bytes")] {
        macro_rules! text {
            ($text:expr) => {{
                let text: String = $text;
                text.into_bytes()
            }}
        }
        type Text = Vec<u8>;
    } else {
        macro_rules! text {
            ($text:expr) => { $text }
        }
        type Text = String;
    }
}

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
    };
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
    };
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
    };
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
    };
}

// USAGE: bench_captures!(name, pattern, groups, haystack);
//
// CONTRACT:
//   Given:
//     ident, the desired benchmarking function name
//     pattern : ::Regex, the regular expression to be executed
//     groups : usize, the number of capture groups
//     haystack : String, the string to search
//   bench_captures will benchmark how fast re.captures() produces
//   the capture groups in question.
macro_rules! bench_captures {
    ($name:ident, $pattern:expr, $count:expr, $haystack:expr) => {
        #[cfg(feature = "re-rust")]
        #[bench]
        fn $name(b: &mut Bencher) {
            use std::sync::Mutex;

            lazy_static! {
                static ref RE: Mutex<Regex> = Mutex::new($pattern);
                static ref TEXT: Mutex<Text> = Mutex::new(text!($haystack));
            };
            let re = RE.lock().unwrap();
            let text = TEXT.lock().unwrap();
            b.bytes = text.len() as u64;
            b.iter(|| match re.captures(&text) {
                None => assert!(false, "no captures"),
                Some(caps) => assert_eq!($count + 1, caps.len()),
            });
        }
    };
}

// USAGE: bench_is_match_set!(name, is_match, regex, haystack)
macro_rules! bench_is_match_set {
    ($name:ident, $is_match:expr, $re:expr, $haystack:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            use std::sync::Mutex;
            lazy_static! {
                static ref RE: Mutex<RegexSet> = Mutex::new($re);
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
    };
}

// USAGE: bench_matches_set!(name, is_match, regex, haystack)
macro_rules! bench_matches_set {
    ($name:ident, $is_match:expr, $re:expr, $haystack:expr) => {
        #[bench]
        fn $name(b: &mut Bencher) {
            use std::sync::Mutex;
            lazy_static! {
                static ref RE: Mutex<RegexSet> = Mutex::new($re);
                static ref TEXT: Mutex<Text> = Mutex::new(text!($haystack));
            };
            let re = RE.lock().unwrap();
            let text = TEXT.lock().unwrap();
            b.bytes = text.len() as u64;
            b.iter(|| {
                if re.matches(&text).matched_any() != $is_match {
                    if $is_match {
                        panic!("expected match, got not match");
                    } else {
                        panic!("expected no match, got match");
                    }
                }
            });
        }
    };
}

cfg_if! {
    if #[cfg(any(
        feature = "re-pcre1",
        feature = "re-onig",
        feature = "re-rust",
        feature = "re-rust-bytes",
        feature = "re-re2",
        feature = "re-dphobos",
        feature = "re-pcre2",
        feature = "re-stdcpp",
        feature = "re-boost",
        feature = "re-tcl"
    ))] {
        mod ffi;
        mod misc;
        mod regexdna;
        mod sherlock;
    }
}
