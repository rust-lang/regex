// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use regex::{Regex, NoExpand};

#[test]
fn eq() {
    assert_eq!(regex!(r"[a-z]+"), Regex::new("[a-z]+").unwrap());
}

#[test]
fn splitn() {
    let re = regex!(r"\d+");
    let text = "cauchy123plato456tyler789binx";
    let subs: Vec<&str> = re.splitn(text, 2).collect();
    assert_eq!(subs, vec!("cauchy", "plato456tyler789binx"));
}

#[test]
fn split() {
    let re = regex!(r"\d+");
    let text = "cauchy123plato456tyler789binx";
    let subs: Vec<&str> = re.split(text).collect();
    assert_eq!(subs, vec!("cauchy", "plato", "tyler", "binx"));
}

#[test]
fn empty_regex_empty_match() {
    let re = regex!("");
    let ms = re.find_iter("").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 0)]);
}

#[test]
fn empty_regex_nonempty_match() {
    let re = regex!("");
    let ms = re.find_iter("abc").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 0), (1, 1), (2, 2), (3, 3)]);
}

#[test]
fn one_zero_length_match() {
    let re = regex!(r"\d*");
    let ms = re.find_iter("a1b2").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 0), (1, 2), (3, 4)]);
}

#[test]
fn many_zero_length_match() {
    let re = regex!(r"\d*");
    let ms = re.find_iter("a1bbb2").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 0), (1, 2), (3, 3), (4, 4), (5, 6)]);
}

#[test]
fn many_sequential_zero_length_match() {
    let re = regex!(r"\d?");
    let ms = re.find_iter("a12b3c").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 0), (1, 2), (2, 3), (4, 5), (6, 6)]);
}

#[test]
fn quoted_bracket_set() {
    let re = regex!(r"([\x{5b}\x{5d}])");
    let ms = re.find_iter("[]").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 1), (1, 2)]);
    let re = regex!(r"([\[\]])");
    let ms = re.find_iter("[]").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 1), (1, 2)]);
}

#[test]
fn first_range_starts_with_left_bracket() {
    let re = regex!(r"([[-z])");
    let ms = re.find_iter("[]").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 1), (1, 2)]);
}

#[test]
fn range_ends_with_escape() {
    let re = regex!(r"([\[-\x{5d}])");
    let ms = re.find_iter("[]").collect::<Vec<_>>();
    assert_eq!(ms, vec![(0, 1), (1, 2)]);
}

#[test]
fn empty_match_find_iter() {
    let re = regex!(r".*?");
    let ms: Vec<_> = re.find_iter("abc").collect();
    assert_eq!(ms, vec![(0, 0), (1, 1), (2, 2), (3, 3)]);
}

#[test]
fn empty_match_captures_iter() {
    let re = regex!(r".*?");
    let ms: Vec<_> = re.captures_iter("abc")
                       .map(|c| c.pos(0).unwrap())
                       .collect();
    assert_eq!(ms, vec![(0, 0), (1, 1), (2, 2), (3, 3)]);
}

#[test]
fn empty_match_unicode_find_iter() {
    let re = regex!(r".*?");
    let ms: Vec<_> = re.find_iter("Ⅰ1Ⅱ2").collect();
    assert_eq!(ms, vec![(0, 0), (3, 3), (4, 4), (7, 7), (8, 8)]);
}

#[test]
fn empty_match_unicode_captures_iter() {
    let re = regex!(r".*?");
    let ms: Vec<_> = re.captures_iter("Ⅰ1Ⅱ2")
                       .map(|c| c.pos(0).unwrap())
                       .collect();
    assert_eq!(ms, vec![(0, 0), (3, 3), (4, 4), (7, 7), (8, 8)]);
}

#[test]
fn invalid_regexes_no_crash() {
    // See: https://github.com/rust-lang/regex/issues/48
    assert!(Regex::new("(*)").is_err());
    assert!(Regex::new("(?:?)").is_err());
    assert!(Regex::new("(?)").is_err());
    assert!(Regex::new("*").is_err());
}

macro_rules! searcher {
    ($name:ident, $re:expr, $haystack:expr) => (
        searcher!($name, $re, $haystack, vec vec![]);
    );
    ($name:ident, $re:expr, $haystack:expr, $($steps:expr,)*) => (
        searcher!($name, $re, $haystack, vec vec![$($steps),*]);
    );
    ($name:ident, $re:expr, $haystack:expr, $($steps:expr),*) => (
        searcher!($name, $re, $haystack, vec vec![$($steps),*]);
    );
    ($name:ident, $re:expr, $haystack:expr, vec $expect_steps:expr) => (
        #[test]
        #[allow(unused_imports)]
        fn $name() {
            searcher_expr! {{
                use std::str::pattern::{Pattern, Searcher};
                use std::str::pattern::SearchStep::{Match, Reject, Done};
                let re = regex!($re);
                let mut se = re.into_searcher($haystack);
                let mut got_steps = vec![];
                loop {
                    match se.next() {
                        Done => break,
                        step => { got_steps.push(step); }
                    }
                }
                assert_eq!(got_steps, $expect_steps);
            }}
        }
    );
}

searcher!(searcher_empty_regex_empty_haystack, r"", "", Match(0, 0));
searcher!(searcher_empty_regex, r"", "ab",
          Match(0, 0), Reject(0, 1), Match(1, 1), Reject(1, 2), Match(2, 2));
searcher!(searcher_empty_haystack, r"\d", "");
searcher!(searcher_one_match, r"\d", "5",
          Match(0, 1));
searcher!(searcher_no_match, r"\d", "a",
          Reject(0, 1));
searcher!(searcher_two_adjacent_matches, r"\d", "56",
          Match(0, 1), Match(1, 2));
searcher!(searcher_two_non_adjacent_matches, r"\d", "5a6",
          Match(0, 1), Reject(1, 2), Match(2, 3));
searcher!(searcher_reject_first, r"\d", "a6",
          Reject(0, 1), Match(1, 2));
searcher!(searcher_one_zero_length_matches, r"\d*", "a1b2",
          Match(0, 0),  // ^
          Reject(0, 1), // a
          Match(1, 2),  // a1
          Reject(2, 3), // a1b
          Match(3, 4),  // a1b2
);
searcher!(searcher_many_zero_length_matches, r"\d*", "a1bbb2",
          Match(0, 0),  // ^
          Reject(0, 1), // a
          Match(1, 2),  // a1
          Reject(2, 3), // a1b
          Match(3, 3),  // a1bb
          Reject(3, 4), // a1bb
          Match(4, 4),  // a1bbb
          Reject(4, 5), // a1bbb
          Match(5, 6),  // a1bbba
);
searcher!(searcher_unicode, r".+?", "Ⅰ1Ⅱ2",
          Match(0, 3), Match(3, 4), Match(4, 7), Match(7, 8));

macro_rules! replace(
    ($name:ident, $which:ident, $re:expr,
     $search:expr, $replace:expr, $result:expr) => (
        #[test]
        fn $name() {
            let re = regex!($re);
            assert_eq!(re.$which($search, $replace), $result);
        }
    );
);

replace!(rep_first, replace, r"\d", "age: 26", "Z", "age: Z6");
replace!(rep_plus, replace, r"\d+", "age: 26", "Z", "age: Z");
replace!(rep_all, replace_all, r"\d", "age: 26", "Z", "age: ZZ");
replace!(rep_groups, replace, r"(\S+)\s+(\S+)", "w1 w2", "$2 $1", "w2 w1");
replace!(rep_double_dollar, replace,
         r"(\S+)\s+(\S+)", "w1 w2", "$2 $$1", "w2 $1");
replace!(rep_adjacent_index, replace,
         r"([^aeiouy])ies$", "skies", "$1y", "sky");
replace!(rep_no_expand, replace,
         r"(\S+)\s+(\S+)", "w1 w2", NoExpand("$2 $1"), "$2 $1");
replace!(rep_named, replace_all,
         r"(?P<first>\S+)\s+(?P<last>\S+)(?P<space>\s*)",
         "w1 w2 w3 w4", "$last $first$space", "w2 w1 w4 w3");
replace!(rep_trim, replace_all, "^[ \t]+|[ \t]+$", " \t  trim me\t   \t",
         "", "trim me");
replace!(rep_number_hypen, replace, r"(.)(.)", "ab", "$1-$2", "a-b");
replace!(rep_number_underscore, replace, r"(.)(.)", "ab", "$1_$2", "a_b");

macro_rules! noparse(
    ($name:ident, $re:expr) => (
        #[test]
        fn $name() {
            let re = $re;
            match Regex::new(re) {
                Err(_) => {},
                Ok(_) => panic!("Regex '{}' should cause a parse error.", re),
            }
        }
    );
);

noparse!(fail_double_repeat, "a**");
noparse!(fail_no_repeat_arg, "*");
noparse!(fail_incomplete_escape, "\\");
noparse!(fail_class_incomplete, "[A-");
noparse!(fail_class_not_closed, "[A");
noparse!(fail_class_no_begin, r"[\A]");
noparse!(fail_class_no_end, r"[\z]");
noparse!(fail_class_no_boundary, r"[\b]");
noparse!(fail_open_paren, "(");
noparse!(fail_close_paren, ")");
noparse!(fail_invalid_range, "[a-Z]");
noparse!(fail_empty_capture_name, "(?P<>a)");
noparse!(fail_empty_capture_exp, "(?P<name>)");
noparse!(fail_bad_capture_name, "(?P<na-me>)");
noparse!(fail_bad_flag, "(?a)a");
noparse!(fail_empty_alt_before, "|a");
noparse!(fail_empty_alt_after, "a|");
noparse!(fail_too_big, "a{10000000}");
noparse!(fail_counted_no_close, "a{1001");
noparse!(fail_unfinished_cap, "(?");
noparse!(fail_unfinished_escape, "\\");
noparse!(fail_octal_digit, r"\8");
noparse!(fail_hex_digit, r"\xG0");
noparse!(fail_hex_short, r"\xF");
noparse!(fail_hex_long_digits, r"\x{fffg}");
noparse!(fail_flag_bad, "(?a)");
noparse!(fail_flag_empty, "(?)");
noparse!(fail_double_neg, "(?-i-i)");
noparse!(fail_neg_empty, "(?i-)");
noparse!(fail_empty_group, "()");
noparse!(fail_dupe_named, "(?P<a>.)(?P<a>.)");
noparse!(fail_range_end_no_class, "[a-[:lower:]]");
noparse!(fail_range_end_no_begin, r"[a-\A]");
noparse!(fail_range_end_no_end, r"[a-\z]");
noparse!(fail_range_end_no_boundary, r"[a-\b]");

macro_rules! matiter(
    ($name:ident, $re:expr, $text:expr) => (
        #[test]
        fn $name() {
            let text = $text;
            let expected: Vec<(usize, usize)> = vec![];
            let r = regex!($re);
            let got: Vec<_> = r.find_iter(text).collect();
            if expected != got {
                panic!("For RE '{}' against '{:?}', \
                        expected '{:?}' but got '{:?}'",
                       $re, text, expected, got);
            }
        }
    );
    ($name:ident, $re:expr, $text:expr, $($loc:tt)+) => (
        #[test]
        fn $name() {
            let text = $text;
            let expected: Vec<_> = vec!($($loc)+);
            let r = regex!($re);
            let got: Vec<_> = r.find_iter(text).collect();
            if expected != got {
                panic!("For RE '{}' against '{:?}', \
                        expected '{:?}' but got '{:?}'",
                       $re, text, expected, got);
            }
        }
    );
);

macro_rules! mat(
    ($name:ident, $re:expr, $text:expr, $($loc:tt)+) => (
        #[test]
        fn $name() {
            let text = $text;
            let expected: Vec<Option<_>> = vec!($($loc)+);
            let r = regex!($re);
            let got = match r.captures(text) {
                Some(c) => c.iter_pos().collect::<Vec<Option<_>>>(),
                None => vec!(None),
            };
            // The test set sometimes leave out capture groups, so truncate
            // actual capture groups to match test set.
            let mut sgot = &got[..];
            if sgot.len() > expected.len() {
                sgot = &sgot[0..expected.len()]
            }
            if expected != sgot {
                panic!("For RE '{}' against '{:?}', \
                        expected '{:?}' but got '{:?}'",
                       $re, text, expected, sgot);
            }
        }
    );
);

// Some crazy expressions from regular-expressions.info.
mat!(match_ranges,
     r"\b(?:[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\b",
     "num: 255", Some((5, 8)));
mat!(match_ranges_not,
     r"\b(?:[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\b",
     "num: 256", None);
mat!(match_float1, r"[-+]?[0-9]*\.?[0-9]+", "0.1", Some((0, 3)));
mat!(match_float2, r"[-+]?[0-9]*\.?[0-9]+", "0.1.2", Some((0, 3)));
mat!(match_float3, r"[-+]?[0-9]*\.?[0-9]+", "a1.2", Some((1, 4)));
mat!(match_float4, r"^[-+]?[0-9]*\.?[0-9]+$", "1.a", None);
mat!(match_email, r"(?i)\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b",
     "mine is jam.slam@gmail.com ", Some((8, 26)));
mat!(match_email_not, r"(?i)\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b",
     "mine is jam.slam@gmail ", None);
mat!(match_email_big, r"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?",
     "mine is jam.slam@gmail.com ", Some((8, 26)));
mat!(match_date1,
     r"^(19|20)\d\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",
     "1900-01-01", Some((0, 10)));
mat!(match_date2,
     r"^(19|20)\d\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",
     "1900-00-01", None);
mat!(match_date3,
     r"^(19|20)\d\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",
     "1900-13-01", None);

// Exercise the flags.
mat!(match_flag_case, "(?i)abc", "ABC", Some((0, 3)));
mat!(match_flag_weird_case, "(?i)a(?-i)bc", "Abc", Some((0, 3)));
mat!(match_flag_weird_case_not, "(?i)a(?-i)bc", "ABC", None);
mat!(match_flag_case_dotnl, "(?is)a.", "A\n", Some((0, 2)));
mat!(match_flag_case_dotnl_toggle, "(?is)a.(?-is)a.", "A\nab", Some((0, 4)));
mat!(match_flag_case_dotnl_toggle_not, "(?is)a.(?-is)a.", "A\na\n", None);
mat!(match_flag_case_dotnl_toggle_ok, "(?is)a.(?-is:a.)?", "A\na\n", Some((0, 2)));
mat!(match_flag_multi, "(?m)(?:^\\d+$\n?)+", "123\n456\n789", Some((0, 11)));
mat!(match_flag_ungreedy, "(?U)a+", "aa", Some((0, 1)));
mat!(match_flag_ungreedy_greedy, "(?U)a+?", "aa", Some((0, 2)));
mat!(match_flag_ungreedy_noop, "(?U)(?-U)a+", "aa", Some((0, 2)));

// More exercising of multi-line flag.
matiter!(match_multi_1, r"(?m)^[a-z]+$", "abc\ndef\nxyz",
         (0, 3), (4, 7), (8, 11));
matiter!(match_multi_2, r"(?m)^$", "abc\ndef\nxyz");
matiter!(match_multi_3, r"(?m)^", "abc\ndef\nxyz",
         (0, 0), (4, 4), (8, 8));
matiter!(match_multi_4, r"(?m)$", "abc\ndef\nxyz",
         (3, 3), (7, 7), (11, 11));
matiter!(match_multi_5, r"(?m)^[a-z]", "abc\ndef\nxyz",
         (0, 1), (4, 5), (8, 9));
matiter!(match_multi_6, r"(?m)[a-z]^", "abc\ndef\nxyz");
matiter!(match_multi_7, r"(?m)[a-z]$", "abc\ndef\nxyz",
         (2, 3), (6, 7), (10, 11));
matiter!(match_multi_8, r"(?m)$[a-z]", "abc\ndef\nxyz");
matiter!(match_multi_9, r"(?m)^$", "", (0, 0));

matiter!(match_multi_rep_1, r"(?m)(?:^$)*", "a\nb\nc",
         (0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5));
matiter!(match_multi_rep_2, r"(?m)(?:^|a)+", "a\naaa\n",
         (0, 0), (2, 2), (3, 5), (6, 6));
matiter!(match_multi_rep_3, r"(?m)(?:^|a)*", "a\naaa\n",
         (0, 1), (2, 5), (6, 6));
matiter!(match_multi_rep_4, r"(?m)(?:^[a-z])+", "abc\ndef\nxyz",
         (0, 1), (4, 5), (8, 9));
matiter!(match_multi_rep_5, r"(?m)(?:^[a-z]{3}\n?)+", "abc\ndef\nxyz",
         (0, 11));
matiter!(match_multi_rep_6, r"(?m)(?:^[a-z]{3}\n?)*", "abc\ndef\nxyz",
         (0, 11));
matiter!(match_multi_rep_7, r"(?m)(?:\n?[a-z]{3}$)+", "abc\ndef\nxyz",
         (0, 11));
matiter!(match_multi_rep_8, r"(?m)(?:\n?[a-z]{3}$)*", "abc\ndef\nxyz",
         (0, 11));
matiter!(match_multi_rep_9, r"(?m)^*", "\naa\n",
         (0, 0), (1, 1), (2, 2), (3, 3), (4, 4));
matiter!(match_multi_rep_10, r"(?m)^+", "\naa\n",
         (0, 0), (1, 1), (4, 4));
matiter!(match_multi_rep_11, r"(?m)$*", "\naa\n",
         (0, 0), (1, 1), (2, 2), (3, 3), (4, 4));
matiter!(match_multi_rep_12, r"(?m)$+", "\naa\n",
         (0, 0), (3, 3), (4, 4));
matiter!(match_multi_rep_13, r"(?m)(?:$\n)+", "\n\naaa\n\n",
         (0, 2), (5, 7));
matiter!(match_multi_rep_14, r"(?m)(?:$\n)*", "\n\naaa\n\n",
         (0, 2), (3, 3), (4, 4), (5, 7));
matiter!(match_multi_rep_15, r"(?m)(?:$\n^)+", "\n\naaa\n\n",
         (0, 2), (5, 7));
matiter!(match_multi_rep_16, r"(?m)(?:^|$)+", "\n\naaa\n\n",
         (0, 0), (1, 1), (2, 2), (5, 5), (6, 6), (7, 7));
// matiter!(match_multi_rep_14, r"(?m)(?:$\n)*", "\n\naaa\n\n",
         // (0, 2), (3, 3), (4, 4), (5, 7));

matiter!(match_start_end_empty, r"^$", "", (0, 0));
matiter!(match_start_end_empty_many_1, r"^$^$^$", "", (0, 0));
matiter!(match_start_end_empty_many_2, r"^^^$$$", "", (0, 0));
matiter!(match_start_end_empty_rev, r"$^", "", (0, 0));
matiter!(match_start_end_empty_rep, r"(?:^$)*", "a\nb\nc",
         (0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5));
matiter!(match_start_end_empty_rep_rev, r"(?:$^)*", "a\nb\nc",
         (0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5));

// Some Unicode tests.
// A couple of these are commented out because something in the guts of macro
// expansion is creating invalid byte strings.
mat!(uni_one, r"\pN", "Ⅰ", Some((0, 3)));
mat!(uni_mixed, r"\pN+", "Ⅰ1Ⅱ2", Some((0, 8)));
mat!(uni_not, r"\PN+", "abⅠ", Some((0, 2)));
mat!(uni_not_class, r"[\PN]+", "abⅠ", Some((0, 2)));
mat!(uni_not_class_neg, r"[^\PN]+", "abⅠ", Some((2, 5)));
mat!(uni_case, r"(?i)Δ", "δ", Some((0, 2)));
mat!(uni_case_upper, r"\p{Lu}+", "ΛΘΓΔα", Some((0, 8)));
mat!(uni_case_upper_nocase_flag, r"(?i)\p{Lu}+", "ΛΘΓΔα", Some((0, 10)));
mat!(uni_case_upper_nocase, r"\p{L}+", "ΛΘΓΔα", Some((0, 10)));
mat!(uni_case_lower, r"\p{Ll}+", "ΛΘΓΔα", Some((8, 10)));

// https://github.com/rust-lang/regex/issues/76
mat!(uni_case_lower_nocase_flag, r"(?i)\p{Ll}+", "ΛΘΓΔα", Some((0, 10)));

// Test the Unicode friendliness of Perl character classes.
mat!(uni_perl_w, r"\w+", "dδd", Some((0, 4)));
mat!(uni_perl_w_not, r"\w+", "⥡", None);
mat!(uni_perl_w_neg, r"\W+", "⥡", Some((0, 3)));
mat!(uni_perl_d, r"\d+", "1२३9", Some((0, 8)));
mat!(uni_perl_d_not, r"\d+", "Ⅱ", None);
mat!(uni_perl_d_neg, r"\D+", "Ⅱ", Some((0, 3)));
mat!(uni_perl_s, r"\s+", " ", Some((0, 3)));
mat!(uni_perl_s_not, r"\s+", "☃", None);
mat!(uni_perl_s_neg, r"\S+", "☃", Some((0, 3)));

// And do the same for word boundaries.
mat!(uni_boundary_none, r"\d\b", "6δ", None);
mat!(uni_boundary_ogham, r"\d\b", "6 ", Some((0, 1)));

// Test negated character classes.
mat!(negclass_letters, r"[^ac]", "acx", Some((2, 3)));
mat!(negclass_letter_comma, r"[^a,]", "a,x", Some((2, 3)));
mat!(negclass_letter_space, r"[^a\s]", "a x", Some((2, 3)));
mat!(negclass_comma, r"[^,]", ",,x", Some((2, 3)));
mat!(negclass_space, r"[^\s]", " a", Some((1, 2)));
mat!(negclass_space_comma, r"[^,\s]", ", a", Some((2, 3)));
mat!(negclass_comma_space, r"[^\s,]", " ,a", Some((2, 3)));
mat!(negclass_ascii, r"[^[:alpha:]Z]", "A1", Some((1, 2)));

// Regression test for https://github.com/rust-lang/regex/issues/75
mat!(regression_unsorted_binary_search_1, r"(?i)[a_]+", "A_", Some((0, 2)));
mat!(regression_unsorted_binary_search_2, r"(?i)[A_]+", "a_", Some((0, 2)));

// Regression tests for https://github.com/rust-lang/regex/issues/99
mat!(regression_negated_char_class_1, r"(?i)[^x]", "x", None);
mat!(regression_negated_char_class_2, r"(?i)[^x]", "X", None);

// Regression test for https://github.com/rust-lang/regex/issues/101
mat!(regression_ascii_word_underscore, r"[:word:]", "_", Some((0, 1)));

// Regression test for https://github.com/rust-lang-nursery/regex/issues/129
#[test]
fn regression_captures_rep() {
    let re = regex!(r"([a-f]){2}(?P<foo>[x-z])");
    let caps = re.captures("abx").unwrap();
    assert_eq!(caps.name("foo").unwrap(), "x");
}

// Regression test for https://github.com/rust-lang-nursery/regex/issues/153
mat!(regression_alt_in_alt1, r"ab?|$", "az", Some((0, 1)));
mat!(regression_alt_in_alt2, r"^(.*?)(\n|\r\n?|$)", "ab\rcd", Some((0, 3)));

mat!(one_unicode, r"☃", "☃", Some((0, 3)));

// Regression test for https://github.com/rust-lang-nursery/regex/issues/169
mat!(regression_leftmost_first_prefix, r"z*azb", "azb", Some((0, 3)));

// A whole mess of tests from Glenn Fowler's regex test suite.
// Generated by the 'src/etc/regex-match-tests' program.
#[path = "matches.rs"]
mod matches;
