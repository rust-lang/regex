// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! This crate provides a native implementation of regular expressions that is
//! heavily based on RE2 both in syntax and in implementation. Notably,
//! backreferences and arbitrary lookahead/lookbehind assertions are not
//! provided. In return, regular expression searching provided by this package
//! has excellent worst-case performance. The specific syntax supported is
//! documented further down.
//!
//! This crate's documentation provides some simple examples, describes Unicode
//! support and exhaustively lists the supported syntax. For more specific
//! details on the API, please see the documentation for the `Regex` type.
//!
//! # Usage
//!
//! This crate is [on crates.io](https://crates.io/crates/regex) and can be
//! used by adding `regex` to your dependencies in your project's `Cargo.toml`.
//!
//! ```toml
//! [dependencies]
//! regex = "0.1.8"
//! ```
//!
//! and this to your crate root:
//!
//! ```rust
//! extern crate regex;
//! ```
//!
//! # First example: find a date
//!
//! General use of regular expressions in this package involves compiling an
//! expression and then using it to search, split or replace text. For example,
//! to confirm that some text resembles a date:
//!
//! ```rust
//! use regex::Regex;
//! let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
//! assert!(re.is_match("2014-01-01"));
//! ```
//!
//! Notice the use of the `^` and `$` anchors. In this crate, every expression
//! is executed with an implicit `.*?` at the beginning and end, which allows
//! it to match anywhere in the text. Anchors can be used to ensure that the
//! full text matches an expression.
//!
//! This example also demonstrates the utility of
//! [raw strings](http://doc.rust-lang.org/stable/reference.html#raw-byte-string-literals)
//! in Rust, which
//! are just like regular strings except they are prefixed with an `r` and do
//! not process any escape sequences. For example, `"\\d"` is the same
//! expression as `r"\d"`.
//!
//! # The `regex!` macro
//!
//! Rust's compile-time meta-programming facilities provide a way to write a
//! `regex!` macro which compiles regular expressions *when your program
//! compiles*. Said differently, if you only use `regex!` to build regular
//! expressions in your program, then your program cannot compile with an
//! invalid regular expression. Moreover, the `regex!` macro compiles the
//! given expression to native Rust code, which ideally makes it faster.
//! Unfortunately (or fortunately), the dynamic implementation has had a lot
//! more optimization work put it into it currently, so it is faster than
//! the `regex!` macro in most cases.
//!
//! To use the `regex!` macro, you must enable the `plugin` feature and import
//! the `regex_macros` crate as a syntax extension:
//!
//! ```ignore
//! #![feature(plugin)]
//! #![plugin(regex_macros)]
//! extern crate regex;
//!
//! fn main() {
//!     let re = regex!(r"^\d{4}-\d{2}-\d{2}$");
//!     assert!(re.is_match("2014-01-01"));
//! }
//! ```
//!
//! There are a few things worth mentioning about using the `regex!` macro.
//! Firstly, the `regex!` macro *only* accepts string *literals*.
//! Secondly, the `regex` crate *must* be linked with the name `regex` since
//! the generated code depends on finding symbols in the `regex` crate.
//!
//! One downside of using the `regex!` macro is that it can increase the
//! size of your program's binary since it generates specialized Rust code.
//! The extra size probably won't be significant for a small number of
//! expressions, but 100+ calls to `regex!` will probably result in a
//! noticeably bigger binary.
//!
//! **NOTE**: This is implemented using a compiler plugin, which is not
//! available on the Rust 1.0 beta/stable channels. Therefore, you'll only
//! be able to use `regex!` on the nightlies.
//!
//! # Example: iterating over capture groups
//!
//! This crate provides convenient iterators for matching an expression
//! repeatedly against a search string to find successive non-overlapping
//! matches. For example, to find all dates in a string and be able to access
//! them by their component pieces:
//!
//! ```rust
//! # extern crate regex; use regex::Regex;
//! # fn main() {
//! let re = Regex::new(r"(\d{4})-(\d{2})-(\d{2})").unwrap();
//! let text = "2012-03-14, 2013-01-01 and 2014-07-05";
//! for cap in re.captures_iter(text) {
//!     println!("Month: {} Day: {} Year: {}",
//!              cap.at(2).unwrap_or(""), cap.at(3).unwrap_or(""),
//!              cap.at(1).unwrap_or(""));
//! }
//! // Output:
//! // Month: 03 Day: 14 Year: 2012
//! // Month: 01 Day: 01 Year: 2013
//! // Month: 07 Day: 05 Year: 2014
//! # }
//! ```
//!
//! Notice that the year is in the capture group indexed at `1`. This is
//! because the *entire match* is stored in the capture group at index `0`.
//!
//! # Example: replacement with named capture groups
//!
//! Building on the previous example, perhaps we'd like to rearrange the date
//! formats. This can be done with text replacement. But to make the code
//! clearer, we can *name*  our capture groups and use those names as variables
//! in our replacement text:
//!
//! ```rust
//! # extern crate regex; use regex::Regex;
//! # fn main() {
//! let re = Regex::new(r"(?P<y>\d{4})-(?P<m>\d{2})-(?P<d>\d{2})").unwrap();
//! let before = "2012-03-14, 2013-01-01 and 2014-07-05";
//! let after = re.replace_all(before, "$m/$d/$y");
//! assert_eq!(after, "03/14/2012, 01/01/2013 and 07/05/2014");
//! # }
//! ```
//!
//! The `replace` methods are actually polymorphic in the replacement, which
//! provides more flexibility than is seen here. (See the documentation for
//! `Regex::replace` for more details.)
//!
//! Note that if your regex gets complicated, you can use the `x` flag to
//! enable insigificant whitespace mode, which also lets you write comments:
//!
//! ```rust
//! # extern crate regex; use regex::Regex;
//! # fn main() {
//! let re = Regex::new(r"(?x)
//!   (?P<y>\d{4}) # the year
//!   -
//!   (?P<m>\d{2}) # the month
//!   -
//!   (?P<d>\d{2}) # the day
//! ").unwrap();
//! let before = "2012-03-14, 2013-01-01 and 2014-07-05";
//! let after = re.replace_all(before, "$m/$d/$y");
//! assert_eq!(after, "03/14/2012, 01/01/2013 and 07/05/2014");
//! # }
//! ```
//!
//! # Pay for what you use
//!
//! With respect to searching text with a regular expression, there are three
//! questions that can be asked:
//!
//! 1. Does the text match this expression?
//! 2. If so, where does it match?
//! 3. Where are the submatches?
//!
//! Generally speaking, this crate could provide a function to answer only #3,
//! which would subsume #1 and #2 automatically. However, it can be
//! significantly more expensive to compute the location of submatches, so it's
//! best not to do it if you don't need to.
//!
//! Therefore, only use what you need. For example, don't use `find` if you
//! only need to test if an expression matches a string. (Use `is_match`
//! instead.)
//!
//! # Unicode
//!
//! This implementation executes regular expressions **only** on sequences of
//! Unicode scalar values while exposing match locations as byte indices into
//! the search string.
//!
//! Currently, only simple case folding is supported. Namely, when matching
//! case-insensitively, the characters are first mapped using the
//! [simple case folding](ftp://ftp.unicode.org/Public/UNIDATA/CaseFolding.txt)
//! mapping.
//!
//! Regular expressions themselves are also **only** interpreted as a sequence
//! of Unicode scalar values. This means you can use Unicode characters
//! directly in your expression:
//!
//! ```rust
//! # extern crate regex; use regex::Regex;
//! # fn main() {
//! let re = Regex::new(r"(?i)Δ+").unwrap();
//! assert_eq!(re.find("ΔδΔ"), Some((0, 6)));
//! # }
//! ```
//!
//! Finally, Unicode general categories and scripts are available as character
//! classes. For example, you can match a sequence of numerals, Greek or
//! Cherokee letters:
//!
//! ```rust
//! # extern crate regex; use regex::Regex;
//! # fn main() {
//! let re = Regex::new(r"[\pN\p{Greek}\p{Cherokee}]+").unwrap();
//! assert_eq!(re.find("abcΔᎠβⅠᏴγδⅡxyz"), Some((3, 23)));
//! # }
//! ```
//!
//! # Syntax
//!
//! The syntax supported in this crate is almost in an exact correspondence
//! with the syntax supported by RE2. It is documented below.
//!
//! Note that the regular expression parser and abstract syntax are exposed in
//! a separate crate,
//! [`regex-syntax`](../regex_syntax/index.html).
//!
//! ## Matching one character
//!
//! <pre class="rust">
//! .           any character except new line (includes new line with s flag)
//! [xyz]       A character class matching either x, y or z.
//! [^xyz]      A character class matching any character except x, y and z.
//! [a-z]       A character class matching any character in range a-z.
//! \d          digit (\p{Nd})
//! \D          not digit
//! [:alpha:]   ASCII character class ([A-Za-z])
//! [:^alpha:]  Negated ASCII character class ([^A-Za-z])
//! \pN         One-letter name Unicode character class
//! \p{Greek}   Unicode character class (general category or script)
//! \PN         Negated one-letter name Unicode character class
//! \P{Greek}   negated Unicode character class (general category or script)
//! </pre>
//!
//! Any named character class may appear inside a bracketed `[...]` character
//! class. For example, `[\p{Greek}\pN]` matches any Greek or numeral
//! character.
//!
//! ## Composites
//!
//! <pre class="rust">
//! xy    concatenation (x followed by y)
//! x|y   alternation (x or y, prefer x)
//! </pre>
//!
//! ## Repetitions
//!
//! <pre class="rust">
//! x*        zero or more of x (greedy)
//! x+        one or more of x (greedy)
//! x?        zero or one of x (greedy)
//! x*?       zero or more of x (ungreedy)
//! x+?       one or more of x (ungreedy)
//! x??       zero or one of x (ungreedy)
//! x{n,m}    at least n x and at most m x (greedy)
//! x{n,}     at least n x (greedy)
//! x{n}      exactly n x
//! x{n,m}?   at least n x and at most m x (ungreedy)
//! x{n,}?    at least n x (ungreedy)
//! x{n}?     exactly n x
//! </pre>
//!
//! ## Empty matches
//!
//! <pre class="rust">
//! ^     the beginning of text (or start-of-line with multi-line mode)
//! $     the end of text (or end-of-line with multi-line mode)
//! \A    only the beginning of text (even with multi-line mode enabled)
//! \z    only the end of text (even with multi-line mode enabled)
//! \b    a Unicode word boundary (\w on one side and \W, \A, or \z on other)
//! \B    not a Unicode word boundary
//! </pre>
//!
//! ## Grouping and flags
//!
//! <pre class="rust">
//! (exp)          numbered capture group (indexed by opening parenthesis)
//! (?P&lt;name&gt;exp)  named (also numbered) capture group (allowed chars: [_0-9a-zA-Z])
//! (?:exp)        non-capturing group
//! (?flags)       set flags within current group
//! (?flags:exp)   set flags for exp (non-capturing)
//! </pre>
//!
//! Flags are each a single character. For example, `(?x)` sets the flag `x`
//! and `(?-x)` clears the flag `x`. Multiple flags can be set or cleared at
//! the same time: `(?xy)` sets both the `x` and `y` flags and `(?x-y)` sets
//! the `x` flag and clears the `y` flag.
//!
//! All flags are by default disabled. They are:
//!
//! <pre class="rust">
//! i     case-insensitive
//! m     multi-line mode: ^ and $ match begin/end of line
//! s     allow . to match \n
//! U     swap the meaning of x* and x*?
//! x     ignore whitespace and allow line comments (starting with `#`)
//! </pre>
//!
//! Here's an example that matches case-insensitively for only part of the
//! expression:
//!
//! ```rust
//! # extern crate regex; use regex::Regex;
//! # fn main() {
//! let re = Regex::new(r"(?i)a+(?-i)b+").unwrap();
//! let cap = re.captures("AaAaAbbBBBb").unwrap();
//! assert_eq!(cap.at(0), Some("AaAaAbb"));
//! # }
//! ```
//!
//! Notice that the `a+` matches either `a` or `A`, but the `b+` only matches
//! `b`.
//!
//! ## Escape sequences
//!
//! <pre class="rust">
//! \*         literal *, works for any punctuation character: \.+*?()|[]{}^$
//! \a         bell (\x07)
//! \f         form feed (\x0C)
//! \t         horizontal tab
//! \n         new line
//! \r         carriage return
//! \v         vertical tab (\x0B)
//! \123       octal character code (up to three digits)
//! \x7F       hex character code (exactly two digits)
//! \x{10FFFF} any hex character code corresponding to a Unicode code point
//! </pre>
//!
//! ## Perl character classes (Unicode friendly)
//!
//! These classes are based on the definitions provided in
//! [UTS#18](http://www.unicode.org/reports/tr18/#Compatibility_Properties):
//!
//! <pre class="rust">
//! \d     digit (\p{Nd})
//! \D     not digit
//! \s     whitespace (\p{White_Space})
//! \S     not whitespace
//! \w     word character (\p{Alphabetic} + \p{M} + \d + \p{Pc} + \p{Join_Control})
//! \W     not word character
//! </pre>
//!
//! ## ASCII character classes
//!
//! <pre class="rust">
//! [:alnum:]    alphanumeric ([0-9A-Za-z])
//! [:alpha:]    alphabetic ([A-Za-z])
//! [:ascii:]    ASCII ([\x00-\x7F])
//! [:blank:]    blank ([\t ])
//! [:cntrl:]    control ([\x00-\x1F\x7F])
//! [:digit:]    digits ([0-9])
//! [:graph:]    graphical ([!-~])
//! [:lower:]    lower case ([a-z])
//! [:print:]    printable ([ -~])
//! [:punct:]    punctuation ([!-/:-@[-`{-~])
//! [:space:]    whitespace ([\t\n\v\f\r ])
//! [:upper:]    upper case ([A-Z])
//! [:word:]     word characters ([0-9A-Za-z_])
//! [:xdigit:]   hex digit ([0-9A-Fa-f])
//! </pre>
//!
//! # Untrusted input
//!
//! This crate can handle both untrusted regular expressions and untrusted
//! search text.
//!
//! Untrusted regular expressions are handled by capping the size of a compiled
//! regular expression. (See `Regex::with_size_limit`.) Without this, it would
//! be trivial for an attacker to exhaust your system's memory with expressions
//! like `a{100}{100}{100}`.
//!
//! Untrusted search text is allowed because the matching engine(s) in this
//! crate have time complexity `O(mn)` (with `m ~ regex` and `n ~ search
//! text`), which means there's no way to cause exponential blow-up like with
//! some other regular expression engines. (We pay for this by disallowing
//! features like arbitrary look-ahead and back-references.)

#![deny(missing_docs)]
#![cfg_attr(test, deny(warnings))]
#![cfg_attr(feature = "pattern", feature(pattern))]
#![doc(html_logo_url = "http://www.rust-lang.org/logos/rust-logo-128x128-blk-v2.png",
       html_favicon_url = "http://www.rust-lang.org/favicon.ico",
       html_root_url = "http://doc.rust-lang.org/regex/")]

extern crate aho_corasick;
extern crate memchr;
extern crate regex_syntax as syntax;

pub use re::{
    Regex, Error, Captures, SubCaptures, SubCapturesPos, SubCapturesNamed,
    FindCaptures, FindMatches,
    Replacer, NoExpand, RegexSplits, RegexSplitsN,
    quote, is_match,
};

mod backtrack;
mod char;
mod compile;
mod input;
mod pool;
mod prefix;
mod program;
mod nfa;
mod re;

/// The `internal` module exists to support the `regex!` macro and other
/// suspicious activity, such as testing different matching engines.
#[doc(hidden)]
pub mod internal {
    pub use char::Char;
    pub use input::{Input, CharInput, InputAt};
    pub use program::{
        Program, MatchEngine, CharRanges, Inst, LookInst, OneChar,
    };
    pub use re::ExNative;
    pub use re::Regex::{Dynamic, Native};
}
