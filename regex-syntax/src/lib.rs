// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!
This crate provides a regular expression parser and an abstract syntax for
regular expressions. The abstract syntax is defined by the `Expr` type. The
concrete syntax is enumerated in the
[`regex`](../regex/index.html#syntax)
crate documentation.

Note that since this crate is first and foremost an implementation detail for
the `regex` crate, it may experience more frequent breaking changes. It is
exposed as a separate crate so that others may use it to do analysis on regular
expressions or even build their own matching engine.

# Example: parsing an expression

Parsing a regular expression can be done with the `Expr::parse` function.

```rust
use regex_syntax::Expr;

assert_eq!(Expr::parse(r"ab|yz").unwrap(), Expr::Alternate(vec![
    Expr::Literal { chars: vec!['a', 'b'], casei: false },
    Expr::Literal { chars: vec!['y', 'z'], casei: false },
]));
```

# Example: inspecting an error

The parser in this crate provides very detailed error values. For example,
if an invalid character class range is given:

```rust
use regex_syntax::{Expr, ErrorKind};

let err = Expr::parse(r"[z-a]").unwrap_err();
assert_eq!(err.position(), 4);
assert_eq!(err.kind(), &ErrorKind::InvalidClassRange {
    start: 'z',
    end: 'a',
});
```

Or unbalanced parentheses:

```rust
use regex_syntax::{Expr, ErrorKind};

let err = Expr::parse(r"ab(cd").unwrap_err();
assert_eq!(err.position(), 2);
assert_eq!(err.kind(), &ErrorKind::UnclosedParen);
```
*/

#![deny(missing_docs)]

#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

mod parser;
mod unicode;

use std::char;
use std::cmp::{Ordering, max, min};
use std::fmt;
use std::iter::IntoIterator;
use std::ops::Deref;
use std::slice;
use std::vec;

use unicode::case_folding;

use self::Expr::*;
use self::Repeater::*;

pub use parser::is_punct;

/// A regular expression abstract syntax tree.
///
/// An `Expr` represents the abstract syntax of a regular expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    /// An empty regex (which never matches any text).
    Empty,
    /// A sequence of one or more literal characters to be matched.
    Literal {
        /// The characters.
        chars: Vec<char>,
        /// Whether to match case insensitively.
        casei: bool,
    },
    /// Match any character, excluding new line.
    AnyChar,
    /// Match any character.
    AnyCharNoNL,
    /// A character class.
    Class(CharClass),
    /// Match the start of a line or beginning of input.
    StartLine,
    /// Match the end of a line or end of input.
    EndLine,
    /// Match the beginning of input.
    StartText,
    /// Match the end of input.
    EndText,
    /// Match a word boundary (word character on one side and a non-word
    /// character on the other).
    WordBoundary,
    /// Match a position that is not a word boundary (word or non-word
    /// characters on both sides).
    NotWordBoundary,
    /// A group, possibly non-capturing.
    Group {
        /// The expression inside the group.
        e: Box<Expr>,
        /// The capture index (starting at `1`) only for capturing groups.
        i: Option<usize>,
        /// The capture name, only for capturing named groups.
        name: Option<String>,
    },
    /// A repeat operator (`?`, `*`, `+` or `{m,n}`).
    Repeat {
        /// The expression to be repeated. Limited to literals, `.`, classes
        /// or grouped expressions.
        e: Box<Expr>,
        /// The type of repeat operator used.
        r: Repeater,
        /// Whether the repeat is greedy (match the most) or not (match the
        /// least).
        greedy: bool,
    },
    /// A concatenation of expressions. Must be matched one after the other.
    ///
    /// N.B. A concat expression can only appear at the top-level or
    /// immediately inside a group expression.
    Concat(Vec<Expr>),
    /// An alternation of expressions. Only one must match.
    ///
    /// N.B. An alternate expression can only appear at the top-level or
    /// immediately inside a group expression.
    Alternate(Vec<Expr>),
}

type CaptureIndex = Option<usize>;

type CaptureName = Option<String>;

/// The type of a repeat operator expression.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Repeater {
    /// Match zero or one (`?`).
    ZeroOrOne,
    /// Match zero or more (`*`).
    ZeroOrMore,
    /// Match one or more (`+`).
    OneOrMore,
    /// Match for at least `min` and at most `max` (`{m,n}`).
    ///
    /// When `max` is `None`, there is no upper bound on the number of matches.
    Range {
        /// Lower bound on the number of matches.
        min: u32,
        /// Optional upper bound on the number of matches.
        max: Option<u32>,
    },
}

/// A character class.
///
/// A character class has a canonical format that the parser guarantees. Its
/// canonical format is defined by the following invariants:
///
/// 1. Given any Unicode scalar value, it is matched by *at most* one character
///    range in a canonical character class.
/// 2. Every adjacent character range is separated by at least one Unicode
///    scalar value.
/// 3. Given any pair of character ranges `r1` and `r2`, if
///    `r1.end < r2.start`, then `r1` comes before `r2` in a canonical
///    character class.
///
/// In sum, any `CharClass` produced by this crate's parser is a sorted
/// sequence of non-overlapping ranges. This makes it possible to test whether
/// a character is matched by a class with a binary search.
///
/// Additionally, a character class may be marked *case insensitive*. If it's
/// case insensitive, then:
///
/// 1. Simple case folding has been applied to all ranges.
/// 2. Simple case folding must be applied to a character before testing
///    whether it matches the character class.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CharClass {
    ranges: Vec<ClassRange>,
    casei: bool,
}

/// A single inclusive range in a character class.
///
/// Since range boundaries are defined by Unicode scalar values, the boundaries
/// can never be in the open interval `(0xD7FF, 0xE000)`. However, a range may
/// *cover* codepoints that are not scalar values.
///
/// Note that this has a few convenient impls on `PartialEq` and `PartialOrd`
/// for testing whether a character is contained inside a given range.
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct ClassRange {
    /// The start character of the range.
    ///
    /// This must be less than or equal to `end`.
    pub start: char,

    /// The end character of the range.
    ///
    /// This must be greater than or equal to `end`.
    pub end: char,
}

impl Expr {
    /// Parses a string in a regular expression syntax tree.
    pub fn parse(s: &str) -> Result<Expr> {
        parser::Parser::parse(s).map(|e| e.simplify())
    }

    /// Returns true iff the expression can be repeated by a quantifier.
    fn can_repeat(&self) -> bool {
        match *self {
            Literal{..}
            | AnyChar
            | AnyCharNoNL
            | Class(_)
            | StartLine | EndLine | StartText | EndText
            | WordBoundary | NotWordBoundary
            | Group{..}
            => true,
            _ => false,
        }
    }

    fn simplify(self) -> Expr {
        fn combine_literals(es: &mut Vec<Expr>, e: Expr) {
            match (es.pop(), e) {
                (None, e) => es.push(e),
                (Some(Literal { chars: mut chars1, casei: casei1 }),
                 Literal { chars: chars2, casei: casei2 }) => {
                    if casei1 == casei2 {
                        chars1.extend(chars2);
                        es.push(Literal { chars: chars1, casei: casei1 });
                    } else {
                        es.push(Literal { chars: chars1, casei: casei1 });
                        es.push(Literal { chars: chars2, casei: casei2 });
                    }
                }
                (Some(e1), e2) => {
                    es.push(e1);
                    es.push(e2);
                }
            }
        }
        match self {
            Repeat { e, r, greedy } => Repeat {
                e: Box::new(e.simplify()),
                r: r,
                greedy: greedy,
            },
            Group { e, i, name } => {
                let e = e.simplify();
                if i.is_none() && name.is_none() && e.can_repeat() {
                    e
                } else {
                    Group { e: Box::new(e), i: i, name: name }
                }
            }
            Concat(es) => {
                let mut new_es = Vec::with_capacity(es.len());
                for e in es {
                    combine_literals(&mut new_es, e.simplify());
                }
                if new_es.len() == 1 {
                    new_es.pop().unwrap()
                } else {
                    Concat(new_es)
                }
            }
            Alternate(es) => Alternate(es.into_iter()
                                         .map(|e| e.simplify())
                                         .collect()),
            e => e,
        }
    }
}

impl Deref for CharClass {
    type Target = Vec<ClassRange>;
    fn deref(&self) -> &Vec<ClassRange> { &self.ranges }
}

impl IntoIterator for CharClass {
    type Item = ClassRange;
    type IntoIter = vec::IntoIter<ClassRange>;
    fn into_iter(self) -> vec::IntoIter<ClassRange> { self.ranges.into_iter() }
}

impl<'a> IntoIterator for &'a CharClass {
    type Item = &'a ClassRange;
    type IntoIter = slice::Iter<'a, ClassRange>;
    fn into_iter(self) -> slice::Iter<'a, ClassRange> { self.iter() }
}

impl CharClass {
    /// Create a new class from an existing set of ranges.
    fn new(ranges: Vec<ClassRange>) -> CharClass {
        CharClass { ranges: ranges, casei: false }
    }

    /// Create an empty class.
    fn empty() -> CharClass {
        CharClass::new(Vec::new())
    }

    /// Returns true if `c` is matched by this character class.
    ///
    /// If this character class is case insensitive, then simple case folding
    /// is applied to `c` before checking for a match.
    pub fn matches(&self, mut c: char) -> bool {
        if self.is_case_insensitive() {
            c = simple_case_fold(c)
        }
        self.binary_search_by(|range| c.partial_cmp(range).unwrap()).is_ok()
    }

    /// Returns true if this character class should be matched case
    /// insensitively.
    ///
    /// When `true`, simple case folding has already been applied to the
    /// class.
    pub fn is_case_insensitive(&self) -> bool {
        self.casei
    }

    /// Create a new empty class from this one.
    ///
    /// Namely, its capacity and case insensitive setting will be the same.
    fn to_empty(&self) -> CharClass {
        CharClass { ranges: Vec::with_capacity(self.len()), casei: self.casei }
    }

    /// Merge two classes and canonicalize them.
    #[cfg(test)]
    fn merge(mut self, other: CharClass) -> CharClass {
        self.ranges.extend(other);
        self.canonicalize()
    }

    /// Canonicalze any sequence of ranges.
    ///
    /// This is responsible for enforcing the canonical format invariants
    /// as described on the docs for the `CharClass` type.
    fn canonicalize(mut self) -> CharClass {
        // TODO: Save some cycles here by checking if already canonicalized.
        self.ranges.sort();
        let mut ordered = self.to_empty(); // TODO: Do this in place?
        for candidate in self {
            // If the candidate overlaps with an existing range, then it must
            // be the most recent range added because we process the candidates
            // in order.
            if let Some(or) = ordered.ranges.last_mut() {
                if or.overlapping(candidate) {
                    *or = or.merge(candidate);
                    continue;
                }
            }
            ordered.ranges.push(candidate);
        }
        ordered
    }

    /// Negates the character class.
    ///
    /// For all `c` where `c` is a Unicode scalar value, `c` matches `self`
    /// if and only if `c` does not match `self.negate()`.
    ///
    /// Note that this cannot be called on a character class that has had
    /// case folding applied to it. (Because case folding turns on a flag
    /// and doesn't store every possible matching character. Therefore,
    /// its negation is tricky to get right. Turns out, we don't need it
    /// anyway!)
    fn negate(mut self) -> CharClass {
        fn range(s: char, e: char) -> ClassRange { ClassRange::new(s, e) }

        // Never allow negating of a class that has been case folded!
        assert!(!self.casei);

        if self.is_empty() { return self; }
        self = self.canonicalize();
        let mut inv = self.to_empty();
        if self[0].start > '\x00' {
            inv.ranges.push(range('\x00', dec_char(self[0].start)));
        }
        for win in self.windows(2) {
            inv.ranges.push(range(inc_char(win[0].end),
                                  dec_char(win[1].start)));
        }
        if self[self.len() - 1].end < char::MAX {
            inv.ranges.push(range(inc_char(self[self.len() - 1].end),
                                  char::MAX));
        }
        inv
    }

    /// Apply case folding to this character class.
    ///
    /// One a class had been case folded, it cannot be negated.
    fn case_fold(self) -> CharClass {
        let mut folded = self.to_empty();
        folded.casei = true;
        for r in self {
            // Applying case folding to a range is expensive because *every*
            // character needed to be examined. Thus, we avoid that drudgery
            // if no character in the current range is in our case folding
            // table.
            if r.needs_case_folding() {
                folded.ranges.extend(r.case_fold());
            } else {
                folded.ranges.push(r);
            }
        }
        folded.canonicalize()
    }
}

impl ClassRange {
    /// Create a new class range.
    ///
    /// If `end < start`, then the two values are swapped so that
    /// the invariant `start <= end` is preserved.
    fn new(start: char, end: char) -> ClassRange {
        if start <= end {
            ClassRange { start: start, end: end }
        } else {
            ClassRange { start: end, end: start }
        }
    }

    /// Create a range of one character.
    fn one(c: char) -> ClassRange {
        ClassRange { start: c, end: c }
    }

    /// Returns true if and only if the two ranges are overlapping. Note that
    /// since ranges are inclusive, `a-c` and `d-f` are overlapping!
    fn overlapping(self, other: ClassRange) -> bool {
        max(self.start, other.start) <= inc_char(min(self.end, other.end))
    }

    /// Creates a new range representing the union of `self` and `other.
    fn merge(self, other: ClassRange) -> ClassRange {
        ClassRange {
            start: min(self.start, other.start),
            end: max(self.end, other.end),
        }
    }

    /// Returns true if and only if this range contains a character that is
    /// in the case folding table.
    fn needs_case_folding(self) -> bool {
        case_folding::C_plus_S_table
        .binary_search_by(|&(c, _)| self.partial_cmp(&c).unwrap()).is_ok()
    }

    /// Apply case folding to this range.
    ///
    /// Since case folding might add characters such that the range is no
    /// longer contiguous, this returns multiple class ranges. They are in
    /// canonical order.
    fn case_fold(self) -> Vec<ClassRange> {
        let (s, e) = (self.start as u32, self.end as u32 + 1);
        let mut start = simple_case_fold(self.start);
        let mut end = start;
        let mut next_case_fold = self.start;
        let mut ranges = Vec::with_capacity(100);
        for mut c in (s+1..e).filter_map(char::from_u32) {
            if c >= next_case_fold {
                c = match simple_case_fold_result(c) {
                    Ok(i) => case_folding::C_plus_S_table[i].1,
                    Err(i) => {
                        if i < case_folding::C_plus_S_table.len() {
                            next_case_fold = case_folding::C_plus_S_table[i].0;
                        } else {
                            next_case_fold = '\u{10FFFF}'
                        }
                        c
                    }
                };
            }
            if c != inc_char(end) {
                ranges.push(ClassRange::new(start, end));
                start = c;
            }
            end = c;
        }
        ranges.push(ClassRange::new(start, end));
        ranges
    }
}

impl PartialEq<char> for ClassRange {
    #[inline]
    fn eq(&self, other: &char) -> bool {
        self.start <= *other && *other <= self.end
    }
}

impl PartialEq<ClassRange> for char {
    #[inline]
    fn eq(&self, other: &ClassRange) -> bool {
        other.eq(self)
    }
}

impl PartialOrd<char> for ClassRange {
    #[inline]
    fn partial_cmp(&self, other: &char) -> Option<Ordering> {
        Some(if self == other {
            Ordering::Equal
        } else if *other > self.end {
            Ordering::Greater
        } else {
            Ordering::Less
        })
    }
}

impl PartialOrd<ClassRange> for char {
    #[inline]
    fn partial_cmp(&self, other: &ClassRange) -> Option<Ordering> {
        other.partial_cmp(self).map(|o| o.reverse())
    }
}

/// This implementation of `Display` will write a regular expression from the
/// syntax tree. It does not write the original string parsed.
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Empty => write!(f, ""),
            Literal { ref chars, casei } => {
                if casei { try!(write!(f, "(?i:")); }
                for &c in chars {
                    try!(write!(f, "{}", quote_char(c)));
                }
                if casei { try!(write!(f, ")")); }
                Ok(())
            }
            AnyChar => write!(f, "(?s:.)"),
            AnyCharNoNL => write!(f, "."),
            Class(ref cls) => write!(f, "{}", cls),
            StartLine => write!(f, "(?m:^)"),
            EndLine => write!(f, "(?m:$)"),
            StartText => write!(f, r"^"),
            EndText => write!(f, r"$"),
            WordBoundary => write!(f, r"\b"),
            NotWordBoundary => write!(f, r"\B"),
            Group { ref e, i: None, name: None } => write!(f, "(?:{})", e),
            Group { ref e, name: None, .. } => write!(f, "({})", e),
            Group { ref e, name: Some(ref n), .. } => {
                write!(f, "(?P<{}>{})", n, e)
            }
            Repeat { ref e, r, greedy } => {
                match &**e {
                    &Literal { ref chars, .. } if chars.len() > 1 => {
                        try!(write!(f, "(?:{}){}", e, r))
                    }
                    _ => try!(write!(f, "{}{}", e, r)),
                }
                if !greedy { try!(write!(f, "?")); }
                Ok(())
            }
            Concat(ref es) => {
                for e in es {
                    try!(write!(f, "{}", e));
                }
                Ok(())
            }
            Alternate(ref es) => {
                for (i, e) in es.iter().enumerate() {
                    if i > 0 { try!(write!(f, "|")); }
                    try!(write!(f, "{}", e));
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Repeater {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ZeroOrOne => write!(f, "?"),
            ZeroOrMore => write!(f, "*"),
            OneOrMore => write!(f, "+"),
            Range { min: s, max: None } => write!(f, "{{{},}}", s),
            Range { min: s, max: Some(e) } if s == e => write!(f, "{{{}}}", s),
            Range { min: s, max: Some(e) } => write!(f, "{{{}, {}}}", s, e),
        }
    }
}

impl fmt::Display for CharClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.casei {
            try!(write!(f, "(?i:"));
        }
        try!(write!(f, "["));
        for range in self.iter() {
            try!(write!(f, "{}", range));
        }
        try!(write!(f, "]"));
        if self.casei {
            try!(write!(f, ")"));
        }
        Ok(())
    }
}

impl fmt::Display for ClassRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", quote_char(self.start), quote_char(self.end))
    }
}

/// An alias for computations that can return a `Error`.
pub type Result<T> = ::std::result::Result<T, Error>;

/// A parse error.
///
/// This includes details about the specific type of error and a rough
/// approximation of where it occurred.
#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pos: usize,
    surround: String,
    kind: ErrorKind,
}

/// The specific type of parse error that can occur.
#[derive(Clone, Debug, PartialEq)]
pub enum ErrorKind {
    /// A negation symbol is used twice in flag settings.
    /// e.g., `(?-i-s)`.
    DoubleFlagNegation,
    /// The same capture name was used more than once.
    /// e.g., `(?P<a>.)(?P<a>.)`.
    DuplicateCaptureName(String),
    /// An alternate is empty. e.g., `(|a)`.
    EmptyAlternate,
    /// A capture group name is empty. e.g., `(?P<>a)`.
    EmptyCaptureName,
    /// A negation symbol was not proceded by any flags. e.g., `(?i-)`.
    EmptyFlagNegation,
    /// A group is empty. e.g., `()`.
    EmptyGroup,
    /// An invalid number was used in a counted repetition. e.g., `a{b}`.
    InvalidBase10(String),
    /// An invalid hexadecimal number was used in an escape sequence.
    /// e.g., `\xAG`.
    InvalidBase16(String),
    /// An invalid capture name was used. e.g., `(?P<0a>b)`.
    InvalidCaptureName(String),
    /// An invalid class range was givien. Specifically, when the start of the
    /// range is greater than the end. e.g., `[z-a]`.
    InvalidClassRange {
        /// The first character specified in the range.
        start: char,
        /// The second character specified in the range.
        end: char,
    },
    /// An escape sequence was used in a character class where it is not
    /// allowed. e.g., `[a-\pN]` or `[\A]`.
    InvalidClassEscape(Expr),
    /// An invalid counted repetition min/max was given. e.g., `a{2,1}`.
    InvalidRepeatRange {
        /// The first number specified in the repetition.
        min: u32,
        /// The second number specified in the repetition.
        max: u32,
    },
    /// An invalid Unicode scalar value was used in a long hexadecimal
    /// sequence. e.g., `\x{D800}`.
    InvalidScalarValue(u32),
    /// An empty counted repetition operator. e.g., `a{}`.
    MissingBase10,
    /// A repetition operator was not applied to an expression. e.g., `*`.
    RepeaterExpectsExpr,
    /// A repetition operator was applied to an expression that cannot be
    /// repeated. e.g., `a+*` or `a|*`.
    RepeaterUnexpectedExpr(Expr),
    /// A capture group name that is never closed. e.g., `(?P<a`.
    UnclosedCaptureName(String),
    /// An unclosed hexadecimal literal. e.g., `\x{a`.
    UnclosedHex,
    /// An unclosed parenthesis. e.g., `(a`.
    UnclosedParen,
    /// An unclosed counted repetition operator. e.g., `a{2`.
    UnclosedRepeat,
    /// An unclosed named Unicode class. e.g., `\p{Yi`.
    UnclosedUnicodeName,
    /// Saw end of regex before class was closed. e.g., `[a`.
    UnexpectedClassEof,
    /// Saw end of regex before escape sequence was closed. e.g., `\`.
    UnexpectedEscapeEof,
    /// Saw end of regex before flags were closed. e.g., `(?i`.
    UnexpectedFlagEof,
    /// Saw end of regex before two hexadecimal digits were seen. e.g., `\xA`.
    UnexpectedTwoDigitHexEof,
    /// Unopened parenthesis. e.g., `)`.
    UnopenedParen,
    /// Unrecognized escape sequence. e.g., `\q`.
    UnrecognizedEscape(char),
    /// Unrecognized flag. e.g., `(?a)`.
    UnrecognizedFlag(char),
    /// Unrecognized named Unicode class. e.g., `\p{Foo}`.
    UnrecognizedUnicodeClass(String),
    /// Hints that destructuring should not be exhaustive.
    ///
    /// This enum may grow additional variants, so this makes sure clients
    /// don't count on exhaustive matching. (Otherwise, adding a new variant
    /// could break existing code.)
    #[doc(hidden)]
    __Nonexhaustive,
}

impl Error {
    /// Returns an approximate *character* offset at which the error occurred.
    ///
    /// The character offset may be equal to the number of characters in the
    /// string, in which case it should be interpreted as pointing to the end
    /// of the regex.
    pub fn position(&self) -> usize {
        self.pos
    }

    /// Returns the type of the regex parse error.
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

impl ErrorKind {
    fn description(&self) -> &str {
        use ErrorKind::*;
        match *self {
            DoubleFlagNegation => "double flag negation",
            DuplicateCaptureName(_) => "duplicate capture name",
            EmptyAlternate => "empty alternate",
            EmptyCaptureName => "empty capture name",
            EmptyFlagNegation => "flag negation without any flags",
            EmptyGroup => "empty group (e.g., '()')",
            InvalidBase10(_) => "invalid base 10 number",
            InvalidBase16(_) => "invalid base 16 number",
            InvalidCaptureName(_) => "invalid capture name",
            InvalidClassRange{..} => "invalid character class range",
            InvalidClassEscape(_) => "invalid escape sequence in class",
            InvalidRepeatRange{..} => "invalid counted repetition range",
            InvalidScalarValue(_) => "invalid Unicode scalar value",
            MissingBase10 => "missing count in repetition operator",
            RepeaterExpectsExpr => "repetition operator missing expression",
            RepeaterUnexpectedExpr(_) => "expression cannot be repeated",
            UnclosedCaptureName(_) => "unclosed capture group name",
            UnclosedHex => "unclosed hexadecimal literal",
            UnclosedParen => "unclosed parenthesis",
            UnclosedRepeat => "unclosed counted repetition operator",
            UnclosedUnicodeName => "unclosed Unicode class literal",
            UnexpectedClassEof => "unexpected EOF in character class",
            UnexpectedEscapeEof => "unexpected EOF in escape sequence",
            UnexpectedFlagEof => "unexpected EOF in flags",
            UnexpectedTwoDigitHexEof => "unexpected EOF in hex literal",
            UnopenedParen => "unopened parenthesis",
            UnrecognizedEscape(_) => "unrecognized escape sequence",
            UnrecognizedFlag(_) => "unrecognized flag",
            UnrecognizedUnicodeClass(_) => "unrecognized Unicode class name",
            __Nonexhaustive => unreachable!(),
        }
    }
}

impl ::std::error::Error for Error {
    fn description(&self) -> &str {
        self.kind.description()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Error parsing regex near '{}' at character offset {}: {}",
               self.surround, self.pos, self.kind)
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ErrorKind::*;
        match *self {
            DoubleFlagNegation =>
                write!(f, "Only one negation symbol is allowed in flags."),
            DuplicateCaptureName(ref s) =>
                write!(f, "Capture name '{}' is used more than once.", s),
            EmptyAlternate =>
                write!(f, "Alternations cannot be empty."),
            EmptyCaptureName =>
                write!(f, "Capture names cannot be empty."),
            EmptyFlagNegation =>
                write!(f, "Flag negation requires setting at least one flag."),
            EmptyGroup =>
                write!(f, "Empty regex groups (e.g., '()') are not allowed."),
            InvalidBase10(ref s) =>
                write!(f, "Not a valid base 10 number: '{}'", s),
            InvalidBase16(ref s) =>
                write!(f, "Not a valid base 16 number: '{}'", s),
            InvalidCaptureName(ref s) =>
                write!(f, "Invalid capture name: '{}'. Capture names must \
                           consist of [_a-zA-Z0-9] and are not allowed to \
                           start with with a number.", s),
            InvalidClassRange { start, end } =>
                write!(f, "Invalid character class range '{}-{}'. \
                           Character class ranges must start with the smaller \
                           character, but {} > {}", start, end, start, end),
            InvalidClassEscape(ref e) =>
                write!(f, "Invalid escape sequence in character \
                           class: '{}'.", e),
            InvalidRepeatRange { min, max } =>
                write!(f, "Invalid counted repetition range: {{{}, {}}}. \
                           Counted repetition ranges must start with the \
                           minimum, but {} > {}", min, max, min, max),
            InvalidScalarValue(c) =>
                write!(f, "Number does not correspond to a Unicode scalar \
                           value: '{}'.", c),
            MissingBase10 =>
                write!(f, "Missing maximum in counted reptition operator."),
            RepeaterExpectsExpr =>
                write!(f, "Missing expression for reptition operator."),
            RepeaterUnexpectedExpr(ref e) =>
                write!(f, "Invalid application of reptition operator to: \
                          '{}'.", e),
            UnclosedCaptureName(ref s) =>
                write!(f, "Capture name group for '{}' is not closed. \
                           (Missing a '>'.)", s),
            UnclosedHex =>
                write!(f, "Unclosed hexadecimal literal (missing a '}}')."),
            UnclosedParen =>
                write!(f, "Unclosed parenthesis."),
            UnclosedRepeat =>
                write!(f, "Unclosed counted repetition (missing a '}}')."),
            UnclosedUnicodeName =>
                write!(f, "Unclosed Unicode literal (missing a '}}')."),
            UnexpectedClassEof =>
                write!(f, "Character class was not closed before the end of \
                           the regex (missing a ']')."),
            UnexpectedEscapeEof =>
                write!(f, "Started an escape sequence that didn't finish \
                           before the end of the regex."),
            UnexpectedFlagEof =>
                write!(f, "Inline flag settings was not closed before the end \
                           of the regex (missing a ')' or ':')."),
            UnexpectedTwoDigitHexEof =>
                write!(f, "Unexpected end of two digit hexadecimal literal."),
            UnopenedParen =>
                write!(f, "Unopened parenthesis."),
            UnrecognizedEscape(c) =>
                write!(f, "Unrecognized escape sequence: '\\{}'.", c),
            UnrecognizedFlag(c) =>
                write!(f, "Unrecognized flag: '{}'. \
                           (Allowed flags: i, s, m, U, x.)", c),
            UnrecognizedUnicodeClass(ref s) =>
                write!(f, "Unrecognized Unicode class name: '{}'.", s),
            __Nonexhaustive => unreachable!(),
        }
    }
}

/// Returns the Unicode *simple* case folding of `c`.
///
/// N.B. This is hidden because it really isn't the responsibility of this
/// crate to do simple case folding. One hopes that either another crate or
/// the standard library will be able to do this for us. In any case, we still
/// expose it because it is used inside the various Regex engines.
#[doc(hidden)]
pub fn simple_case_fold(c: char) -> char {
    simple_case_fold_result(c)
        .map(|i| case_folding::C_plus_S_table[i].1)
        .unwrap_or(c)
}

/// The result of binary search on the simple case folding table.
///
/// This level of detail is exposed so that we can do case folding on a
/// range of characters efficiently.
fn simple_case_fold_result(c: char) -> ::std::result::Result<usize, usize> {
    case_folding::C_plus_S_table.binary_search_by(|&(x, _)| x.cmp(&c))
}

/// Escapes all regular expression meta characters in `text`.
///
/// The string returned may be safely used as a literal in a regular
/// expression.
pub fn quote(text: &str) -> String {
    let mut quoted = String::with_capacity(text.len());
    for c in text.chars() {
        if parser::is_punct(c) {
            quoted.push('\\');
        }
        quoted.push(c);
    }
    quoted
}

fn quote_char(c: char) -> String {
    let mut s = String::new();
    if parser::is_punct(c) {
        s.push('\\');
    }
    s.push(c);
    s
}

fn inc_char(c: char) -> char {
    match c {
        char::MAX => char::MAX,
        '\u{D7FF}' => '\u{E000}',
        c => char::from_u32(c as u32 + 1).unwrap(),
    }
}

fn dec_char(c: char) -> char {
    match c {
        '\x00' => '\x00',
        '\u{E000}' => '\u{D7FF}',
        c => char::from_u32(c as u32 - 1).unwrap(),
    }
}

/// Returns true if and only if `c` is a word character.
#[doc(hidden)]
pub fn is_word_char(c: char) -> bool {
    match c {
        '_' | '0' ... '9' | 'a' ... 'z' | 'A' ... 'Z'  => true,
        _ => ::unicode::regex::PERLW.binary_search_by(|&(start, end)| {
            if c >= start && c <= end {
                Ordering::Equal
            } else if start > c {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        }).is_ok(),
    }
}

#[cfg(test)]
mod properties;

#[cfg(test)]
mod tests {
    use {CharClass, ClassRange};

    fn class(ranges: &[(char, char)]) -> CharClass {
        let ranges = ranges.iter().cloned()
                           .map(|(c1, c2)| ClassRange::new(c1, c2)).collect();
        CharClass::new(ranges)
    }

    fn classi(ranges: &[(char, char)]) -> CharClass {
        let mut cls = class(ranges);
        cls.casei = true;
        cls
    }

    #[test]
    fn class_canon_no_change() {
        let cls = class(&[('a', 'c'), ('x', 'z')]);
        assert_eq!(cls.clone().canonicalize(), cls);
    }

    #[test]
    fn class_canon_unordered() {
        let cls = class(&[('x', 'z'), ('a', 'c')]);
        assert_eq!(cls.canonicalize(), class(&[
            ('a', 'c'), ('x', 'z'),
        ]));
    }

    #[test]
    fn class_canon_overlap() {
        let cls = class(&[('x', 'z'), ('w', 'y')]);
        assert_eq!(cls.canonicalize(), class(&[
            ('w', 'z'),
        ]));
    }

    #[test]
    fn class_canon_overlap_many() {
        let cls = class(&[
            ('c', 'f'), ('a', 'g'), ('d', 'j'), ('a', 'c'),
            ('m', 'p'), ('l', 's'),
        ]);
        assert_eq!(cls.clone().canonicalize(), class(&[
            ('a', 'j'), ('l', 's'),
        ]));
    }

    #[test]
    fn class_canon_overlap_many_case_fold() {
        let cls = class(&[
            ('C', 'F'), ('A', 'G'), ('D', 'J'), ('A', 'C'),
            ('M', 'P'), ('L', 'S'), ('c', 'f'),
        ]);
        assert_eq!(cls.case_fold(), classi(&[
            ('a', 'j'), ('l', 's'),
        ]));
    }

    #[test]
    fn class_canon_overlap_boundary() {
        let cls = class(&[('x', 'z'), ('u', 'w')]);
        assert_eq!(cls.canonicalize(), class(&[
            ('u', 'z'),
        ]));
    }

    #[test]
    fn class_canon_extreme_edge_case() {
        let cls = class(&[('\x00', '\u{10FFFF}'), ('\x00', '\u{10FFFF}')]);
        assert_eq!(cls.canonicalize(), class(&[
            ('\x00', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_canon_singles() {
        let cls = class(&[('a', 'a'), ('b', 'b')]);
        assert_eq!(cls.canonicalize(), class(&[('a', 'b')]));
    }

    #[test]
    fn class_negate_single() {
        let cls = class(&[('a', 'a')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\x60'), ('\x62', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_singles() {
        let cls = class(&[('a', 'a'), ('b', 'b')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\x60'), ('\x63', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_multiples() {
        let cls = class(&[('a', 'c'), ('x', 'z')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\x60'), ('\x64', '\x77'), ('\x7b', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_min_scalar() {
        let cls = class(&[('\x00', 'a')]);
        assert_eq!(cls.negate(), class(&[
            ('\x62', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_max_scalar() {
        let cls = class(&[('a', '\u{10FFFF}')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\x60'),
        ]));
    }

    #[test]
    fn class_negate_everything() {
        let cls = class(&[('\x00', '\u{10FFFF}')]);
        assert_eq!(cls.negate(), class(&[]));
    }

    #[test]
    fn class_negate_everything_sans_one() {
        let cls = class(&[
            ('\x00', '\u{10FFFD}'), ('\u{10FFFF}', '\u{10FFFF}')
        ]);
        assert_eq!(cls.negate(), class(&[
            ('\u{10FFFE}', '\u{10FFFE}'),
        ]));
    }

    #[test]
    fn class_negate_surrogates_min() {
        let cls = class(&[('\x00', '\u{D7FF}')]);
        assert_eq!(cls.negate(), class(&[
            ('\u{E000}', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_surrogates_min_edge() {
        let cls = class(&[('\x00', '\u{D7FE}')]);
        assert_eq!(cls.negate(), class(&[
            ('\u{D7FF}', '\u{10FFFF}'),
        ]));
    }

    #[test]
    fn class_negate_surrogates_max() {
        let cls = class(&[('\u{E000}', '\u{10FFFF}')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\u{D7FF}'),
        ]));
    }

    #[test]
    fn class_negate_surrogates_max_edge() {
        let cls = class(&[('\u{E001}', '\u{10FFFF}')]);
        assert_eq!(cls.negate(), class(&[
            ('\x00', '\u{E000}'),
        ]));
    }

    #[test]
    fn class_fold_retain_only_needed() {
        let cls = class(&[('A', 'Z'), ('a', 'z')]);
        assert_eq!(cls.case_fold(), classi(&[
            ('a', 'z'),
        ]));
    }

    #[test]
    fn class_fold_az() {
        let cls = class(&[('A', 'Z')]);
        assert_eq!(cls.case_fold(), classi(&[
            ('a', 'z'),
        ]));
    }

    #[test]
    fn class_fold_a_underscore() {
        let cls = class(&[('A', 'A'), ('_', '_')]);
        assert_eq!(cls.clone().canonicalize(), class(&[
            ('A', 'A'), ('_', '_'),
        ]));
        assert_eq!(cls.case_fold(), classi(&[
            ('_', '_'), ('a', 'a'),
        ]));
    }

    #[test]
    fn class_fold_a_equals() {
        let cls = class(&[('A', 'A'), ('=', '=')]);
        assert_eq!(cls.clone().canonicalize(), class(&[
            ('=', '='), ('A', 'A'),
        ]));
        assert_eq!(cls.case_fold(), classi(&[
            ('=', '='), ('a', 'a'),
        ]));
    }

    #[test]
    fn class_fold_no_folding_needed() {
        let cls = class(&[('\x00', '\x10')]);
        assert_eq!(cls.case_fold(), classi(&[
            ('\x00', '\x10'),
        ]));
    }
}
