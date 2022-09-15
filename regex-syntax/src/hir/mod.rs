/*!
Defines a high-level intermediate representation for regular expressions.
*/

use core::{char, cmp};

use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use crate::{
    ast::Span,
    hir::interval::{Interval, IntervalSet, IntervalSetIter},
    unicode,
};

pub use crate::{
    hir::visitor::{visit, Visitor},
    unicode::CaseFoldError,
};

mod interval;
pub mod literal;
pub mod print;
pub mod translate;
mod visitor;

/// An error that can occur while translating an `Ast` to a `Hir`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Error {
    /// The kind of error.
    kind: ErrorKind,
    /// The original pattern that the translator's Ast was parsed from. Every
    /// span in an error is a valid range into this string.
    pattern: String,
    /// The span of this error, derived from the Ast given to the translator.
    span: Span,
}

impl Error {
    /// Return the type of this error.
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// The original pattern string in which this error occurred.
    ///
    /// Every span reported by this error is reported in terms of this string.
    pub fn pattern(&self) -> &str {
        &self.pattern
    }

    /// Return the span at which this error occurred.
    pub fn span(&self) -> &Span {
        &self.span
    }
}

/// The type of an error that occurred while building an `Hir`.
///
/// This error type is marked as `non_exhaustive`. This means that adding a
/// new variant is not considered a breaking change.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    /// This error occurs when a Unicode feature is used when Unicode
    /// support is disabled. For example `(?-u:\pL)` would trigger this error.
    UnicodeNotAllowed,
    /// This error occurs when translating a pattern that could match a byte
    /// sequence that isn't UTF-8 and `allow_invalid_utf8` was disabled.
    InvalidUtf8,
    /// This occurs when an unrecognized Unicode property name could not
    /// be found.
    UnicodePropertyNotFound,
    /// This occurs when an unrecognized Unicode property value could not
    /// be found.
    UnicodePropertyValueNotFound,
    /// This occurs when a Unicode-aware Perl character class (`\w`, `\s` or
    /// `\d`) could not be found. This can occur when the `unicode-perl`
    /// crate feature is not enabled.
    UnicodePerlClassNotFound,
    /// This occurs when the Unicode simple case mapping tables are not
    /// available, and the regular expression required Unicode aware case
    /// insensitivity.
    UnicodeCaseUnavailable,
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        crate::error::Formatter::from(self).fmt(f)
    }
}

impl core::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use self::ErrorKind::*;

        let msg = match *self {
            UnicodeNotAllowed => "Unicode not allowed here",
            InvalidUtf8 => "pattern can match invalid UTF-8",
            UnicodePropertyNotFound => "Unicode property not found",
            UnicodePropertyValueNotFound => "Unicode property value not found",
            UnicodePerlClassNotFound => {
                "Unicode-aware Perl class not found \
                 (make sure the unicode-perl feature is enabled)"
            }
            UnicodeCaseUnavailable => {
                "Unicode-aware case insensitivity matching is not available \
                 (make sure the unicode-case feature is enabled)"
            }
        };
        f.write_str(msg)
    }
}

/// A high-level intermediate representation (HIR) for a regular expression.
///
/// The HIR of a regular expression represents an intermediate step between its
/// abstract syntax (a structured description of the concrete syntax) and
/// compiled byte codes. The purpose of HIR is to make regular expressions
/// easier to analyze. In particular, the AST is much more complex than the
/// HIR. For example, while an AST supports arbitrarily nested character
/// classes, the HIR will flatten all nested classes into a single set. The HIR
/// will also "compile away" every flag present in the concrete syntax. For
/// example, users of HIR expressions never need to worry about case folding;
/// it is handled automatically by the translator (e.g., by translating `(?i)A`
/// to `[aA]`).
///
/// If the HIR was produced by a translator that disallows invalid UTF-8, then
/// the HIR is guaranteed to match UTF-8 exclusively.
///
/// This type defines its own destructor that uses constant stack space and
/// heap space proportional to the size of the HIR.
///
/// The specific type of an HIR expression can be accessed via its `kind`
/// or `into_kind` methods. This extra level of indirection exists for two
/// reasons:
///
/// 1. Construction of an HIR expression *must* use the constructor methods
///    on this `Hir` type instead of building the `HirKind` values directly.
///    This permits construction to enforce invariants like "concatenations
///    always consist of two or more sub-expressions."
/// 2. Every HIR expression contains attributes that are defined inductively,
///    and can be computed cheaply during the construction process. For
///    example, one such attribute is whether the expression must match at the
///    beginning of the text.
///
/// Also, an `Hir`'s `fmt::Display` implementation prints an HIR as a regular
/// expression pattern string, and uses constant stack space and heap space
/// proportional to the size of the `Hir`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Hir {
    /// The underlying HIR kind.
    kind: HirKind,
    /// Analysis info about this HIR, computed during construction.
    props: Properties,
}

/// The kind of an arbitrary `Hir` expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HirKind {
    /// The empty regular expression, which matches everything, including the
    /// empty string.
    Empty,
    /// A literalstring that matches exactly these bytes.
    Literal(Literal),
    /// A single character class that matches any of the characters in the
    /// class. A class can either consist of Unicode scalar values as
    /// characters, or it can use bytes.
    Class(Class),
    /// A look-around assertion. A look-around match always has zero length.
    Look(Look),
    /// A repetition operation applied to a child expression.
    Repetition(Repetition),
    /// A possibly capturing group, which contains a child expression.
    Group(Group),
    /// A concatenation of expressions. A concatenation always has at least two
    /// child expressions.
    ///
    /// A concatenation matches only if each of its child expression matches
    /// one after the other.
    Concat(Vec<Hir>),
    /// An alternation of expressions. An alternation always has at least two
    /// child expressions.
    ///
    /// An alternation matches only if at least one of its child expression
    /// matches. If multiple expressions match, then the leftmost is preferred.
    Alternation(Vec<Hir>),
}

/// Methods for accessing the underlying `HirKind` and `Properties`.
impl Hir {
    /// Returns a reference to the underlying HIR kind.
    pub fn kind(&self) -> &HirKind {
        &self.kind
    }

    /// Consumes ownership of this HIR expression and returns its underlying
    /// `HirKind`.
    pub fn into_kind(mut self) -> HirKind {
        core::mem::replace(&mut self.kind, HirKind::Empty)
    }

    /// Returns the properties computed for this `Hir`.
    pub fn properties(&self) -> &Properties {
        &self.props
    }
}

/// Smart constructors for HIR values.
///
/// These constructors are called "smart" because they inductive work or
/// simplifications. For example, calling `Hir::repetition` with a repetition
/// like `a{0}` will actually return a `Hir` with a `HirKind::Empty` kind
/// since it is equivalent to an empty regex. Another example is calling
/// `Hir::concat(vec![expr])`. Instead of getting a `HirKind::Concat`, you'll
/// just get back the original `expr` since it's precisely equivalent.
///
/// Smart constructors enable maintaining invariants about the HIR data type
/// while also simulanteously keeping the representation as simple as possible.
impl Hir {
    /// Returns an empty HIR expression.
    ///
    /// An empty HIR expression always matches, including the empty string.
    pub fn empty() -> Hir {
        let props = Properties::empty();
        Hir { kind: HirKind::Empty, props }
    }

    /// Creates a literal HIR expression.
    ///
    /// If the given literal has a `Byte` variant with an ASCII byte, then this
    /// method panics. This enforces the invariant that `Byte` variants are
    /// only used to express matching of invalid UTF-8.
    pub fn literal<B: Into<Box<[u8]>>>(lit: B) -> Hir {
        let bytes = lit.into();
        if bytes.is_empty() {
            return Hir::empty();
        }

        let lit = Literal(bytes);
        let props = Properties::literal(&lit);
        Hir { kind: HirKind::Literal(lit), props }
    }

    /// Creates a class HIR expression.
    pub fn class(class: Class) -> Hir {
        let props = Properties::class(&class);
        Hir { kind: HirKind::Class(class), props }
    }

    /// Creates a look-around assertion HIR expression.
    pub fn look(look: Look) -> Hir {
        let props = Properties::look(look);
        Hir { kind: HirKind::Look(look), props }
    }

    /// Creates a repetition HIR expression.
    pub fn repetition(rep: Repetition) -> Hir {
        // The regex 'a{0}' is always equivalent to the empty regex. This is
        // true even when 'a' is an expression that never matches anything
        // (like '\P{any}').
        if rep.min == 0 && rep.max == Some(0) {
            return Hir::empty();
        }
        let props = Properties::repetition(&rep);
        Hir { kind: HirKind::Repetition(rep), props }
    }

    /// Creates a group HIR expression.
    pub fn group(group: Group) -> Hir {
        let props = Properties::group(&group);
        Hir { kind: HirKind::Group(group), props }
    }

    /// Returns the concatenation of the given expressions.
    ///
    /// This flattens the concatenation as appropriate.
    pub fn concat(mut exprs: Vec<Hir>) -> Hir {
        match exprs.len() {
            0 => Hir::empty(),
            1 => exprs.pop().unwrap(),
            _ => {
                let props = Properties::concat(&exprs);
                Hir { kind: HirKind::Concat(exprs), props }
            }
        }
    }

    /// Returns the alternation of the given expressions.
    ///
    /// This flattens the alternation as appropriate.
    pub fn alternation(mut exprs: Vec<Hir>) -> Hir {
        match exprs.len() {
            0 => Hir::empty(),
            1 => exprs.pop().unwrap(),
            _ => {
                let props = Properties::alternation(&exprs);
                Hir { kind: HirKind::Alternation(exprs), props }
            }
        }
    }

    /// Build an HIR expression for `.`.
    ///
    /// A `.` expression matches any character except for a newline terminator.
    /// To build an expression that matches any character, including newline
    /// terminators, use the `any_char` method.
    pub fn dot_char() -> Hir {
        let mut cls = ClassUnicode::empty();
        cls.push(ClassUnicodeRange::new('\0', '\x09'));
        cls.push(ClassUnicodeRange::new('\x0B', '\u{10FFFF}'));
        Hir::class(Class::Unicode(cls))
    }

    /// Build an HIR expression for `(?-u:.)`.
    ///
    /// A non-Unicode `.` expression matches any byte except for a newline
    /// terminator. To build an expression that matches any byte, including
    /// newline terminators, use the `any_byte` method.
    pub fn dot_byte() -> Hir {
        let mut cls = ClassBytes::empty();
        cls.push(ClassBytesRange::new(b'\0', b'\x09'));
        cls.push(ClassBytesRange::new(b'\x0B', b'\xFF'));
        Hir::class(Class::Bytes(cls))
    }

    /// Build an HIR expression for `(?s:.)`.
    ///
    /// A `(?s:.)` expression matches any character, including newline
    /// terminators. To build an expression that matches any character except
    /// for newline terminators, use the `dot_char` method.
    ///
    /// Note that `(?s:)` is equivalent to `\p{any}`.
    pub fn any_char() -> Hir {
        let mut cls = ClassUnicode::empty();
        cls.push(ClassUnicodeRange::new('\0', '\u{10FFFF}'));
        Hir::class(Class::Unicode(cls))
    }

    /// Build an HIR expression for `(?s-u:.)`.
    ///
    /// A `(?s-u:.)` expression matches any byte, including newline terminators.
    /// To build an expression that matches any byte except for newline
    /// terminators, use the `dot_byte` method.
    ///
    /// Note that `(?s-u:.)` is equivalent to `(?-u:[\x00-\xFF])`.
    pub fn any_byte() -> Hir {
        let mut cls = ClassBytes::empty();
        cls.push(ClassBytesRange::new(b'\0', b'\xFF'));
        Hir::class(Class::Bytes(cls))
    }
}

impl HirKind {
    /// Return true if and only if this HIR is the empty regular expression.
    ///
    /// Note that this is not defined inductively. That is, it only tests if
    /// this kind is the `Empty` variant. To get the inductive definition, use
    /// the `is_match_empty` method on [`Hir`].
    pub fn is_empty(&self) -> bool {
        match *self {
            HirKind::Empty => true,
            _ => false,
        }
    }

    /// Returns true if and only if this kind has any (including possibly
    /// empty) subexpressions.
    pub fn has_subexprs(&self) -> bool {
        match *self {
            HirKind::Empty
            | HirKind::Literal(_)
            | HirKind::Class(_)
            | HirKind::Look(_) => false,
            HirKind::Group(_)
            | HirKind::Repetition(_)
            | HirKind::Concat(_)
            | HirKind::Alternation(_) => true,
        }
    }
}

/// Print a display representation of this Hir.
///
/// The result of this is a valid regular expression pattern string.
///
/// This implementation uses constant stack space and heap space proportional
/// to the size of the `Hir`.
impl core::fmt::Display for Hir {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        crate::hir::print::Printer::new().print(self, f)
    }
}

/// The high-level intermediate representation of a literal.
///
/// A literal corresponds to a single character, where a character is either
/// defined by a Unicode scalar value or an arbitrary byte. Unicode characters
/// are preferred whenever possible. In particular, a `Byte` variant is only
/// ever produced when it could match invalid UTF-8.
#[derive(Clone, Eq, PartialEq)]
pub struct Literal(pub Box<[u8]>);

impl core::fmt::Debug for Literal {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        crate::debug::Bytes(&self.0).fmt(f)
    }
}

/// The high-level intermediate representation of a character class.
///
/// A character class corresponds to a set of characters. A character is either
/// defined by a Unicode scalar value or a byte. Unicode characters are used
/// by default, while bytes are used when Unicode mode (via the `u` flag) is
/// disabled.
///
/// A character class, regardless of its character type, is represented by a
/// sequence of non-overlapping non-adjacent ranges of characters.
///
/// Note that `Bytes` variant may be produced even when it exclusively matches
/// valid UTF-8. This is because a `Bytes` variant represents an intention by
/// the author of the regular expression to disable Unicode mode, which in turn
/// impacts the semantics of case insensitive matching. For example, `(?i)k`
/// and `(?i-u)k` will not match the same set of strings.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Class {
    /// A set of characters represented by Unicode scalar values.
    Unicode(ClassUnicode),
    /// A set of characters represented by arbitrary bytes (one byte per
    /// character).
    Bytes(ClassBytes),
}

impl Class {
    /// Apply Unicode simple case folding to this character class, in place.
    /// The character class will be expanded to include all simple case folded
    /// character variants.
    ///
    /// If this is a byte oriented character class, then this will be limited
    /// to the ASCII ranges `A-Z` and `a-z`.
    ///
    /// # Panics
    ///
    /// This routine panics when the case mapping data necessary for this
    /// routine to complete is unavailable. This occurs when the `unicode-case`
    /// feature is not enabled and the underlying class is Unicode oriented.
    ///
    /// Callers should prefer using `try_case_fold_simple` instead, which will
    /// return an error instead of panicking.
    pub fn case_fold_simple(&mut self) {
        match *self {
            Class::Unicode(ref mut x) => x.case_fold_simple(),
            Class::Bytes(ref mut x) => x.case_fold_simple(),
        }
    }

    /// Apply Unicode simple case folding to this character class, in place.
    /// The character class will be expanded to include all simple case folded
    /// character variants.
    ///
    /// If this is a byte oriented character class, then this will be limited
    /// to the ASCII ranges `A-Z` and `a-z`.
    ///
    /// # Error
    ///
    /// This routine returns an error when the case mapping data necessary
    /// for this routine to complete is unavailable. This occurs when the
    /// `unicode-case` feature is not enabled and the underlying class is
    /// Unicode oriented.
    pub fn try_case_fold_simple(
        &mut self,
    ) -> core::result::Result<(), CaseFoldError> {
        match *self {
            Class::Unicode(ref mut x) => x.try_case_fold_simple()?,
            Class::Bytes(ref mut x) => x.case_fold_simple(),
        }
        Ok(())
    }

    /// Negate this character class in place.
    ///
    /// After completion, this character class will contain precisely the
    /// characters that weren't previously in the class.
    pub fn negate(&mut self) {
        match *self {
            Class::Unicode(ref mut x) => x.negate(),
            Class::Bytes(ref mut x) => x.negate(),
        }
    }

    /// Returns true if and only if this character class will only ever match
    /// valid UTF-8.
    ///
    /// A character class can match invalid UTF-8 only when the following
    /// conditions are met:
    ///
    /// 1. The translator was configured to permit generating an expression
    ///    that can match invalid UTF-8. (By default, this is disabled.)
    /// 2. Unicode mode (via the `u` flag) was disabled either in the concrete
    ///    syntax or in the parser builder. By default, Unicode mode is
    ///    enabled.
    pub fn is_utf8(&self) -> bool {
        match *self {
            Class::Unicode(_) => true,
            Class::Bytes(ref x) => x.is_all_ascii(),
        }
    }

    /// Returns the length, in bytes, of the smallest string matched by this
    /// character class.
    ///
    /// For non-empty byte oriented classes, this always returns `1`. For
    /// non-empty Unicode oriented classes, this can return `1`, `2`, `3` or
    /// `4`. For empty classes, `None` is returned. It is impossible for `0` to
    /// be returned.
    pub fn minimum_len(&self) -> Option<usize> {
        match *self {
            Class::Unicode(ref x) => x.minimum_len(),
            Class::Bytes(ref x) => x.minimum_len(),
        }
    }

    /// Returns the length, in bytes, of the longest string matched by this
    /// character class.
    ///
    /// For non-empty byte oriented classes, this always returns `1`. For
    /// non-empty Unicode oriented classes, this can return `1`, `2`, `3` or
    /// `4`. For empty classes, `None` is returned. It is impossible for `0` to
    /// be returned.
    pub fn maximum_len(&self) -> Option<usize> {
        match *self {
            Class::Unicode(ref x) => x.maximum_len(),
            Class::Bytes(ref x) => x.maximum_len(),
        }
    }
}

/// A set of characters represented by Unicode scalar values.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassUnicode {
    set: IntervalSet<ClassUnicodeRange>,
}

impl ClassUnicode {
    /// Create a new class from a sequence of ranges.
    ///
    /// The given ranges do not need to be in any specific order, and ranges
    /// may overlap.
    pub fn new<I>(ranges: I) -> ClassUnicode
    where
        I: IntoIterator<Item = ClassUnicodeRange>,
    {
        ClassUnicode { set: IntervalSet::new(ranges) }
    }

    /// Create a new class with no ranges.
    pub fn empty() -> ClassUnicode {
        ClassUnicode::new(vec![])
    }

    /// Add a new range to this set.
    pub fn push(&mut self, range: ClassUnicodeRange) {
        self.set.push(range);
    }

    /// Return an iterator over all ranges in this class.
    ///
    /// The iterator yields ranges in ascending order.
    pub fn iter(&self) -> ClassUnicodeIter<'_> {
        ClassUnicodeIter(self.set.iter())
    }

    /// Return the underlying ranges as a slice.
    pub fn ranges(&self) -> &[ClassUnicodeRange] {
        self.set.intervals()
    }

    /// Expand this character class such that it contains all case folded
    /// characters, according to Unicode's "simple" mapping. For example, if
    /// this class consists of the range `a-z`, then applying case folding will
    /// result in the class containing both the ranges `a-z` and `A-Z`.
    ///
    /// # Panics
    ///
    /// This routine panics when the case mapping data necessary for this
    /// routine to complete is unavailable. This occurs when the `unicode-case`
    /// feature is not enabled.
    ///
    /// Callers should prefer using `try_case_fold_simple` instead, which will
    /// return an error instead of panicking.
    pub fn case_fold_simple(&mut self) {
        self.set
            .case_fold_simple()
            .expect("unicode-case feature must be enabled");
    }

    /// Expand this character class such that it contains all case folded
    /// characters, according to Unicode's "simple" mapping. For example, if
    /// this class consists of the range `a-z`, then applying case folding will
    /// result in the class containing both the ranges `a-z` and `A-Z`.
    ///
    /// # Error
    ///
    /// This routine returns an error when the case mapping data necessary
    /// for this routine to complete is unavailable. This occurs when the
    /// `unicode-case` feature is not enabled.
    pub fn try_case_fold_simple(
        &mut self,
    ) -> core::result::Result<(), CaseFoldError> {
        self.set.case_fold_simple()
    }

    /// Negate this character class.
    ///
    /// For all `c` where `c` is a Unicode scalar value, if `c` was in this
    /// set, then it will not be in this set after negation.
    pub fn negate(&mut self) {
        self.set.negate();
    }

    /// Union this character class with the given character class, in place.
    pub fn union(&mut self, other: &ClassUnicode) {
        self.set.union(&other.set);
    }

    /// Intersect this character class with the given character class, in
    /// place.
    pub fn intersect(&mut self, other: &ClassUnicode) {
        self.set.intersect(&other.set);
    }

    /// Subtract the given character class from this character class, in place.
    pub fn difference(&mut self, other: &ClassUnicode) {
        self.set.difference(&other.set);
    }

    /// Compute the symmetric difference of the given character classes, in
    /// place.
    ///
    /// This computes the symmetric difference of two character classes. This
    /// removes all elements in this class that are also in the given class,
    /// but all adds all elements from the given class that aren't in this
    /// class. That is, the class will contain all elements in either class,
    /// but will not contain any elements that are in both classes.
    pub fn symmetric_difference(&mut self, other: &ClassUnicode) {
        self.set.symmetric_difference(&other.set);
    }

    /// Returns true if and only if this character class will either match
    /// nothing or only ASCII bytes. Stated differently, this returns false
    /// if and only if this class contains a non-ASCII codepoint.
    pub fn is_all_ascii(&self) -> bool {
        self.set.intervals().last().map_or(true, |r| r.end <= '\x7F')
    }

    /// Returns the length, in bytes, of the smallest string matched by this
    /// character class.
    ///
    /// Returns `None` when the class is empty.
    pub fn minimum_len(&self) -> Option<usize> {
        let first = self.ranges().get(0)?;
        // Correct because c1 < c2 implies c1.len_utf8() < c2.len_utf8().
        Some(first.start.len_utf8())
    }

    /// Returns the length, in bytes, of the longest string matched by this
    /// character class.
    ///
    /// Returns `None` when the class is empty.
    pub fn maximum_len(&self) -> Option<usize> {
        let last = self.ranges().last()?;
        // Correct because c1 < c2 implies c1.len_utf8() < c2.len_utf8().
        Some(last.end.len_utf8())
    }
}

/// An iterator over all ranges in a Unicode character class.
///
/// The lifetime `'a` refers to the lifetime of the underlying class.
#[derive(Debug)]
pub struct ClassUnicodeIter<'a>(IntervalSetIter<'a, ClassUnicodeRange>);

impl<'a> Iterator for ClassUnicodeIter<'a> {
    type Item = &'a ClassUnicodeRange;

    fn next(&mut self) -> Option<&'a ClassUnicodeRange> {
        self.0.next()
    }
}

/// A single range of characters represented by Unicode scalar values.
///
/// The range is closed. That is, the start and end of the range are included
/// in the range.
#[derive(Clone, Copy, Default, Eq, PartialEq, PartialOrd, Ord)]
pub struct ClassUnicodeRange {
    start: char,
    end: char,
}

impl core::fmt::Debug for ClassUnicodeRange {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let start = if !self.start.is_whitespace() && !self.start.is_control()
        {
            self.start.to_string()
        } else {
            format!("0x{:X}", u32::from(self.start))
        };
        let end = if !self.end.is_whitespace() && !self.end.is_control() {
            self.end.to_string()
        } else {
            format!("0x{:X}", u32::from(self.end))
        };
        f.debug_struct("ClassUnicodeRange")
            .field("start", &start)
            .field("end", &end)
            .finish()
    }
}

impl Interval for ClassUnicodeRange {
    type Bound = char;

    #[inline]
    fn lower(&self) -> char {
        self.start
    }
    #[inline]
    fn upper(&self) -> char {
        self.end
    }
    #[inline]
    fn set_lower(&mut self, bound: char) {
        self.start = bound;
    }
    #[inline]
    fn set_upper(&mut self, bound: char) {
        self.end = bound;
    }

    /// Apply simple case folding to this Unicode scalar value range.
    ///
    /// Additional ranges are appended to the given vector. Canonical ordering
    /// is *not* maintained in the given vector.
    fn case_fold_simple(
        &self,
        ranges: &mut Vec<ClassUnicodeRange>,
    ) -> Result<(), unicode::CaseFoldError> {
        if !unicode::contains_simple_case_mapping(self.start, self.end)? {
            return Ok(());
        }
        let (start, end) = (u32::from(self.start), u32::from(self.end));
        let mut next_simple_cp = None;
        for cp in (start..=end).filter_map(char::from_u32) {
            if next_simple_cp.map_or(false, |next| cp < next) {
                continue;
            }
            let it = match unicode::simple_fold(cp)? {
                Ok(it) => it,
                Err(next) => {
                    next_simple_cp = next;
                    continue;
                }
            };
            for cp_folded in it {
                ranges.push(ClassUnicodeRange::new(cp_folded, cp_folded));
            }
        }
        Ok(())
    }
}

impl ClassUnicodeRange {
    /// Create a new Unicode scalar value range for a character class.
    ///
    /// The returned range is always in a canonical form. That is, the range
    /// returned always satisfies the invariant that `start <= end`.
    pub fn new(start: char, end: char) -> ClassUnicodeRange {
        ClassUnicodeRange::create(start, end)
    }

    /// Return the start of this range.
    ///
    /// The start of a range is always less than or equal to the end of the
    /// range.
    pub fn start(&self) -> char {
        self.start
    }

    /// Return the end of this range.
    ///
    /// The end of a range is always greater than or equal to the start of the
    /// range.
    pub fn end(&self) -> char {
        self.end
    }

    /// Returns the number of codepoints in this range.
    pub fn len(&self) -> usize {
        let diff = 1 + u32::from(self.end) - u32::from(self.start);
        // This is likely to panic in 16-bit targets since a usize can only fit
        // 2^16. It's not clear what to do here, other than to return an error
        // when building a Unicode class that contains a range whose length
        // overflows usize. (Which, to be honest, is probably quite common on
        // 16-bit targets. For example, this would imply that '.' and '\p{any}'
        // would be impossible to build.)
        usize::try_from(diff).expect("char class len fits in usize")
    }
}

/// A set of characters represented by arbitrary bytes (where one byte
/// corresponds to one character).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassBytes {
    set: IntervalSet<ClassBytesRange>,
}

impl ClassBytes {
    /// Create a new class from a sequence of ranges.
    ///
    /// The given ranges do not need to be in any specific order, and ranges
    /// may overlap.
    pub fn new<I>(ranges: I) -> ClassBytes
    where
        I: IntoIterator<Item = ClassBytesRange>,
    {
        ClassBytes { set: IntervalSet::new(ranges) }
    }

    /// Create a new class with no ranges.
    pub fn empty() -> ClassBytes {
        ClassBytes::new(vec![])
    }

    /// Add a new range to this set.
    pub fn push(&mut self, range: ClassBytesRange) {
        self.set.push(range);
    }

    /// Return an iterator over all ranges in this class.
    ///
    /// The iterator yields ranges in ascending order.
    pub fn iter(&self) -> ClassBytesIter<'_> {
        ClassBytesIter(self.set.iter())
    }

    /// Return the underlying ranges as a slice.
    pub fn ranges(&self) -> &[ClassBytesRange] {
        self.set.intervals()
    }

    /// Expand this character class such that it contains all case folded
    /// characters. For example, if this class consists of the range `a-z`,
    /// then applying case folding will result in the class containing both the
    /// ranges `a-z` and `A-Z`.
    ///
    /// Note that this only applies ASCII case folding, which is limited to the
    /// characters `a-z` and `A-Z`.
    pub fn case_fold_simple(&mut self) {
        self.set.case_fold_simple().expect("ASCII case folding never fails");
    }

    /// Negate this byte class.
    ///
    /// For all `b` where `b` is a any byte, if `b` was in this set, then it
    /// will not be in this set after negation.
    pub fn negate(&mut self) {
        self.set.negate();
    }

    /// Union this byte class with the given byte class, in place.
    pub fn union(&mut self, other: &ClassBytes) {
        self.set.union(&other.set);
    }

    /// Intersect this byte class with the given byte class, in place.
    pub fn intersect(&mut self, other: &ClassBytes) {
        self.set.intersect(&other.set);
    }

    /// Subtract the given byte class from this byte class, in place.
    pub fn difference(&mut self, other: &ClassBytes) {
        self.set.difference(&other.set);
    }

    /// Compute the symmetric difference of the given byte classes, in place.
    ///
    /// This computes the symmetric difference of two byte classes. This
    /// removes all elements in this class that are also in the given class,
    /// but all adds all elements from the given class that aren't in this
    /// class. That is, the class will contain all elements in either class,
    /// but will not contain any elements that are in both classes.
    pub fn symmetric_difference(&mut self, other: &ClassBytes) {
        self.set.symmetric_difference(&other.set);
    }

    /// Returns true if and only if this character class will either match
    /// nothing or only ASCII bytes. Stated differently, this returns false
    /// if and only if this class contains a non-ASCII byte.
    pub fn is_all_ascii(&self) -> bool {
        self.set.intervals().last().map_or(true, |r| r.end <= 0x7F)
    }

    /// Returns the length, in bytes, of the smallest string matched by this
    /// character class.
    ///
    /// Returns `None` when the class is empty.
    pub fn minimum_len(&self) -> Option<usize> {
        if self.ranges().is_empty() {
            None
        } else {
            Some(1)
        }
    }

    /// Returns the length, in bytes, of the longest string matched by this
    /// character class.
    ///
    /// Returns `None` when the class is empty.
    pub fn maximum_len(&self) -> Option<usize> {
        if self.ranges().is_empty() {
            None
        } else {
            Some(1)
        }
    }
}

/// An iterator over all ranges in a byte character class.
///
/// The lifetime `'a` refers to the lifetime of the underlying class.
#[derive(Debug)]
pub struct ClassBytesIter<'a>(IntervalSetIter<'a, ClassBytesRange>);

impl<'a> Iterator for ClassBytesIter<'a> {
    type Item = &'a ClassBytesRange;

    fn next(&mut self) -> Option<&'a ClassBytesRange> {
        self.0.next()
    }
}

/// A single range of characters represented by arbitrary bytes.
///
/// The range is closed. That is, the start and end of the range are included
/// in the range.
#[derive(Clone, Copy, Default, Eq, PartialEq, PartialOrd, Ord)]
pub struct ClassBytesRange {
    start: u8,
    end: u8,
}

impl Interval for ClassBytesRange {
    type Bound = u8;

    #[inline]
    fn lower(&self) -> u8 {
        self.start
    }
    #[inline]
    fn upper(&self) -> u8 {
        self.end
    }
    #[inline]
    fn set_lower(&mut self, bound: u8) {
        self.start = bound;
    }
    #[inline]
    fn set_upper(&mut self, bound: u8) {
        self.end = bound;
    }

    /// Apply simple case folding to this byte range. Only ASCII case mappings
    /// (for a-z) are applied.
    ///
    /// Additional ranges are appended to the given vector. Canonical ordering
    /// is *not* maintained in the given vector.
    fn case_fold_simple(
        &self,
        ranges: &mut Vec<ClassBytesRange>,
    ) -> Result<(), unicode::CaseFoldError> {
        if !ClassBytesRange::new(b'a', b'z').is_intersection_empty(self) {
            let lower = cmp::max(self.start, b'a');
            let upper = cmp::min(self.end, b'z');
            ranges.push(ClassBytesRange::new(lower - 32, upper - 32));
        }
        if !ClassBytesRange::new(b'A', b'Z').is_intersection_empty(self) {
            let lower = cmp::max(self.start, b'A');
            let upper = cmp::min(self.end, b'Z');
            ranges.push(ClassBytesRange::new(lower + 32, upper + 32));
        }
        Ok(())
    }
}

impl ClassBytesRange {
    /// Create a new byte range for a character class.
    ///
    /// The returned range is always in a canonical form. That is, the range
    /// returned always satisfies the invariant that `start <= end`.
    pub fn new(start: u8, end: u8) -> ClassBytesRange {
        ClassBytesRange::create(start, end)
    }

    /// Return the start of this range.
    ///
    /// The start of a range is always less than or equal to the end of the
    /// range.
    pub fn start(&self) -> u8 {
        self.start
    }

    /// Return the end of this range.
    ///
    /// The end of a range is always greater than or equal to the start of the
    /// range.
    pub fn end(&self) -> u8 {
        self.end
    }

    /// Returns the number of bytes in this range.
    pub fn len(&self) -> usize {
        usize::from(self.end.checked_sub(self.start).unwrap())
            .checked_add(1)
            .unwrap()
    }
}

impl core::fmt::Debug for ClassBytesRange {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ClassBytesRange")
            .field("start", &crate::debug::Byte(self.start))
            .field("end", &crate::debug::Byte(self.end))
            .finish()
    }
}

/// The high-level intermediate representation for a look-around assertion.
///
/// An assertion match is always zero-length. Also called an "empty match."
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Look {
    /// Match the beginning of text. Specifically, this matches at the starting
    /// position of the input.
    Start,
    /// Match the end of text. Specifically, this matches at the ending
    /// position of the input.
    End,
    /// Match the beginning of a line or the beginning of text. Specifically,
    /// this matches at the starting position of the input, or at the position
    /// immediately following a `\n` character.
    StartLF,
    /// Match the end of a line or the end of text. Specifically, this matches
    /// at the end position of the input, or at the position immediately
    /// preceding a `\n` character.
    EndLF,
    /// Match an ASCII-only word boundary. That is, this matches a position
    /// where the left adjacent character and right adjacent character
    /// correspond to a word and non-word or a non-word and word character.
    WordAscii,
    /// Match an ASCII-only negation of a word boundary.
    WordAsciiNegate,
    /// Match a Unicode-aware word boundary. That is, this matches a position
    /// where the left adjacent character and right adjacent character
    /// correspond to a word and non-word or a non-word and word character.
    WordUnicode,
    /// Match a Unicode-aware negation of a word boundary.
    WordUnicodeNegate,
}

impl Look {
    fn from_repr(repr: u8) -> Option<Look> {
        match repr {
            0 => Some(Look::Start),
            1 => Some(Look::End),
            2 => Some(Look::StartLF),
            3 => Some(Look::EndLF),
            4 => Some(Look::WordAscii),
            5 => Some(Look::WordAsciiNegate),
            6 => Some(Look::WordUnicode),
            7 => Some(Look::WordUnicodeNegate),
            _ => None,
        }
    }

    fn as_repr(&self) -> u8 {
        match *self {
            Look::Start => 0,
            Look::End => 1,
            Look::StartLF => 2,
            Look::EndLF => 3,
            Look::WordAscii => 4,
            Look::WordAsciiNegate => 5,
            Look::WordUnicode => 6,
            Look::WordUnicodeNegate => 7,
        }
    }

    fn as_char(self) -> char {
        match self {
            Look::Start => 'A',
            Look::End => 'z',
            Look::StartLF => '^',
            Look::EndLF => '$',
            Look::WordAscii => 'b',
            Look::WordAsciiNegate => 'B',
            Look::WordUnicode => '𝛃',
            Look::WordUnicodeNegate => '𝚩',
        }
    }
}

/// The high-level intermediate representation for a capturing group.
///
/// A capturing group always has an index and a child expression. It may
/// also have a name associated with it (e.g., `(?P<foo>\w)`), but it's not
/// necessary.
///
/// Note that there is no explicit representation of a non-capturing group
/// in a `Hir`. Instead, non-capturing grouping is handled automatically by
/// the recursive structure of the `Hir` itself.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Group {
    /// The capture index of the group.
    pub index: u32,
    /// The name of the group, if it exists.
    pub name: Option<Box<str>>,
    /// The expression inside the capturing group, which may be empty.
    pub hir: Box<Hir>,
}

/// The high-level intermediate representation of a repetition operator.
///
/// A repetition operator permits the repetition of an arbitrary
/// sub-expression.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Repetition {
    /// The minimum range of the repetition.
    ///
    /// Note that special cases like `?`, `+` and `*` all get translated into
    /// the ranges `{0,1}`, `{1,}` and `{0,}`, respectively.
    pub min: u32,
    /// The maximum range of the repetition.
    ///
    /// Note that when `max` is `None`, `min` acts as a lower bound but where
    /// there is no upper bound. For something like `x{5}` where the min and
    /// max are equivalent, `min` will be set to `5` and `max` will be set to
    /// `Some(5)`.
    pub max: Option<u32>,
    /// Whether this repetition operator is greedy or not. A greedy operator
    /// will match as much as it can. A non-greedy operator will match as
    /// little as it can.
    ///
    /// Typically, operators are greedy by default and are only non-greedy when
    /// a `?` suffix is used, e.g., `(expr)*` is greedy while `(expr)*?` is
    /// not. However, this can be inverted via the `U` "ungreedy" flag.
    pub greedy: bool,
    /// The expression being repeated.
    pub hir: Box<Hir>,
}

impl Repetition {
    /// Returns true if and only if this repetition operator makes it possible
    /// to match the empty string.
    ///
    /// Note that this is not defined inductively. For example, while `a*`
    /// will report `true`, `()+` will not, even though `()` matches the empty
    /// string and one or more occurrences of something that matches the
    /// empty string will always match the empty string. In order to get the
    /// inductive definition, see the corresponding method on [`Hir`].
    ///
    /// This returns true in precisely the cases that [`Repetition::min`]
    /// is equal to `0`.
    pub fn is_match_empty(&self) -> bool {
        self.min == 0
    }
}

/// A custom `Drop` impl is used for `HirKind` such that it uses constant stack
/// space but heap space proportional to the depth of the total `Hir`.
impl Drop for Hir {
    fn drop(&mut self) {
        use core::mem;

        match *self.kind() {
            HirKind::Empty
            | HirKind::Literal(_)
            | HirKind::Class(_)
            | HirKind::Look(_) => return,
            HirKind::Group(ref x) if !x.hir.kind.has_subexprs() => return,
            HirKind::Repetition(ref x) if !x.hir.kind.has_subexprs() => return,
            HirKind::Concat(ref x) if x.is_empty() => return,
            HirKind::Alternation(ref x) if x.is_empty() => return,
            _ => {}
        }

        let mut stack = vec![mem::replace(self, Hir::empty())];
        while let Some(mut expr) = stack.pop() {
            match expr.kind {
                HirKind::Empty
                | HirKind::Literal(_)
                | HirKind::Class(_)
                | HirKind::Look(_) => {}
                HirKind::Group(ref mut x) => {
                    stack.push(mem::replace(&mut x.hir, Hir::empty()));
                }
                HirKind::Repetition(ref mut x) => {
                    stack.push(mem::replace(&mut x.hir, Hir::empty()));
                }
                HirKind::Concat(ref mut x) => {
                    stack.extend(x.drain(..));
                }
                HirKind::Alternation(ref mut x) => {
                    stack.extend(x.drain(..));
                }
            }
        }
    }
}

/// A type that collects various properties of an HIR value.
///
/// Properties are always scalar values and represent meta data that is
/// computed inductively on an HIR value. Properties are defined for all
/// HIR values.
///
/// All methods on a `Properties` value take constant time.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Properties(Box<PropertiesI>);

/// The property definition. It is split out so that we can box it, and
/// there by make `Properties` use less stack size. This is kind-of important
/// because every HIR value has a `Properties` attached to it.
///
/// This does have the unfortunate consequence that creating any HIR value
/// always leads to at least one alloc for properties, but this is generally
/// true anyway (for pretty much all HirKinds except for look-arounds).
#[derive(Clone, Debug, Eq, PartialEq)]
struct PropertiesI {
    minimum_len: Option<usize>,
    maximum_len: Option<usize>,
    look_set: LookSet,
    look_set_prefix: LookSet,
    look_set_suffix: LookSet,
    utf8: bool,
    literal: bool,
    alternation_literal: bool,
}

impl Properties {
    /// Returns the length (in bytes) of the smallest string matched by this
    /// HIR.
    ///
    /// A return value of `0` is possible and occurs when the HIR can match an
    /// empty string.
    ///
    /// `None` is returned when there is no minimum length. This occurs in
    /// precisely the cases where the HIR matches nothing. i.e., The language
    /// the regex matches is empty. An example of such a regex is `\P{any}`.
    pub fn minimum_len(&self) -> Option<usize> {
        self.0.minimum_len
    }

    /// Returns the length (in bytes) of the longest string matched by this
    /// HIR.
    ///
    /// A return value of `0` is possible and occurs when nothing longer than
    /// the empty string is in the language described by this HIR.
    ///
    /// `None` is returned when there is no longest matching string. This
    /// occurs when the HIR matches nothing or when there is no upper bound
    /// on the length of matching strings. An example of such a regex is
    /// `\P{any}`.
    pub fn maximum_len(&self) -> Option<usize> {
        self.0.maximum_len
    }

    /// Returns a set of all look-around assertions that appear at least once
    /// in this HIR value.
    pub fn look_set(&self) -> LookSet {
        self.0.look_set
    }

    /// Returns a set of all look-around assertions that appear as a prefix for
    /// this HIR value. That is, the set returned corresponds to the set of
    /// assertions that must be passed before matching any bytes in a haystack.
    ///
    /// For example, `hir.look_set_prefix().contains(Look::Start)` returns true
    /// if and only if the HIR is fully anchored at the start.
    pub fn look_set_prefix(&self) -> LookSet {
        self.0.look_set_prefix
    }

    /// Returns a set of all look-around assertions that appear as a suffix for
    /// this HIR value. That is, the set returned corresponds to the set of
    /// assertions that must be passed in order to be considered a match after
    /// all other consuming HIR expressions.
    ///
    /// For example, `hir.look_set_suffix().contains(Look::End)` returns true
    /// if and only if the HIR is fully anchored at the end.
    pub fn look_set_suffix(&self) -> LookSet {
        self.0.look_set_suffix
    }

    /// Return true if and only if the corresponding HIR will always match
    /// valid UTF-8.
    ///
    /// When this returns false, then it is possible for this HIR expression to
    /// match invalid UTF-8.
    ///
    /// Note that this returns true even when the corresponding HIR can match
    /// the empty string. Since an empty string can technically appear between
    /// UTF-8 code units, it is possible for a match to be reported that splits
    /// a codepoint which could in turn be considered matching invalid UTF-8.
    /// However, it is generally assumed that such empty matches are handled
    /// specially by the search routine if it is absolutely required that
    /// matches not split a codepoint.
    pub fn is_utf8(&self) -> bool {
        self.0.utf8
    }

    /// Return true if and only if this HIR is a simple literal. This is only
    /// true when this HIR expression is either itself a `Literal` or a
    /// concatenation of only `Literal`s.
    ///
    /// For example, `f` and `foo` are literals, but `f+`, `(foo)`, `foo()`,
    /// `` are not (even though that contain sub-expressions that are literals).
    pub fn is_literal(&self) -> bool {
        self.0.literal
    }

    /// Return true if and only if this HIR is either a simple literal or an
    /// alternation of simple literals. This is only
    /// true when this HIR expression is either itself a `Literal` or a
    /// concatenation of only `Literal`s or an alternation of only `Literal`s.
    ///
    /// For example, `f`, `foo`, `a|b|c`, and `foo|bar|baz` are alternation
    /// literals, but `f+`, `(foo)`, `foo()`, ``
    /// are not (even though that contain sub-expressions that are literals).
    pub fn is_alternation_literal(&self) -> bool {
        self.0.alternation_literal
    }
}

impl Properties {
    /// Create a new set of HIR properties for an empty regex.
    fn empty() -> Properties {
        let inner = PropertiesI {
            minimum_len: Some(0),
            maximum_len: Some(0),
            look_set: LookSet::empty(),
            look_set_prefix: LookSet::empty(),
            look_set_suffix: LookSet::empty(),
            // It is debatable whether an empty regex always matches at valid
            // UTF-8 boundaries. Strictly speaking, at a byte oriented view,
            // it is clearly false. There are, for example, many empty strings
            // between the bytes encoding a '☃'.
            //
            // However, when Unicode mode is enabled, the fundamental atom
            // of matching is really a codepoint. And in that scenario, an
            // empty regex is defined to only match at valid UTF-8 boundaries
            // and to never split a codepoint. It just so happens that this
            // enforcement is somewhat tricky to do for regexes that match
            // the empty string inside regex engines themselves. It usually
            // requires some layer above the regex engine to filter out such
            // matches.
            //
            // In any case, 'true' is really the only coherent option. If it
            // were false, for example, then 'a*' would also need to be false
            // since it too can match the empty string.
            utf8: true,
            literal: false,
            alternation_literal: false,
        };
        Properties(Box::new(inner))
    }

    /// Create a new set of HIR properties for a literal regex.
    fn literal(lit: &Literal) -> Properties {
        let inner = PropertiesI {
            minimum_len: Some(lit.0.len()),
            maximum_len: Some(lit.0.len()),
            look_set: LookSet::empty(),
            look_set_prefix: LookSet::empty(),
            look_set_suffix: LookSet::empty(),
            utf8: core::str::from_utf8(&lit.0).is_ok(),
            literal: true,
            alternation_literal: true,
        };
        Properties(Box::new(inner))
    }

    /// Create a new set of HIR properties for a character class.
    fn class(class: &Class) -> Properties {
        let inner = PropertiesI {
            minimum_len: class.minimum_len(),
            maximum_len: class.maximum_len(),
            look_set: LookSet::empty(),
            look_set_prefix: LookSet::empty(),
            look_set_suffix: LookSet::empty(),
            utf8: class.is_utf8(),
            literal: false,
            alternation_literal: false,
        };
        Properties(Box::new(inner))
    }

    /// Create a new set of HIR properties for a look-around assertion.
    fn look(look: Look) -> Properties {
        use self::Look::*;

        let utf8 = match look {
            Start | End | StartLF | EndLF | WordAscii | WordUnicode
            | WordUnicodeNegate => true,
            // FIXME: Negated ASCII word boundaries can match invalid UTF-8.
            // But why is this 'false' when 'HirKind::Empty' is true? After
            // all, isn't WordAsciiNegate just a subset of HirKind::Empty? It
            // seems to me that if we handle HirKind::Empty correctly even when
            // it splits a codepoint, then we should be able to automatically
            // handle WordAsciiNegate correctly too...
            //
            // For now, this returns 'false' because that's what it did before.
            // But we should revisit this before the next release.
            WordAsciiNegate => false,
        };
        let inner = PropertiesI {
            minimum_len: Some(0),
            maximum_len: Some(0),
            look_set: LookSet::singleton(look),
            look_set_prefix: LookSet::singleton(look),
            look_set_suffix: LookSet::singleton(look),
            utf8,
            literal: false,
            alternation_literal: false,
        };
        Properties(Box::new(inner))
    }

    /// Create a new set of HIR properties for a repetition.
    fn repetition(rep: &Repetition) -> Properties {
        let minimum_len =
            rep.hir.properties().minimum_len().map(|child_min| {
                let rep_min = usize::try_from(rep.min).unwrap_or(usize::MAX);
                child_min.saturating_mul(rep_min)
            });
        let maximum_len = rep.max.and_then(|rep_max| {
            let rep_max = usize::try_from(rep_max).ok()?;
            let child_max = rep.hir.properties().maximum_len()?;
            child_max.checked_mul(rep_max)
        });

        let mut inner = PropertiesI {
            minimum_len,
            maximum_len,
            look_set: rep.hir.properties().look_set(),
            look_set_prefix: LookSet::empty(),
            look_set_suffix: LookSet::empty(),
            utf8: rep.hir.properties().is_utf8(),
            literal: false,
            alternation_literal: false,
        };
        if !rep.is_match_empty() {
            let child_props = rep.hir.properties();
            inner.look_set_prefix = child_props.look_set_prefix();
            inner.look_set_suffix = child_props.look_set_suffix();
        }
        Properties(Box::new(inner))
    }

    /// Create a new set of HIR properties for a group.
    fn group(group: &Group) -> Properties {
        // FIXME: Groups really should always have the same properties as
        // their child expressions. But the literal properties somewhat
        // over-constrained in what they represent in order to make downstream
        // analyses a bit more straight-forward.
        Properties(Box::new(PropertiesI {
            literal: false,
            alternation_literal: false,
            ..*group.hir.properties().0.clone()
        }))
    }

    /// Create a new set of HIR properties for a concatenation.
    fn concat(concat: &[Hir]) -> Properties {
        // The base case is an empty concatenation, which matches the empty
        // string. Note though that empty concatenations aren't possible,
        // because the Hir::concat smart constructor rewrites those as
        // Hir::empty.
        let mut props = PropertiesI {
            minimum_len: Some(0),
            maximum_len: Some(0),
            look_set: LookSet::empty(),
            look_set_prefix: LookSet::empty(),
            look_set_suffix: LookSet::empty(),
            utf8: true,
            literal: true,
            alternation_literal: true,
        };
        // Handle properties that need to visit every child hir.
        for x in concat.iter() {
            props.look_set.union(x.properties().look_set());
            props.utf8 = props.utf8 && x.properties().is_utf8();
            props.literal = props.literal && x.properties().is_literal();
            props.alternation_literal = props.alternation_literal
                && x.properties().is_alternation_literal();
            if let Some(ref mut minimum_len) = props.minimum_len {
                match x.properties().minimum_len() {
                    None => props.minimum_len = None,
                    Some(x) => *minimum_len += x,
                }
            }
            if let Some(ref mut maximum_len) = props.maximum_len {
                match x.properties().maximum_len() {
                    None => props.maximum_len = None,
                    Some(x) => *maximum_len += x,
                }
            }
        }
        // Handle the prefix properties, which only requires visiting
        // child exprs until one matches more than the empty string.
        let mut it = concat.iter();
        while let Some(x) = it.next() {
            props.look_set_prefix.union(x.properties().look_set_prefix());
            if x.properties().maximum_len().map_or(true, |x| x > 0) {
                break;
            }
        }
        // Same thing for the suffix properties, but in reverse.
        let mut it = concat.iter().rev();
        while let Some(x) = it.next() {
            props.look_set_suffix.union(x.properties().look_set_suffix());
            if x.properties().maximum_len().map_or(true, |x| x > 0) {
                break;
            }
        }
        Properties(Box::new(props))
    }

    /// Create a new set of HIR properties for a concatenation.
    fn alternation(alts: &[Hir]) -> Properties {
        // While empty alternations aren't possible, we still behave as if they
        // are. When we have an empty alternate, then clearly the look-around
        // prefix and suffix is empty. Otherwise, it is the intersection of all
        // prefixes and suffixes (respectively) of the branches.
        let fix =
            if alts.is_empty() { LookSet::empty() } else { LookSet::full() };
        // The base case is an empty alternation, which matches nothing.
        // Note though that empty alternations aren't possible, because the
        // Hir::alternation smart constructor rewrites those as empty character
        // classes.
        let mut props = PropertiesI {
            minimum_len: None,
            maximum_len: None,
            look_set: LookSet::empty(),
            look_set_prefix: fix,
            look_set_suffix: fix,
            utf8: true,
            literal: false,
            alternation_literal: true,
        };
        // Handle properties that need to visit every child hir.
        for x in alts.iter() {
            props.look_set.union(x.properties().look_set());
            props.look_set_prefix.intersect(x.properties().look_set_prefix());
            props.look_set_suffix.intersect(x.properties().look_set_suffix());
            props.utf8 = props.utf8 && x.properties().is_utf8();
            props.alternation_literal = props.alternation_literal
                && x.properties().is_alternation_literal();
            if let Some(xmin) = x.properties().minimum_len() {
                if props.minimum_len.map_or(true, |pmin| xmin < pmin) {
                    props.minimum_len = Some(xmin);
                }
            }
            if let Some(xmax) = x.properties().maximum_len() {
                if props.maximum_len.map_or(true, |pmax| xmax > pmax) {
                    props.maximum_len = Some(xmax);
                }
            }
        }
        Properties(Box::new(props))
    }
}

/// A set of look-around assertions.
///
/// This is useful for efficiently tracking look-around assertions. For
/// example, an [`Hir`] provides properties that return `LookSet`s.
#[derive(Clone, Copy, Default, Eq, PartialEq)]
pub struct LookSet {
    bits: u8,
}

impl LookSet {
    /// Create an empty set of look-around assertions.
    pub fn empty() -> LookSet {
        LookSet { bits: 0 }
    }

    /// Create a full set of look-around assertions.
    ///
    /// This set contains all possible look-around assertions.
    pub fn full() -> LookSet {
        LookSet { bits: !0 }
    }

    /// Create a look-around set containing the look-around assertion given.
    ///
    /// This is a convenience routine for creating an empty set and inserting
    /// one look-around assertions.
    pub fn singleton(look: Look) -> LookSet {
        let mut set = LookSet::empty();
        set.insert(look);
        set
    }

    /// Returns the total number of look-around assertions in this set.
    pub fn len(&self) -> usize {
        // OK because max value always fits in a u8, which in turn always
        // fits in a usize, regardless of target.
        usize::try_from(self.bits.count_ones()).unwrap()
    }

    /// Returns true if and only if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Insert the given look-around assertions into this set. If the assertion
    /// is already in the set, then this is a no-op.
    pub fn insert(&mut self, look: Look) {
        self.bits |= 1 << look.as_repr();
    }

    /// Remove the given look-around assertion from this set. If it wasn't
    /// previously in the set, then this is a no-op.
    pub fn remove(&mut self, look: Look) {
        self.bits &= !(1 << look.as_repr());
    }

    /// Returns true if and only if the given look-around assertion is in this
    /// set.
    pub fn contains(&self, look: Look) -> bool {
        self.bits & (1 << look.as_repr()) != 0
    }

    /// Modifies this set to be the union of itself and the set given.
    pub fn union(&mut self, other: LookSet) {
        self.bits |= other.bits;
    }

    /// Modifies this set to be the intersection of itself and the set given.
    pub fn intersect(&mut self, other: LookSet) {
        self.bits &= other.bits;
    }

    /// Returns an iterator over all of the look-around assertions in this set.
    #[inline]
    pub fn iter(self) -> LookSetIter {
        LookSetIter { set: self }
    }
}

impl core::fmt::Debug for LookSet {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.is_empty() {
            return write!(f, "∅");
        }
        for look in self.iter() {
            write!(f, "{}", look.as_char())?;
        }
        Ok(())
    }
}

/// An iterator over all look-around assertions in a [`LookSet`].
///
/// This iterator is created by [`LookSet::iter`].
#[derive(Clone, Debug)]
pub struct LookSetIter {
    set: LookSet,
}

impl Iterator for LookSetIter {
    type Item = Look;

    #[inline]
    fn next(&mut self) -> Option<Look> {
        // We'll never have more than u8::MAX distinct look-around assertions,
        // so 'repr' will always fit into a usize.
        let repr = u8::try_from(self.set.bits.trailing_zeros()).unwrap();
        let look = Look::from_repr(repr)?;
        self.set.remove(look);
        Some(look)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn uclass(ranges: &[(char, char)]) -> ClassUnicode {
        let ranges: Vec<ClassUnicodeRange> = ranges
            .iter()
            .map(|&(s, e)| ClassUnicodeRange::new(s, e))
            .collect();
        ClassUnicode::new(ranges)
    }

    fn bclass(ranges: &[(u8, u8)]) -> ClassBytes {
        let ranges: Vec<ClassBytesRange> =
            ranges.iter().map(|&(s, e)| ClassBytesRange::new(s, e)).collect();
        ClassBytes::new(ranges)
    }

    fn uranges(cls: &ClassUnicode) -> Vec<(char, char)> {
        cls.iter().map(|x| (x.start(), x.end())).collect()
    }

    #[cfg(feature = "unicode-case")]
    fn ucasefold(cls: &ClassUnicode) -> ClassUnicode {
        let mut cls_ = cls.clone();
        cls_.case_fold_simple();
        cls_
    }

    fn uunion(cls1: &ClassUnicode, cls2: &ClassUnicode) -> ClassUnicode {
        let mut cls_ = cls1.clone();
        cls_.union(cls2);
        cls_
    }

    fn uintersect(cls1: &ClassUnicode, cls2: &ClassUnicode) -> ClassUnicode {
        let mut cls_ = cls1.clone();
        cls_.intersect(cls2);
        cls_
    }

    fn udifference(cls1: &ClassUnicode, cls2: &ClassUnicode) -> ClassUnicode {
        let mut cls_ = cls1.clone();
        cls_.difference(cls2);
        cls_
    }

    fn usymdifference(
        cls1: &ClassUnicode,
        cls2: &ClassUnicode,
    ) -> ClassUnicode {
        let mut cls_ = cls1.clone();
        cls_.symmetric_difference(cls2);
        cls_
    }

    fn unegate(cls: &ClassUnicode) -> ClassUnicode {
        let mut cls_ = cls.clone();
        cls_.negate();
        cls_
    }

    fn branges(cls: &ClassBytes) -> Vec<(u8, u8)> {
        cls.iter().map(|x| (x.start(), x.end())).collect()
    }

    fn bcasefold(cls: &ClassBytes) -> ClassBytes {
        let mut cls_ = cls.clone();
        cls_.case_fold_simple();
        cls_
    }

    fn bunion(cls1: &ClassBytes, cls2: &ClassBytes) -> ClassBytes {
        let mut cls_ = cls1.clone();
        cls_.union(cls2);
        cls_
    }

    fn bintersect(cls1: &ClassBytes, cls2: &ClassBytes) -> ClassBytes {
        let mut cls_ = cls1.clone();
        cls_.intersect(cls2);
        cls_
    }

    fn bdifference(cls1: &ClassBytes, cls2: &ClassBytes) -> ClassBytes {
        let mut cls_ = cls1.clone();
        cls_.difference(cls2);
        cls_
    }

    fn bsymdifference(cls1: &ClassBytes, cls2: &ClassBytes) -> ClassBytes {
        let mut cls_ = cls1.clone();
        cls_.symmetric_difference(cls2);
        cls_
    }

    fn bnegate(cls: &ClassBytes) -> ClassBytes {
        let mut cls_ = cls.clone();
        cls_.negate();
        cls_
    }

    #[test]
    fn class_range_canonical_unicode() {
        let range = ClassUnicodeRange::new('\u{00FF}', '\0');
        assert_eq!('\0', range.start());
        assert_eq!('\u{00FF}', range.end());
    }

    #[test]
    fn class_range_canonical_bytes() {
        let range = ClassBytesRange::new(b'\xFF', b'\0');
        assert_eq!(b'\0', range.start());
        assert_eq!(b'\xFF', range.end());
    }

    #[test]
    fn class_canonicalize_unicode() {
        let cls = uclass(&[('a', 'c'), ('x', 'z')]);
        let expected = vec![('a', 'c'), ('x', 'z')];
        assert_eq!(expected, uranges(&cls));

        let cls = uclass(&[('x', 'z'), ('a', 'c')]);
        let expected = vec![('a', 'c'), ('x', 'z')];
        assert_eq!(expected, uranges(&cls));

        let cls = uclass(&[('x', 'z'), ('w', 'y')]);
        let expected = vec![('w', 'z')];
        assert_eq!(expected, uranges(&cls));

        let cls = uclass(&[
            ('c', 'f'),
            ('a', 'g'),
            ('d', 'j'),
            ('a', 'c'),
            ('m', 'p'),
            ('l', 's'),
        ]);
        let expected = vec![('a', 'j'), ('l', 's')];
        assert_eq!(expected, uranges(&cls));

        let cls = uclass(&[('x', 'z'), ('u', 'w')]);
        let expected = vec![('u', 'z')];
        assert_eq!(expected, uranges(&cls));

        let cls = uclass(&[('\x00', '\u{10FFFF}'), ('\x00', '\u{10FFFF}')]);
        let expected = vec![('\x00', '\u{10FFFF}')];
        assert_eq!(expected, uranges(&cls));

        let cls = uclass(&[('a', 'a'), ('b', 'b')]);
        let expected = vec![('a', 'b')];
        assert_eq!(expected, uranges(&cls));
    }

    #[test]
    fn class_canonicalize_bytes() {
        let cls = bclass(&[(b'a', b'c'), (b'x', b'z')]);
        let expected = vec![(b'a', b'c'), (b'x', b'z')];
        assert_eq!(expected, branges(&cls));

        let cls = bclass(&[(b'x', b'z'), (b'a', b'c')]);
        let expected = vec![(b'a', b'c'), (b'x', b'z')];
        assert_eq!(expected, branges(&cls));

        let cls = bclass(&[(b'x', b'z'), (b'w', b'y')]);
        let expected = vec![(b'w', b'z')];
        assert_eq!(expected, branges(&cls));

        let cls = bclass(&[
            (b'c', b'f'),
            (b'a', b'g'),
            (b'd', b'j'),
            (b'a', b'c'),
            (b'm', b'p'),
            (b'l', b's'),
        ]);
        let expected = vec![(b'a', b'j'), (b'l', b's')];
        assert_eq!(expected, branges(&cls));

        let cls = bclass(&[(b'x', b'z'), (b'u', b'w')]);
        let expected = vec![(b'u', b'z')];
        assert_eq!(expected, branges(&cls));

        let cls = bclass(&[(b'\x00', b'\xFF'), (b'\x00', b'\xFF')]);
        let expected = vec![(b'\x00', b'\xFF')];
        assert_eq!(expected, branges(&cls));

        let cls = bclass(&[(b'a', b'a'), (b'b', b'b')]);
        let expected = vec![(b'a', b'b')];
        assert_eq!(expected, branges(&cls));
    }

    #[test]
    #[cfg(feature = "unicode-case")]
    fn class_case_fold_unicode() {
        let cls = uclass(&[
            ('C', 'F'),
            ('A', 'G'),
            ('D', 'J'),
            ('A', 'C'),
            ('M', 'P'),
            ('L', 'S'),
            ('c', 'f'),
        ]);
        let expected = uclass(&[
            ('A', 'J'),
            ('L', 'S'),
            ('a', 'j'),
            ('l', 's'),
            ('\u{17F}', '\u{17F}'),
        ]);
        assert_eq!(expected, ucasefold(&cls));

        let cls = uclass(&[('A', 'Z')]);
        let expected = uclass(&[
            ('A', 'Z'),
            ('a', 'z'),
            ('\u{17F}', '\u{17F}'),
            ('\u{212A}', '\u{212A}'),
        ]);
        assert_eq!(expected, ucasefold(&cls));

        let cls = uclass(&[('a', 'z')]);
        let expected = uclass(&[
            ('A', 'Z'),
            ('a', 'z'),
            ('\u{17F}', '\u{17F}'),
            ('\u{212A}', '\u{212A}'),
        ]);
        assert_eq!(expected, ucasefold(&cls));

        let cls = uclass(&[('A', 'A'), ('_', '_')]);
        let expected = uclass(&[('A', 'A'), ('_', '_'), ('a', 'a')]);
        assert_eq!(expected, ucasefold(&cls));

        let cls = uclass(&[('A', 'A'), ('=', '=')]);
        let expected = uclass(&[('=', '='), ('A', 'A'), ('a', 'a')]);
        assert_eq!(expected, ucasefold(&cls));

        let cls = uclass(&[('\x00', '\x10')]);
        assert_eq!(cls, ucasefold(&cls));

        let cls = uclass(&[('k', 'k')]);
        let expected =
            uclass(&[('K', 'K'), ('k', 'k'), ('\u{212A}', '\u{212A}')]);
        assert_eq!(expected, ucasefold(&cls));

        let cls = uclass(&[('@', '@')]);
        assert_eq!(cls, ucasefold(&cls));
    }

    #[test]
    #[cfg(not(feature = "unicode-case"))]
    fn class_case_fold_unicode_disabled() {
        let mut cls = uclass(&[
            ('C', 'F'),
            ('A', 'G'),
            ('D', 'J'),
            ('A', 'C'),
            ('M', 'P'),
            ('L', 'S'),
            ('c', 'f'),
        ]);
        assert!(cls.try_case_fold_simple().is_err());
    }

    #[test]
    #[should_panic]
    #[cfg(not(feature = "unicode-case"))]
    fn class_case_fold_unicode_disabled_panics() {
        let mut cls = uclass(&[
            ('C', 'F'),
            ('A', 'G'),
            ('D', 'J'),
            ('A', 'C'),
            ('M', 'P'),
            ('L', 'S'),
            ('c', 'f'),
        ]);
        cls.case_fold_simple();
    }

    #[test]
    fn class_case_fold_bytes() {
        let cls = bclass(&[
            (b'C', b'F'),
            (b'A', b'G'),
            (b'D', b'J'),
            (b'A', b'C'),
            (b'M', b'P'),
            (b'L', b'S'),
            (b'c', b'f'),
        ]);
        let expected =
            bclass(&[(b'A', b'J'), (b'L', b'S'), (b'a', b'j'), (b'l', b's')]);
        assert_eq!(expected, bcasefold(&cls));

        let cls = bclass(&[(b'A', b'Z')]);
        let expected = bclass(&[(b'A', b'Z'), (b'a', b'z')]);
        assert_eq!(expected, bcasefold(&cls));

        let cls = bclass(&[(b'a', b'z')]);
        let expected = bclass(&[(b'A', b'Z'), (b'a', b'z')]);
        assert_eq!(expected, bcasefold(&cls));

        let cls = bclass(&[(b'A', b'A'), (b'_', b'_')]);
        let expected = bclass(&[(b'A', b'A'), (b'_', b'_'), (b'a', b'a')]);
        assert_eq!(expected, bcasefold(&cls));

        let cls = bclass(&[(b'A', b'A'), (b'=', b'=')]);
        let expected = bclass(&[(b'=', b'='), (b'A', b'A'), (b'a', b'a')]);
        assert_eq!(expected, bcasefold(&cls));

        let cls = bclass(&[(b'\x00', b'\x10')]);
        assert_eq!(cls, bcasefold(&cls));

        let cls = bclass(&[(b'k', b'k')]);
        let expected = bclass(&[(b'K', b'K'), (b'k', b'k')]);
        assert_eq!(expected, bcasefold(&cls));

        let cls = bclass(&[(b'@', b'@')]);
        assert_eq!(cls, bcasefold(&cls));
    }

    #[test]
    fn class_negate_unicode() {
        let cls = uclass(&[('a', 'a')]);
        let expected = uclass(&[('\x00', '\x60'), ('\x62', '\u{10FFFF}')]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('a', 'a'), ('b', 'b')]);
        let expected = uclass(&[('\x00', '\x60'), ('\x63', '\u{10FFFF}')]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('a', 'c'), ('x', 'z')]);
        let expected = uclass(&[
            ('\x00', '\x60'),
            ('\x64', '\x77'),
            ('\x7B', '\u{10FFFF}'),
        ]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('\x00', 'a')]);
        let expected = uclass(&[('\x62', '\u{10FFFF}')]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('a', '\u{10FFFF}')]);
        let expected = uclass(&[('\x00', '\x60')]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('\x00', '\u{10FFFF}')]);
        let expected = uclass(&[]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[]);
        let expected = uclass(&[('\x00', '\u{10FFFF}')]);
        assert_eq!(expected, unegate(&cls));

        let cls =
            uclass(&[('\x00', '\u{10FFFD}'), ('\u{10FFFF}', '\u{10FFFF}')]);
        let expected = uclass(&[('\u{10FFFE}', '\u{10FFFE}')]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('\x00', '\u{D7FF}')]);
        let expected = uclass(&[('\u{E000}', '\u{10FFFF}')]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('\x00', '\u{D7FE}')]);
        let expected = uclass(&[('\u{D7FF}', '\u{10FFFF}')]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('\u{E000}', '\u{10FFFF}')]);
        let expected = uclass(&[('\x00', '\u{D7FF}')]);
        assert_eq!(expected, unegate(&cls));

        let cls = uclass(&[('\u{E001}', '\u{10FFFF}')]);
        let expected = uclass(&[('\x00', '\u{E000}')]);
        assert_eq!(expected, unegate(&cls));
    }

    #[test]
    fn class_negate_bytes() {
        let cls = bclass(&[(b'a', b'a')]);
        let expected = bclass(&[(b'\x00', b'\x60'), (b'\x62', b'\xFF')]);
        assert_eq!(expected, bnegate(&cls));

        let cls = bclass(&[(b'a', b'a'), (b'b', b'b')]);
        let expected = bclass(&[(b'\x00', b'\x60'), (b'\x63', b'\xFF')]);
        assert_eq!(expected, bnegate(&cls));

        let cls = bclass(&[(b'a', b'c'), (b'x', b'z')]);
        let expected = bclass(&[
            (b'\x00', b'\x60'),
            (b'\x64', b'\x77'),
            (b'\x7B', b'\xFF'),
        ]);
        assert_eq!(expected, bnegate(&cls));

        let cls = bclass(&[(b'\x00', b'a')]);
        let expected = bclass(&[(b'\x62', b'\xFF')]);
        assert_eq!(expected, bnegate(&cls));

        let cls = bclass(&[(b'a', b'\xFF')]);
        let expected = bclass(&[(b'\x00', b'\x60')]);
        assert_eq!(expected, bnegate(&cls));

        let cls = bclass(&[(b'\x00', b'\xFF')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bnegate(&cls));

        let cls = bclass(&[]);
        let expected = bclass(&[(b'\x00', b'\xFF')]);
        assert_eq!(expected, bnegate(&cls));

        let cls = bclass(&[(b'\x00', b'\xFD'), (b'\xFF', b'\xFF')]);
        let expected = bclass(&[(b'\xFE', b'\xFE')]);
        assert_eq!(expected, bnegate(&cls));
    }

    #[test]
    fn class_union_unicode() {
        let cls1 = uclass(&[('a', 'g'), ('m', 't'), ('A', 'C')]);
        let cls2 = uclass(&[('a', 'z')]);
        let expected = uclass(&[('a', 'z'), ('A', 'C')]);
        assert_eq!(expected, uunion(&cls1, &cls2));
    }

    #[test]
    fn class_union_bytes() {
        let cls1 = bclass(&[(b'a', b'g'), (b'm', b't'), (b'A', b'C')]);
        let cls2 = bclass(&[(b'a', b'z')]);
        let expected = bclass(&[(b'a', b'z'), (b'A', b'C')]);
        assert_eq!(expected, bunion(&cls1, &cls2));
    }

    #[test]
    fn class_intersect_unicode() {
        let cls1 = uclass(&[]);
        let cls2 = uclass(&[('a', 'a')]);
        let expected = uclass(&[]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'a')]);
        let cls2 = uclass(&[('a', 'a')]);
        let expected = uclass(&[('a', 'a')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'a')]);
        let cls2 = uclass(&[('b', 'b')]);
        let expected = uclass(&[]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'a')]);
        let cls2 = uclass(&[('a', 'c')]);
        let expected = uclass(&[('a', 'a')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b')]);
        let cls2 = uclass(&[('a', 'c')]);
        let expected = uclass(&[('a', 'b')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b')]);
        let cls2 = uclass(&[('b', 'c')]);
        let expected = uclass(&[('b', 'b')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b')]);
        let cls2 = uclass(&[('c', 'd')]);
        let expected = uclass(&[]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('b', 'c')]);
        let cls2 = uclass(&[('a', 'd')]);
        let expected = uclass(&[('b', 'c')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b'), ('d', 'e'), ('g', 'h')]);
        let cls2 = uclass(&[('a', 'h')]);
        let expected = uclass(&[('a', 'b'), ('d', 'e'), ('g', 'h')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b'), ('d', 'e'), ('g', 'h')]);
        let cls2 = uclass(&[('a', 'b'), ('d', 'e'), ('g', 'h')]);
        let expected = uclass(&[('a', 'b'), ('d', 'e'), ('g', 'h')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b'), ('g', 'h')]);
        let cls2 = uclass(&[('d', 'e'), ('k', 'l')]);
        let expected = uclass(&[]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b'), ('d', 'e'), ('g', 'h')]);
        let cls2 = uclass(&[('h', 'h')]);
        let expected = uclass(&[('h', 'h')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b'), ('e', 'f'), ('i', 'j')]);
        let cls2 = uclass(&[('c', 'd'), ('g', 'h'), ('k', 'l')]);
        let expected = uclass(&[]);
        assert_eq!(expected, uintersect(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'b'), ('c', 'd'), ('e', 'f')]);
        let cls2 = uclass(&[('b', 'c'), ('d', 'e'), ('f', 'g')]);
        let expected = uclass(&[('b', 'f')]);
        assert_eq!(expected, uintersect(&cls1, &cls2));
    }

    #[test]
    fn class_intersect_bytes() {
        let cls1 = bclass(&[]);
        let cls2 = bclass(&[(b'a', b'a')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'a')]);
        let cls2 = bclass(&[(b'a', b'a')]);
        let expected = bclass(&[(b'a', b'a')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'a')]);
        let cls2 = bclass(&[(b'b', b'b')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'a')]);
        let cls2 = bclass(&[(b'a', b'c')]);
        let expected = bclass(&[(b'a', b'a')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b')]);
        let cls2 = bclass(&[(b'a', b'c')]);
        let expected = bclass(&[(b'a', b'b')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b')]);
        let cls2 = bclass(&[(b'b', b'c')]);
        let expected = bclass(&[(b'b', b'b')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b')]);
        let cls2 = bclass(&[(b'c', b'd')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'b', b'c')]);
        let cls2 = bclass(&[(b'a', b'd')]);
        let expected = bclass(&[(b'b', b'c')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b'), (b'd', b'e'), (b'g', b'h')]);
        let cls2 = bclass(&[(b'a', b'h')]);
        let expected = bclass(&[(b'a', b'b'), (b'd', b'e'), (b'g', b'h')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b'), (b'd', b'e'), (b'g', b'h')]);
        let cls2 = bclass(&[(b'a', b'b'), (b'd', b'e'), (b'g', b'h')]);
        let expected = bclass(&[(b'a', b'b'), (b'd', b'e'), (b'g', b'h')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b'), (b'g', b'h')]);
        let cls2 = bclass(&[(b'd', b'e'), (b'k', b'l')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b'), (b'd', b'e'), (b'g', b'h')]);
        let cls2 = bclass(&[(b'h', b'h')]);
        let expected = bclass(&[(b'h', b'h')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b'), (b'e', b'f'), (b'i', b'j')]);
        let cls2 = bclass(&[(b'c', b'd'), (b'g', b'h'), (b'k', b'l')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bintersect(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'b'), (b'c', b'd'), (b'e', b'f')]);
        let cls2 = bclass(&[(b'b', b'c'), (b'd', b'e'), (b'f', b'g')]);
        let expected = bclass(&[(b'b', b'f')]);
        assert_eq!(expected, bintersect(&cls1, &cls2));
    }

    #[test]
    fn class_difference_unicode() {
        let cls1 = uclass(&[('a', 'a')]);
        let cls2 = uclass(&[('a', 'a')]);
        let expected = uclass(&[]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'a')]);
        let cls2 = uclass(&[]);
        let expected = uclass(&[('a', 'a')]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[]);
        let cls2 = uclass(&[('a', 'a')]);
        let expected = uclass(&[]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'z')]);
        let cls2 = uclass(&[('a', 'a')]);
        let expected = uclass(&[('b', 'z')]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'z')]);
        let cls2 = uclass(&[('z', 'z')]);
        let expected = uclass(&[('a', 'y')]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'z')]);
        let cls2 = uclass(&[('m', 'm')]);
        let expected = uclass(&[('a', 'l'), ('n', 'z')]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'c'), ('g', 'i'), ('r', 't')]);
        let cls2 = uclass(&[('a', 'z')]);
        let expected = uclass(&[]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'c'), ('g', 'i'), ('r', 't')]);
        let cls2 = uclass(&[('d', 'v')]);
        let expected = uclass(&[('a', 'c')]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'c'), ('g', 'i'), ('r', 't')]);
        let cls2 = uclass(&[('b', 'g'), ('s', 'u')]);
        let expected = uclass(&[('a', 'a'), ('h', 'i'), ('r', 'r')]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'c'), ('g', 'i'), ('r', 't')]);
        let cls2 = uclass(&[('b', 'd'), ('e', 'g'), ('s', 'u')]);
        let expected = uclass(&[('a', 'a'), ('h', 'i'), ('r', 'r')]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('x', 'z')]);
        let cls2 = uclass(&[('a', 'c'), ('e', 'g'), ('s', 'u')]);
        let expected = uclass(&[('x', 'z')]);
        assert_eq!(expected, udifference(&cls1, &cls2));

        let cls1 = uclass(&[('a', 'z')]);
        let cls2 = uclass(&[('a', 'c'), ('e', 'g'), ('s', 'u')]);
        let expected = uclass(&[('d', 'd'), ('h', 'r'), ('v', 'z')]);
        assert_eq!(expected, udifference(&cls1, &cls2));
    }

    #[test]
    fn class_difference_bytes() {
        let cls1 = bclass(&[(b'a', b'a')]);
        let cls2 = bclass(&[(b'a', b'a')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'a')]);
        let cls2 = bclass(&[]);
        let expected = bclass(&[(b'a', b'a')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[]);
        let cls2 = bclass(&[(b'a', b'a')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'z')]);
        let cls2 = bclass(&[(b'a', b'a')]);
        let expected = bclass(&[(b'b', b'z')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'z')]);
        let cls2 = bclass(&[(b'z', b'z')]);
        let expected = bclass(&[(b'a', b'y')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'z')]);
        let cls2 = bclass(&[(b'm', b'm')]);
        let expected = bclass(&[(b'a', b'l'), (b'n', b'z')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'c'), (b'g', b'i'), (b'r', b't')]);
        let cls2 = bclass(&[(b'a', b'z')]);
        let expected = bclass(&[]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'c'), (b'g', b'i'), (b'r', b't')]);
        let cls2 = bclass(&[(b'd', b'v')]);
        let expected = bclass(&[(b'a', b'c')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'c'), (b'g', b'i'), (b'r', b't')]);
        let cls2 = bclass(&[(b'b', b'g'), (b's', b'u')]);
        let expected = bclass(&[(b'a', b'a'), (b'h', b'i'), (b'r', b'r')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'c'), (b'g', b'i'), (b'r', b't')]);
        let cls2 = bclass(&[(b'b', b'd'), (b'e', b'g'), (b's', b'u')]);
        let expected = bclass(&[(b'a', b'a'), (b'h', b'i'), (b'r', b'r')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'x', b'z')]);
        let cls2 = bclass(&[(b'a', b'c'), (b'e', b'g'), (b's', b'u')]);
        let expected = bclass(&[(b'x', b'z')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));

        let cls1 = bclass(&[(b'a', b'z')]);
        let cls2 = bclass(&[(b'a', b'c'), (b'e', b'g'), (b's', b'u')]);
        let expected = bclass(&[(b'd', b'd'), (b'h', b'r'), (b'v', b'z')]);
        assert_eq!(expected, bdifference(&cls1, &cls2));
    }

    #[test]
    fn class_symmetric_difference_unicode() {
        let cls1 = uclass(&[('a', 'm')]);
        let cls2 = uclass(&[('g', 't')]);
        let expected = uclass(&[('a', 'f'), ('n', 't')]);
        assert_eq!(expected, usymdifference(&cls1, &cls2));
    }

    #[test]
    fn class_symmetric_difference_bytes() {
        let cls1 = bclass(&[(b'a', b'm')]);
        let cls2 = bclass(&[(b'g', b't')]);
        let expected = bclass(&[(b'a', b'f'), (b'n', b't')]);
        assert_eq!(expected, bsymdifference(&cls1, &cls2));
    }

    // We use a thread with an explicit stack size to test that our destructor
    // for Hir can handle arbitrarily sized expressions in constant stack
    // space. In case we run on a platform without threads (WASM?), we limit
    // this test to Windows/Unix.
    #[test]
    #[cfg(any(unix, windows))]
    fn no_stack_overflow_on_drop() {
        use std::thread;

        let run = || {
            let mut expr = Hir::empty();
            for _ in 0..100 {
                expr = Hir::group(Group {
                    index: 1,
                    name: None,
                    hir: Box::new(expr),
                });
                expr = Hir::repetition(Repetition {
                    min: 0,
                    max: Some(1),
                    greedy: true,
                    hir: Box::new(expr),
                });

                expr = Hir {
                    kind: HirKind::Concat(vec![expr]),
                    props: Properties::empty(),
                };
                expr = Hir {
                    kind: HirKind::Alternation(vec![expr]),
                    props: Properties::empty(),
                };
            }
            assert!(!expr.kind.is_empty());
        };

        // We run our test on a thread with a small stack size so we can
        // force the issue more easily.
        //
        // NOTE(2023-03-21): See the corresponding test in 'crate::ast::tests'
        // for context on the specific stack size chosen here.
        thread::Builder::new()
            .stack_size(16 << 10)
            .spawn(run)
            .unwrap()
            .join()
            .unwrap();
    }
}
