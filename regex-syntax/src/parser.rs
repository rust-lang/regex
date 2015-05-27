// Copyright 2014-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::cmp::{max, min};

use unicode::regex::UNICODE_CLASSES;

use {
    Expr, Repeater, CharClass, ClassRange, CaptureIndex, CaptureName,
    Error, ErrorKind, Result,
};

/// Parser state.
///
/// Keeps the entire input in memory and maintains a cursor (char offset).
///
/// It also keeps an expression stack, which is responsible for managing
/// grouped expressions and flag state.
#[derive(Debug)]
pub struct Parser {
    chars: Vec<char>,
    chari: usize,
    stack: Vec<Build>,
    caps: usize,
    names: Vec<String>, // to check for duplicates
    flags: Flags,
}

/// An empheral type for representing the expression stack.
///
/// Everything on the stack is either a regular expression or a marker
/// indicating the opening of a group (possibly non-capturing). The opening
/// of a group copies the current flag state, which is reset on the parser
/// state once the group closes.
#[derive(Debug)]
enum Build {
    Expr(Expr),
    LeftParen {
        i: CaptureIndex,
        name: CaptureName,
        chari: usize,
        old_flags: Flags,
    },
}

/// Flag state.
#[derive(Clone, Copy, Debug)]
struct Flags {
    casei: bool,
    multi: bool,
    dotnl: bool,
    swap_greed: bool,
    ignore_space: bool,
}

// Primary expression parsing routines.
impl Parser {
    pub fn parse(s: &str) -> Result<Expr> {
        Parser {
            chars: s.chars().collect(),
            chari: 0,
            stack: vec![],
            caps: 0,
            names: vec![],
            flags: Flags {
                casei: false,
                multi: false,
                dotnl: false,
                swap_greed: false,
                ignore_space: false,
            },
        }.parse_expr()
    }

    // Top-level expression parser.
    //
    // Starts at the beginning of the input and consumes until either the end
    // of input or an error.
    fn parse_expr(mut self) -> Result<Expr> {
        while !self.eof() {
            let build_expr = match self.cur() {
                '\\' => try!(self.parse_escape()),
                '|' => { let e = try!(self.alternate()); self.bump(); e }
                '?' => try!(self.parse_simple_repeat(Repeater::ZeroOrOne)),
                '*' => try!(self.parse_simple_repeat(Repeater::ZeroOrMore)),
                '+' => try!(self.parse_simple_repeat(Repeater::OneOrMore)),
                '{' => try!(self.parse_counted_repeat()),
                '[' => match self.maybe_parse_ascii() {
                    None => try!(self.parse_class()),
                    Some(cls) => Build::Expr(Expr::Class(cls)),
                },
                '^' => {
                    if self.flags.multi {
                        self.parse_one(Expr::StartLine)
                    } else {
                        self.parse_one(Expr::StartText)
                    }
                }
                '$' => {
                    if self.flags.multi {
                        self.parse_one(Expr::EndLine)
                    } else {
                        self.parse_one(Expr::EndText)
                    }
                }
                '.' => {
                    if self.flags.dotnl {
                        self.parse_one(Expr::AnyChar)
                    } else {
                        self.parse_one(Expr::AnyCharNoNL)
                    }
                }
                '(' => try!(self.parse_group()),
                ')' => {
                    let (old_flags, e) = try!(self.close_paren());
                    self.bump();
                    self.flags = old_flags;
                    e
                }
                _ => Build::Expr(Expr::Literal {
                    chars: vec![self.bump()],
                    casei: self.flags.casei,
                }),
            };
            if !build_expr.is_empty() {
                let build_expr = self.maybe_class_case_fold(build_expr);
                self.stack.push(build_expr);
            }
        }
        self.finish_concat()
    }

    // Parses an escape sequence, e.g., \Ax
    //
    // Start: `\`
    // End:   `x`
    fn parse_escape(&mut self) -> Result<Build> {
        self.bump();
        if self.eof() {
            return Err(self.err(ErrorKind::UnexpectedEscapeEof));
        }
        let c = self.cur();
        if is_punct(c) {
            return Ok(Build::Expr(Expr::Literal {
                chars: vec![self.bump()],
                casei: self.flags.casei,
            }));
        }

        fn lit(c: char) -> Build {
            Build::Expr(Expr::Literal { chars: vec![c], casei: false })
        }
        match c {
            'a' => { self.bump(); Ok(lit('\x07')) }
            'f' => { self.bump(); Ok(lit('\x0C')) }
            't' => { self.bump(); Ok(lit('\t')) }
            'n' => { self.bump(); Ok(lit('\n')) }
            'r' => { self.bump(); Ok(lit('\r')) }
            'v' => { self.bump(); Ok(lit('\x0B')) }
            'A' => { self.bump(); Ok(Build::Expr(Expr::StartText)) }
            'z' => { self.bump(); Ok(Build::Expr(Expr::EndText)) }
            'b' => { self.bump(); Ok(Build::Expr(Expr::WordBoundary)) }
            'B' => { self.bump(); Ok(Build::Expr(Expr::NotWordBoundary)) }
            '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7' => self.parse_octal(),
            'x' => { self.bump(); self.parse_hex() }
            'p'|'P' => {
                self.bump();
                self.parse_unicode_class(c == 'P')
                    .map(|cls| Build::Expr(Expr::Class(cls)))
            }
            'd'|'s'|'w'|'D'|'S'|'W' => {
                self.bump();
                Ok(Build::Expr(Expr::Class(self.parse_perl_class(c))))
            }
            c => Err(self.err(ErrorKind::UnrecognizedEscape(c))),
        }
    }

    // Parses a group, e.g., `(abc)`.
    //
    // Start: `(`
    // End:   `a`
    //
    // A more interesting example, `(?P<foo>abc)`.
    //
    // Start: `(`
    // End:   `a`
    fn parse_group(&mut self) -> Result<Build> {
        let chari = self.chari;
        let mut name: CaptureName = None;
        self.bump();
        if self.bump_if("?P<") {
            let n = try!(self.parse_group_name());
            if self.names.iter().any(|n2| n2 == &n) {
                return Err(self.err(ErrorKind::DuplicateCaptureName(n)));
            }
            self.names.push(n.clone());
            name = Some(n);
        } else if self.bump_if("?") {
            // This can never be capturing. It's either setting flags for
            // the current group, or it's opening a non-capturing group or
            // it's opening a group with a specific set of flags (which is
            // also non-capturing).
            // Anything else is an error.
            return self.parse_group_flags(chari);
        }
        self.caps = checkadd(self.caps, 1);
        Ok(Build::LeftParen {
            i: Some(self.caps),
            name: name,
            chari: chari,
            old_flags: self.flags, // no flags changed if we're here
        })
    }

    // Parses flags (inline or grouped), e.g., `(?s-i:abc)`.
    //
    // Start: `s`
    // End:   `a`
    //
    // Another example, `(?s-i)a`.
    //
    // Start: `s`
    // End:   `a`
    fn parse_group_flags(&mut self, opening_chari: usize) -> Result<Build> {
        let old_flags = self.flags;
        let mut sign = true;
        let mut saw_flag = false;
        loop {
            if self.eof() {
                // e.g., (?i
                return Err(self.err(ErrorKind::UnexpectedFlagEof));
            }
            match self.cur() {
                'i' => { self.flags.casei = sign; saw_flag = true }
                'm' => { self.flags.multi = sign; saw_flag = true }
                's' => { self.flags.dotnl = sign; saw_flag = true }
                'U' => { self.flags.swap_greed = sign; saw_flag = true }
                'x' => { self.flags.ignore_space = sign; saw_flag = true }
                '-' => {
                    if !sign {
                        // e.g., (?-i-s)
                        return Err(self.err(ErrorKind::DoubleFlagNegation));
                    }
                    sign = false;
                    saw_flag = false;
                }
                ')' => {
                    if !saw_flag {
                        // e.g., (?)
                        return Err(self.err(ErrorKind::EmptyFlagNegation));
                    }
                    // At this point, we're just changing the flags inside
                    // the current group, which means the old flags have
                    // been saved elsewhere. Our modifications in place are
                    // okey dokey!
                    //
                    // This particular flag expression only has a stateful
                    // impact on a regex's AST, so nothing gets explicitly
                    // added.
                    self.bump();
                    return Ok(Build::Expr(Expr::Empty));
                }
                ':' => {
                    if !sign && !saw_flag {
                        // e.g., (?i-:a)
                        // Note that if there's no negation, it's OK not
                        // to see flag, because you end up with a regular
                        // non-capturing group: `(?:a)`.
                        return Err(self.err(ErrorKind::EmptyFlagNegation));
                    }
                    self.bump();
                    return Ok(Build::LeftParen {
                        i: None,
                        name: None,
                        chari: opening_chari,
                        old_flags: old_flags,
                    });
                }
                // e.g., (?z:a)
                c => return Err(self.err(ErrorKind::UnrecognizedFlag(c))),
            }
            self.bump();
        }
    }

    // Parses a group name, e.g., `foo` in `(?P<foo>abc)`.
    //
    // Start: `f`
    // End:   `a`
    fn parse_group_name(&mut self) -> Result<String> {
        let mut name = String::new();
        while !self.eof() && !self.peek_is('>') {
            name.push(self.bump());
        }
        if self.eof() {
            // e.g., (?P<a
            return Err(self.err(ErrorKind::UnclosedCaptureName(name)));
        }
        let all_valid = name.chars().all(is_valid_capture_char);
        match name.chars().next() {
            // e.g., (?P<>a)
            None => Err(self.err(ErrorKind::EmptyCaptureName)),
            Some(c) if (c >= '0' && c <= '9') || !all_valid => {
                // e.g., (?P<a#>x)
                // e.g., (?P<1a>x)
                Err(self.err(ErrorKind::InvalidCaptureName(name)))
            }
            _ => {
                self.bump(); // for `>`
                Ok(name)
            }
        }
    }

    // Parses a counted repeition operator, e.g., `a{2,4}?z`.
    //
    // Start: `{`
    // End:   `z`
    fn parse_counted_repeat(&mut self) -> Result<Build> {
        let e = try!(self.pop(ErrorKind::RepeaterExpectsExpr)); // e.g., ({5}
        if !e.can_repeat() {
            // e.g., a*{5}
            return Err(self.err(ErrorKind::RepeaterUnexpectedExpr(e)));
        }
        self.bump();
        let min = try!(self.parse_decimal(|c| c != ',' && c != '}'));
        let mut max_opt = Some(min);
        if self.bump_if(',') {
            if self.peek_is('}') {
                max_opt = None;
            } else {
                let max = try!(self.parse_decimal(|c| c != '}'));
                if min > max {
                    // e.g., a{2,1}
                    return Err(self.err(ErrorKind::InvalidRepeatRange {
                        min: min,
                        max: max,
                    }));
                }
                max_opt = Some(max);
            }
        }
        if !self.bump_if('}') {
            Err(self.err(ErrorKind::UnclosedRepeat))
        } else {
            Ok(Build::Expr(Expr::Repeat {
                e: Box::new(e),
                r: Repeater::Range { min: min, max: max_opt },
                greedy: !self.bump_if('?') ^ self.flags.swap_greed,
            }))
        }
    }

    // Parses a simple repetition operator, e.g., `a+?z`.
    //
    // Start: `+`
    // End:   `z`
    //
    // N.B. "simple" in this context means "not min/max repetition",
    // e.g., `a{1,2}`.
    fn parse_simple_repeat(&mut self, rep: Repeater) -> Result<Build> {
        let e = try!(self.pop(ErrorKind::RepeaterExpectsExpr)); // e.g., (*
        if !e.can_repeat() {
            // e.g., a**
            return Err(self.err(ErrorKind::RepeaterUnexpectedExpr(e)));
        }
        self.bump();
        Ok(Build::Expr(Expr::Repeat {
            e: Box::new(e),
            r: rep,
            greedy: !self.bump_if('?') ^ self.flags.swap_greed,
        }))
    }

    // Parses a decimal number until the given character, e.g., `a{123,456}`.
    //
    // Start: `1`
    // End:   `,` (where `until == ','`)
    fn parse_decimal<B: Bumpable>(&mut self, until: B) -> Result<u32> {
        match self.bump_get(until) {
            // e.g., a{}
            None => Err(self.err(ErrorKind::MissingBase10)),
            Some(n) => {
                // e.g., a{xyz
                // e.g., a{9999999999}
                let n = n.trim();
                u32::from_str_radix(n, 10)
                    .map_err(|_| self.err(ErrorKind::InvalidBase10(n.into())))
            }
        }
    }

    // Parses an octal number, up to 3 digits, e.g., `a\123b`
    //
    // Start: `1`
    // End:   `b`
    fn parse_octal(&mut self) -> Result<Build> {
        use std::char;
        let mut i = 0; // counter for limiting octal to 3 digits.
        let n = self.bump_get(|c| { i += 1; i <= 3 && c >= '0' && c <= '7' })
                    .expect("octal string"); // guaranteed at least 1 digit
        // I think both of the following unwraps are impossible to fail.
        // We limit it to a three digit octal number, which maxes out at
        // `0777` or `511` in decimal. Since all digits are in `0...7`, we'll
        // always have a valid `u32` number. Moreover, since all numbers in
        // the range `0...511` are valid Unicode scalar values, it will always
        // be a valid `char`.
        //
        // Hence, we `unwrap` with reckless abandon.
        let n = u32::from_str_radix(&n, 8).ok().expect("valid octal number");
        Ok(Build::Expr(Expr::Literal {
            chars: vec![char::from_u32(n).expect("Unicode scalar value")],
            casei: self.flags.casei,
        }))
    }

    // Parses a hex number, e.g., `a\x5ab`.
    //
    // Start: `5`
    // End:   `b`
    //
    // And also, `a\x{2603}b`.
    //
    // Start: `{`
    // End:   `b`
    fn parse_hex(&mut self) -> Result<Build> {
        if self.bump_if('{') {
            self.parse_hex_many_digits()
        } else {
            self.parse_hex_two_digits()
        }
    }

    // Parses a many-digit hex number, e.g., `a\x{2603}b`.
    //
    // Start: `2`
    // End:   `b`
    fn parse_hex_many_digits(&mut self) -> Result<Build> {
        use std::char;

        let s = self.bump_get(|c| c != '}').unwrap_or("".into());
        let n = try!(u32::from_str_radix(&s, 16)
                         .map_err(|_| self.err(ErrorKind::InvalidBase16(s))));
        let c = try!(char::from_u32(n)
                          .ok_or(self.err(ErrorKind::InvalidScalarValue(n))));
        if !self.bump_if('}') {
            // e.g., a\x{d
            return Err(self.err(ErrorKind::UnclosedHex));
        }
        Ok(Build::Expr(Expr::Literal {
            chars: vec![c],
            casei: self.flags.casei,
        }))
    }

    // Parses a two-digit hex number, e.g., `a\x5ab`.
    //
    // Start: `5`
    // End:   `b`
    fn parse_hex_two_digits(&mut self) -> Result<Build> {
        use std::char;

        let mut i = 0;
        let s = self.bump_get(|_| { i += 1; i <= 2 }).unwrap_or("".into());
        if s.len() < 2 {
            // e.g., a\x
            // e.g., a\xf
            return Err(self.err(ErrorKind::UnexpectedTwoDigitHexEof));
        }
        let n = try!(u32::from_str_radix(&s, 16)
                         .map_err(|_| self.err(ErrorKind::InvalidBase16(s))));
        Ok(Build::Expr(Expr::Literal {
            // Because 0...255 are all valid Unicode scalar values.
            chars: vec![char::from_u32(n).expect("Unicode scalar value")],
            casei: self.flags.casei,
        }))
    }

    // Parses a character class, e.g., `[^a-zA-Z0-9]+`.
    //
    // Start: `[`
    // End:   `+`
    fn parse_class(&mut self) -> Result<Build> {
        self.bump();
        let negated = self.bump_if('^');
        let mut class = CharClass::empty();
        while self.bump_if('-') {
            class.ranges.push(ClassRange::one('-'));
        }
        loop {
            if self.eof() {
                // e.g., [a
                return Err(self.err(ErrorKind::UnexpectedClassEof));
            }
            match self.cur() {
                // If no ranges have been added, then `]` is the first
                // character (sans, perhaps, the `^` symbol), so it should
                // be interpreted as a `]` instead of a closing class bracket.
                ']' if class.len() > 0 => { self.bump(); break }
                '[' => match self.maybe_parse_ascii() {
                    Some(class2) => class.ranges.extend(class2),
                    None => {
                        self.bump();
                        try!(self.parse_class_range(&mut class, '['))
                    }
                },
                '\\' => match try!(self.parse_escape()) {
                    Build::Expr(Expr::Class(class2)) => {
                        class.ranges.extend(class2);
                    }
                    Build::Expr(Expr::Literal { chars, .. }) => {
                        try!(self.parse_class_range(&mut class, chars[0]));
                    }
                    Build::Expr(e) => {
                        let err = ErrorKind::InvalidClassEscape(e);
                        return Err(self.err(err));
                    }
                    // Because `parse_escape` can never return `LeftParen`.
                    _ => unreachable!(),
                },
                start => {
                    self.bump();
                    try!(self.parse_class_range(&mut class, start));
                }
            }
        }
        if negated {
            class = class.negate();
        }
        Ok(Build::Expr(Expr::Class(class.canonicalize())))
    }

    // Parses a single range in a character class.
    //
    // Since this is a helper for `parse_class`, its signature sticks out.
    // Namely, it requires the start character of the range and the char
    // class to mutate.
    //
    // e.g., `[a-z]`
    //
    // Start: `-` (with start == `a`)
    // End:   `]`
    fn parse_class_range(&mut self, class: &mut CharClass, start: char)
                        -> Result<()> {
        if !self.bump_if('-') {
            // Not a range, so just push a singleton range.
            class.ranges.push(ClassRange::one(start));
            return Ok(());
        }
        if self.eof() {
            // e.g., [a-
            return Err(self.err(ErrorKind::UnexpectedClassEof));
        }
        if self.peek_is(']') {
            // This is the end of the class, so we permit use of `-` as a
            // regular char (just like we do in the beginning).
            class.ranges.push(ClassRange::one(start));
            class.ranges.push(ClassRange::one('-'));
            return Ok(());
        }

        // We have a real range. Just need to check to parse literal and
        // make sure it's a valid range.
        let end = match self.cur() {
            '\\' => match try!(self.parse_escape()) {
                Build::Expr(Expr::Literal { chars, .. }) => chars[0],
                Build::Expr(e) => {
                    return Err(self.err(ErrorKind::InvalidClassEscape(e)));
                }
                // Because `parse_escape` can never return `LeftParen`.
                _ => unreachable!(),
            },
            _ => self.bump(),
        };
        if end < start {
            // e.g., [z-a]
            return Err(self.err(ErrorKind::InvalidClassRange {
                start: start,
                end: end,
            }));
        }
        class.ranges.push(ClassRange::new(start, end));
        Ok(())
    }

    // Parses an ASCII class, e.g., `[:alnum:]+`.
    //
    // Start: `[`
    // End:   `+`
    //
    // Also supports negation, e.g., `[:^alnum:]`.
    //
    // This parsing routine is distinct from the others in that it doesn't
    // actually report any errors. Namely, if it fails, then the parser should
    // fall back to parsing a regular class.
    //
    // This method will only make progress in the parser if it succeeds.
    // Otherwise, the input remains where it started.
    fn maybe_parse_ascii(&mut self) -> Option<CharClass> {
        fn parse(p: &mut Parser) -> Option<CharClass> {
            p.bump(); // the `[`
            if !p.bump_if(':') { return None; }
            let negate = p.bump_if('^');
            let name = match p.bump_get(|c| c != ':') {
                None => return None,
                Some(name) => name,
            };
            if !p.bump_if(":]") { return None; }
            ascii_class(&name).map(|c| if !negate { c } else { c.negate() })
        }
        let start = self.chari;
        match parse(self) {
            None => { self.chari = start; None }
            result => result,
        }
    }

    // Parses a Uncode class name, e.g., `a\pLb`.
    //
    // Start: `L`
    // End:   `b`
    //
    // And also, `a\p{Greek}b`.
    //
    // Start: `{`
    // End:   `b`
    //
    // `negate` is true when the class name is used with `\P`.
    fn parse_unicode_class(&mut self, neg: bool) -> Result<CharClass> {
        let name =
            if self.bump_if('{') {
                let n = self.bump_get(|c| c != '}').unwrap_or("".into());
                if n.is_empty() || !self.bump_if('}') {
                    // e.g., \p{Greek
                    return Err(self.err(ErrorKind::UnclosedUnicodeName));
                }
                n
            } else {
                if self.eof() {
                    // e.g., \p
                    return Err(self.err(ErrorKind::UnexpectedEscapeEof));
                }
                self.bump().to_string()
            };
        match unicode_class(&name) {
            None => Err(self.err(ErrorKind::UnrecognizedUnicodeClass(name))),
            Some(cls) => if neg { Ok(cls.negate()) } else { Ok(cls) },
        }
    }

    // Parses a perl character class with Unicode support.
    //
    // `name` must be one of d, s, w, D, S, W. If not, this function panics.
    //
    // No parser state is changed.
    fn parse_perl_class(&mut self, name: char) -> CharClass {
        use unicode::regex::{PERLD, PERLS, PERLW};
        match name {
            'd' => raw_class_to_expr(PERLD),
            'D' => raw_class_to_expr(PERLD).negate(),
            's' => raw_class_to_expr(PERLS),
            'S' => raw_class_to_expr(PERLS).negate(),
            'w' => raw_class_to_expr(PERLW),
            'W' => raw_class_to_expr(PERLW).negate(),
            _ => unreachable!(),
        }
    }

    // Always bump to the next input and return the given expression as a
    // `Build`.
    //
    // This is mostly for convenience when the surrounding context implies
    // that the next character corresponds to the given expression.
    fn parse_one(&mut self, e: Expr) -> Build {
        self.bump();
        Build::Expr(e)
    }
}

// Auxiliary helper methods.
impl Parser {
    fn chars(&self) -> Chars {
        Chars::new(&self.chars[self.chari..], self.flags.ignore_space)
    }

    fn bump(&mut self) -> char {
        let c = self.cur();
        self.chari = checkadd(self.chari, self.chars().next_count());
        c
    }

    fn cur(&self) -> char { self.chars().next().unwrap() }

    fn eof(&self) -> bool { self.chars().next().is_none() }

    fn bump_get<B: Bumpable>(&mut self, s: B) -> Option<String> {
        let n = s.match_end(self);
        if n == 0 {
            None
        } else {
            let end = checkadd(self.chari, n);
            let s = self.chars[self.chari..end]
                        .iter().cloned().collect::<String>();
            self.chari = end;
            Some(s)
        }
    }

    fn bump_if<B: Bumpable>(&mut self, s: B) -> bool {
        let n = s.match_end(self);
        if n == 0 {
            false
        } else {
            self.chari = checkadd(self.chari, n);
            true
        }
    }

    fn peek_is<B: Bumpable>(&self, s: B) -> bool {
        s.match_end(self) > 0
    }

    fn err(&self, kind: ErrorKind) -> Error {
        self.errat(self.chari, kind)
    }

    fn errat(&self, pos: usize, kind: ErrorKind) -> Error {
        Error { pos: pos, surround: self.windowat(pos), kind: kind }
    }

    fn windowat(&self, pos: usize) -> String {
        let s = max(5, pos) - 5;
        let e = min(self.chars.len(), checkadd(pos, 5));
        self.chars[s..e].iter().cloned().collect()
    }

    fn pop(&mut self, expected: ErrorKind) -> Result<Expr> {
        match self.stack.pop() {
            None | Some(Build::LeftParen{..}) => Err(self.err(expected)),
            Some(Build::Expr(e)) => Ok(e),
        }
    }

    // If the current contexts calls for case insensitivity and if the expr
    // given is a character class, do case folding on it and return the new
    // class.
    //
    // Otherwise, return the expression unchanged.
    fn maybe_class_case_fold(&mut self, bexpr: Build) -> Build {
        match bexpr {
            Build::Expr(Expr::Class(cls)) => {
                Build::Expr(Expr::Class(
                    if self.flags.casei && !cls.casei {
                        cls.case_fold()
                    } else {
                        cls
                    }
                ))
            }
            bexpr => bexpr,
        }
    }
}

struct Chars<'a> {
    chars: &'a [char],
    cur: usize,
    ignore_space: bool,
    in_comment: bool,
}

impl<'a> Iterator for Chars<'a> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        self.skip();
        if self.cur < self.chars.len() {
            let c = self.chars[self.cur];
            self.cur = checkadd(self.cur, 1);
            Some(c)
        } else {
            None
        }
    }
}

impl<'a> Chars<'a> {
    fn new(chars: &[char], ignore_space: bool) -> Chars {
        Chars {
            chars: chars,
            cur: 0,
            ignore_space: ignore_space,
            in_comment: false,
        }
    }

    fn skip(&mut self) {
        if !self.ignore_space { return; }
        while self.cur < self.chars.len() {
            if !self.in_comment && self.c() == '#' {
                self.in_comment = true;
            } else if self.in_comment && self.c() == '\n' {
                self.in_comment = false;
            }
            if self.in_comment || self.c().is_whitespace() {
                self.cur = checkadd(self.cur, 1);
            } else {
                break;
            }
        }
    }

    fn c(&self) -> char {
        self.chars[self.cur]
    }

    fn next_count(&mut self) -> usize {
        self.next();
        self.cur
    }
}

// Auxiliary methods for manipulating the expression stack.
impl Parser {
    // Called whenever an alternate (`|`) is found.
    //
    // This pops the expression stack until:
    //
    //  1. The stack is empty. Pushes an alternation with one arm.
    //  2. An opening parenthesis is found. Leave the parenthesis
    //     on the stack and push an alternation with one arm.
    //  3. An alternate (`|`) is found. Pop the existing alternation,
    //     add an arm and push the modified alternation.
    //
    // Each "arm" in the above corresponds to the concatenation of all
    // popped expressions.
    //
    // In the first two cases, the stack is left in an invalid state
    // because an alternation with one arm is not allowed. This
    // particular state will be detected by `finish_concat` and an
    // error will be reported.
    //
    // In none of the cases is an empty arm allowed. If an empty arm
    // is found, an error is reported.
    fn alternate(&mut self) -> Result<Build> {
        let mut concat = vec![];
        let alts = |es| Ok(Build::Expr(Expr::Alternate(es)));
        loop {
            match self.stack.pop() {
                None => {
                    if concat.is_empty() {
                        // e.g., |a
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    return alts(vec![rev_concat(concat)]);
                }
                Some(e @ Build::LeftParen{..}) => {
                    if concat.is_empty() {
                        // e.g., (|a)
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    self.stack.push(e);
                    return alts(vec![rev_concat(concat)]);
                }
                Some(Build::Expr(Expr::Alternate(mut es))) => {
                    if concat.is_empty() {
                        // e.g., a||
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    es.push(rev_concat(concat));
                    return alts(es);
                }
                Some(Build::Expr(e)) => { concat.push(e); }
            }
        }
    }

    // Called whenever a closing parenthesis (`)`) is found.
    //
    // This pops the expression stack until:
    //
    //  1. The stack is empty. An error is reported because this
    //     indicates an unopened parenthesis.
    //  2. An opening parenthesis is found. Pop the opening parenthesis
    //     and push a `Group` expression.
    //  3. An alternate (`|`) is found. Pop the existing alternation
    //     and an arm to it in place. Pop one more item from the stack.
    //     If the stack was empty, then report an unopened parenthesis
    //     error, otherwise assume it is an opening parenthesis and
    //     push a `Group` expression with the popped alternation.
    //     (We can assume this is an opening parenthesis because an
    //     alternation either corresponds to the entire Regex or it
    //     corresponds to an entire group. This is guaranteed by the
    //     `alternate` method.)
    //
    // Each "arm" in the above corresponds to the concatenation of all
    // popped expressions.
    //
    // Empty arms nor empty groups are allowed.
    fn close_paren(&mut self) -> Result<(Flags, Build)> {
        let mut concat = vec![];
        loop {
            match self.stack.pop() {
                // e.g., )
                None => return Err(self.err(ErrorKind::UnopenedParen)),
                Some(Build::LeftParen { i, name, old_flags, .. }) => {
                    if concat.is_empty() {
                        // e.g., ()
                        return Err(self.err(ErrorKind::EmptyGroup));
                    }
                    return Ok((old_flags, Build::Expr(Expr::Group {
                        e: Box::new(rev_concat(concat)),
                        i: i,
                        name: name,
                    })));
                }
                Some(Build::Expr(Expr::Alternate(mut es))) => {
                    if concat.is_empty() {
                        // e.g., (a|)
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    es.push(rev_concat(concat));
                    match self.stack.pop() {
                        // e.g., a|b)
                        None => return Err(self.err(ErrorKind::UnopenedParen)),
                        Some(Build::Expr(_)) => unreachable!(),
                        Some(Build::LeftParen { i, name, old_flags, .. }) => {
                            return Ok((old_flags, Build::Expr(Expr::Group {
                                e: Box::new(Expr::Alternate(es)),
                                i: i,
                                name: name,
                            })));
                        }
                    }
                }
                Some(Build::Expr(e)) => { concat.push(e); }
            }
        }
    }

    // Called only when the parser reaches the end of input.
    //
    // This pops the expression stack until:
    //
    //  1. The stack is empty. Return concatenation of popped
    //     expressions. This concatenation may be empty!
    //  2. An alternation is found. Pop the alternation and push
    //     a new arm. Return the alternation as the entire Regex.
    //
    // If an opening parenthesis is popped, then an error is
    // returned since it indicates an unclosed parenthesis.
    fn finish_concat(&mut self) -> Result<Expr> {
        let mut concat = vec![];
        loop {
            match self.stack.pop() {
                None => { return Ok(rev_concat(concat)); }
                Some(Build::LeftParen{ chari, ..}) => {
                    // e.g., a(b
                    return Err(self.errat(chari, ErrorKind::UnclosedParen));
                }
                Some(Build::Expr(Expr::Alternate(mut es))) => {
                    if concat.is_empty() {
                        // e.g., a|
                        return Err(self.err(ErrorKind::EmptyAlternate));
                    }
                    es.push(rev_concat(concat));
                    return Ok(Expr::Alternate(es));
                }
                Some(Build::Expr(e)) => { concat.push(e); }
            }
        }
    }
}

impl Build {
    fn is_empty(&self) -> bool {
        match *self {
            Build::Expr(Expr::Empty) => true,
            _ => false,
        }
    }
}

// Make it ergonomic to conditionally bump the parser.
// i.e., `bump_if('a')` or `bump_if("abc")`.
trait Bumpable {
    fn match_end(self, p: &Parser) -> usize;
}

impl Bumpable for char {
    fn match_end(self, p: &Parser) -> usize {
        let mut chars = p.chars();
        if chars.next().map(|c| c == self).unwrap_or(false) {
            chars.cur
        } else {
            0
        }
    }
}

impl<'a> Bumpable for &'a str {
    fn match_end(self, p: &Parser) -> usize {
        let mut search = self.chars();
        let mut rest = p.chars();
        let mut count = 0;
        loop {
            match (rest.next(), search.next()) {
                (Some(c1), Some(c2)) if c1 == c2 => count = rest.cur,
                (_, None) => return count,
                _ => return 0,
            }
        }
    }
}

impl<F: FnMut(char) -> bool> Bumpable for F {
    fn match_end(mut self, p: &Parser) -> usize {
        let mut chars = p.chars();
        let mut count = 0;
        while let Some(c) = chars.next() {
            if !self(c) {
                break
            }
            count = chars.cur;
        }
        count
    }
}

// Turn a sequence of expressions into a concatenation.
// This only uses `Concat` if there are 2 or more expressions.
fn rev_concat(mut exprs: Vec<Expr>) -> Expr {
    if exprs.len() == 0 {
        Expr::Empty
    } else if exprs.len() == 1 {
        exprs.pop().unwrap()
    } else {
        exprs.reverse();
        Expr::Concat(exprs)
    }
}

// Returns ture iff the given character is allowed in a capture name.
// Note that the first char of a capture name must not be numeric.
fn is_valid_capture_char(c: char) -> bool {
    c == '_' || (c >= '0' && c <= '9')
    || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

/// Returns true iff the give character has significance in a regex.
#[doc(hidden)]
pub fn is_punct(c: char) -> bool {
    match c {
        '\\' | '.' | '+' | '*' | '?' | '(' | ')' | '|' |
        '[' | ']' | '{' | '}' | '^' | '$' => true,
        _ => false,
    }
}

fn checkadd(x: usize, y: usize) -> usize {
    x.checked_add(y).expect("regex length overflow")
}

fn unicode_class(name: &str) -> Option<CharClass> {
    UNICODE_CLASSES.binary_search_by(|&(s, _)| s.cmp(name)).ok().map(|i| {
        raw_class_to_expr(UNICODE_CLASSES[i].1)
    })
}

fn ascii_class(name: &str) -> Option<CharClass> {
    ASCII_CLASSES.binary_search_by(|&(s, _)| s.cmp(name)).ok().map(|i| {
        raw_class_to_expr(ASCII_CLASSES[i].1)
    })
}

fn raw_class_to_expr(raw: &[(char, char)]) -> CharClass {
    let range = |&(s, e)| ClassRange { start: s, end: e };
    CharClass::new(raw.iter().map(range).collect())
}

type Class = &'static [(char, char)];
type NamedClasses = &'static [(&'static str, Class)];

const ASCII_CLASSES: NamedClasses = &[
    // Classes must be in alphabetical order so that bsearch works.
    // [:alnum:]      alphanumeric (== [0-9A-Za-z])
    // [:alpha:]      alphabetic (== [A-Za-z])
    // [:ascii:]      ASCII (== [\x00-\x7F])
    // [:blank:]      blank (== [\t ])
    // [:cntrl:]      control (== [\x00-\x1F\x7F])
    // [:digit:]      digits (== [0-9])
    // [:graph:]      graphical (== [!-~])
    // [:lower:]      lower case (== [a-z])
    // [:print:]      printable (== [ -~] == [ [:graph:]])
    // [:punct:]      punctuation (== [!-/:-@[-`{-~])
    // [:space:]      whitespace (== [\t\n\v\f\r ])
    // [:upper:]      upper case (== [A-Z])
    // [:word:]       word characters (== [0-9A-Za-z_])
    // [:xdigit:]     hex digit (== [0-9A-Fa-f])
    // Taken from: http://golang.org/pkg/regex/syntax/
    ("alnum", &ALNUM),
    ("alpha", &ALPHA),
    ("ascii", &ASCII),
    ("blank", &BLANK),
    ("cntrl", &CNTRL),
    ("digit", &DIGIT),
    ("graph", &GRAPH),
    ("lower", &LOWER),
    ("print", &PRINT),
    ("punct", &PUNCT),
    ("space", &SPACE),
    ("upper", &UPPER),
    ("word", &WORD),
    ("xdigit", &XDIGIT),
];

const ALNUM: Class = &[('0', '9'), ('A', 'Z'), ('a', 'z')];
const ALPHA: Class = &[('A', 'Z'), ('a', 'z')];
const ASCII: Class = &[('\x00', '\x7F')];
const BLANK: Class = &[(' ', ' '), ('\t', '\t')];
const CNTRL: Class = &[('\x00', '\x1F'), ('\x7F', '\x7F')];
const DIGIT: Class = &[('0', '9')];
const GRAPH: Class = &[('!', '~')];
const LOWER: Class = &[('a', 'z')];
const PRINT: Class = &[(' ', '~')];
const PUNCT: Class = &[('!', '/'), (':', '@'), ('[', '`'), ('{', '~')];
const SPACE: Class = &[('\t', '\t'), ('\n', '\n'), ('\x0B', '\x0B'),
                       ('\x0C', '\x0C'), ('\r', '\r'), (' ', ' ')];
const UPPER: Class = &[('A', 'Z')];
const WORD: Class = &[('0', '9'), ('A', 'Z'), ('a', 'z'), ('_', '_')];
const XDIGIT: Class = &[('0', '9'), ('A', 'F'), ('a', 'f')];

#[cfg(test)]
mod tests {
    use { CharClass, ClassRange, Expr, Repeater, ErrorKind };
    use unicode::regex::{PERLD, PERLS, PERLW};
    use super::Parser;
    use super::{LOWER, UPPER};

    static YI: &'static [(char, char)] = &[
        ('\u{a000}', '\u{a48c}'), ('\u{a490}', '\u{a4c6}'),
    ];

    fn p(s: &str) -> Expr { Parser::parse(s).unwrap() }
    fn lit(c: char) -> Expr { Expr::Literal { chars: vec![c], casei: false } }
    fn liti(c: char) -> Expr { Expr::Literal { chars: vec![c], casei: true } }
    fn b<T>(v: T) -> Box<T> { Box::new(v) }
    fn c(es: &[Expr]) -> Expr { Expr::Concat(es.to_vec()) }

    fn class(ranges: &[(char, char)]) -> CharClass {
        let ranges = ranges.iter().cloned()
                           .map(|(c1, c2)| ClassRange::new(c1, c2)).collect();
        CharClass::new(ranges)
    }

    fn classes(classes: &[&[(char, char)]]) -> CharClass {
        let mut cls = CharClass::empty();
        for &ranges in classes {
            cls.ranges.extend(class(ranges));
        }
        cls.canonicalize()
    }

    #[test]
    fn empty() {
        assert_eq!(p(""), Expr::Empty);
    }

    #[test]
    fn literal() {
        assert_eq!(p("a"), lit('a'));
    }

    #[test]
    fn literal_string() {
        assert_eq!(p("ab"), Expr::Concat(vec![lit('a'), lit('b')]));
    }

    #[test]
    fn start_literal() {
        assert_eq!(p("^a"), Expr::Concat(vec![
            Expr::StartText,
            Expr::Literal { chars: vec!['a'], casei: false },
        ]));
    }

    #[test]
    fn repeat_zero_or_one_greedy() {
        assert_eq!(p("a?"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrOne,
            greedy: true,
        });
    }

    #[test]
    fn repeat_zero_or_one_greedy_concat() {
        assert_eq!(p("ab?"), Expr::Concat(vec![
            lit('a'),
            Expr::Repeat {
                e: b(lit('b')),
                r: Repeater::ZeroOrOne,
                greedy: true,
            },
        ]));
    }

    #[test]
    fn repeat_zero_or_one_nongreedy() {
        assert_eq!(p("a??"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrOne,
            greedy: false,
        });
    }

    #[test]
    fn repeat_one_or_more_greedy() {
        assert_eq!(p("a+"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::OneOrMore,
            greedy: true,
        });
    }

    #[test]
    fn repeat_one_or_more_nongreedy() {
        assert_eq!(p("a+?"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::OneOrMore,
            greedy: false,
        });
    }

    #[test]
    fn repeat_zero_or_more_greedy() {
        assert_eq!(p("a*"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrMore,
            greedy: true,
        });
    }

    #[test]
    fn repeat_zero_or_more_nongreedy() {
        assert_eq!(p("a*?"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrMore,
            greedy: false,
        });
    }

    #[test]
    fn repeat_counted_exact() {
        assert_eq!(p("a{5}"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::Range { min: 5, max: Some(5) },
            greedy: true,
        });
    }

    #[test]
    fn repeat_counted_min() {
        assert_eq!(p("a{5,}"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::Range { min: 5, max: None },
            greedy: true,
        });
    }

    #[test]
    fn repeat_counted_min_max() {
        assert_eq!(p("a{5,10}"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::Range { min: 5, max: Some(10) },
            greedy: true,
        });
    }

    #[test]
    fn repeat_counted_exact_nongreedy() {
        assert_eq!(p("a{5}?"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::Range { min: 5, max: Some(5) },
            greedy: false,
        });
    }

    #[test]
    fn repeat_counted_min_nongreedy() {
        assert_eq!(p("a{5,}?"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::Range { min: 5, max: None },
            greedy: false,
        });
    }

    #[test]
    fn repeat_counted_min_max_nongreedy() {
        assert_eq!(p("a{5,10}?"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::Range { min: 5, max: Some(10) },
            greedy: false,
        });
    }

    #[test]
    fn repeat_counted_whitespace() {
        assert_eq!(p("a{ 5   }"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::Range { min: 5, max: Some(5) },
            greedy: true,
        });
        assert_eq!(p("a{ 5 , 10 }"), Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::Range { min: 5, max: Some(10) },
            greedy: true,
        });
    }

    #[test]
    fn group_literal() {
        assert_eq!(p("(a)"), Expr::Group {
            e: b(lit('a')),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn group_literal_concat() {
        assert_eq!(p("(ab)"), Expr::Group {
            e: b(c(&[lit('a'), lit('b')])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn alt_two() {
        assert_eq!(p("a|b"), Expr::Alternate(vec![lit('a'), lit('b')]));
    }

    #[test]
    fn alt_many() {
        assert_eq!(p("a|b|c"), Expr::Alternate(vec![
            lit('a'), lit('b'), lit('c'),
        ]));
    }

    #[test]
    fn alt_many_concat() {
        assert_eq!(p("ab|bc|cd"), Expr::Alternate(vec![
            c(&[lit('a'), lit('b')]),
            c(&[lit('b'), lit('c')]),
            c(&[lit('c'), lit('d')]),
        ]));
    }

    #[test]
    fn alt_group_two() {
        assert_eq!(p("(a|b)"), Expr::Group {
            e: b(Expr::Alternate(vec![lit('a'), lit('b')])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn alt_group_many() {
        assert_eq!(p("(a|b|c)"), Expr::Group {
            e: b(Expr::Alternate(vec![lit('a'), lit('b'), lit('c')])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn alt_group_many_concat() {
        assert_eq!(p("(ab|bc|cd)"), Expr::Group {
            e: b(Expr::Alternate(vec![
                c(&[lit('a'), lit('b')]),
                c(&[lit('b'), lit('c')]),
                c(&[lit('c'), lit('d')]),
            ])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn alt_group_nested() {
        assert_eq!(p("(ab|(bc|(cd)))"), Expr::Group {
            e: b(Expr::Alternate(vec![
                c(&[lit('a'), lit('b')]),
                Expr::Group {
                    e: b(Expr::Alternate(vec![
                        c(&[lit('b'), lit('c')]),
                        Expr::Group {
                            e: b(c(&[lit('c'), lit('d')])),
                            i: Some(3),
                            name: None,
                        }
                    ])),
                    i: Some(2),
                    name: None,
                },
            ])),
            i: Some(1),
            name: None,
        });
    }

    #[test]
    fn group_name() {
        assert_eq!(p("(?P<foo>a)"), Expr::Group {
            e: b(lit('a')),
            i: Some(1),
            name: Some("foo".into()),
        });
    }

    #[test]
    fn group_no_capture() {
        assert_eq!(p("(?:a)"), Expr::Group {
            e: b(lit('a')),
            i: None,
            name: None,
        });
    }

    #[test]
    fn group_flags() {
        assert_eq!(p("(?i:a)"), Expr::Group {
            e: b(liti('a')),
            i: None,
            name: None,
        });
    }

    #[test]
    fn group_flags_returned() {
        assert_eq!(p("(?i:a)a"), c(&[
            Expr::Group {
                e: b(liti('a')),
                i: None,
                name: None,
            },
            lit('a'),
        ]));
    }

    #[test]
    fn group_flags_retained() {
        assert_eq!(p("(?i)(?-i:a)a"), c(&[
            Expr::Group {
                e: b(lit('a')),
                i: None,
                name: None,
            },
            liti('a'),
        ]));
    }

    #[test]
    fn flags_inline() {
        assert_eq!(p("(?i)a"), liti('a'));
    }

    #[test]
    fn flags_inline_multiple() {
        assert_eq!(p("(?is)a."), c(&[liti('a'), Expr::AnyChar]));
    }

    #[test]
    fn flags_inline_multiline() {
        assert_eq!(p("(?m)^(?-m)$"), c(&[Expr::StartLine, Expr::EndText]));
    }

    #[test]
    fn flags_inline_swap_greed() {
        assert_eq!(p("(?U)a*a*?(?i-U)a*a*?"), c(&[
            Expr::Repeat {
                e: b(lit('a')),
                r: Repeater::ZeroOrMore,
                greedy: false,
            },
            Expr::Repeat {
                e: b(lit('a')),
                r: Repeater::ZeroOrMore,
                greedy: true,
            },
            Expr::Repeat {
                e: b(liti('a')),
                r: Repeater::ZeroOrMore,
                greedy: true,
            },
            Expr::Repeat {
                e: b(liti('a')),
                r: Repeater::ZeroOrMore,
                greedy: false,
            },
        ]));
    }

    #[test]
    fn flags_inline_multiple_negate_one() {
        assert_eq!(p("(?is)a.(?i-s)a."), c(&[
            liti('a'), Expr::AnyChar, liti('a'), Expr::AnyCharNoNL,
        ]));
    }

    #[test]
    fn flags_inline_negate() {
        assert_eq!(p("(?i)a(?-i)a"), c(&[liti('a'), lit('a')]));
    }

    #[test]
    fn flags_group_inline() {
        assert_eq!(p("(a(?i)a)a"), c(&[
            Expr::Group {
                e: b(c(&[lit('a'), liti('a')])),
                i: Some(1),
                name: None,
            },
            lit('a'),
        ]));
    }

    #[test]
    fn flags_group_inline_retain() {
        assert_eq!(p("(?i)((?-i)a)a"), c(&[
            Expr::Group {
                e: b(lit('a')),
                i: Some(1),
                name: None,
            },
            liti('a'),
        ]));
    }

    #[test]
    fn escape_simple() {
        assert_eq!(p(r"\a\f\t\n\r\v"), c(&[
            lit('\x07'), lit('\x0C'), lit('\t'),
            lit('\n'), lit('\r'), lit('\x0B'),
        ]));
    }

    #[test]
    fn escape_boundaries() {
        assert_eq!(p(r"\A\z\b\B"), c(&[
            Expr::StartText, Expr::EndText,
            Expr::WordBoundary, Expr::NotWordBoundary,
        ]));
    }

    #[test]
    fn escape_punctuation() {
        assert_eq!(p(r"\\\.\+\*\?\(\)\|\[\]\{\}\^\$"), c(&[
            lit('\\'), lit('.'), lit('+'), lit('*'), lit('?'),
            lit('('), lit(')'), lit('|'), lit('['), lit(']'),
            lit('{'), lit('}'), lit('^'), lit('$'),
        ]));
    }

    #[test]
    fn escape_octal() {
        assert_eq!(p(r"\123"), lit('S'));
        assert_eq!(p(r"\1234"), c(&[lit('S'), lit('4')]));
    }

    #[test]
    fn escape_hex2() {
        assert_eq!(p(r"\x53"), lit('S'));
        assert_eq!(p(r"\x534"), c(&[lit('S'), lit('4')]));
    }

    #[test]
    fn escape_hex() {
        assert_eq!(p(r"\x{53}"), lit('S'));
        assert_eq!(p(r"\x{53}4"), c(&[lit('S'), lit('4')]));
        assert_eq!(p(r"\x{2603}"), lit('\u{2603}'));
    }

    #[test]
    fn escape_unicode_name() {
        assert_eq!(p(r"\p{Yi}"), Expr::Class(class(YI)));
    }

    #[test]
    fn escape_unicode_letter() {
        assert_eq!(p(r"\pZ"), Expr::Class(class(&[
            ('\u{20}', '\u{20}'), ('\u{a0}', '\u{a0}'),
            ('\u{1680}', '\u{1680}'), ('\u{2000}', '\u{200a}'),
            ('\u{2028}', '\u{2029}'), ('\u{202f}', '\u{202f}'),
            ('\u{205f}', '\u{205f}'), ('\u{3000}', '\u{3000}'),
        ])));
    }

    #[test]
    fn escape_unicode_name_case_fold() {
        assert_eq!(p(r"(?i)\p{Yi}"), Expr::Class(class(YI).case_fold()));
    }

    #[test]
    fn escape_unicode_letter_case_fold() {
        assert_eq!(p(r"(?i)\pZ"), Expr::Class(class(&[
            ('\u{20}', '\u{20}'), ('\u{a0}', '\u{a0}'),
            ('\u{1680}', '\u{1680}'), ('\u{2000}', '\u{200a}'),
            ('\u{2028}', '\u{2029}'), ('\u{202f}', '\u{202f}'),
            ('\u{205f}', '\u{205f}'), ('\u{3000}', '\u{3000}'),
        ]).case_fold()));
    }

    #[test]
    fn escape_unicode_name_negate() {
        assert_eq!(p(r"\P{Yi}"), Expr::Class(class(YI).negate()));
    }

    #[test]
    fn escape_unicode_letter_negate() {
        assert_eq!(p(r"\PZ"), Expr::Class(class(&[
            ('\u{20}', '\u{20}'), ('\u{a0}', '\u{a0}'),
            ('\u{1680}', '\u{1680}'), ('\u{2000}', '\u{200a}'),
            ('\u{2028}', '\u{2029}'), ('\u{202f}', '\u{202f}'),
            ('\u{205f}', '\u{205f}'), ('\u{3000}', '\u{3000}'),
        ]).negate()));
    }

    #[test]
    fn escape_unicode_name_negate_case_fold() {
        assert_eq!(p(r"(?i)\P{Yi}"),
                   Expr::Class(class(YI).negate().case_fold()));
    }

    #[test]
    fn escape_unicode_letter_negate_case_fold() {
        assert_eq!(p(r"(?i)\PZ"), Expr::Class(class(&[
            ('\u{20}', '\u{20}'), ('\u{a0}', '\u{a0}'),
            ('\u{1680}', '\u{1680}'), ('\u{2000}', '\u{200a}'),
            ('\u{2028}', '\u{2029}'), ('\u{202f}', '\u{202f}'),
            ('\u{205f}', '\u{205f}'), ('\u{3000}', '\u{3000}'),
        ]).negate().case_fold()));
    }

    #[test]
    fn escape_perl_d() {
        assert_eq!(p(r"\d"), Expr::Class(class(PERLD)));
    }

    #[test]
    fn escape_perl_s() {
        assert_eq!(p(r"\s"), Expr::Class(class(PERLS)));
    }

    #[test]
    fn escape_perl_w() {
        assert_eq!(p(r"\w"), Expr::Class(class(PERLW)));
    }

    #[test]
    fn escape_perl_d_negate() {
        assert_eq!(p(r"\D"), Expr::Class(class(PERLD).negate()));
    }

    #[test]
    fn escape_perl_s_negate() {
        assert_eq!(p(r"\S"), Expr::Class(class(PERLS).negate()));
    }

    #[test]
    fn escape_perl_w_negate() {
        assert_eq!(p(r"\W"), Expr::Class(class(PERLW).negate()));
    }

    #[test]
    fn escape_perl_d_case_fold() {
        assert_eq!(p(r"(?i)\d"), Expr::Class(class(PERLD).case_fold()));
    }

    #[test]
    fn escape_perl_s_case_fold() {
        assert_eq!(p(r"(?i)\s"), Expr::Class(class(PERLS).case_fold()));
    }

    #[test]
    fn escape_perl_w_case_fold() {
        assert_eq!(p(r"(?i)\w"), Expr::Class(class(PERLW).case_fold()));
    }

    #[test]
    fn escape_perl_d_case_fold_negate() {
        assert_eq!(p(r"(?i)\D"),
                   Expr::Class(class(PERLD).negate().case_fold()));
    }

    #[test]
    fn escape_perl_s_case_fold_negate() {
        assert_eq!(p(r"(?i)\S"),
                   Expr::Class(class(PERLS).negate().case_fold()));
    }

    #[test]
    fn escape_perl_w_case_fold_negate() {
        assert_eq!(p(r"(?i)\W"),
                   Expr::Class(class(PERLW).negate().case_fold()));
    }

    #[test]
    fn class_singleton() {
        assert_eq!(p(r"[a]"), Expr::Class(class(&[('a', 'a')])));
        assert_eq!(p(r"[\x00]"), Expr::Class(class(&[('\x00', '\x00')])));
        assert_eq!(p(r"[\n]"), Expr::Class(class(&[('\n', '\n')])));
        assert_eq!(p("[\n]"), Expr::Class(class(&[('\n', '\n')])));
    }

    #[test]
    fn class_singleton_negate() {
        assert_eq!(p(r"[^a]"), Expr::Class(class(&[
            ('\x00', '\x60'), ('\x62', '\u{10FFFF}'),
        ])));
        assert_eq!(p(r"[^\x00]"), Expr::Class(class(&[
            ('\x01', '\u{10FFFF}'),
        ])));
        assert_eq!(p(r"[^\n]"), Expr::Class(class(&[
            ('\x00', '\x09'), ('\x0b', '\u{10FFFF}'),
        ])));
        assert_eq!(p("[^\n]"), Expr::Class(class(&[
            ('\x00', '\x09'), ('\x0b', '\u{10FFFF}'),
        ])));
    }

    #[test]
    fn class_singleton_class() {
        assert_eq!(p(r"[\d]"), Expr::Class(class(PERLD)));
        assert_eq!(p(r"[\p{Yi}]"), Expr::Class(class(YI)));
    }

    #[test]
    fn class_singleton_class_negate() {
        assert_eq!(p(r"[^\d]"), Expr::Class(class(PERLD).negate()));
        assert_eq!(p(r"[^\w]"), Expr::Class(class(PERLW).negate()));
        assert_eq!(p(r"[^\s]"), Expr::Class(class(PERLS).negate()));
    }

    #[test]
    fn class_singleton_class_negate_negate() {
        assert_eq!(p(r"[^\D]"), Expr::Class(class(PERLD)));
        assert_eq!(p(r"[^\W]"), Expr::Class(class(PERLW)));
        assert_eq!(p(r"[^\S]"), Expr::Class(class(PERLS)));
    }

    #[test]
    fn class_singleton_class_casei() {
        assert_eq!(p(r"(?i)[\d]"), Expr::Class(class(PERLD).case_fold()));
        assert_eq!(p(r"(?i)[\p{Yi}]"), Expr::Class(class(YI).case_fold()));
    }

    #[test]
    fn class_singleton_class_negate_casei() {
        assert_eq!(p(r"(?i)[^\d]"),
                   Expr::Class(class(PERLD).negate().case_fold()));
        assert_eq!(p(r"(?i)[^\w]"),
                   Expr::Class(class(PERLW).negate().case_fold()));
        assert_eq!(p(r"(?i)[^\s]"),
                   Expr::Class(class(PERLS).negate().case_fold()));
    }

    #[test]
    fn class_singleton_class_negate_negate_casei() {
        assert_eq!(p(r"(?i)[^\D]"), Expr::Class(class(PERLD).case_fold()));
        assert_eq!(p(r"(?i)[^\W]"), Expr::Class(class(PERLW).case_fold()));
        assert_eq!(p(r"(?i)[^\S]"), Expr::Class(class(PERLS).case_fold()));
    }

    #[test]
    fn class_multiple_class() {
        assert_eq!(p(r"[\d\p{Yi}]"), Expr::Class(classes(&[
            PERLD, YI,
        ])));
    }

    #[test]
    fn class_multiple_class_negate() {
        assert_eq!(p(r"[^\d\p{Yi}]"), Expr::Class(classes(&[
            PERLD, YI,
        ]).negate()));
    }

    #[test]
    fn class_multiple_class_negate_negate() {
        let nperld = class(PERLD).negate();
        let nyi = class(YI).negate();
        let cls = CharClass::empty().merge(nperld).merge(nyi);
        assert_eq!(p(r"[^\D\P{Yi}]"), Expr::Class(cls.negate()));
    }

    #[test]
    fn class_multiple_class_casei() {
        assert_eq!(p(r"(?i)[\d\p{Yi}]"), Expr::Class(classes(&[
            PERLD, YI,
        ]).case_fold()));
    }

    #[test]
    fn class_multiple_class_negate_casei() {
        assert_eq!(p(r"(?i)[^\d\p{Yi}]"), Expr::Class(classes(&[
            PERLD, YI,
        ]).negate().case_fold()));
    }

    #[test]
    fn class_multiple_class_negate_negate_casei() {
        let nperld = class(PERLD).negate();
        let nyi = class(YI).negate();
        let class = CharClass::empty().merge(nperld).merge(nyi);
        assert_eq!(p(r"(?i)[^\D\P{Yi}]"),
                   Expr::Class(class.negate().case_fold()));
    }

    #[test]
    fn class_class_hypen() {
        assert_eq!(p(r"[\p{Yi}-]"), Expr::Class(classes(&[
            &[('-', '-')], YI,
        ])));
        assert_eq!(p(r"[\p{Yi}-a]"), Expr::Class(classes(&[
            &[('-', '-')], &[('a', 'a')], YI,
        ])));
    }

    #[test]
    fn class_brackets() {
        assert_eq!(p("[]]"), Expr::Class(class(&[(']', ']')])));
        assert_eq!(p("[][]"), Expr::Class(class(&[('[', '['), (']', ']')])));
        assert_eq!(p("[[]]"), Expr::Concat(vec![
            Expr::Class(class(&[('[', '[')])),
            lit(']'),
        ]));
    }

    #[test]
    fn class_brackets_hypen() {
        assert_eq!(p("[]-]"), Expr::Class(class(&[('-', '-'), (']', ']')])));
        assert_eq!(p("[-]]"), Expr::Concat(vec![
            Expr::Class(class(&[('-', '-')])),
            lit(']'),
        ]));
    }

    #[test]
    fn class_overlapping() {
        assert_eq!(p("[a-fd-h]"), Expr::Class(class(&[('a', 'h')])));
        assert_eq!(p("[a-fg-m]"), Expr::Class(class(&[('a', 'm')])));
    }

    #[test]
    fn ascii_class() {
        assert_eq!(p("[:upper:]"), Expr::Class(class(UPPER)));
        assert_eq!(p("[[:upper:]]"), Expr::Class(class(UPPER)));
    }

    #[test]
    fn ascii_class_not() {
        assert_eq!(p("[:abc:]"),
                   Expr::Class(class(&[(':', ':'), ('a', 'c')])));
    }

    #[test]
    fn ascii_class_multiple() {
        assert_eq!(p("[[:lower:][:upper:]]"),
                   Expr::Class(classes(&[UPPER, LOWER])));
    }

    #[test]
    fn ascii_class_negate() {
        assert_eq!(p("[[:^upper:]]"), Expr::Class(class(UPPER).negate()));
        assert_eq!(p("[^[:^upper:]]"), Expr::Class(class(UPPER)));
    }

    #[test]
    fn ascii_class_negate_multiple() {
        let (nlower, nupper) = (class(LOWER).negate(), class(UPPER).negate());
        let cls = CharClass::empty().merge(nlower).merge(nupper);
        assert_eq!(p("[[:^lower:][:^upper:]]"), Expr::Class(cls.clone()));
        assert_eq!(p("[^[:^lower:][:^upper:]]"), Expr::Class(cls.negate()));
    }

    #[test]
    fn ascii_class_case_fold() {
        assert_eq!(p("(?i)[:upper:]"), Expr::Class(class(UPPER).case_fold()));
        assert_eq!(p("(?i)[[:upper:]]"),
                   Expr::Class(class(UPPER).case_fold()));
    }

    #[test]
    fn ascii_class_negate_case_fold() {
        assert_eq!(p("(?i)[[:^upper:]]"),
                   Expr::Class(class(UPPER).negate().case_fold()));
        assert_eq!(p("(?i)[^[:^upper:]]"),
                   Expr::Class(class(UPPER).case_fold()));
    }

    #[test]
    fn ignore_space_literal() {
        assert_eq!(p("(?x) a b c"), Expr::Concat(vec![
            lit('a'), lit('b'), lit('c'),
        ]));
    }

    #[test]
    fn ignore_space_literal_off() {
        assert_eq!(p("(?x) a b c(?-x) a"), Expr::Concat(vec![
            lit('a'), lit('b'), lit('c'), lit(' '), lit('a'),
        ]));
    }

    #[test]
    fn ignore_space_class() {
        assert_eq!(p("(?x)[a
        - z
]"), Expr::Class(class(&[('a', 'z')])));
        assert_eq!(p("(?x)[  ^   a
        - z
]"), Expr::Class(class(&[('a', 'z')]).negate()));
    }

    #[test]
    fn ignore_space_escape() {
        assert_eq!(p(r"(?x)\ d"), Expr::Class(class(PERLD)));
        assert_eq!(p(r"(?x)\
                     D"), Expr::Class(class(PERLD).negate()));
    }

    #[test]
    fn ignore_space_comments() {
        assert_eq!(p(r"(?x)(?P<foo>
    a # comment 1
)(?P<bar>
    z # comment 2
)"), Expr::Concat(vec![
        Expr::Group {
            e: Box::new(lit('a')),
            i: Some(1),
            name: Some("foo".into()),
        },
        Expr::Group {
            e: Box::new(lit('z')),
            i: Some(2),
            name: Some("bar".into()),
        },
    ]));
    }

    #[test]
    fn ignore_space_comments_re_enable() {
        assert_eq!(p(r"(?x)a # hi
(?-x:#) # sweet"), Expr::Concat(vec![
            lit('a'),
            Expr::Group {
                e: Box::new(lit('#')),
                i: None,
                name: None,
            },
        ]));
    }

    // Test every single possible error case.

    macro_rules! test_err {
        ($re:expr, $pos:expr, $kind:expr) => {{
            let err = Parser::parse($re).unwrap_err();
            assert_eq!($pos, err.pos);
            assert_eq!($kind, err.kind);
            assert!($re.contains(&err.surround));
        }}
    }

    #[test]
    fn error_repeat_no_expr_simple() {
        test_err!("(*", 1, ErrorKind::RepeaterExpectsExpr);
    }

    #[test]
    fn error_repeat_no_expr_counted() {
        test_err!("({5}", 1, ErrorKind::RepeaterExpectsExpr);
    }

    #[test]
    fn error_repeat_beginning_counted() {
        test_err!("{5}", 0, ErrorKind::RepeaterExpectsExpr);
    }

    #[test]
    fn error_repeat_illegal_exprs_simple() {
        test_err!("a**", 2, ErrorKind::RepeaterUnexpectedExpr(Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrMore,
            greedy: true,
        }));
        test_err!("a|*", 2,
            ErrorKind::RepeaterUnexpectedExpr(Expr::Alternate(vec![lit('a')]))
        );
    }

    #[test]
    fn error_repeat_illegal_exprs_counted() {
        test_err!("a*{5}", 2, ErrorKind::RepeaterUnexpectedExpr(Expr::Repeat {
            e: b(lit('a')),
            r: Repeater::ZeroOrMore,
            greedy: true,
        }));
        test_err!("a|{5}", 2,
            ErrorKind::RepeaterUnexpectedExpr(Expr::Alternate(vec![lit('a')]))
        );
    }

    #[test]
    fn error_repeat_empty_number() {
        test_err!("a{}", 2, ErrorKind::MissingBase10);
    }

    #[test]
    fn error_repeat_eof() {
        test_err!("a{5", 3, ErrorKind::UnclosedRepeat);
    }

    #[test]
    fn error_repeat_empty_number_eof() {
        test_err!("a{xyz", 5, ErrorKind::InvalidBase10("xyz".into()));
        test_err!("a{12,xyz", 8, ErrorKind::InvalidBase10("xyz".into()));
    }

    #[test]
    fn error_repeat_invalid_number() {
        test_err!("a{9999999999}", 12,
                  ErrorKind::InvalidBase10("9999999999".into()));
        test_err!("a{1,9999999999}", 14,
                  ErrorKind::InvalidBase10("9999999999".into()));
    }

    #[test]
    fn error_repeat_invalid_number_extra() {
        test_err!("a{12x}", 5, ErrorKind::InvalidBase10("12x".into()));
        test_err!("a{1,12x}", 7, ErrorKind::InvalidBase10("12x".into()));
    }

    #[test]
    fn error_repeat_invalid_range() {
        test_err!("a{2,1}", 5,
                  ErrorKind::InvalidRepeatRange { min: 2, max: 1 });
    }

    #[test]
    fn error_alternate_empty() {
        test_err!("|a", 0, ErrorKind::EmptyAlternate);
    }

    #[test]
    fn error_alternate_empty_with_group() {
        test_err!("(|a)", 1, ErrorKind::EmptyAlternate);
    }

    #[test]
    fn error_alternate_empty_with_alternate() {
        test_err!("a||", 2, ErrorKind::EmptyAlternate);
    }

    #[test]
    fn error_close_paren_unopened_empty() {
        test_err!(")", 0, ErrorKind::UnopenedParen);
    }

    #[test]
    fn error_close_paren_unopened() {
        test_err!("ab)", 2, ErrorKind::UnopenedParen);
    }

    #[test]
    fn error_close_paren_unopened_with_alt() {
        test_err!("a|b)", 3, ErrorKind::UnopenedParen);
    }

    #[test]
    fn error_close_paren_empty_alt() {
        test_err!("(a|)", 3, ErrorKind::EmptyAlternate);
    }

    #[test]
    fn error_close_paren_empty_group() {
        test_err!("()", 1, ErrorKind::EmptyGroup);
    }

    #[test]
    fn error_close_paren_empty_group_with_name() {
        test_err!("(?P<foo>)", 8, ErrorKind::EmptyGroup);
    }

    #[test]
    fn error_finish_concat_unclosed() {
        test_err!("ab(xy", 2, ErrorKind::UnclosedParen);
    }

    #[test]
    fn error_finish_concat_empty_alt() {
        test_err!("a|", 2, ErrorKind::EmptyAlternate);
    }

    #[test]
    fn error_group_name_invalid() {
        test_err!("(?P<a#>x)", 6, ErrorKind::InvalidCaptureName("a#".into()));
    }

    #[test]
    fn error_group_name_invalid_leading() {
        test_err!("(?P<1a>a)", 6, ErrorKind::InvalidCaptureName("1a".into()));
    }

    #[test]
    fn error_group_name_unexpected_eof() {
        test_err!("(?P<a", 5, ErrorKind::UnclosedCaptureName("a".into()));
    }

    #[test]
    fn error_group_name_empty() {
        test_err!("(?P<>a)", 4, ErrorKind::EmptyCaptureName);
    }

    #[test]
    fn error_group_opts_unrecognized_flag() {
        test_err!("(?z:a)", 2, ErrorKind::UnrecognizedFlag('z'));
    }

    #[test]
    fn error_group_opts_unexpected_eof() {
        test_err!("(?i", 3, ErrorKind::UnexpectedFlagEof);
    }

    #[test]
    fn error_group_opts_double_negation() {
        test_err!("(?-i-s:a)", 4, ErrorKind::DoubleFlagNegation);
    }

    #[test]
    fn error_group_opts_empty_negation() {
        test_err!("(?i-:a)", 4, ErrorKind::EmptyFlagNegation);
    }

    #[test]
    fn error_group_opts_empty() {
        test_err!("(?)", 2, ErrorKind::EmptyFlagNegation);
    }

    #[test]
    fn error_escape_unexpected_eof() {
        test_err!(r"\", 1, ErrorKind::UnexpectedEscapeEof);
    }

    #[test]
    fn error_escape_unrecognized() {
        test_err!(r"\m", 1, ErrorKind::UnrecognizedEscape('m'));
    }

    #[test]
    fn error_escape_hex2_eof0() {
        test_err!(r"\x", 2, ErrorKind::UnexpectedTwoDigitHexEof);
    }

    #[test]
    fn error_escape_hex2_eof1() {
        test_err!(r"\xA", 3, ErrorKind::UnexpectedTwoDigitHexEof);
    }

    #[test]
    fn error_escape_hex2_invalid() {
        test_err!(r"\xAG", 4, ErrorKind::InvalidBase16("AG".into()));
    }

    #[test]
    fn error_escape_hex_eof0() {
        test_err!(r"\x{", 3, ErrorKind::InvalidBase16("".into()));
    }

    #[test]
    fn error_escape_hex_eof1() {
        test_err!(r"\x{A", 4, ErrorKind::UnclosedHex);
    }

    #[test]
    fn error_escape_hex_invalid() {
        test_err!(r"\x{AG}", 5, ErrorKind::InvalidBase16("AG".into()));
    }

    #[test]
    fn error_escape_hex_invalid_scalar_value_surrogate() {
        test_err!(r"\x{D800}", 7, ErrorKind::InvalidScalarValue(0xD800));
    }

    #[test]
    fn error_escape_hex_invalid_scalar_value_high() {
        test_err!(r"\x{110000}", 9, ErrorKind::InvalidScalarValue(0x110000));
    }

    #[test]
    fn error_escape_hex_invalid_u32() {
        test_err!(r"\x{9999999999}", 13,
                  ErrorKind::InvalidBase16("9999999999".into()));
    }

    #[test]
    fn error_unicode_unclosed() {
        test_err!(r"\p{", 3, ErrorKind::UnclosedUnicodeName);
        test_err!(r"\p{Greek", 8, ErrorKind::UnclosedUnicodeName);
    }

    #[test]
    fn error_unicode_no_letter() {
        test_err!(r"\p", 2, ErrorKind::UnexpectedEscapeEof);
    }

    #[test]
    fn error_unicode_unknown_letter() {
        test_err!(r"\pA", 3, ErrorKind::UnrecognizedUnicodeClass("A".into()));
    }

    #[test]
    fn error_unicode_unknown_name() {
        test_err!(r"\p{Yii}", 7,
                  ErrorKind::UnrecognizedUnicodeClass("Yii".into()));
    }

    #[test]
    fn error_class_eof_empty() {
        test_err!("[", 1, ErrorKind::UnexpectedClassEof);
        test_err!("[^", 2, ErrorKind::UnexpectedClassEof);
    }

    #[test]
    fn error_class_eof_non_empty() {
        test_err!("[a", 2, ErrorKind::UnexpectedClassEof);
        test_err!("[^a", 3, ErrorKind::UnexpectedClassEof);
    }

    #[test]
    fn error_class_eof_range() {
        test_err!("[a-", 3, ErrorKind::UnexpectedClassEof);
        test_err!("[^a-", 4, ErrorKind::UnexpectedClassEof);
        test_err!("[---", 4, ErrorKind::UnexpectedClassEof);
    }

    #[test]
    fn error_class_invalid_escape() {
        test_err!(r"[\pA]", 4,
                  ErrorKind::UnrecognizedUnicodeClass("A".into()));
    }

    #[test]
    fn error_class_valid_escape_not_allowed() {
        test_err!(r"[\A]", 3, ErrorKind::InvalidClassEscape(Expr::StartText));
    }

    #[test]
    fn error_class_range_valid_escape_not_allowed() {
        test_err!(r"[a-\d]", 5,
                  ErrorKind::InvalidClassEscape(Expr::Class(class(PERLD))));
        test_err!(r"[a-\A]", 5,
                  ErrorKind::InvalidClassEscape(Expr::StartText));
        test_err!(r"[\A-a]", 3,
                  ErrorKind::InvalidClassEscape(Expr::StartText));
    }

    #[test]
    fn error_class_invalid_range() {
        test_err!("[z-a]", 4, ErrorKind::InvalidClassRange {
            start: 'z',
            end: 'a',
        });
    }

    #[test]
    fn error_class_empty_range() {
        test_err!("[]", 2, ErrorKind::UnexpectedClassEof);
        test_err!("[^]", 3, ErrorKind::UnexpectedClassEof);
    }

    #[test]
    fn error_duplicate_capture_name() {
        test_err!("(?P<a>.)(?P<a>.)", 14,
                  ErrorKind::DuplicateCaptureName("a".into()));
    }
}
