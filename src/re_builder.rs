use regex_automata::util::syntax;

/// The set of user configurable options for compiling zero or more regexes.
/// This is shared among all top-level regex APIs.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
struct RegexOptions {
    pats: Vec<String>,
    size_limit: usize,
    dfa_size_limit: usize,
    line_terminator: u8,
    syntax: syntax::Config,
}

impl Default for RegexOptions {
    fn default() -> Self {
        RegexOptions {
            pats: vec![],
            size_limit: 10 * (1 << 20),
            dfa_size_limit: 2 * (1 << 20),
            line_terminator: b'\n',
            syntax: syntax::Config::default(),
        }
    }
}

macro_rules! define_builder {
    ($name:ident, $regex_mod:ident, $utf8:expr) => {
        pub mod $name {
            use std::sync::Arc;

            use regex_automata::meta;

            use crate::{error::Error, $regex_mod::Regex};

            use super::RegexOptions;

            /// A configurable builder for a regular expression.
            ///
            /// A builder can be used to configure how the regex is built, for example, by
            /// setting the default flags (which can be overridden in the expression
            /// itself) or setting various limits.
            #[derive(Debug)]
            pub struct RegexBuilder(RegexOptions);

            impl RegexBuilder {
                /// Create a new regular expression builder with the given pattern.
                ///
                /// If the pattern is invalid, then an error will be returned when
                /// `build` is called.
                pub fn new(pattern: &str) -> RegexBuilder {
                    let mut builder = RegexBuilder(RegexOptions::default());
                    builder.0.pats.push(pattern.to_owned());
                    builder
                }

                /// Consume the builder and compile the regular expression.
                ///
                /// Note that calling `as_str` on the resulting `Regex` will produce the
                /// pattern given to `new` verbatim. Notably, it will not incorporate any
                /// of the flags set on this builder.
                pub fn build(&self) -> Result<Regex, Error> {
                    let config = meta::Config::new()
                        .match_kind(regex_automata::MatchKind::LeftmostFirst)
                        .utf8_empty($utf8)
                        .line_terminator(self.0.line_terminator)
                        .nfa_size_limit(Some(self.0.size_limit))
                        .hybrid_cache_capacity(self.0.dfa_size_limit);
                    meta::Builder::new()
                        .configure(config)
                        .syntax(self.0.syntax.clone().utf8($utf8))
                        .build(&self.0.pats[0])
                        .map(|meta| Regex {
                            meta,
                            pattern: Arc::from(self.0.pats[0].as_str()),
                        })
                        .map_err(Error::from_meta_build_error)
                }

                /// Set the value for the case insensitive (`i`) flag.
                ///
                /// When enabled, letters in the pattern will match both upper case and
                /// lower case variants.
                pub fn case_insensitive(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexBuilder {
                    self.0.syntax = self.0.syntax.case_insensitive(yes);
                    self
                }

                /// Set the value for the multi-line matching (`m`) flag.
                ///
                /// When enabled, `^` matches the beginning of lines and `$` matches the
                /// end of lines.
                ///
                /// By default, they match beginning/end of the input.
                pub fn multi_line(&mut self, yes: bool) -> &mut RegexBuilder {
                    self.0.syntax = self.0.syntax.multi_line(yes);
                    self
                }

                /// Set the value for the any character (`s`) flag, where in `.` matches
                /// anything when `s` is set and matches anything except for new line when
                /// it is not set (the default).
                ///
                /// N.B. "matches anything" means "any byte" when Unicode is disabled and
                /// means "any valid UTF-8 encoding of any Unicode scalar value" when
                /// Unicode is enabled.
                pub fn dot_matches_new_line(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexBuilder {
                    self.0.syntax = self.0.syntax.dot_matches_new_line(yes);
                    self
                }

                /// Enable or disable the CRLF mode flag by default.
                ///
                /// By default this is disabled. It may alternatively be selectively
                /// enabled in the regular expression itself via the `R` flag.
                ///
                /// When CRLF mode is enabled, the following happens:
                ///
                /// * Unless `dot_matches_new_line` is enabled, `.` will match any character
                /// except for `\r` and `\n`.
                /// * When `multi_line` mode is enabled, `^` and `$` will treat `\r\n`,
                /// `\r` and `\n` as line terminators. And in particular, neither will
                /// match between a `\r` and a `\n`.
                pub fn crlf(&mut self, yes: bool) -> &mut RegexBuilder {
                    self.0.syntax = self.0.syntax.crlf(yes);
                    self
                }

                /// Set the line terminator to be used by the `^` and `$` anchors in
                /// multi-line mode.
                ///
                /// This option has no effect when CRLF mode is enabled. That is,
                /// regardless of this setting, `(?Rm:^)` and `(?Rm:$)` will always treat
                /// `\r` and `\n` as line terminators (and will never match between a `\r`
                /// and a `\n`).
                ///
                /// By default, `\n` is the line terminator.
                pub fn line_terminator(
                    &mut self,
                    byte: u8,
                ) -> &mut RegexBuilder {
                    self.0.line_terminator = byte;
                    self
                }

                /// Set the value for the greedy swap (`U`) flag.
                ///
                /// When enabled, a pattern like `a*` is lazy (tries to find shortest
                /// match) and `a*?` is greedy (tries to find longest match).
                ///
                /// By default, `a*` is greedy and `a*?` is lazy.
                pub fn swap_greed(&mut self, yes: bool) -> &mut RegexBuilder {
                    self.0.syntax = self.0.syntax.swap_greed(yes);
                    self
                }

                /// Set the value for the ignore whitespace (`x`) flag.
                ///
                /// When enabled, whitespace such as new lines and spaces will be ignored
                /// between expressions of the pattern, and `#` can be used to start a
                /// comment until the next new line.
                pub fn ignore_whitespace(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexBuilder {
                    self.0.syntax = self.0.syntax.ignore_whitespace(yes);
                    self
                }

                /// Set the value for the Unicode (`u`) flag.
                ///
                /// Enabled by default. When disabled, character classes such as `\w` only
                /// match ASCII word characters instead of all Unicode word characters.
                pub fn unicode(&mut self, yes: bool) -> &mut RegexBuilder {
                    self.0.syntax = self.0.syntax.unicode(yes);
                    self
                }

                /// Whether to support octal syntax or not.
                ///
                /// Octal syntax is a little-known way of uttering Unicode codepoints in
                /// a regular expression. For example, `a`, `\x61`, `\u0061` and
                /// `\141` are all equivalent regular expressions, where the last example
                /// shows octal syntax.
                ///
                /// While supporting octal syntax isn't in and of itself a problem, it does
                /// make good error messages harder. That is, in PCRE based regex engines,
                /// syntax like `\0` invokes a backreference, which is explicitly
                /// unsupported in Rust's regex engine. However, many users expect it to
                /// be supported. Therefore, when octal support is disabled, the error
                /// message will explicitly mention that backreferences aren't supported.
                ///
                /// Octal syntax is disabled by default.
                pub fn octal(&mut self, yes: bool) -> &mut RegexBuilder {
                    self.0.syntax = self.0.syntax.octal(yes);
                    self
                }

                /// Set the approximate size limit of the compiled regular expression.
                ///
                /// This roughly corresponds to the number of bytes occupied by a single
                /// compiled program. If the program exceeds this number, then a
                /// compilation error is returned.
                pub fn size_limit(
                    &mut self,
                    limit: usize,
                ) -> &mut RegexBuilder {
                    self.0.size_limit = limit;
                    self
                }

                /// Set the approximate size of the cache used by the DFA.
                ///
                /// This roughly corresponds to the number of bytes that the DFA will
                /// use while searching.
                ///
                /// Note that this is a *per thread* limit. There is no way to set a global
                /// limit. In particular, if a regex is used from multiple threads
                /// simultaneously, then each thread may use up to the number of bytes
                /// specified here.
                pub fn dfa_size_limit(
                    &mut self,
                    limit: usize,
                ) -> &mut RegexBuilder {
                    self.0.dfa_size_limit = limit;
                    self
                }

                /// Set the nesting limit for this parser.
                ///
                /// The nesting limit controls how deep the abstract syntax tree is allowed
                /// to be. If the AST exceeds the given limit (e.g., with too many nested
                /// groups), then an error is returned by the parser.
                ///
                /// The purpose of this limit is to act as a heuristic to prevent stack
                /// overflow for consumers that do structural induction on an `Ast` using
                /// explicit recursion. While this crate never does this (instead using
                /// constant stack space and moving the call stack to the heap), other
                /// crates may.
                ///
                /// This limit is not checked until the entire Ast is parsed. Therefore,
                /// if callers want to put a limit on the amount of heap space used, then
                /// they should impose a limit on the length, in bytes, of the concrete
                /// pattern string. In particular, this is viable since this parser
                /// implementation will limit itself to heap space proportional to the
                /// length of the pattern string.
                ///
                /// Note that a nest limit of `0` will return a nest limit error for most
                /// patterns but not all. For example, a nest limit of `0` permits `a` but
                /// not `ab`, since `ab` requires a concatenation, which results in a nest
                /// depth of `1`. In general, a nest limit is not something that manifests
                /// in an obvious way in the concrete syntax, therefore, it should not be
                /// used in a granular way.
                pub fn nest_limit(&mut self, limit: u32) -> &mut RegexBuilder {
                    self.0.syntax.nest_limit(limit);
                    self
                }
            }
        }
    };
}

define_builder!(bytes, re_bytes, false);
define_builder!(unicode, re_unicode, true);

macro_rules! define_set_builder {
    ($name:ident, $regex_mod:ident, $utf8:expr) => {
        pub mod $name {
            use std::sync::Arc;

            use regex_automata::meta;

            use crate::{error::Error, re_set::$regex_mod::RegexSet};

            use super::RegexOptions;

            /// A configurable builder for a set of regular expressions.
            ///
            /// A builder can be used to configure how the regexes are built, for example,
            /// by setting the default flags (which can be overridden in the expression
            /// itself) or setting various limits.
            #[derive(Debug)]
            pub struct RegexSetBuilder(RegexOptions);

            impl RegexSetBuilder {
                /// Create a new regular expression builder with the given pattern.
                ///
                /// If the pattern is invalid, then an error will be returned when
                /// `build` is called.
                pub fn new<I, S>(patterns: I) -> RegexSetBuilder
                where
                    S: AsRef<str>,
                    I: IntoIterator<Item = S>,
                {
                    let mut builder = RegexSetBuilder(RegexOptions::default());
                    for pat in patterns {
                        builder.0.pats.push(pat.as_ref().to_owned());
                    }
                    builder
                }

                /// Consume the builder and compile the regular expressions into a set.
                pub fn build(&self) -> Result<RegexSet, Error> {
                    let config = meta::Config::new()
                        .match_kind(regex_automata::MatchKind::All)
                        .utf8_empty($utf8)
                        .line_terminator(self.0.line_terminator)
                        .nfa_size_limit(Some(self.0.size_limit))
                        .hybrid_cache_capacity(self.0.dfa_size_limit);
                    meta::Builder::new()
                        .configure(config)
                        .syntax(self.0.syntax.clone().utf8($utf8))
                        .build_many(&self.0.pats)
                        .map(|meta| RegexSet {
                            meta,
                            patterns: Arc::from(&*self.0.pats),
                        })
                        .map_err(Error::from_meta_build_error)
                }

                /// Set the value for the case insensitive (`i`) flag.
                pub fn case_insensitive(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder {
                    self.0.syntax = self.0.syntax.case_insensitive(yes);
                    self
                }

                /// Set the value for the multi-line matching (`m`) flag.
                pub fn multi_line(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder {
                    self.0.syntax = self.0.syntax.multi_line(yes);
                    self
                }

                /// Set the value for the any character (`s`) flag, where in `.` matches
                /// anything when `s` is set and matches anything except for new line when
                /// it is not set (the default).
                ///
                /// N.B. "matches anything" means "any byte" for `regex::bytes::RegexSet`
                /// expressions and means "any Unicode scalar value" for `regex::RegexSet`
                /// expressions.
                pub fn dot_matches_new_line(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder {
                    self.0.syntax = self.0.syntax.dot_matches_new_line(yes);
                    self
                }

                /// Enable or disable the CRLF mode flag by default.
                ///
                /// By default this is disabled. It may alternatively be selectively
                /// enabled in the regular expression itself via the `R` flag.
                ///
                /// When CRLF mode is enabled, the following happens:
                ///
                /// * Unless `dot_matches_new_line` is enabled, `.` will match any character
                /// except for `\r` and `\n`.
                /// * When `multi_line` mode is enabled, `^` and `$` will treat `\r\n`,
                /// `\r` and `\n` as line terminators. And in particular, neither will
                /// match between a `\r` and a `\n`.
                pub fn crlf(&mut self, yes: bool) -> &mut RegexSetBuilder {
                    self.0.syntax = self.0.syntax.crlf(yes);
                    self
                }

                /// Set the line terminator to be used by the `^` and `$` anchors in
                /// multi-line mode.
                ///
                /// This option has no effect when CRLF mode is enabled. That is,
                /// regardless of this setting, `(?Rm:^)` and `(?Rm:$)` will always treat
                /// `\r` and `\n` as line terminators (and will never match between a `\r`
                /// and a `\n`).
                ///
                /// By default, `\n` is the line terminator.
                pub fn line_terminator(
                    &mut self,
                    byte: u8,
                ) -> &mut RegexSetBuilder {
                    self.0.line_terminator = byte;
                    self
                }

                /// Set the value for the greedy swap (`U`) flag.
                pub fn swap_greed(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder {
                    self.0.syntax = self.0.syntax.swap_greed(yes);
                    self
                }

                /// Set the value for the ignore whitespace (`x`) flag.
                pub fn ignore_whitespace(
                    &mut self,
                    yes: bool,
                ) -> &mut RegexSetBuilder {
                    self.0.syntax = self.0.syntax.ignore_whitespace(yes);
                    self
                }

                /// Set the value for the Unicode (`u`) flag.
                pub fn unicode(&mut self, yes: bool) -> &mut RegexSetBuilder {
                    self.0.syntax = self.0.syntax.unicode(yes);
                    self
                }

                /// Whether to support octal syntax or not.
                ///
                /// Octal syntax is a little-known way of uttering Unicode codepoints in
                /// a regular expression. For example, `a`, `\x61`, `\u0061` and
                /// `\141` are all equivalent regular expressions, where the last example
                /// shows octal syntax.
                ///
                /// While supporting octal syntax isn't in and of itself a problem, it does
                /// make good error messages harder. That is, in PCRE based regex engines,
                /// syntax like `\0` invokes a backreference, which is explicitly
                /// unsupported in Rust's regex engine. However, many users expect it to
                /// be supported. Therefore, when octal support is disabled, the error
                /// message will explicitly mention that backreferences aren't supported.
                ///
                /// Octal syntax is disabled by default.
                pub fn octal(&mut self, yes: bool) -> &mut RegexSetBuilder {
                    self.0.syntax = self.0.syntax.octal(yes);
                    self
                }

                /// Set the approximate size limit of the compiled regular expression.
                ///
                /// This roughly corresponds to the number of bytes occupied by a single
                /// compiled program. If the program exceeds this number, then a
                /// compilation error is returned.
                pub fn size_limit(
                    &mut self,
                    limit: usize,
                ) -> &mut RegexSetBuilder {
                    self.0.size_limit = limit;
                    self
                }

                /// Set the approximate size of the cache used by the DFA.
                ///
                /// This roughly corresponds to the number of bytes that the DFA will
                /// use while searching.
                ///
                /// Note that this is a *per thread* limit. There is no way to set a global
                /// limit. In particular, if a regex is used from multiple threads
                /// simultaneously, then each thread may use up to the number of bytes
                /// specified here.
                pub fn dfa_size_limit(
                    &mut self,
                    limit: usize,
                ) -> &mut RegexSetBuilder {
                    self.0.dfa_size_limit = limit;
                    self
                }

                /// Set the nesting limit for this parser.
                ///
                /// The nesting limit controls how deep the abstract syntax tree is allowed
                /// to be. If the AST exceeds the given limit (e.g., with too many nested
                /// groups), then an error is returned by the parser.
                ///
                /// The purpose of this limit is to act as a heuristic to prevent stack
                /// overflow for consumers that do structural induction on an `Ast` using
                /// explicit recursion. While this crate never does this (instead using
                /// constant stack space and moving the call stack to the heap), other
                /// crates may.
                ///
                /// This limit is not checked until the entire Ast is parsed. Therefore,
                /// if callers want to put a limit on the amount of heap space used, then
                /// they should impose a limit on the length, in bytes, of the concrete
                /// pattern string. In particular, this is viable since this parser
                /// implementation will limit itself to heap space proportional to the
                /// length of the pattern string.
                ///
                /// Note that a nest limit of `0` will return a nest limit error for most
                /// patterns but not all. For example, a nest limit of `0` permits `a` but
                /// not `ab`, since `ab` requires a concatenation, which results in a nest
                /// depth of `1`. In general, a nest limit is not something that manifests
                /// in an obvious way in the concrete syntax, therefore, it should not be
                /// used in a granular way.
                pub fn nest_limit(
                    &mut self,
                    limit: u32,
                ) -> &mut RegexSetBuilder {
                    self.0.syntax.nest_limit(limit);
                    self
                }
            }
        }
    };
}

define_set_builder!(set_bytes, bytes, false);
define_set_builder!(set_unicode, unicode, true);
