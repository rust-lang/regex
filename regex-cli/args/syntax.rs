use std::borrow::Borrow;

use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::util::syntax,
    regex_syntax::{ast::Ast, hir::Hir},
};

use crate::args::{self, Configurable, Usage};

/// This exposes all of the configuration knobs on a
/// regex_automata::util::syntax::Config via CLI flags.
#[derive(Debug, Default)]
pub struct Config {
    syntax: syntax::Config,
}

impl Config {
    /// Return a `syntax::Config` object from this configuration.
    pub fn syntax(&self) -> anyhow::Result<syntax::Config> {
        Ok(self.syntax.clone())
    }

    /// Parses the given pattern into an `Ast`.
    fn ast(&self, pattern: &str) -> anyhow::Result<Ast> {
        regex_syntax::ast::parse::ParserBuilder::new()
            .nest_limit(self.syntax.get_nest_limit())
            .octal(self.syntax.get_octal())
            .ignore_whitespace(self.syntax.get_ignore_whitespace())
            .build()
            .parse(pattern)
            .context("failed to parse pattern")
    }

    /// Parses the given patterns into a corresponding sequence of `Ast`s. If
    /// any of the patterns fail to parse, then an error is returned.
    pub fn asts<P: AsRef<str>>(
        &self,
        patterns: &[P],
    ) -> anyhow::Result<Vec<Ast>> {
        patterns
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let p = p.as_ref();
                self.ast(p).with_context(|| {
                    format!("failed to parse pattern {i} to AST: '{p}'",)
                })
            })
            .collect()
    }

    /// Translates the given pattern and `Ast` into an `Hir`.
    pub fn hir(&self, pattern: &str, ast: &Ast) -> anyhow::Result<Hir> {
        regex_syntax::hir::translate::TranslatorBuilder::new()
            .utf8(self.syntax.get_utf8())
            .case_insensitive(self.syntax.get_case_insensitive())
            .multi_line(self.syntax.get_multi_line())
            .dot_matches_new_line(self.syntax.get_dot_matches_new_line())
            .swap_greed(self.syntax.get_swap_greed())
            .unicode(self.syntax.get_unicode())
            .build()
            .translate(pattern, ast)
            .context("failed to translate pattern")
    }

    /// Translates the given patterns and corresponding `Ast`s into a
    /// corresponding sequence of `Hir`s. If any of the patterns fail to
    /// translate, then an error is returned.
    pub fn hirs<P: AsRef<str>, A: Borrow<Ast>>(
        &self,
        patterns: &[P],
        asts: &[A],
    ) -> anyhow::Result<Vec<Hir>> {
        patterns
            .iter()
            .zip(asts.iter())
            .enumerate()
            .map(|(i, (pat, ast))| {
                let (pat, ast) = (pat.as_ref(), ast.borrow());
                self.hir(pat, ast).with_context(|| {
                    format!("failed to translate pattern {i} to HIR: '{pat}'",)
                })
            })
            .collect()
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Short('i') | Arg::Long("case-insensitive") => {
                self.syntax = self.syntax.case_insensitive(true);
            }
            Arg::Long("multi-line") => {
                self.syntax = self.syntax.multi_line(true);
            }
            Arg::Long("dot-matches-new-line") => {
                self.syntax = self.syntax.dot_matches_new_line(true);
            }
            Arg::Long("crlf") => {
                self.syntax = self.syntax.crlf(true);
            }
            Arg::Long("swap-greed") => {
                self.syntax = self.syntax.swap_greed(true);
            }
            Arg::Long("ignore-whitespace") => {
                self.syntax = self.syntax.ignore_whitespace(true);
            }
            Arg::Short('U') | Arg::Long("no-unicode") => {
                self.syntax = self.syntax.unicode(false);
            }
            Arg::Short('b') | Arg::Long("no-utf8-syntax") => {
                self.syntax = self.syntax.utf8(false);
            }
            Arg::Long("nest-limit") => {
                let limit = args::parse(p, "--nest-limit")?;
                self.syntax = self.syntax.nest_limit(limit);
            }
            Arg::Long("octal") => {
                self.syntax = self.syntax.octal(true);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "-i, --case-insensitive",
                "Enable case insensitive mode.",
                r#"
This enables case insensitive mode for all regex patterns given. When absent,
all patterns are matched case sensitively.

Note that individual patterns can have case insensitivity enabled via the
inline regex flag 'i'. For example, '(?i:abc)'.
"#,
            ),
            Usage::new(
                "--multi-line",
                "Enable multi-line mode.",
                r#"
This enables multi-line mode for all regex patterns given. When multi-line
mode is enabled, the anchors '^' and '$' turn into line anchors. That is,
in addition to matching the start and end of a haystack, they also match at
the start and end of a line, respectively.

Note that individual patterns can have multi-line mode enabled via the
inline regex flag 'm'. For example, '(?m:^)'.
"#,
            ),
            Usage::new(
                "--dot-matches-new-line",
                "Make a dot match \\n.",
                r#"
Enabling this causes a '.' (dot) to match the line terminator. By default, a
dot is equivalent to '[^\n]'. (When CRLF mode is enabled it is equivalent to
'[^\r\n]'.)

Note that individual patterns can have this mode enabled via the inline regex
flag 's'. For example, '(?s:.)'.
"#,
            ),
            Usage::new(
                "--crlf",
                "Line anchors are CRLF aware.",
                r#"
When enabled, line anchors become CRLF aware. That is, patterns like '(?m:^)'
and '(?m:$)' only consider '\n' by default. But when CRLF mode is enabled, line
anchors consider both '\r' and '\n'. In particular, line anchors will match
both '\r' and '\n', but never between '\r' and '\n'.

Additionally, when this mode is enabled, '.' is equivalent to '[^\r\n]' instead
of '[^\n]'.

Note that this does not enable multi-line mode by itself. This only applies to
'^' and '$' when multi-line mode is enabled.

Note that individual patterns can have CRLF mode enabled via the inline regex
flag 'R'. For example, '(?Rm:^)'.
"#,
            ),
            Usage::new(
                "--swap-greed",
                "Swap the meaning of greediness.",
                r#"
This enables "swap greed" mode for all regex patterns given. When greediness
is swapped, greedy patterns like 'a+' become equivalent to 'a+?', and ungreedy
patterns like 'a+?' become equivalent to 'a+'.

Note that individual patterns can have "swap greed" mode enabled via the inline
regex flag 'U'. For example, '(?U:a+)'.
"#,
            ),
            Usage::new(
                "--ignore-whitespace",
                "Enable whitespace insensitive mode.",
                r#"
This enables whitespace insensitive mode for all regex patterns given. When
enabled, all whitespace in regex patterns is ignored. Moreover, any lines whose
first non-whitespace character is '#' will be ignored and treated as a comment.

Note that individual patterns can have whitespace insensitivity enabled via the
inline regex flag 'x'. For example, '(?x:a b c)' is equivalent to 'abc'.
"#,
            ),
            Usage::new(
                "-U, --no-unicode",
                "Disable Unicode mode.",
                r#"
This disables Unicode mode for all regex patterns given. When Unicode mode is
disabled, the logical unit of searching is a single byte, where as when it
is enabled the logical unit of searching is a single codepoint. In practice,
this means that Unicode mode makes a number of alterations to the syntax and
semantics of a regex. 1) '[^a]' matches any codepoint that isn't 'a' instead
of any byte that isn't 'a'. 2) Case insensitive mode takes Unicode simple case
folding rules into account. 3) Unicode literals and character classes are
allowed.

Note that individual patterns can have Unicode mode disabled via the inline
regex flag 'u'. For example, '(?-u:\xFF)' matches the byte '\xFF' where as
'(?u:\xFF)' matches the UTF-8 encoding of the Unicode codepoint U+00FF.
"#,
            ),
            Usage::new(
                "-b, --no-utf8-syntax",
                "Disable UTF-8 mode for the regex syntax.",
                r#"
This disables UTF-8 mode for all regex patterns given. Disabling UTF-8 mode
permits regexes that match invalid UTF-8. When UTF-8 mode is enabled, then
patterns are limited to matches corresponding to valid UTF-8.

This only applies to non-empty matches. For empty matches, the UTF-8 mode is
controlled on the NFA, via --no-utf8-nfa (if applicable).
"#,
            ),
            Usage::new(
                "--nest-limit",
                "Set the nest limit on the syntax.",
                r#"
This sets the nesting limit of the regex syntax on all patterns. This controls
how many "nested" constructs are permitted in the pattern. This is useful for
preventing pathological regexes that require too much nesting. For example,
if one wants to do recursive analysis on the syntax of a regex, you usually
need to check that it doesn't have too much nesting or else you risk a stack
overflow.

Note that the default is likely big enough to permit most regex patterns.
"#,
            ),
            Usage::new(
                "--octal",
                "Permit octal escapes.",
                r#"
This permits octal escape sequences in the regex syntax. For example, it treats
'\17' as equivalent to '\x0F'. This is disabled by default.
"#,
            ),
        ];
        USAGES
    }
}
