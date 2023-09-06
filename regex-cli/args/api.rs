use {
    lexopt::{Arg, Parser},
    regex::bytes::{Regex, RegexSet},
    regex_automata::util::syntax,
};

use crate::args::{self, Configurable, Usage};

/// Exposes the configuration for the top-level `Regex` API.
#[derive(Debug, Default)]
pub struct Config {
    size_limit: Option<usize>,
    dfa_size_limit: Option<usize>,
}

impl Config {
    /// Builds a `Regex` from the given syntax configuration and sequence of
    /// patterns. This returns an error is `patterns.len() != 1`.
    ///
    /// Note that this also returns an error if UTF-8 mode is enabled in
    /// the given syntax configuration. This is mostly because we stick to
    /// returning a `regex::bytes::Regex` which requires hard-codes disabling
    /// UTF-8 mode. We could add another constructor for `regex::Regex` which
    /// requires that UTF-8 mode is enabled if it's needed, but I don't think
    /// it is.
    pub fn from_patterns(
        &self,
        syntax: &syntax::Config,
        patterns: &[String],
    ) -> anyhow::Result<Regex> {
        anyhow::ensure!(
            !syntax.get_utf8(),
            "API-level regex requires that UTF-8 syntax mode be disabled",
        );
        anyhow::ensure!(
            patterns.len() == 1,
            "API-level regex requires exactly one pattern, \
                 but {} were given",
            patterns.len(),
        );
        let mut b = regex::bytes::RegexBuilder::new(&patterns[0]);
        b.case_insensitive(syntax.get_case_insensitive());
        b.multi_line(syntax.get_multi_line());
        b.dot_matches_new_line(syntax.get_dot_matches_new_line());
        b.swap_greed(syntax.get_swap_greed());
        b.ignore_whitespace(syntax.get_ignore_whitespace());
        b.unicode(syntax.get_unicode());
        b.octal(syntax.get_octal());
        b.nest_limit(syntax.get_nest_limit());
        b.size_limit(self.size_limit.unwrap_or(usize::MAX));
        if let Some(limit) = self.dfa_size_limit {
            b.dfa_size_limit(limit);
        }
        b.build().map_err(anyhow::Error::from)
    }

    /// Builds a `RegexSet` from the given syntax configuration and sequence of
    /// patterns.
    ///
    /// Note that this returns an error if UTF-8 mode is enabled in the given
    /// syntax configuration. This is mostly because we stick to returning a
    /// `regex::bytes::RegexSet` which requires hard-codes disabling UTF-8
    /// mode. We could add another constructor for `regex::RegexSet` which
    /// requires that UTF-8 mode is enabled if it's needed, but I don't think
    /// it is.
    pub fn from_patterns_set(
        &self,
        syntax: &syntax::Config,
        patterns: &[String],
    ) -> anyhow::Result<RegexSet> {
        anyhow::ensure!(
            !syntax.get_utf8(),
            "API-level regex requires that UTF-8 syntax mode be disabled",
        );
        let mut b = regex::bytes::RegexSetBuilder::new(patterns);
        b.case_insensitive(syntax.get_case_insensitive());
        b.multi_line(syntax.get_multi_line());
        b.dot_matches_new_line(syntax.get_dot_matches_new_line());
        b.swap_greed(syntax.get_swap_greed());
        b.ignore_whitespace(syntax.get_ignore_whitespace());
        b.unicode(syntax.get_unicode());
        b.octal(syntax.get_octal());
        b.nest_limit(syntax.get_nest_limit());
        b.size_limit(self.size_limit.unwrap_or(usize::MAX));
        if let Some(limit) = self.dfa_size_limit {
            b.dfa_size_limit(limit);
        }
        b.build().map_err(anyhow::Error::from)
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("size-limit") => {
                self.size_limit = args::parse_maybe(p, "--size-limit")?;
            }
            Arg::Long("dfa-size-limit") => {
                self.dfa_size_limit =
                    Some(args::parse(p, "--dfa-size-limit")?);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "--size-limit",
                "Set a limit on heap used by a regex.",
                r#"
This sets a limit, in bytes, on the heap memory used by a regex.

The special value 'none' indicates that no size limit should be imposed.
"#,
            ),
            Usage::new(
                "--dfa-size-limit",
                "Set a limit on the heap used by a regex's internal lazy DFA.",
                r#"
This sets a capacity, in bytes, on the approximate maximum total heap memory
used by a regex's internal lazy DFA. This only applies if a lazy DFA is used.

Note that one cannot set this to 'none' since it represents a capacity. When
it isn't set, then some reasonable default is used.
"#,
            ),
        ];
        USAGES
    }
}
