use {
    lexopt::{Arg, Parser},
    regex_automata::util::syntax,
    regex_lite::Regex,
};

use crate::args::{self, Configurable, Usage};

/// Exposes the configuration for the top-level `Regex` API.
#[derive(Debug, Default)]
pub struct Config {
    size_limit: Option<usize>,
}

impl Config {
    /// Builds a `Regex` from the given syntax configuration and sequence of
    /// patterns. This returns an error is `patterns.len() != 1`.
    ///
    /// Note that this also returns an error if any syntax options are set
    /// that aren't supported by `regex-lite`.
    pub fn from_patterns(
        &self,
        syntax: &syntax::Config,
        patterns: &[String],
    ) -> anyhow::Result<Regex> {
        anyhow::ensure!(
            patterns.len() == 1,
            "API-level regex requires exactly one pattern, \
                 but {} were given",
            patterns.len(),
        );
        anyhow::ensure!(
            !syntax.get_octal(),
            "regex-lite does not support octal mode",
        );
        anyhow::ensure!(
            syntax.get_utf8(),
            "regex-lite does not support disabling UTF-8 mode",
        );
        anyhow::ensure!(
            syntax.get_unicode(),
            "regex-lite does not support disabling Unicode mode",
        );
        let mut b = regex_lite::RegexBuilder::new(&patterns[0]);
        b.case_insensitive(syntax.get_case_insensitive());
        b.multi_line(syntax.get_multi_line());
        b.crlf(syntax.get_crlf());
        b.dot_matches_new_line(syntax.get_dot_matches_new_line());
        b.swap_greed(syntax.get_swap_greed());
        b.ignore_whitespace(syntax.get_ignore_whitespace());
        b.nest_limit(syntax.get_nest_limit());
        b.size_limit(self.size_limit.unwrap_or(usize::MAX));
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
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[Usage::new(
            "--size-limit",
            "Set a limit on heap used by a regex.",
            r#"
This sets a limit, in bytes, on the heap memory used by a regex.

The special value 'none' indicates that no size limit should be imposed.
"#,
        )];
        USAGES
    }
}
