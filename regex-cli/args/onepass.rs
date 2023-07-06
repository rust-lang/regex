use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::{dfa::onepass, nfa::thompson::NFA},
};

use crate::args::{self, flags, Configurable, Usage};

/// Exposes the configuration of a one-pass DFA.
#[derive(Debug, Default)]
pub struct Config {
    onepass: onepass::Config,
}

impl Config {
    /// Return a `dfa::onepass::Config` object from this configuration.
    pub fn onepass(&self) -> anyhow::Result<onepass::Config> {
        Ok(self.onepass.clone())
    }

    /// Attempts to convert the given NFA into a one-pass DFA. If the NFA isn't
    /// one-pass or if one of a few different limits is hit, then an error
    /// is returned.
    pub fn from_nfa(&self, nfa: &NFA) -> anyhow::Result<onepass::DFA> {
        onepass::Builder::new()
            .configure(self.onepass()?)
            .build_from_nfa(nfa.clone())
            .context("failed to compile onepass DFA")
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Short('k') | Arg::Long("match-kind") => {
                let kind: flags::MatchKind =
                    args::parse(p, "-k/--match-kind")?;
                self.onepass = self.onepass.clone().match_kind(kind.kind);
            }
            Arg::Long("starts-for-each-pattern") => {
                self.onepass =
                    self.onepass.clone().starts_for_each_pattern(true);
            }
            Arg::Short('C') | Arg::Long("no-byte-classes") => {
                self.onepass = self.onepass.clone().byte_classes(false);
            }
            Arg::Long("onepass-size-limit") => {
                let limit = args::parse_maybe(p, "--onepass-size-limit")?;
                self.onepass = self.onepass.clone().size_limit(limit);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            flags::MatchKind::USAGE,
            Usage::new(
                "--starts-for-each-pattern",
                "Add anchored start states for each pattern.",
                r#"
Add anchored start states for each pattern. This permits running an anchored
search for a specific pattern using the --pattern-id flag. (Assuming this is
a search command.)
"#,
            ),
            Usage::new(
                "-C, --no-byte-classes",
                "Disable byte classes.",
                r#"
This causes all bytes to be an equivalence class unto themselves. By default,
bytes are grouped into equivalence classes to reduce the size of the alphabet
for a DFA, and therefore decreases overall space usage.

It can be quite convenient to disable byte classes when looking at the debug
representation of a DFA. Otherwise, the transitions are much harder for a human
to read.
"#,
            ),
            Usage::new(
                "--onepass-size-limit",
                "Set a limit on heap used by a one-pass DFA in bytes.",
                r#"
This sets a limit on the number of heap memory a one-pass DFA can use. The
limit is enforced at one-pass DFA construction time. If the limit is exceeded,
then construction will fail.

A special value of 'none' may be given, which disables the limit.
"#,
            ),
        ];
        USAGES
    }
}
