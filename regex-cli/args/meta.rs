use std::borrow::Borrow;

use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::meta,
    regex_syntax::hir::Hir,
};

use crate::args::{self, flags, Configurable, Usage};

/// Exposes the configuration of a lazy DFA.
#[derive(Debug, Default)]
pub struct Config {
    meta: meta::Config,
    build_from_patterns: bool,
}

impl Config {
    /// Return a `meta::Config` object from this configuration.
    pub fn meta(&self) -> anyhow::Result<meta::Config> {
        Ok(self.meta.clone())
    }

    /// Whether to build a meta regex directly from the pattern strings, or to
    /// require the caller to build their own HIR first.
    ///
    /// i.e., Whether the caller should use `from_patterns` or `from_hirs`.
    pub fn build_from_patterns(&self) -> bool {
        self.build_from_patterns
    }

    /// Build a meta regex from the pattern strings given.
    pub fn from_patterns<P: AsRef<str>>(
        &self,
        syntax: &crate::args::syntax::Config,
        patterns: &[P],
    ) -> anyhow::Result<meta::Regex> {
        meta::Builder::new()
            .configure(self.meta()?)
            .syntax(syntax.syntax()?)
            .build_many(patterns)
            .context("failed to compile meta regex")
    }

    /// Build a meta regex from the HIRs given.
    pub fn from_hirs<H: Borrow<Hir>>(
        &self,
        hirs: &[H],
    ) -> anyhow::Result<meta::Regex> {
        meta::Builder::new()
            .configure(self.meta()?)
            .build_many_from_hir(hirs)
            .context("failed to compile meta regex")
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("build-from-patterns") => {
                self.build_from_patterns = true;
            }
            Arg::Short('k') | Arg::Long("match-kind") => {
                let kind: flags::MatchKind =
                    args::parse(p, "-k/--match-kind")?;
                self.meta = self.meta.clone().match_kind(kind.kind);
            }
            Arg::Short('B') | Arg::Long("no-utf8-nfa") => {
                self.meta = self.meta.clone().utf8_empty(false);
            }
            Arg::Long("no-auto-prefilter") => {
                self.meta = self.meta.clone().auto_prefilter(false);
            }
            Arg::Long("nfa-size-limit") => {
                let limit = args::parse_maybe(p, "--nfa-size-limit")?;
                self.meta = self.meta.clone().nfa_size_limit(limit);
            }
            Arg::Long("onepass-size-limit") => {
                let limit = args::parse_maybe(p, "--onepass-size-limit")?;
                self.meta = self.meta.clone().onepass_size_limit(limit);
            }
            Arg::Long("cache-capacity") => {
                let capacity = args::parse(p, "--cache-capacity")?;
                self.meta = self.meta.clone().hybrid_cache_capacity(capacity);
            }
            Arg::Long("dfa-size-limit") => {
                let limit = args::parse_maybe(p, "--dfa-size-limit")?;
                self.meta = self.meta.clone().dfa_size_limit(limit);
            }
            Arg::Long("dfa-state-limit") => {
                let limit = args::parse_maybe(p, "--dfa-state-limit")?;
                self.meta = self.meta.clone().dfa_state_limit(limit);
            }
            Arg::Short('C') | Arg::Long("no-byte-classes") => {
                self.meta = self.meta.clone().byte_classes(false);
            }
            Arg::Long("line-terminator") => {
                let byte: flags::OneByte =
                    args::parse(p, "--line-terminator")?;
                self.meta = self.meta.clone().line_terminator(byte.0);
            }
            Arg::Long("no-hybrid") => {
                self.meta = self.meta.clone().hybrid(false);
            }
            Arg::Long("no-dfa") => {
                self.meta = self.meta.clone().dfa(false);
            }
            Arg::Long("no-onepass") => {
                self.meta = self.meta.clone().onepass(false);
            }
            Arg::Long("no-backtrack") => {
                self.meta = self.meta.clone().backtrack(false);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "--build-from-patterns",
                "Build a meta regex directly from pattern strings.",
                r#"
Build a meta regex directly from pattern strings.

By default, a meta regex is built in this tool by first explicitly parsing the
patterns into ASTs, then translating them into HIRs and finally providing the
HIRs to the meta regex builder. This flag changes the behavior to pass the
pattern strings directly to the meta regex builder such that the builder is
responsible for parsing and translating.

The main reason to use this is if you specifically want to test the meta regex
builder from patterns directly, as it may contain optimizations for skipping
aspects of parsing.

The default behavior splits these steps out in order to time them so that
one gets a good idea of where most time is being spent during meta regex
construction.
"#,
            ),
            flags::MatchKind::USAGE,
            Usage::new(
                "-B, --no-utf8-nfa",
                "Disable UTF-8 mode for empty matches.",
                r#"
Disables UTF-8 mode for empty matches. When this flag is given, empty matches
that split a codepoint are permitted. Otherwise, they are banned.
"#,
            ),
            Usage::new(
                "--no-auto-prefilter",
                "Disable automatic prefilters.",
                r#"
By default, a meta regex is accelerated via prefilters if one can be extracted
from the literals in the pattern. By passing this flag, the automatic prefilter
optimization is disabled.
"#,
            ),
            Usage::new(
                "--nfa-size-limit",
                "Sets a limit on the memory used by an NFA.",
                r#"
Sets a limit on the memory used by the NFA built by the meta regex engine, in
terms of bytes of heap usage. This limit is applied during NFA construction. If
the limit is exceeded, then construction will fail.

A special 'none' value disables the limit entirely.
"#,
            ),
            Usage::new(
                "--onepass-size-limit",
                "Set a limit on heap used by a one-pass DFA in bytes.",
                r#"
This sets a limit on the number of heap memory a one-pass DFA built by the meta
regex engine can use. The limit is enforced at one-pass DFA construction time.
If the limit is exceeded, then construction will fail.

A special value of 'none' may be given, which disables the limit.
"#,
            ),
            Usage::new(
                "--cache-capacity",
                "Set the total cache capacity for the lazy DFA.",
                r#"
This sets an approximate limit on the total amount of heap memory used by the
lazy DFA. This only applies when the meta regex engine uses a lazy DFA. Once
the cache reaches capacity and there's no more room for additional states, the
cache is cleared and the lazy DFA keeps rebuilding itself.
"#,
            ),
            Usage::new(
                "--dfa-size-limit",
                "Set a limit on heap used by a DFA in bytes.",
                r#"
This sets a limit on the number of heap memory a DFA built by the meta regex
engine can use. The limit is enforced at DFA construction time. If the limit is
exceeded, then construction will fail.

A special value of 'none' may be given, which disables the limit.
"#,
            ),
            Usage::new(
                "--dfa-state-limit",
                "Only build a DFA when the NFA is below this limit.",
                r#"
When an NFA built by the meta regex engine has a number of states below this
limit, the meta regex engine may choose to build a fully compiled DFA.

A special value of 'none' may be given, which disables the limit.

When not set, the default is a very small number. Unless you know what you're
doing, the limit should be kept small since DFA construction is exponential
in the number of DFA states. Even 2^N where N is a small number can be quite
large, and this is why there is also the --dfa-size-limit to ensure the DFA
cannot get too big.
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
                "--line-terminator",
                "Set the line terminator used by line anchors.",
                r#"
Set the line terminator used by line anchors. The line anchors are '(?m:^)' and
'(?m:$)'. By default, they both use '\n' as line terminators for matching
purposes. This option changes the line terminator to any arbitrary byte.

Note that CRLF aware line anchors, that is, '(?Rm:^)' and '(?Rm:$)', are
unaffected by this option. CRLF aware line anchors always use '\r' and '\n'
as line terminators and do not match between a '\r' and '\n'.
"#,
            ),
            Usage::new(
                "--no-hybrid",
                "Disable the use of a lazy DFA.",
                r#"
This prevents the meta regex engine from building and using a lazy DFA.
"#,
            ),
            Usage::new(
                "--no-dfa",
                "Disable the use of a fully compiled DFA.",
                r#"
This prevents the meta regex engine from building and using a fully compiled
DFA.
"#,
            ),
            Usage::new(
                "--no-onepass",
                "Disable the use of a one-pass DFA.",
                r#"
This prevents the meta regex engine from building and using a one-pass DFA.
"#,
            ),
            Usage::new(
                "--no-backtrack",
                "Disable the use of a bounded backtracker.",
                r#"
This prevents the meta regex engine from building and using a bounded
backtracker.
"#,
            ),
        ];
        USAGES
    }
}
