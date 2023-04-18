use std::borrow::Borrow;

use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::{nfa::thompson, util::look::LookMatcher},
    regex_syntax::hir::Hir,
};

use crate::args::{self, flags, Configurable, Usage};

/// This exposes all of the configuration knobs on a regex_automata::Input via
/// CLI flags. The only aspect of regex_automata::Input that this does not
/// cover is the haystack, which should be provided by other means (usually
/// with `Haystack`).
#[derive(Debug, Default)]
pub struct Config {
    thompson: thompson::Config,
}

impl Config {
    /// Return a `thompson::Config` object from this configuration.
    pub fn thompson(&self) -> anyhow::Result<thompson::Config> {
        Ok(self.thompson.clone())
    }

    /// Returns a new configuration that compiles a reverse NFA.
    pub fn reversed(&self) -> Config {
        // Reverse DFAs require that captures are disabled. In practice, there
        // is no current use case for a reverse NFA with capture groups.
        let thompson = self.thompson.clone().reverse(true).captures(false);
        Config { thompson }
    }

    /// Compiles the given `Hir` expressions into an NFA. If compilation fails,
    /// then an error is returned. (And there is generally no way to know which
    /// pattern caused a failure.)
    pub fn from_hirs<H: Borrow<Hir>>(
        &self,
        hirs: &[H],
    ) -> anyhow::Result<thompson::NFA> {
        thompson::Compiler::new()
            .configure(self.thompson()?)
            .build_many_from_hir(hirs)
            .context("failed to compile Thompson NFA")
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Short('B') | Arg::Long("no-utf8-nfa") => {
                self.thompson = self.thompson.clone().utf8(false);
            }
            Arg::Short('r') | Arg::Long("reverse") => {
                self.thompson = self.thompson.clone().reverse(true);
            }
            Arg::Long("nfa-size-limit") => {
                let limit = args::parse_maybe(p, "--nfa-size-limit")?;
                self.thompson = self.thompson.clone().nfa_size_limit(limit);
            }
            Arg::Long("shrink") => {
                self.thompson = self.thompson.clone().shrink(true);
            }
            Arg::Long("no-captures") => {
                self.thompson = self.thompson.clone().captures(false);
            }
            Arg::Long("line-terminator") => {
                let byte: flags::OneByte =
                    args::parse(p, "--line-terminator")?;
                let mut lookm = LookMatcher::new();
                lookm.set_line_terminator(byte.0);
                self.thompson = self.thompson.clone().look_matcher(lookm);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "-B, --no-utf8-nfa",
                "Disable UTF-8 mode for empty matches.",
                r#"
Disables UTF-8 mode for empty matches. When this flag is given, empty matches
that split a codepoint are permitted. Otherwise, they are banned.
"#,
            ),
            Usage::new(
                "-r, --reverse",
                "Build a reverse Thompson NFA.",
                r#"
Build a reverse Thompson NFA. The reverse NFA matches the language described
by the corresponding forward NFA, but in reverse. In general, this is done by
reversing the concatenations of the regex and inverting look-around assertions
that depend on the direction of matching. So for example, `^` becomes `$` and
`$` becomes `^`. But `\b` and `\B` remain the same.

Note that at time of writing, using this flag requires also using
--no-captures.
"#,
            ),
            Usage::new(
                "--nfa-size-limit",
                "Sets a limit on the memory used by an NFA.",
                r#"
Sets a limit on the memory used by an NFA, in terms of bytes of heap usage.
This limit is applied during NFA construction. If the limit is exceeded, then
construction will fail.

A special 'none' value disables the limit entirely.
"#,
            ),
            Usage::new(
                "--shrink",
                "Enables NFA shrinking.",
                r#"
This flag enables NFA shrinking. At time of writing, this is an expensive
process that only applies to reverse NFA compilation. The process may get
cheaper or more widely applicable in the future, but it generally results
in making the state graph of large Unicode character classes much smaller.
Moreover, if you're building a fully compiled reverse DFA, the extra time
spent shrinking the NFA can lead to far larger savings in the subsequent DFA
determinization.
"#,
            ),
            Usage::new(
                "--no-captures",
                "Disable capture states.",
                r#"
Disables capture states. By default, NFAs include special "capture" states that
instruct some regex engines (like the PikeVM) to record offset positions in
ancillary state.

It can be useful to disable capture states in order to reduce "clutter" in the
automaton when debugging it. Also, at time of writing, reverse NFAs require
that capture groups are disabled.
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
        ];
        USAGES
    }
}
