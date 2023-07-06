use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::{
        dfa::{self, dense, sparse},
        nfa::thompson::NFA,
        MatchKind,
    },
};

use crate::args::{self, flags, Configurable, Usage};

/// Exposes the configuration for a dense (and also therefore sparse) DFAs.
#[derive(Debug, Default)]
pub struct Config {
    dense: dense::Config,
}

impl Config {
    /// Return a `dfa::dense::Config` object from this configuration.
    pub fn dense(&self) -> anyhow::Result<dense::Config> {
        Ok(self.dense.clone())
    }

    /// Returns a new configuration that compiles a reverse DFA from a reverse
    /// NFA. The caller is responsible for reversing the NFA.
    pub fn reversed(&self) -> Config {
        let dense = self
            .dense
            .clone()
            .prefilter(None)
            .start_kind(dfa::StartKind::Anchored)
            .match_kind(MatchKind::All);
        Config { dense }
    }

    /// Runs determinization on the given NFA to produce a dense DFA. If
    /// determinization fails, then an error is returned.
    pub fn from_nfa(&self, nfa: &NFA) -> anyhow::Result<dense::DFA<Vec<u32>>> {
        dense::Builder::new()
            .configure(self.dense()?)
            .build_from_nfa(nfa)
            .context("failed to compile dense DFA")
    }

    /// Runs determinization on the given NFA to produce a dense DFA, and then
    /// converts it to a sparse DFA. If determinization or conversion to a
    /// sparse DFA fails, then an error is returned.
    pub fn from_nfa_sparse(
        &self,
        nfa: &NFA,
    ) -> anyhow::Result<sparse::DFA<Vec<u8>>> {
        self.from_nfa(nfa)?.to_sparse().context("failed to compile sparse DFA")
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Short('A') | Arg::Long("no-accelerate") => {
                self.dense = self.dense.clone().accelerate(false);
            }
            Arg::Long("minimize") => {
                self.dense = self.dense.clone().minimize(true);
            }
            Arg::Short('k') | Arg::Long("match-kind") => {
                let kind: flags::MatchKind =
                    args::parse(p, "-k/--match-kind")?;
                self.dense = self.dense.clone().match_kind(kind.kind);
            }
            Arg::Long("start-kind") => {
                let kind: flags::StartKind = args::parse(p, "--start-kind")?;
                self.dense = self.dense.clone().start_kind(kind.kind);
            }
            Arg::Long("starts-for-each-pattern") => {
                self.dense = self.dense.clone().starts_for_each_pattern(true);
            }
            Arg::Short('C') | Arg::Long("no-byte-classes") => {
                self.dense = self.dense.clone().byte_classes(false);
            }
            Arg::Long("unicode-word-boundary") => {
                self.dense = self.dense.clone().unicode_word_boundary(true);
            }
            Arg::Long("quit") => {
                let set: flags::ByteSet = args::parse(p, "--quit")?;
                for &byte in set.0.iter() {
                    self.dense = self.dense.clone().quit(byte, true);
                }
            }
            Arg::Long("specialize-start-states") => {
                self.dense = self.dense.clone().specialize_start_states(true);
            }
            Arg::Long("dfa-size-limit") => {
                let limit = args::parse_maybe(p, "--dfa-size-limit")?;
                self.dense = self.dense.clone().dfa_size_limit(limit);
            }
            Arg::Long("determinize-size-limit") => {
                let limit = args::parse_maybe(p, "--determinize-size-limit")?;
                self.dense = self.dense.clone().determinize_size_limit(limit);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "-A, --no-accelerate",
                "Disable DFA state acceleration.",
                r#"
Disable DFA state acceleration. It is enabled by default. When enabled, DFA
states with few outgoing transitions are detected and tagged with special
information that fast vector routines should search for those outgoing
transitions whenever that state is entered.

Acceleration is generally a heuristic optimization, since if the vector routine
doesn't in practice skip over many bytes, it can overall make the DFA search
slower.
"#,
            ),
            Usage::new(
                "--minimize",
                "Minimize the DFA.",
                r#"
When enabled, the DFA is minimized. A minimized DFA is said to be as small as
it possibly can be for the given regular language.

Note that DFA minimization can take a very long time. Generally speaking, the
benefits of minimization are a smaller DFA. Usually that doesn't directly
translate to faster search times, but it can if it enables more efficient use
of your CPU's cache. DFA minimization can also enable more opportunities for
DFA acceleration.
"#,
            ),
            flags::MatchKind::USAGE,
            flags::StartKind::USAGE,
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
                "--unicode-word-boundary",
                "Enable heuristic support for Unicode word boundaries.",
                r#"
Enable heuristic support for Unicode word boundaries. When enabled, a DFA
will treat a Unicode word boundary as if it were an ASCII boundary, but will
quit if it sees any non-ASCII byte.

This is disabled by default, in which case, attempting to compile a DFA with a
Unicode word boundary will result in an error.

Note that enabling this is very similar to using the --quit flag and providing
every non-ASCII byte as a quit byte. The only difference is that when this flag
is used, the quit bytes are only added if the pattern contains a Unicode word
boundary.
"#,
            ),
            Usage::new(
                "--quit",
                "Add quit bytes to this DFA.",
                r#"
Add quit bytes to this DFA. When a quit byte is added to a DFA, then an
outgoing transition to every state for this byte is added to the DFA that
points to a special sentinel "quit" state. If the "quit" state is entered
during a search, then an error is returned.

The bytes given represent a set and may be specified as a sequence. Escape
sequences like \n and \xFF are supported.
"#,
            ),
            Usage::new(
                "--specialize-start-states",
                "Specializes start states for prefilter support.",
                r#"
When given, start states are "specialized" such that prefilters are better
supported. Namely, when start states are specialized they are given a special
tag that results in them being treated as a special case when entered at search
time. The special case is that a prefilter can be run at that point in an
attempt to accelerate the search.

In general, it only makes sense to specialize start states when a prefilter is
also enabled.

Note also that if start states are not specialized (the default), then it is in
general not possible to determine whether any given state ID is a start state,
unless you've enumerated all possible start states and checked it against that
set.
"#,
            ),
            Usage::new(
                "--dfa-size-limit",
                "Set a limit on heap used by a DFA in bytes.",
                r#"
This sets a limit on the number of heap memory a DFA can use. The limit is
enforced at DFA construction time. If the limit is exceeded, then construction
will fail.

A special value of 'none' may be given, which disables the limit.
"#,
            ),
            Usage::new(
                "--determinize-size-limit",
                "Set a limit on heap used by a DFA in bytes.",
                r#"
This sets a limit on the number of heap memory that determinization can use.
The limit is enforced during determinization. If the limit is exceeded, then
determinization and therefore construction of the DFA will fail.

This limit only applies to ancillary heap memory used by determinization and
not to the heap memory used by the DFA's transition table itself. The limit the
size of the DFA, use the --dfa-size-limit flag.

A special value of 'none' may be given, which disables the limit.
"#,
            ),
        ];
        USAGES
    }
}
