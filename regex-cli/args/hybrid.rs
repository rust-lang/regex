use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::{hybrid, nfa::thompson::NFA, MatchKind},
};

use crate::args::{self, flags, Configurable, Usage};

/// Exposes the configuration of a lazy DFA.
#[derive(Debug, Default)]
pub struct Config {
    hybrid: hybrid::dfa::Config,
}

impl Config {
    /// Return a `hybrid::dfa::Config` object from this configuration.
    pub fn hybrid(&self) -> anyhow::Result<hybrid::dfa::Config> {
        Ok(self.hybrid.clone())
    }

    /// Returns a new configuration that compiles a reverse lazy DFA from a
    /// reverse NFA. The caller is responsible for reversing the NFA.
    pub fn reversed(&self) -> Config {
        let hybrid =
            self.hybrid.clone().prefilter(None).match_kind(MatchKind::All);
        Config { hybrid }
    }

    /// Build a lazy DFA from the NFA given.
    ///
    /// Building a lazy DFA is generally cheap. It only does a little bit of
    /// work, but otherwise, the actual determinization process is carried out
    /// on demand at search time.
    pub fn from_nfa(&self, nfa: &NFA) -> anyhow::Result<hybrid::dfa::DFA> {
        hybrid::dfa::Builder::new()
            .configure(self.hybrid()?)
            .build_from_nfa(nfa.clone())
            .context("failed to compile lazy DFA")
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
                self.hybrid = self.hybrid.clone().match_kind(kind.kind);
            }
            Arg::Long("starts-for-each-pattern") => {
                self.hybrid =
                    self.hybrid.clone().starts_for_each_pattern(true);
            }
            Arg::Short('C') | Arg::Long("no-byte-classes") => {
                self.hybrid = self.hybrid.clone().byte_classes(false);
            }
            Arg::Long("unicode-word-boundary") => {
                self.hybrid = self.hybrid.clone().unicode_word_boundary(true);
            }
            Arg::Long("quit") => {
                let set: flags::ByteSet = args::parse(p, "--quit")?;
                for &byte in set.0.iter() {
                    self.hybrid = self.hybrid.clone().quit(byte, true);
                }
            }
            Arg::Long("specialize-start-states") => {
                self.hybrid =
                    self.hybrid.clone().specialize_start_states(true);
            }
            Arg::Long("cache-capacity") => {
                let capacity = args::parse(p, "--cache-capacity")?;
                self.hybrid = self.hybrid.clone().cache_capacity(capacity);
            }
            Arg::Long("skip-cache-capacity-check") => {
                self.hybrid =
                    self.hybrid.clone().skip_cache_capacity_check(true);
            }
            Arg::Long("minimum-cache-clear-count") => {
                let min = args::parse_maybe(p, "--minimum-cache-clear-count")?;
                self.hybrid =
                    self.hybrid.clone().minimum_cache_clear_count(min);
            }
            Arg::Long("minimum-bytes-per-state") => {
                let min = args::parse_maybe(p, "--minimum-bytes-per-state")?;
                self.hybrid = self.hybrid.clone().minimum_bytes_per_state(min);
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
                "--cache-capacity",
                "Set the total cache capacity for the lazy DFA.",
                r#"
This sets an approximate limit on the total amount of heap memory used by
the lazy DFA. Once the cache reaches capacity and there's no more room for
additional states, the cache is cleared and the lazy DFA keeps rebuilding
itself.
"#,
            ),
            Usage::new(
                "--skip-cache-capacity-check",
                "Use the minimum cache capacity instead of failing.",
                r#"
By default, building a lazy DFA will fail if the configured cache capacity
is too small to hold a small minimum number of DFA states. But enabling this
option will "skip" that check and instead force the cache to be at least as big
as the minimum size required.

This can be useful if you are trying hard to avoid cases where the lazy DFA
fails to build, but is in general not recommended. Namely, if the cache is too
small, then it's plausible that it will not be used efficiently and the overall
search would be slow. But it's not guaranteed to be slow, which is why this
setting is configurable.
"#,
            ),
            Usage::new(
                "--minimum-cache-clear-count",
                "Set the minimum number of times the cache must be cleared.",
                r#"
Sets the minimum number of times the cache must be cleared before the lazy DFA
is permitted to give up on the search and return an error.

This may be set to the special value 'none', which sets no minimum. In this
case, the lazy DFA will never give up. When its cache gets full, it will clear
it and keep going.

This flag is usually used in combination with --minimum-bytes-per-state. It
sets a baseline number of cache clearings but then also requires an efficiency
below a certain amount before the lazy DFA will quit.

When --minimum-bytes-per-state is not set and a minimum cache clear count has
been set, then the lazy DFA unconditionally quits after the cache has been
cleared the minimum number of times.
"#,
            ),
            Usage::new(
                "--minimum-bytes-per-state",
                "Sets the minimum efficiency for lazy DFA.",
                r#"
This flag sets the minimum efficiency of the lazy DFA in terms of the number of
bytes processed per new state added to the cache. If that number falls below
the efficiency given by this flag *and* the cache has been cleared the minimum
number of times, then the lazy DFA will quit the search and return an error.

This flag has no effect if --minimum-cache-clear-count is set to 'none'.
"#,
            ),
        ];
        USAGES
    }
}
