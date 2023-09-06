use regex_automata::{Anchored, Input, PatternID};

use lexopt::{Arg, Parser};

use crate::args::{self, Configurable, Usage};

/// This exposes all of the configuration knobs on a regex_automata::Input via
/// CLI flags. The only aspect of regex_automata::Input that this does not
/// cover is the haystack, which should be provided by other means (usually
/// with `Haystack`).
#[derive(Debug, Default)]
pub struct Config {
    start: Option<usize>,
    end: Option<usize>,
    anchored: bool,
    pattern_id: Option<PatternID>,
    earliest: bool,
}

impl Config {
    /// Return an `Input` given the haystack to search. The input configuration
    /// (other than the haystack) is drawn from this configuration.
    ///
    /// If an `Input` could not be constructed from this configuration (for
    /// example, invalid start/end bounds), then an error is returned.
    pub fn input<'h>(&self, haystack: &'h [u8]) -> anyhow::Result<Input<'h>> {
        let mut input = Input::new(haystack).earliest(self.earliest);
        if let Some(start) = self.start {
            anyhow::ensure!(
                start <= haystack.len(),
                "start bound {} exceeds haystack length {}",
                start,
                haystack.len(),
            );
            input.set_start(start);
        }
        if let Some(end) = self.end {
            anyhow::ensure!(
                end <= haystack.len(),
                "end bound {} exceeds haystack length {}",
                end,
                haystack.len(),
            );
            input.set_end(end);
        }
        if let Some(pid) = self.pattern_id {
            input.set_anchored(Anchored::Pattern(pid));
        } else if self.anchored {
            input.set_anchored(Anchored::Yes)
        } else {
            // The default, but we set it explicitly anyway.
            input.set_anchored(Anchored::No)
        }
        Ok(input)
    }

    /// Pass the `Input` (derived from this configuration) to the closure
    /// given. Any error returned by the closure is returned by this routine.
    /// Similarly, an error is returned if an `Input` could not be constructed
    /// from this configuration.
    ///
    /// The `Input` is constructed with the given haystack. The intent of this
    /// routine is that if the haystack is specified via a file path, then this
    /// will memory map the haystack.
    pub fn with<T>(
        &self,
        haystack: &args::haystack::Config,
        mut f: impl FnMut(Input<'_>) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        haystack.with(|bytes| f(self.input(bytes)?))
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("start") => {
                self.start = Some(args::parse(p, "--start")?);
            }
            Arg::Long("end") => {
                self.end = Some(args::parse(p, "--end")?);
            }
            Arg::Short('a') | Arg::Long("anchored") => {
                self.anchored = true;
            }
            Arg::Long("pattern-id") => {
                let pid = args::parse(p, "--pattern-id")?;
                self.pattern_id = Some(PatternID::new(pid)?);
            }
            Arg::Long("earliest") => {
                self.earliest = true;
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "--start <bound>",
                "Set the start of the search.",
                r#"
This sets the start bound of a search. It must be a valid offset for the
haystack, up to and including the length of the haystack.

When not set, the start bound is 0.
"#,
            ),
            Usage::new(
                "--end <bound>",
                "Set the end of the search.",
                r#"
This sets the end bound of a search. It must be a valid offset for the
haystack, up to and including the length of the haystack.

When not set, the end bound is the length of the haystack.
"#,
            ),
            Usage::new(
                "-a, --anchored",
                "Enable anchored mode for the search.",
                r#"
Enabled anchored mode for the search. When enabled and if a match is found, the
start of the match is guaranteed to be equivalent to the start bound of the
search.
"#,
            ),
            Usage::new(
                "--pattern-id <pid>",
                "Set pattern to search for.",
                r#"
Set the pattern to search for. This automatically enables anchored mode for the
search since regex engines for this crate only support anchored searches for
specific patterns.

When set and if a match is found, the start of the match is guaranteed to be
equivalent to the start bound of the search and the pattern ID is guaranteed
to be equivalent to the one set by this flag.

When not set, a search may match any of the patterns given.
"#,
            ),
            Usage::new(
                "--earliest",
                "Returns a match as soon as it is known.",
                r#"
This enables "earliest" mode, which asks the regex engine to stop searching as
soon as a match is found. The specific offset returned may vary depending on
the regex engine since not all regex engines detect matches in the same way.
"#,
            ),
        ];
        USAGES
    }
}
