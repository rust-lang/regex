use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::nfa::thompson::{backtrack, NFA},
};

use crate::args::{self, Configurable, Usage};

/// This exposes the configuration knobs for a `BoundedBacktracker`.
#[derive(Debug, Default)]
pub struct Config {
    backtrack: backtrack::Config,
}

impl Config {
    /// Return a `backtrack::Config` object from this configuration.
    pub fn backtrack(&self) -> anyhow::Result<backtrack::Config> {
        Ok(self.backtrack.clone())
    }

    /// Builds a `BoundedBacktracker` regex engine from the NFA given.
    pub fn from_nfa(
        &self,
        nfa: &NFA,
    ) -> anyhow::Result<backtrack::BoundedBacktracker> {
        backtrack::Builder::new()
            .configure(self.backtrack()?)
            .build_from_nfa(nfa.clone())
            .context("failed to build BoundedBacktracker matcher")
    }
}

impl Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("visited-capacity") => {
                let capacity = args::parse(p, "--visited-capacity")?;
                self.backtrack =
                    self.backtrack.clone().visited_capacity(capacity);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[Usage::new(
            "--visited-capacity <capacity>",
            "Set the visited capacity for the bounded backtracker.",
            r#"
Set the visited set capacity used to bound backtracking.

The visited capacity represents the amount of heap memory (in bytes) to
allocate toward tracking which parts of the backtracking search have been done
before. The heap memory needed for any particular search is proportional to
'haystack.len() * nfa.states().len()', whichc an be quite large. Therefore, the
bounded backtracker is typically only able to run on shorter haystacks.

For a given regex, increasing the visited capacity means that the maximum
haystack length that can be searched is increased.

The default capacity is a reasonable but empirically chosen size.
"#,
        )];
        USAGES
    }
}
