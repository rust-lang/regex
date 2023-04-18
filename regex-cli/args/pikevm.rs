use {
    anyhow::Context,
    lexopt::{Arg, Parser},
    regex_automata::nfa::thompson::{pikevm, NFA},
};

use crate::args::{self, flags, Configurable, Usage};

/// This exposes the configuration knobs for a `PikeVM`.
#[derive(Debug, Default)]
pub struct Config {
    pikevm: pikevm::Config,
}

impl Config {
    /// Return a `pikevm::Config` object from this configuration.
    pub fn pikevm(&self) -> anyhow::Result<pikevm::Config> {
        Ok(self.pikevm.clone())
    }

    /// Builds a `PikeVM` regex engine from the NFA given.
    pub fn from_nfa(&self, nfa: &NFA) -> anyhow::Result<pikevm::PikeVM> {
        pikevm::Builder::new()
            .configure(self.pikevm()?)
            .build_from_nfa(nfa.clone())
            .context("failed to build PikeVM matcher")
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
                self.pikevm = self.pikevm.clone().match_kind(kind.kind);
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[flags::MatchKind::USAGE];
        USAGES
    }
}
