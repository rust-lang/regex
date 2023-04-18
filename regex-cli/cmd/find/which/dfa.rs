use regex_automata::{dfa::Automaton, Input, PatternSet};

use crate::{
    args,
    util::{self, Table},
};

pub fn run_hybrid(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the lazy DFA regex engine.

USAGE:
    regex-cli find match hybrid [-p <pattern> ...] <haystack-path>
    regex-cli find match hybrid [-p <pattern> ...] -y <haystack>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut input = args::input::Config::default();
    let mut patterns = args::patterns::Config::only_flags();
    let mut haystack = args::haystack::Config::default();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut hybrid = args::hybrid::Config::default();
    let mut find = super::super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut input,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut thompson,
            &mut hybrid,
            &mut find,
        ],
    )?;
    anyhow::ensure!(
        !find.count,
        "'which' command does not support reporting counts",
    );

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfafwd, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile forward nfa time", time);
    let (re, time) = util::timeitr(|| hybrid.from_nfa(&nfafwd))?;
    table.add("build forward hybrid time", time);
    let (mut cache, time) = util::timeit(|| re.create_cache());
    table.add("cache creation time", time);

    let search = |input: &Input<'_>, patset: &mut PatternSet| {
        re.try_which_overlapping_matches(&mut cache, input, patset)
    };
    super::run_search(
        &mut table,
        &common,
        &find,
        &input,
        &haystack,
        re.pattern_len(),
        search,
    )?;
    Ok(())
}

pub fn run_dense(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the dense DFA regex engine.

USAGE:
    regex-cli find match dense [-p <pattern> ...] <haystack-path>
    regex-cli find match dense [-p <pattern> ...] -y <haystack>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut input = args::input::Config::default();
    let mut patterns = args::patterns::Config::only_flags();
    let mut haystack = args::haystack::Config::default();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dense = args::dfa::Config::default();
    let mut find = super::super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut input,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut thompson,
            &mut dense,
            &mut find,
        ],
    )?;
    anyhow::ensure!(
        !find.count,
        "'which' command does not support reporting counts",
    );

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfafwd, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile forward nfa time", time);
    let (re, time) = util::timeitr(|| dense.from_nfa(&nfafwd))?;
    table.add("build forward dense DFA time", time);

    let search = |input: &Input<'_>, patset: &mut PatternSet| {
        re.try_which_overlapping_matches(input, patset)
    };
    super::run_search(
        &mut table,
        &common,
        &find,
        &input,
        &haystack,
        re.pattern_len(),
        search,
    )?;
    Ok(())
}

pub fn run_sparse(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the sparse DFA regex engine.

USAGE:
    regex-cli find match sparse [-p <pattern> ...] <haystack-path>
    regex-cli find match sparse [-p <pattern> ...] -y <haystack>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut input = args::input::Config::default();
    let mut patterns = args::patterns::Config::only_flags();
    let mut haystack = args::haystack::Config::default();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut sparse = args::dfa::Config::default();
    let mut find = super::super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut input,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut thompson,
            &mut sparse,
            &mut find,
        ],
    )?;
    anyhow::ensure!(
        !find.count,
        "'which' command does not support reporting counts",
    );

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfafwd, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile forward nfa time", time);
    let (re, time) = util::timeitr(|| sparse.from_nfa_sparse(&nfafwd))?;
    table.add("build forward sparse DFA time", time);

    let search = |input: &Input<'_>, patset: &mut PatternSet| {
        re.try_which_overlapping_matches(input, patset)
    };
    super::run_search(
        &mut table,
        &common,
        &find,
        &input,
        &haystack,
        re.pattern_len(),
        search,
    )?;
    Ok(())
}
