use regex_automata::{dfa::Automaton, Input};

use crate::{
    args,
    util::{self, Table},
};

pub fn run_hybrid(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    use regex_automata::hybrid::dfa::OverlappingState;

    const USAGE: &'static str = "\
Executes a search for half matches using the lazy DFA regex engine.

USAGE:
    regex-cli find half hybrid [-p <pattern> ...] <haystack-path>
    regex-cli find half hybrid [-p <pattern> ...] -y <haystack>

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
    let mut overlapping = args::overlapping::Config::default();
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
            &mut overlapping,
            &mut find,
        ],
    )?;

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

    if overlapping.enabled {
        let search = |input: &Input<'_>, state: &mut OverlappingState| {
            re.try_search_overlapping_fwd(&mut cache, input, state)
        };
        if find.count {
            super::run_counts_overlapping(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                re.pattern_len(),
                || OverlappingState::start(),
                |s| s.get_match(),
                search,
            )?;
        } else {
            super::run_search_overlapping(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                || OverlappingState::start(),
                |s| s.get_match(),
                search,
            )?;
        }
    } else {
        let search = |input: &Input<'_>| re.try_search_fwd(&mut cache, input);
        if find.count {
            super::run_counts(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                re.pattern_len(),
                search,
            )?;
        } else {
            super::run_search(
                &mut table, &common, &find, &input, &haystack, search,
            )?;
        }
    }
    Ok(())
}

pub fn run_dense(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    use regex_automata::dfa::OverlappingState;

    const USAGE: &'static str = "\
Executes a search for half matches using the dense DFA regex engine.

USAGE:
    regex-cli find half dense [-p <pattern> ...] <haystack-path>
    regex-cli find half dense [-p <pattern> ...] -y <haystack>

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
    let mut overlapping = args::overlapping::Config::default();
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
            &mut overlapping,
            &mut find,
        ],
    )?;

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

    if overlapping.enabled {
        let search = |input: &Input<'_>, state: &mut OverlappingState| {
            re.try_search_overlapping_fwd(input, state)
        };
        if find.count {
            super::run_counts_overlapping(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                re.pattern_len(),
                || OverlappingState::start(),
                |s| s.get_match(),
                search,
            )?;
        } else {
            super::run_search_overlapping(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                || OverlappingState::start(),
                |s| s.get_match(),
                search,
            )?;
        }
    } else {
        let search = |input: &Input<'_>| re.try_search_fwd(input);
        if find.count {
            super::run_counts(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                re.pattern_len(),
                search,
            )?;
        } else {
            super::run_search(
                &mut table, &common, &find, &input, &haystack, search,
            )?;
        }
    }
    Ok(())
}

pub fn run_sparse(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    use regex_automata::dfa::OverlappingState;

    const USAGE: &'static str = "\
Executes a search for half matches using the sparse DFA regex engine.

USAGE:
    regex-cli find half sparse [-p <pattern> ...] <haystack-path>
    regex-cli find half sparse [-p <pattern> ...] -y <haystack>

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
    let mut overlapping = args::overlapping::Config::default();
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
            &mut overlapping,
            &mut find,
        ],
    )?;

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

    if overlapping.enabled {
        let search = |input: &Input<'_>, state: &mut OverlappingState| {
            re.try_search_overlapping_fwd(input, state)
        };
        if find.count {
            super::run_counts_overlapping(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                re.pattern_len(),
                || OverlappingState::start(),
                |s| s.get_match(),
                search,
            )?;
        } else {
            super::run_search_overlapping(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                || OverlappingState::start(),
                |s| s.get_match(),
                search,
            )?;
        }
    } else {
        let search = |input: &Input<'_>| re.try_search_fwd(input);
        if find.count {
            super::run_counts(
                &mut table,
                &common,
                &find,
                &input,
                &haystack,
                re.pattern_len(),
                search,
            )?;
        } else {
            super::run_search(
                &mut table, &common, &find, &input, &haystack, search,
            )?;
        }
    }
    Ok(())
}
