use regex_automata::Input;

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

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfafwd, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile forward nfa time", time);
    let (nfarev, time) =
        util::timeitr(|| thompson.reversed().from_hirs(&hirs))?;
    table.add("compile reverse nfa time", time);
    let (dfafwd, time) = util::timeitr(|| hybrid.from_nfa(&nfafwd))?;
    table.add("build forward hybrid time", time);
    let (dfarev, time) =
        util::timeitr(|| hybrid.reversed().from_nfa(&nfarev))?;
    table.add("build reverse hybrid time", time);
    let (re, time) = util::timeit(|| {
        regex_automata::hybrid::regex::Builder::new()
            .build_from_dfas(dfafwd, dfarev)
    });
    table.add("build regex time", time);
    let (mut cache, time) = util::timeit(|| re.create_cache());
    table.add("cache creation time", time);

    let search = |input: &Input<'_>| re.try_search(&mut cache, input);
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

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfafwd, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile forward nfa time", time);
    let (nfarev, time) =
        util::timeitr(|| thompson.reversed().from_hirs(&hirs))?;
    table.add("compile reverse nfa time", time);
    let (dfafwd, time) = util::timeitr(|| dense.from_nfa(&nfafwd))?;
    table.add("build forward dense DFA time", time);
    let (dfarev, time) = util::timeitr(|| dense.reversed().from_nfa(&nfarev))?;
    table.add("build reverse dense DFA time", time);
    let (re, time) = util::timeit(|| {
        regex_automata::dfa::regex::Builder::new()
            .build_from_dfas(dfafwd, dfarev)
    });
    table.add("build regex time", time);

    let search = |input: &Input<'_>| re.try_search(input);
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

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfafwd, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile forward nfa time", time);
    let (nfarev, time) =
        util::timeitr(|| thompson.reversed().from_hirs(&hirs))?;
    table.add("compile reverse nfa time", time);
    let (dfafwd, time) = util::timeitr(|| sparse.from_nfa_sparse(&nfafwd))?;
    table.add("build forward sparse DFA time", time);
    let (dfarev, time) =
        util::timeitr(|| sparse.reversed().from_nfa_sparse(&nfarev))?;
    table.add("build reverse sparse DFA time", time);
    let (re, time) = util::timeit(|| {
        regex_automata::dfa::regex::Builder::new()
            .build_from_dfas(dfafwd, dfarev)
    });
    table.add("build regex time", time);

    let search = |input: &Input<'_>| re.try_search(input);
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
    Ok(())
}

pub fn run_onepass(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the one-pass DFA regex engine.

USAGE:
    regex-cli find match onepass [-p <pattern> ...] <haystack-path>
    regex-cli find match onepass [-p <pattern> ...] -y <haystack>

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
    let mut onepass = args::onepass::Config::default();
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
            &mut onepass,
            &mut find,
        ],
    )?;

    let pats = patterns.get()?;
    let mut table = Table::empty();
    let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
    table.add("parse time", time);
    let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
    table.add("translate time", time);
    let (nfa, time) = util::timeitr(|| thompson.from_hirs(&hirs))?;
    table.add("compile nfa time", time);
    let (re, time) = util::timeitr(|| onepass.from_nfa(&nfa))?;
    table.add("build onepass time", time);
    let (mut cache, time) = util::timeit(|| re.create_cache());
    table.add("cache creation time", time);
    let (mut caps, time) = util::timeit(|| re.create_captures());
    table.add("captures creation time", time);

    let search = |input: &Input<'_>| {
        re.try_search(&mut cache, input, &mut caps)?;
        Ok(caps.get_match())
    };
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
    Ok(())
}
