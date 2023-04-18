use regex_automata::Input;

use crate::{
    args,
    util::{self, Table},
};

pub fn run_backtrack(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the BoundedBacktracker regex engine.

USAGE:
    regex-cli find match backtrack [-p <pattern> ...] <haystack-path>
    regex-cli find match backtrack [-p <pattern> ...] -y <haystack>

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
    let mut backtrack = args::backtrack::Config::default();
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
            &mut backtrack,
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
    let (re, time) = util::timeitr(|| backtrack.from_nfa(&nfa))?;
    table.add("build backtracker time", time);
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

pub fn run_pikevm(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the PikeVM regex engine.

USAGE:
    regex-cli find match pikevm [-p <pattern> ...] <haystack-path>
    regex-cli find match pikevm [-p <pattern> ...] -y <haystack>

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
    let mut pikevm = args::pikevm::Config::default();
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
            &mut pikevm,
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
    let (re, time) = util::timeitr(|| pikevm.from_nfa(&nfa))?;
    table.add("build pikevm time", time);
    let (mut cache, time) = util::timeit(|| re.create_cache());
    table.add("cache creation time", time);
    let (mut caps, time) = util::timeit(|| re.create_captures());
    table.add("captures creation time", time);

    let search = |input: &Input<'_>| {
        re.search(&mut cache, input, &mut caps);
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
