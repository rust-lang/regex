use regex_automata::{util::captures::Captures, Input};

use crate::{
    args,
    util::{self, Table},
};

pub fn run_onepass(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the one-pass DFA regex engine.

USAGE:
    regex-cli find capture onepass [-p <pattern> ...] <haystack-path>
    regex-cli find capture onepass [-p <pattern> ...] -y <haystack>

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

    let search = |input: &Input<'_>, caps: &mut Captures| {
        re.try_search(&mut cache, input, caps)
    };
    if find.count {
        super::run_counts(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            re.get_nfa().group_info(),
            search,
        )?;
    } else {
        super::run_search(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            re.get_nfa().group_info(),
            search,
        )?;
    }
    Ok(())
}
