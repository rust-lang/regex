use std::io::{stdout, Write};

use {
    anyhow::Context,
    lexopt::Parser,
    regex_automata::{Input, MatchError, PatternID, PatternSet},
};

use crate::{
    args,
    util::{self, Table},
};

mod dfa;
mod nfa;

pub fn run(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a 'which' search. This type of search reports *only* which patterns
match a haystack. It doesn't report positions or even how many times each
pattern matches. (Therefore, the -c/--count flag doesn't work with this
command.)

It is generally expected to use '--match-kind all' with this command, as the
intent is to report all overlapping matches.

Note that the search will usually scan the entire haystack. It can sometimes
short circuit if all patterns are anchored or if the search knows no more
patterns will match.

This type of search is somewhat of a legacy feature because of how the
top-level RegexSet API works in the 'regex' crate. Its API is pretty limited
and it is difficult to extend to the more flexible meta regex API in
regex-automata.

The 'backtrack' engine isn't supported here because it doesn't have a 'which'
search routine. In theory it could, but it would likely be slow and no better
than just running each regex over the haystack one at a time.

The 'onepass' engine also does not support this API. (At least, not currently.)

USAGE:
    regex-cli find which <engine>

ENGINES:
    dense      Search with the dense DFA regex engine.
    hybrid     Search with the lazy DFA regex engine.
    meta       Search with the meta regex engine.
    pikevm     Search with the PikeVM regex engine.
    regexset   Search with the top-level API regex engine.
    sparse     Search with the sparse DFA regex engine.
";
    let cmd = args::next_as_command(USAGE, p)?;
    match &*cmd {
        "dense" => dfa::run_dense(p),
        "hybrid" => dfa::run_hybrid(p),
        "meta" => run_meta(p),
        "pikevm" => nfa::run_pikevm(p),
        "regex" => run_regex(p),
        "sparse" => dfa::run_sparse(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}

fn run_regex(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the top-level API regex engine.

USAGE:
    regex-cli find match regex [-p <pattern> ...] <haystack-path>
    regex-cli find match regex [-p <pattern> ...] -y <haystack>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::only_flags();
    let mut haystack = args::haystack::Config::default();
    let mut syntax = args::syntax::Config::default();
    let mut api = args::api::Config::default();
    let mut find = super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut api,
            &mut find,
        ],
    )?;
    anyhow::ensure!(
        !find.count,
        "'which' command does not support reporting counts",
    );

    let pats = patterns.get()?;
    let syn = syntax.syntax()?;
    let mut table = Table::empty();
    let (re, time) = util::timeitr(|| api.from_patterns_set(&syn, &pats))?;
    table.add("build regex time", time);

    // The top-level API doesn't support regex-automata's more granular Input
    // abstraction.
    let input = args::input::Config::default();
    let search = |input: &Input<'_>, patset: &mut PatternSet| {
        let matches = re.matches(input.haystack());
        for pid in matches.iter() {
            let pid = PatternID::new(pid).unwrap();
            patset.try_insert(pid).unwrap();
        }
        Ok(())
    };
    run_search(
        &mut table,
        &common,
        &find,
        &input,
        &haystack,
        re.len(),
        search,
    )?;
    Ok(())
}

fn run_meta(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the meta regex engine.

USAGE:
    regex-cli find match meta [-p <pattern> ...] <haystack-path>
    regex-cli find match meta [-p <pattern> ...] -y <haystack>

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
    let mut meta = args::meta::Config::default();
    let mut find = super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut input,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut meta,
            &mut find,
        ],
    )?;
    anyhow::ensure!(
        !find.count,
        "'which' command does not support reporting counts",
    );

    let pats = patterns.get()?;
    let mut table = Table::empty();

    let re = if meta.build_from_patterns() {
        let (re, time) = util::timeitr(|| meta.from_patterns(&syntax, &pats))?;
        table.add("build meta time", time);
        re
    } else {
        let (asts, time) = util::timeitr(|| syntax.asts(&pats))?;
        table.add("parse time", time);
        let (hirs, time) = util::timeitr(|| syntax.hirs(&pats, &asts))?;
        table.add("translate time", time);
        let (re, time) = util::timeitr(|| meta.from_hirs(&hirs))?;
        table.add("build meta time", time);
        re
    };

    let search = |input: &Input<'_>, patset: &mut PatternSet| {
        Ok(re.which_overlapping_matches(input, patset))
    };
    run_search(
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

/// Like `run_counts`, but prints the actual matches instead.
fn run_search(
    table: &mut Table,
    common: &args::common::Config,
    find: &super::Config,
    input: &args::input::Config,
    haystack: &args::haystack::Config,
    pattern_len: usize,
    mut search: impl FnMut(&Input<'_>, &mut PatternSet) -> Result<(), MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (patset, time) = util::timeitr(|| {
            let mut patset = PatternSet::new(pattern_len);
            for _ in 0..find.repeat() {
                search(&input, &mut patset)?;
            }
            Ok::<_, anyhow::Error>(patset)
        })?;
        table.add("search time", time);
        table.add("patterns that matched", patset.len());
        if common.table() {
            table.print(&mut out)?;
        }
        if !common.quiet {
            for i in 0..pattern_len {
                let pid = PatternID::new(i).context("invalid pattern ID")?;
                writeln!(
                    out,
                    "{}:{:?}",
                    pid.as_usize(),
                    patset.contains(pid)
                )?;
            }
        }
        Ok(())
    })
}
