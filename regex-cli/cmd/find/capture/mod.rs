use std::io::{stdout, Write};

use {
    anyhow::Context,
    bstr::ByteSlice,
    lexopt::Parser,
    regex_automata::{
        util::captures::{Captures, GroupInfo},
        Input, MatchError, PatternID,
    },
};

use crate::{
    args,
    util::{self, Table},
};

mod dfa;
mod nfa;

pub fn run(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for capturing groups.

This command is limited to regex engines that support resolving capture groups.
It prints each match, corresponding to possibly many matching capture groups,
on its own line. It is prefixed with the pattern ID of the match. Each group
contains both the spans matched and the actual substring that matches.

USAGE:
    regex-cli find capture <engine>

ENGINES:
    backtrack  Search with the bounded backtracker regex engine.
    lite       Search with the regex-lite engine.
    meta       Search with the meta regex engine.
    onepass    Search with the one-pass DFA regex engine.
    pikevm     Search with the PikeVM regex engine.
    regex      Search with the top-level API regex engine.
";
    let cmd = args::next_as_command(USAGE, p)?;
    match &*cmd {
        "backtrack" => nfa::run_backtrack(p),
        "lite" => run_lite(p),
        "meta" => run_meta(p),
        "onepass" => dfa::run_onepass(p),
        "pikevm" => nfa::run_pikevm(p),
        "regex" => run_regex(p),
        unk => anyhow::bail!("unrecognized command '{unk}'"),
    }
}

fn run_regex(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the top-level API regex engine.

USAGE:
    regex-cli find capture regex [-p <pattern> ...] <haystack-path>
    regex-cli find capture regex [-p <pattern> ...] -y <haystack>

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

    let pats = patterns.get()?;
    let syn = syntax.syntax()?;
    let mut table = Table::empty();
    let (re, time) = util::timeitr(|| api.from_patterns(&syn, &pats))?;
    table.add("build regex time", time);

    // The top-level API doesn't support regex-automata's more granular Input
    // abstraction.
    let input = args::input::Config::default();
    // The top-level API also doesn't use 'Captures' from regex-automata
    // directly, but we can map between them with some annoyance.
    let group_info = GroupInfo::new([re.capture_names()])
        .context("could not build capture group info")?;
    let mut locs = re.capture_locations();
    let search = |input: &Input<'_>, caps: &mut Captures| {
        caps.set_pattern(None);
        if !re
            .captures_read_at(&mut locs, input.haystack(), input.start())
            .is_some()
        {
            return Ok(());
        }
        caps.set_pattern(Some(PatternID::ZERO));
        for i in 0..locs.len() {
            use regex_automata::util::primitives::NonMaxUsize;

            let slot_start = i * 2;
            let slot_end = slot_start + 1;
            match locs.get(i) {
                None => {
                    caps.slots_mut()[slot_start] = None;
                    caps.slots_mut()[slot_end] = None;
                }
                Some((start, end)) => {
                    caps.slots_mut()[slot_start] = NonMaxUsize::new(start);
                    caps.slots_mut()[slot_end] = NonMaxUsize::new(end);
                }
            }
        }
        Ok(())
    };
    if find.count {
        run_counts(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            &group_info,
            search,
        )?;
    } else {
        run_search(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            &group_info,
            search,
        )?;
    }
    Ok(())
}

fn run_meta(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the meta regex engine.

USAGE:
    regex-cli find capture meta [-p <pattern> ...] <haystack-path>
    regex-cli find capture meta [-p <pattern> ...] -y <haystack>

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

    let search = |input: &Input<'_>, caps: &mut Captures| {
        Ok(re.search_captures(input, caps))
    };
    if find.count {
        run_counts(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            re.group_info(),
            search,
        )?;
    } else {
        run_search(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            re.group_info(),
            search,
        )?;
    }
    Ok(())
}

fn run_lite(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Executes a search for full matches using the top-level regex-lite engine.

USAGE:
    regex-cli find capture lite [-p <pattern> ...] <haystack-path>
    regex-cli find capture lite [-p <pattern> ...] -y <haystack>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut common = args::common::Config::default();
    let mut patterns = args::patterns::Config::only_flags();
    let mut haystack = args::haystack::Config::default();
    let mut syntax = args::syntax::Config::default();
    let mut lite = args::lite::Config::default();
    let mut find = super::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            &mut common,
            &mut patterns,
            &mut haystack,
            &mut syntax,
            &mut lite,
            &mut find,
        ],
    )?;

    let pats = patterns.get()?;
    let syn = syntax.syntax()?;
    let mut table = Table::empty();
    let (re, time) = util::timeitr(|| lite.from_patterns(&syn, &pats))?;
    table.add("build regex time", time);

    // Check that the haystack is valid UTF-8 since regex-lite doesn't support
    // searching arbitrary byte sequences. (At time of writing.)
    haystack.get()?.to_str()?;

    // The top-level API doesn't support regex-automata's more granular Input
    // abstraction.
    let input = args::input::Config::default();
    // The top-level API also doesn't use 'Captures' from regex-automata
    // directly, but we can map between them with some annoyance.
    let group_info = GroupInfo::new([re.capture_names()])
        .context("could not build capture group info")?;
    let mut locs = re.capture_locations();
    let search = |input: &Input<'_>, caps: &mut Captures| {
        let haystack = input.haystack().to_str().unwrap();
        caps.set_pattern(None);
        if !re.captures_read_at(&mut locs, haystack, input.start()).is_some() {
            return Ok(());
        }
        caps.set_pattern(Some(PatternID::ZERO));
        for i in 0..locs.len() {
            use regex_automata::util::primitives::NonMaxUsize;

            let slot_start = i * 2;
            let slot_end = slot_start + 1;
            match locs.get(i) {
                None => {
                    caps.slots_mut()[slot_start] = None;
                    caps.slots_mut()[slot_end] = None;
                }
                Some((start, end)) => {
                    caps.slots_mut()[slot_start] = NonMaxUsize::new(start);
                    caps.slots_mut()[slot_end] = NonMaxUsize::new(end);
                }
            }
        }
        Ok(())
    };
    if find.count {
        run_counts(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            &group_info,
            search,
        )?;
    } else {
        run_search(
            &mut table,
            &common,
            &find,
            &input,
            &haystack,
            &group_info,
            search,
        )?;
    }
    Ok(())
}

/// A function that takes in a bunch of configuration, runs the given search
/// routine, and prints out a table of counts.
fn run_counts(
    table: &mut Table,
    common: &args::common::Config,
    find: &super::Config,
    input: &args::input::Config,
    haystack: &args::haystack::Config,
    group_info: &GroupInfo,
    mut search: impl FnMut(&Input<'_>, &mut Captures) -> Result<(), MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (counts, time) = util::timeitr(|| {
            let mut counts = vec![vec![]; group_info.pattern_len()];
            for i in 0..group_info.pattern_len() {
                let pid = PatternID::new(i).context("invalid pattern ID")?;
                counts[i] = vec![0; group_info.group_len(pid)];
            }
            let mut caps = Captures::all(group_info.clone());
            for _ in 0..find.repeat() {
                let mut it =
                    regex_automata::util::iter::Searcher::new(input.clone());
                loop {
                    let m = it.try_advance(|input| {
                        search(input, &mut caps)?;
                        Ok(caps.get_match())
                    })?;
                    match m {
                        None => break,
                        Some(m) => {
                            for (i, span) in caps.iter().enumerate() {
                                if span.is_some() {
                                    counts[m.pattern()][i] += 1;
                                }
                            }
                        }
                    }
                }
            }
            Ok::<_, anyhow::Error>(counts)
        })?;
        table.add("search time", time);
        table.add("total matches", counts.iter().map(|c| c[0]).sum::<u64>());
        if common.table() {
            table.print(&mut out)?;
        }
        if !common.quiet {
            for (i, pattern_counts) in counts.iter().enumerate() {
                let pid = PatternID::new(i).context("invalid pattern ID")?;
                write!(out, "{}:{{ ", pid.as_usize())?;
                let names = group_info.pattern_names(pid);
                for (group_index, maybe_name) in names.enumerate() {
                    if group_index > 0 {
                        write!(out, ", ")?;
                    }
                    let count = pattern_counts[group_index];
                    if let Some(name) = maybe_name {
                        write!(out, "{group_index}/{name}: {count}")?;
                    } else {
                        write!(out, "{group_index}: {count}")?;
                    }
                }
                write!(out, " }}\n")?;
            }
        }
        Ok(())
    })
}

/// Like `run_counts`, but prints the actual matches instead.
fn run_search(
    table: &mut Table,
    common: &args::common::Config,
    find: &super::Config,
    input: &args::input::Config,
    haystack: &args::haystack::Config,
    group_info: &GroupInfo,
    mut search: impl FnMut(&Input<'_>, &mut Captures) -> Result<(), MatchError>,
) -> anyhow::Result<()> {
    let mut out = stdout();
    input.with(haystack, |input| {
        let (matches, time) = util::timeitr(|| {
            let mut matches = vec![];
            for _ in 0..find.repeat() {
                let caps = Captures::all(group_info.clone());
                let it =
                    regex_automata::util::iter::Searcher::new(input.clone())
                        .into_captures_iter(caps, &mut search);
                for caps in it {
                    matches.push(caps?);
                }
            }
            Ok::<_, anyhow::Error>(matches)
        })?;
        table.add("search time", time);
        table.add("total matches", matches.len());
        if common.table() {
            table.print(&mut out)?;
        }
        if !common.quiet {
            for caps in matches.iter() {
                let pid = caps.pattern().unwrap();
                write!(out, "{}:{{ ", pid.as_usize())?;
                let names = caps.group_info().pattern_names(pid);
                for (group_index, maybe_name) in names.enumerate() {
                    if group_index > 0 {
                        write!(out, ", ")?;
                    }
                    if let Some(name) = maybe_name {
                        write!(out, "{group_index}/{name}: ")?;
                    } else {
                        write!(out, "{group_index}: ")?;
                    }
                    match caps.get_group(group_index) {
                        None => write!(out, "NONE")?,
                        Some(sp) => {
                            let string = input.haystack()[sp].escape_bytes();
                            write!(
                                out,
                                "{}..{}/{}",
                                sp.start, sp.end, string
                            )?;
                        }
                    }
                }
                write!(out, " }}\n")?;
            }
        }
        Ok(())
    })
}
