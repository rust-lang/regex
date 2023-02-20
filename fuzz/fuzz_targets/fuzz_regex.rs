#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use regex_syntax::hir::print::Printer;
use regex_syntax::hir::Hir;
use std::{convert::TryFrom, hint::black_box};

#[derive(Arbitrary, Debug, Clone)]
enum Pattern {
    WellFormed(Hir),
    Random(String),
}

impl TryFrom<Pattern> for String {
    type Error = std::fmt::Error;

    fn try_from(pattern: Pattern) -> Result<Self, Self::Error> {
        match pattern {
            Pattern::WellFormed(hir) => {
                let mut printer = Printer::new();
                let mut dst = String::new();
                printer.print(&hir, &mut dst)?;
                return Ok(dst);
            }
            Pattern::Random(s) => {
                return Ok(s);
            }
        }
    }
}

#[derive(Arbitrary, Debug)]
struct Data<'a> {
    pattern: Pattern,
    replacen: (usize, &'a str),
    replacen_bytes: (usize, &'a [u8]),
    input: &'a str,
    input_bytes: &'a [u8],
    pattern_set: Vec<Pattern>,
    set_input: &'a str,
    set_input_bytes: &'a [u8],
}

fn fuzz_regex(
    pattern: &Pattern,
    input: &str,
    replacen: &(usize, &str),
) -> Result<(), Box<dyn std::error::Error>> {
    let re = regex::Regex::new(&String::try_from(pattern.clone())?)?;
    _ = black_box(re.is_match(&input));
    _ = black_box(re.captures_iter(&input).collect::<Vec<regex::Captures>>());
    _ = black_box(re.split(&input).collect::<Vec<&str>>());

    let (limit, replace) = *replacen;
    _ = black_box(re.replacen(&input, limit, replace));

    _ = black_box(re.find(&input));
    _ = black_box(re.shortest_match(&input));
    Ok(())
}

fn fuzz_regex_bytes(
    pattern: &Pattern,
    input: &[u8],
    replacen: &(usize, &[u8]),
) -> Result<(), Box<dyn std::error::Error>> {
    let re = regex::bytes::Regex::new(&String::try_from(pattern.clone())?)?;
    _ = black_box(re.is_match(&input));
    _ = black_box(
        re.captures_iter(&input).collect::<Vec<regex::bytes::Captures>>(),
    );
    _ = black_box(re.split(&input).collect::<Vec<&[u8]>>());

    let (limit, replace) = *replacen;
    _ = black_box(re.replacen(&input, limit, replace));

    _ = black_box(re.find(&input));
    _ = black_box(re.shortest_match(&input));
    Ok(())
}

fn fuzz_regex_set(
    pattern_set: &Vec<Pattern>,
    input: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    let set = regex::RegexSet::new(
        pattern_set
            .into_iter()
            .filter_map(|x| String::try_from(x.clone()).ok()),
    )?;
    _ = black_box(set.is_match(&input));
    _ = black_box(set.matches(&input).into_iter().collect::<Vec<_>>());
    Ok(())
}

fn fuzz_regex_set_bytes(
    pattern_set: &Vec<Pattern>,
    input: &[u8],
) -> Result<(), Box<dyn std::error::Error>> {
    let set = regex::bytes::RegexSet::new(
        pattern_set
            .into_iter()
            .filter_map(|x| String::try_from(x.clone()).ok()),
    )?;
    _ = black_box(set.is_match(&input));
    _ = black_box(set.matches(&input).into_iter().collect::<Vec<_>>());
    Ok(())
}

fuzz_target!(|data: Data| {
    if data.pattern_set.len() > 10 {
        return;
    }
    let (_, replace) = data.replacen;
    if replace.len() > 100 {
        return;
    }
    let (_, replace) = data.replacen_bytes;
    if replace.len() > 100 {
        return;
    }
    if data.set_input.len() > 500 {
        return;
    }
    if data.set_input_bytes.len() > 500 {
        return;
    }
    if data.input_bytes.len() > 500 {
        return;
    }
    if data.input.len() > 500 {
        return;
    }

    if let Err(e) =
        black_box(fuzz_regex(&data.pattern, &data.input, &data.replacen))
    {
        black_box(format!("{e:?}"));
    }

    if let Err(e) = black_box(fuzz_regex_bytes(
        &data.pattern,
        &data.input_bytes,
        &data.replacen_bytes,
    )) {
        black_box(format!("{e:?}"));
    }
    if let Err(e) =
        black_box(fuzz_regex_set(&data.pattern_set, &data.set_input))
    {
        black_box(format!("{e:?}"));
    }

    if let Err(e) = black_box(fuzz_regex_set_bytes(
        &data.pattern_set,
        &data.set_input_bytes,
    )) {
        black_box(format!("{e:?}"));
    }
});
