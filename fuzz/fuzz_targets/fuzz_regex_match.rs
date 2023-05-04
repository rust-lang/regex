#![no_main]
use libfuzzer_sys::fuzz_target;

use libfuzzer_sys::arbitrary;

#[derive(arbitrary::Arbitrary)]
struct FuzzCase<'a> {
    pattern: &'a str,
    input: &'a str,
    case_insensitive: bool,
    multi_line: bool,
    dot_matches_new_line: bool,
    swap_greed: bool,
    ignore_whitespace: bool,
    unicode: bool,
    octal: bool,
}

impl std::fmt::Debug for FuzzCase<'_> {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let Self {
            pattern,
            case_insensitive,
            multi_line,
            dot_matches_new_line,
            swap_greed,
            ignore_whitespace,
            unicode,
            octal,
            input,
        } = self;

        write!(
            fmt,
            r#"
let r = regex::RegexBuilder::new({pattern:?})
    .case_insensitive({case_insensitive:?})
    .multi_line({multi_line:?})
    .dot_matches_new_line({dot_matches_new_line:?})
    .swap_greed({swap_greed:?})
    .ignore_whitespace({ignore_whitespace:?})
    .unicode({unicode:?})
    .octal({octal:?})
    .build();

if let Ok(re) = r {{
    re.is_match({input:?});
}}
        "#
        )
    }
}

fuzz_target!(|case: FuzzCase| {
    let r = regex::RegexBuilder::new(case.pattern)
        .case_insensitive(case.case_insensitive)
        .multi_line(case.multi_line)
        .dot_matches_new_line(case.dot_matches_new_line)
        .swap_greed(case.swap_greed)
        .ignore_whitespace(case.ignore_whitespace)
        .unicode(case.unicode)
        .octal(case.octal)
        .build();

    if let Ok(re) = r {
        re.is_match(case.input);
    }
});
