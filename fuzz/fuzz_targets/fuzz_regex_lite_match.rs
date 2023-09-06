#![no_main]

use libfuzzer_sys::{arbitrary, fuzz_target, Corpus};

#[derive(arbitrary::Arbitrary)]
struct FuzzCase<'a> {
    pattern: &'a str,
    haystack: &'a str,
    case_insensitive: bool,
    multi_line: bool,
    crlf: bool,
    dot_matches_new_line: bool,
    swap_greed: bool,
    ignore_whitespace: bool,
}

impl std::fmt::Debug for FuzzCase<'_> {
    fn fmt(
        &self,
        fmt: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let FuzzCase {
            pattern,
            case_insensitive,
            multi_line,
            crlf,
            dot_matches_new_line,
            swap_greed,
            ignore_whitespace,
            haystack,
        } = self;

        write!(
            fmt,
            r#"
let Ok(re) = regex_lite::RegexBuilder::new({pattern:?})
    .case_insensitive({case_insensitive:?})
    .multi_line({multi_line:?})
    .crlf({crlf:?})
    .dot_matches_new_line({dot_matches_new_line:?})
    .swap_greed({swap_greed:?})
    .ignore_whitespace({ignore_whitespace:?})
    .build() else {{ return }};
re.is_match({haystack:?});
        "#
        )
    }
}

fuzz_target!(|case: FuzzCase| -> Corpus {
    let _ = env_logger::try_init();

    let Ok(re) = regex_lite::RegexBuilder::new(case.pattern)
        .case_insensitive(case.case_insensitive)
        .multi_line(case.multi_line)
        .crlf(case.crlf)
        .dot_matches_new_line(case.dot_matches_new_line)
        .swap_greed(case.swap_greed)
        .ignore_whitespace(case.ignore_whitespace)
        .size_limit(1<<20)
        .build() else { return Corpus::Reject };
    re.is_match(case.haystack);
    Corpus::Keep
});
