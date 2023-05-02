#![no_main]

use {
    libfuzzer_sys::{fuzz_target, Corpus},
    regex::bytes::RegexBuilder,
    regex_syntax::ast::Ast,
};

#[derive(arbitrary::Arbitrary, Eq, PartialEq)]
struct FuzzData {
    ast: Ast,
    haystack: Vec<u8>,
}

impl std::fmt::Debug for FuzzData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("FuzzData");
        builder.field("ast", &format!("{}", self.ast));
        builder.field("haystack", &self.haystack);
        builder.finish()
    }
}

fuzz_target!(|data: FuzzData| -> Corpus {
    let _ = env_logger::try_init();

    let pattern = format!("{}", data.ast);
    let Ok(re) = RegexBuilder::new(&pattern).size_limit(1<<20).build() else {
        return Corpus::Reject;
    };
    re.is_match(&data.haystack);
    re.find(&data.haystack);
    re.captures(&data.haystack).map_or(0, |c| c.len());
    Corpus::Keep
});
