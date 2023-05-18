#![no_main]

use {
    libfuzzer_sys::fuzz_target, regex::RegexBuilder, regex_syntax::ast::Ast,
};

#[derive(Eq, PartialEq, arbitrary::Arbitrary)]
struct FuzzData {
    ast: Ast,
}

impl std::fmt::Debug for FuzzData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("FuzzData");
        builder.field("ast", &format!("{}", self.ast));
        builder.finish()
    }
}

fuzz_target!(|data: FuzzData| {
    let _ = env_logger::try_init();

    let pattern = format!("{}", data.ast);
    RegexBuilder::new(&pattern).size_limit(1 << 20).build().ok();
});
