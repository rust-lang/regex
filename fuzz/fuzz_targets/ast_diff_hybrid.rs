#![no_main]

use regex_automata::Input;
use {
    libfuzzer_sys::{fuzz_target, Corpus},
    regex_automata::{
        hybrid::{dfa::DFA, regex::Builder as RegexBuilder},
        nfa::thompson::{pikevm::PikeVM as NfaRegex, NFA},
    },
    regex_syntax::ast::Ast,
};

#[derive(Eq, PartialEq, arbitrary::Arbitrary)]
struct FuzzData {
    ast: Ast,
    haystack: String,
}

impl std::fmt::Debug for FuzzData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("FuzzData");
        builder.field("ast", &format!("{}", self.ast));
        builder.field("haystack", &self.haystack);
        builder.finish()
    }
}

fn do_fuzz(data: FuzzData) -> Corpus {
    let _ = env_logger::try_init();

    let pattern = format!("{}", data.ast);
    let config = NFA::config().nfa_size_limit(Some(1 << 20));
    let Ok(nfa) = NFA::compiler().configure(config).build(&pattern) else {
        return Corpus::Reject;
    };
    let Ok(baseline) = NfaRegex::new_from_nfa(nfa) else {
        return Corpus::Reject;
    };
    let mut cache = baseline.create_cache();

    let config = DFA::config().cache_capacity(1 << 20);
    let Ok(re) = RegexBuilder::new().dfa(config).build(&pattern) else {
        return Corpus::Reject;
    };
    let mut hybrid_cache = re.create_cache();

    if let Ok(maybe_match) =
        re.try_search(&mut hybrid_cache, &Input::new(&pattern))
    {
        assert_eq!(
            maybe_match.is_some(),
            baseline.is_match(&mut cache, &data.haystack)
        );
        let found2 = baseline.find(&mut cache, &data.haystack);
        if let Some(found1) = maybe_match {
            let found2 =
                found2.expect("Found in target, but not in baseline!");
            assert_eq!(found1.start(), found2.start());
            assert_eq!(found1.end(), found2.end());
        }
    }

    // no captures

    Corpus::Keep
}

fuzz_target!(|data: FuzzData| -> Corpus { do_fuzz(data) });
