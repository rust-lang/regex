#![no_main]

use {
    libfuzzer_sys::{fuzz_target, Corpus},
    regex_automata::{
        dfa::{onepass::Builder as RegexBuilder, onepass::DFA},
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
    let mut baseline_cache = baseline.create_cache();

    let config = DFA::config().size_limit(Some(1 << 20));
    let Ok(onepass) = RegexBuilder::new().configure(config).build(&pattern) else {
        return Corpus::Reject;
    };
    let mut onepass_cache = onepass.create_cache();

    assert_eq!(
        onepass.is_match(&mut onepass_cache, &data.haystack),
        baseline.is_match(&mut baseline_cache, &data.haystack)
    );
    let found1 = onepass.find(&mut onepass_cache, &data.haystack);
    let found2 = baseline.find(&mut baseline_cache, &data.haystack);
    if let Some(found1) = found1 {
        let found2 = found2.expect("Found in target, but not in baseline!");
        assert_eq!(found1.start(), found2.start());
        assert_eq!(found1.end(), found2.end());
    }

    let mut onepass_captures = onepass.create_captures();
    onepass.captures(
        &mut onepass_cache,
        &data.haystack,
        &mut onepass_captures,
    );

    let mut baseline_captures = baseline.create_captures();
    baseline.captures(
        &mut baseline_cache,
        &data.haystack,
        &mut baseline_captures,
    );
    drop(baseline_cache);
    assert_eq!(onepass_captures.group_len(), baseline_captures.group_len());
    for (c1, c2) in onepass_captures.iter().zip(baseline_captures.iter()) {
        if let Some(c1) = c1 {
            let c2 = c2.expect("Matched in target, but not baseline!");
            assert_eq!(c1.start, c2.start);
            assert_eq!(c1.end, c2.end);
        } else {
            assert!(c2.is_none(), "Matched in baseline, but not target!");
        }
    }

    Corpus::Keep
}

fuzz_target!(|data: FuzzData| -> Corpus { do_fuzz(data) });
