#![no_main]

use {
    libfuzzer_sys::{fuzz_target, Corpus},
    regex_automata::{
        dfa::{dense::DFA, regex::Builder as RegexBuilder},
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

    let config = DFA::config().dfa_size_limit(Some(1 << 20));
    let Ok(dense_re) = RegexBuilder::new().dense(config).build(&pattern) else {
        return Corpus::Reject;
    };
    let (Ok(fwd), Ok(rev)) = (dense_re.forward().to_sparse(), dense_re.reverse().to_sparse()) else {
        return Corpus::Reject;
    };
    let re = RegexBuilder::new().build_from_dfas(fwd, rev);

    assert_eq!(
        re.is_match(&data.haystack),
        baseline.is_match(&mut cache, &data.haystack)
    );
    let found1 = re.find(&data.haystack);
    let found2 = baseline.find(&mut cache, &data.haystack);
    if let Some(found1) = found1 {
        let found2 = found2.expect("Found in target, but not in baseline!");
        assert_eq!(found1.start(), found2.start());
        assert_eq!(found1.end(), found2.end());
    }

    // no captures

    Corpus::Keep
}

fuzz_target!(|data: FuzzData| -> Corpus { do_fuzz(data) });
