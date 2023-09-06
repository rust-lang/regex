#![no_main]

use libfuzzer_sys::{fuzz_target, Corpus};

fuzz_target!(|data: &[u8]| -> Corpus {
    run(data).map_or(Corpus::Reject, |_| Corpus::Keep)
});

fn run(given_data: &[u8]) -> Option<()> {
    use regex_automata::dfa::Automaton;

    let _ = env_logger::try_init();

    if given_data.len() < 2 {
        return None;
    }
    let haystack_len = usize::from(given_data[0]);
    let haystack = given_data.get(1..1 + haystack_len)?;
    let given_dfa_bytes = given_data.get(1 + haystack_len..)?;

    // We help the fuzzer along by adding a preamble to the bytes that should
    // at least make these first parts valid. The preamble expects a very
    // specific sequence of bytes, so it makes sense to just force this.
    let label = "rust-regex-automata-dfa-sparse\x00\x00";
    assert_eq!(0, label.len() % 4);
    let endianness_check = 0xFEFFu32.to_ne_bytes().to_vec();
    let version_check = 2u32.to_ne_bytes().to_vec();
    let mut dfa_bytes: Vec<u8> = vec![];
    dfa_bytes.extend(label.as_bytes());
    dfa_bytes.extend(&endianness_check);
    dfa_bytes.extend(&version_check);
    dfa_bytes.extend(given_dfa_bytes);
    // This is the real test: checking that any input we give to
    // DFA::from_bytes will never result in a panic.
    let (dfa, _) =
        regex_automata::dfa::sparse::DFA::from_bytes(&dfa_bytes).ok()?;
    let _ = dfa.try_search_fwd(&regex_automata::Input::new(haystack));
    Some(())
}
