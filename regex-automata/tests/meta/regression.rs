use regex_automata::meta::Regex;

// Tests that a regex whose NFA is too big for the bounded backtracker's
// visited capacity doesn't panic on an empty haystack.
//
// See: https://github.com/rust-lang/regex/issues/1344
#[test]
fn backtracker_visited_capacity_too_small_empty_haystack() {
    // The other engines are disabled so that this exercises the backtracker
    // path rather than relying on them declining this pattern.
    let config = Regex::config()
        .nfa_size_limit(Some(1_000_000_000))
        .dfa(false)
        .hybrid(false)
        .onepass(false);
    let re =
        Regex::builder().configure(config).build(r"^.{0,404600}$").unwrap();

    assert!(re.is_match(""));
    assert_eq!(Some((0, 0)), re.find("").map(|m| (m.start(), m.end())));
}
