
//
// Just some unit tests that I found it useful to focus on while
// debugging the onepass DFA. Mostly these are simplifications
// of existing tests, so their value is not that huge, but
// why throw out tests that have been useful in the past.
// This is definitely not an appropriate permanent home
// for them. I should ask @burntsushi about where a better place
// for them would be (maybe in misc.rs?). Alternatively I could
// not be lazy and just actually try to grok each of the test
// modules.
//


mat!(trailing_repeat, "ab(?:ab)?", "abac", Some((0, 2)));

// Currently fail to compile because empty branches are not allowed!
// Yay! In the future we might have to worry about this though.
//
// mat!(trailing_alt_with_empty_branch, "ab(?:ab|)", "abac", Some((0, 2)));
// mat!(trailing_lazy_alt_with_empty_branch, "ab(?:|ab)", "abab", Some((0, 2)));

matiter!(match_multi_rep_4, r"(?m)(?:^a)+", "aaa\naaa\naaa",
         (0, 1), (4, 5), (8, 9));

mat!(startline_a_rep, r"(?m)(?:^a)+", "aaa", Some((0, 1)));
