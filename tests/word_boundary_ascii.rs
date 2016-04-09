// ASCII word boundaries are completely oblivious to Unicode characters.
// For Unicode word boundaries, the tests are precisely inverted.
matiter!(ascii1, r"\bx\b", "áxβ", (2, 3));
matiter!(ascii2, r"\Bx\B", "áxβ");

// We can still get Unicode mode in byte regexes.
matiter!(unicode1, r"(?u:\b)x(?u:\b)", "áxβ");
matiter!(unicode2, r"(?u:\B)x(?u:\B)", "áxβ", (2, 3));
