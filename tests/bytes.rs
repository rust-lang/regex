// These are tests specifically crafted for regexes that can match arbitrary
// bytes.

// A silly wrapper to make it possible to write and match raw bytes.
struct R<'a>(&'a [u8]);
impl<'a> R<'a> { fn as_bytes(&self) -> &'a [u8] { &self.0 } }

mat!(word_boundary, r"(?-u) \b", " δ", None);
mat!(word_boundary_unicode, r" \b", " δ", Some((0, 1)));
mat!(word_not_boundary, r"(?-u) \B", " δ", Some((0, 1)));
mat!(word_not_boundary_unicode, r" \B", " δ", None);

mat!(perl_w_ascii, r"(?-u)\w+", "aδ", Some((0, 1)));
mat!(perl_w_unicode, r"\w+", "aδ", Some((0, 3)));
mat!(perl_d_ascii, r"(?-u)\d+", "1२३9", Some((0, 1)));
mat!(perl_d_unicode, r"\d+", "1२३9", Some((0, 8)));
mat!(perl_s_ascii, r"(?-u)\s+", " \u{1680}", Some((0, 1)));
mat!(perl_s_unicode, r"\s+", " \u{1680}", Some((0, 4)));

// The first `(.+)` matches two Unicode codepoints, but can't match the 5th
// byte, which isn't valid UTF-8. The second (byte based) `(.+)` takes over and
// matches.
mat!(mixed1, r"(.+)(?-u)(.+)", R(b"\xCE\x93\xCE\x94\xFF"),
     Some((0, 5)), Some((0, 4)), Some((4, 5)));

mat!(case_ascii_one, r"(?i-u)a", "A", Some((0, 1)));
mat!(case_ascii_class, r"(?i-u)[a-z]+", "AaAaA", Some((0, 5)));
mat!(case_unicode, r"(?i)[a-z]+", "aA\u{212A}aA", Some((0, 7)));
mat!(case_not_unicode, r"(?i-u)[a-z]+", "aA\u{212A}aA", Some((0, 2)));

mat!(negate_unicode, r"[^a]", "δ", Some((0, 2)));
mat!(negate_not_unicode, r"(?-u)[^a]", "δ", Some((0, 1)));

// This doesn't match in a normal Unicode regex because the implicit preceding
// `.*?` is Unicode aware.
mat!(dotstar_prefix_not_unicode1, r"(?-u)a", R(b"\xFFa"), Some((1, 2)));
mat!(dotstar_prefix_not_unicode2, r"a", R(b"\xFFa"), Some((1, 2)));

// Have fun with null bytes.
mat!(null_bytes, r"(?-u)(?P<cstr>[^\x00]+)\x00",
     R(b"foo\x00"), Some((0, 4)), Some((0, 3)));
