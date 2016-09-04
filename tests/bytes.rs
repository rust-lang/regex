// These are tests specifically crafted for regexes that can match arbitrary
// bytes.

// A silly wrapper to make it possible to write and match raw bytes.
struct R<'a>(&'a [u8]);
impl<'a> R<'a> { fn as_bytes(&self) -> &'a [u8] { &self.0 } }

mat!(word_boundary, r" \b", " δ", None);
mat!(word_boundary_unicode, r"(?u) \b", " δ", Some((0, 1)));
mat!(word_not_boundary, r" \B", " δ", Some((0, 1)));
mat!(word_not_boundary_unicode, r"(?u) \B", " δ", None);

mat!(perl_w_ascii, r"\w+", "aδ", Some((0, 1)));
mat!(perl_w_unicode, r"(?u)\w+", "aδ", Some((0, 3)));
mat!(perl_d_ascii, r"\d+", "1२३9", Some((0, 1)));
mat!(perl_d_unicode, r"(?u)\d+", "1२३9", Some((0, 8)));
mat!(perl_s_ascii, r"\s+", " \u{1680}", Some((0, 1)));
mat!(perl_s_unicode, r"(?u)\s+", " \u{1680}", Some((0, 4)));

// The first `(.+)` matches two Unicode codepoints, but can't match the 5th
// byte, which isn't valid UTF-8. The second (byte based) `(.+)` takes over and
// matches.
mat!(mixed1, r"(?u)(.+)(?-u)(.+)", R(b"\xCE\x93\xCE\x94\xFF"),
     Some((0, 5)), Some((0, 4)), Some((4, 5)));

mat!(case_ascii_one, r"(?i)a", "A", Some((0, 1)));
mat!(case_ascii_class, r"(?i)[a-z]+", "AaAaA", Some((0, 5)));
mat!(case_unicode, r"(?iu)[a-z]+", "aA\u{212A}aA", Some((0, 7)));
mat!(case_not_unicode, r"(?i)[a-z]+", "aA\u{212A}aA", Some((0, 2)));

mat!(negate_unicode, r"(?u)[^a]", "δ", Some((0, 2)));
mat!(negate_not_unicode, r"[^a]", "δ", Some((0, 1)));

// This doesn't match in a normal Unicode regex because the implicit preceding
// `.*?` is Unicode aware.
mat!(dotstar_prefix_not_unicode, r"a", R(b"\xFFa"), Some((1, 2)));

// Have fun with null bytes.
mat!(null_bytes, r"(?P<cstr>[^\x00]+)\x00",
     R(b"foo\x00"), Some((0, 4)), Some((0, 3)));

// Test that lookahead operators work properly in the face of invalid UTF-8.
// See: https://github.com/rust-lang-nursery/regex/issues/277
matiter!(invalidutf8_anchor1,
         r"\xcc?^",
         R(b"\x8d#;\x1a\xa4s3\x05foobarX\\\x0f0t\xe4\x9b\xa4"),
         (0, 0));
matiter!(invalidutf8_anchor2,
         r"^\xf7|4\xff\d\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a##########[] d\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a\x8a##########[] #####\x80\S7|$",
         R(b"\x8d#;\x1a\xa4s3\x05foobarX\\\x0f0t\xe4\x9b\xa4"),
         (22, 22));
matiter!(invalidutf8_anchor3,
         r"^|ddp\xff\xffdddddlQd@\x80",
         R(b"\x8d#;\x1a\xa4s3\x05foobarX\\\x0f0t\xe4\x9b\xa4"),
         (0, 0));
