module d_phobos_ct;

version(CtRegex):

static immutable PATTERNS = [
    r"y", // misc::literal
    r".y", // misc::not_literal
    "[abcdw]", // misc::match_class
    "[ac]", // misc::match_class_in_range
    r"\p{L}", // misc::match_class_unicode / sherlock::letters
    r"^zbc(d|e)", // misc::anchored_literal_long_non_match / misc::anchored_literal_short_non_match
    r"^.bc(d|e)", // misc::anchored_literal_short_match / misc::anchored_literal_long_match
    r"^.bc(d|e)*$", // misc::one_pass_short
    r".bc(d|e)*$", // misc::one_pass_short_not
    r"^abcdefghijklmnopqrstuvwxyz.*$", // misc::one_pass_long_prefix
    r"^.bcdefghijklmnopqrstuvwxyz.*$", // misc::one_pass_long_prefix_not
    r"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", // misc::long_needle1
    r"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbba", // misc::long_needle2
    r"[r-z].*bcdefghijklmnopq", // misc::reverse_suffix_no_quadratic
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ$", // misc::easy0
    r"A[AB]B[BC]C[CD]D[DE]E[EF]F[FG]G[GH]H[HI]I[IJ]J$", // misc::easy1
    r"[XYZ]ABCDEFGHIJKLMNOPQRSTUVWXYZ$", // misc::medium
    r"[ -~]*ABCDEFGHIJKLMNOPQRSTUVWXYZ$", // misc::hard
    r"[ -~]*ABCDEFGHIJKLMNOPQRSTUVWXYZ.*", // misc::reallyhard
    r"\w+\s+Holmes", // misc::reallyhard2
    // This causes compile times to go from ~40s to ~9m with dmd 2.077.1
    //r"a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", // misc::no_exponential
    r">[^\n]*\n|\n", // dna::find_new_lines
    r"agggtaaa|tttaccct", // dna::variant1
    r"[cgt]gggtaaa|tttaccc[acg]", // dna::variant2
    r"a[act]ggtaaa|tttacc[agt]t", // dna::variant3
    r"ag[act]gtaaa|tttac[agt]ct", // dna::variant4
    r"agg[act]taaa|ttta[agt]cct", // dna::variant5
    r"aggg[acg]aaa|ttt[cgt]ccct", // dna::variant6
    r"agggt[cgt]aa|tt[acg]accct", // dna::variant7
    r"agggta[cgt]a|t[acg]taccct", // dna::variant8
    r"agggtaa[cgt]|[acg]ttaccct", // dna::variant9
    r"B", // dna::subst1
    r"D", // dna::subst2
    r"H", // dna::subst3
    r"K", // dna::subst4
    r"M", // dna::subst5
    r"N", // dna::subst6
    r"R", // dna::subst7
    r"S", // dna::subst8
    r"V", // dna::subst9
    r"W", // dna::subst10
    r"Y", // dna::subst11
    r"Sherlock", // sherlock::name_sherlock
    r"Holmes", // sherlock::name_holmes
    r"Sherlock Holmes", // sherlock::name_sherlock_holmes
    r"(?i)Sherlock", // sherlock::name_sherlock_nocase
    r"(?i)Holmes", // sherlock::name_holmes_nocase
    r"(?i)Sherlock Holmes", // sherlock::name_sherlock_holmes_nocase
    r"Sherlock\s+Holmes", // sherlock::name_whitespace
    r"Sherlock|Street", // sherlock::name_alt1
    r"Sherlock|Holmes", // sherlock::name_alt2
    r"Sherlock|Holmes|Watson|Irene|Adler|John|Baker", // sherlock::name_alt3
    r"(?i)Sherlock|Holmes|Watson|Irene|Adler|John|Baker", // sherlock::name_alt3_nocase
    r"Sher[a-z]+|Hol[a-z]+", // sherlock::name_alt4
    r"(?i)Sher[a-z]+|Hol[a-z]+", // sherlock::name_alt4_nocase
    r"Sherlock|Holmes|Watson", // sherlock::name_alt5
    r"(?i)Sherlock|Holmes|Watson", // sherlock::name_alt5_nocase
    r"zqj", // sherlock::no_match_uncommon
    r"aqj", // sherlock::no_match_common
    r"aei", // sherlock::no_match_really_common
    r"the", // sherlock::the_lower
    r"The", // sherlock::the_upper
    r"(?i)the", // sherlock::the_nocase
    r"the\s+\w+", // sherlock::the_whitespace
    r"\p{Lu}", // sherlock::letters_upper
    r"\p{Ll}", // sherlock::letters_lower
    r"\w+", // sherlock::words
    r"\w+\s+Holmes", // sherlock::before_holmes
    r"\w+\s+Holmes\s+\w+", // sherlock::before_after_holmes
    r"Holmes.{0,25}Watson|Watson.{0,25}Holmes", // sherlock::holmes_cochar_watson
    r"Holmes(?:\s*.+\s*){0,10}Watson|Watson(?:\s*.+\s*){0,10}Holmes", // sherlock::holmes_coword_watson
    `["'][^"']{0,30}[?!.]["']`, // sherlock::quotes
    r"(?m)^Sherlock Holmes|Sherlock Holmes$", // sherlock::line_boundary_sherlock_holmes
    r"\b\w+n\b", // sherlock::word_ending_n
    r"[a-q][^u-z]{13}x", // sherlock::repeated_class_negation
    r"[a-zA-Z]+ing", // sherlock::ing_suffix
    r"\s[a-zA-Z]{0,12}ing\s", // sherlock::ing_suffix_limited_space
];

public auto getCtRegex() {
    import std.regex;
    import std.string;

    Regex!char[string] aa;

    static foreach (pattern; PATTERNS) {
        static if (pattern.startsWith("(?i)")) {
            aa[pattern] = ctRegex!(pattern[4..$], "gi");
        } else static if (pattern.startsWith("(?m)")) {
            aa[pattern] = ctRegex!(pattern[4..$], "gm");
        } else {
            aa[pattern] = ctRegex!(pattern, "g");
        }
    }

    return aa;
}
