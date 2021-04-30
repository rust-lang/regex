use test::Bencher;

use crate::{Regex, Text};

// USAGE: dna!(name, pattern, count)
//
// This is same as bench_find, except it always uses the regexdna haystack.
macro_rules! dna {
    ($name:ident, $pattern:expr, $count:expr) => {
        bench_find!(
            $name,
            $pattern,
            $count,
            include_str!("data/regexdna.txt").to_owned()
        );
    };
}

dna!(find_new_lines, r">[^\n]*\n|\n", 83337);
dna!(variant1, r"agggtaaa|tttaccct", 32);
dna!(variant2, r"[cgt]gggtaaa|tttaccc[acg]", 115);
dna!(variant3, r"a[act]ggtaaa|tttacc[agt]t", 368);
dna!(variant4, r"ag[act]gtaaa|tttac[agt]ct", 254);
dna!(variant5, r"agg[act]taaa|ttta[agt]cct", 466);
dna!(variant6, r"aggg[acg]aaa|ttt[cgt]ccct", 135);
dna!(variant7, r"agggt[cgt]aa|tt[acg]accct", 137);
dna!(variant8, r"agggta[cgt]a|t[acg]taccct", 139);
dna!(variant9, r"agggtaa[cgt]|[acg]ttaccct", 197);
dna!(subst1, r"B", 29963);
dna!(subst2, r"D", 29987);
dna!(subst3, r"H", 30004);
dna!(subst4, r"K", 29974);
dna!(subst5, r"M", 29979);
dna!(subst6, r"N", 29959);
dna!(subst7, r"R", 30012);
dna!(subst8, r"S", 29994);
dna!(subst9, r"V", 29996);
dna!(subst10, r"W", 29984);
dna!(subst11, r"Y", 29947);
