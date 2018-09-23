use syntax::hir::{
    Hir, HirKind, Literal, ClassBytes, ClassBytesRange,
    Class, RepetitionRange, RepetitionKind
};
use utf8_ranges::Utf8Sequences;

/// True iff the given expression is onepass
///
/// The general approach here is to find all the places in
/// the given Hir where any sort of branching occurs,
/// and examine the start of each expression at the branch
/// to see if there is an ambiguity.
///
/// For example, given the regex `a|b`, we would examine
/// both branches of the alternation `a` and `b` and
/// notice that they don't clash, so the regex is onepass.
/// On the other hand the branches of `a|a` do clash,
/// so that regex is not onepass.
///
/// Alternations are not the only branch points in a regex.
/// We also have to make sure to consider repetitions like
/// `a*a`, which is not onepass because there is no way
/// to tell whether we have to loop back to the repeated
/// expression or continue on by looking at just one byte.
/// `a*b` is onepass because you can figure out what to do.
/// If you see an `a`, go back to the start of the loop,
/// and if you see a `b` continue onward.
///
/// A third, more subtle case is the case of concatenations
/// of expressions where some of the expressions can
/// accept the empty string. Consider `a(b|)ba`. This
/// regex is not onepass because it is not clear what to
/// do upon seeing the input `ab`. The problem is that `(b|)`
/// and `ba` clash with one other.
///
/// To get a bit more specific about what it means for two
/// expressions to clash, we introduce the concept of first
/// sets. The first set of an expression is the set of
/// bytes which might begin a word in the language of that
/// expression. If the expression can accept the empty string,
/// the first set takes note of that as well.
///
/// To handle these three cases, we use a visitor to
/// find the alternations, repetitions, and concatenations.
/// Whenever we find one of the above cases, we compute
/// the first set of the various branches involved,
/// then check to see if the first sets intersect. If
/// we ever find a non-empty intersection, the regex
/// is not onepass.
pub fn is_onepass(expr: &Hir) -> bool {
    fset_of(expr).is_onepass
}

/// Compute the first set of a given regular expression.
///
/// The first set of a regular expression is the set of all bytes
/// which might begin it. This is a less general version of the
/// notion of a regular expression preview (the first set can be
/// thought of as the 1-preview of a regular expression).
///
/// Note that first sets are byte-oriented because the DFA is
/// byte oriented. This means an expression like /Δ|δ/ is actually not
/// onepass, even though there is clearly no non-determinism inherent
/// to the regex at a unicode code point level (big delta and little
/// delta start with the same byte).
fn fset_of(expr: &Hir) -> FirstSet {
    fn singleton(b: u8) -> FirstSet {
        let mut f = FirstSet::empty();
        f.push_bytes(ClassBytesRange::new(b, b));
        f
    }

    match expr.kind() {
        &HirKind::Empty => FirstSet::epsilon(),
        &HirKind::Literal(ref lit) => {
            match lit {
                &Literal::Unicode(c) => singleton(first_byte(c)),
                &Literal::Byte(b) => singleton(b),
            }
        }
        &HirKind::Class(ref class) => {
            let mut fset = match class {
                &Class::Unicode(ref c) => {
                    // Get all the bytes which might begin this unicode
                    // class.
                    let mut cb = FirstSet::empty();
                    for cr in c.iter() {
                        for br in Utf8Sequences::new(cr.start(), cr.end()) {
                            let first = br.as_slice()[0];
                            cb.push_bytes(
                                ClassBytesRange::new(first.start, first.end));
                        }
                    }
                    cb
                }
                &Class::Bytes(ref b) =>
                    FirstSet::new(b.iter().map(|x| *x), false),
            };

            fset.is_onepass = class_is_onepass(class);
            fset
        }

        // When an empty look (Anchor or WordBoundary) is at the start of
        // a concatenation, we conservatively assume that the assertion
        // will pass, so we just drop it. Then we can only get to this
        // point if we are dealing with some sort of naked empty look.
        // For now we just do the most conservative thing and say
        // that such an emptylook could potentially match on any character.
        &HirKind::Anchor(_) | &HirKind::WordBoundary(_) => FirstSet::anychar(),

        &HirKind::Repetition(ref rep) => {
            let mut fset = fset_of(&rep.hir);

            fset.accepts_empty = match rep.kind {
                RepetitionKind::ZeroOrOne => true,
                RepetitionKind::ZeroOrMore => true,
                RepetitionKind::OneOrMore => fset.accepts_empty,
                RepetitionKind::Range(ref range) => {
                    match range {
                        &RepetitionRange::Exactly(0)
                        | &RepetitionRange::AtLeast(0)
                        | &RepetitionRange::Bounded(0, _) => true,
                        _ => fset.accepts_empty,
                    }
                }
            };

            fset
        },
        &HirKind::Group(ref group) => fset_of(&group.hir),

        // The most involved case. We need to strip leading empty-looks
        // as well as take the union of the first sets of the first n+1
        // expressions where n is the number of leading expressions which
        // accept the empty string.
        &HirKind::Concat(ref es) => {
            let mut fset = FirstSet::empty();
            let mut inner_fsets = Vec::with_capacity(es.len());
            for e in es.iter() {
                inner_fsets.push(fset_of(e));
            }
            for (i, e) in es.iter().enumerate() {
                match e.kind() {
                    &HirKind::Anchor(_) | &HirKind::WordBoundary(_) => {
                        // Ignore any leading emptylooks, but any in tail
                        // position have to be accounted for.
                        if i == es.len() - 1 {
                            fset.union(&FirstSet::anychar());
                            fset.accepts_empty = false;
                        }
                    }
                    _ => {
                        fset.union(&inner_fsets[i]);

                        if !inner_fsets[i].accepts_empty {
                            fset.accepts_empty = false;
                            // We can stop accumulating after we stop seeing
                            // first sets which contain epsilon.
                            break;
                        }
                    }
                }
            }

            fset.is_onepass = concat_is_onepass(es, &inner_fsets);

            fset
        }
        &HirKind::Alternation(ref es) => {
            let mut fset = FirstSet::empty();
            let mut inner_fsets = Vec::with_capacity(es.len());
            for (i, e) in es.iter().enumerate() {
                inner_fsets.push(fset_of(e));
                fset.union(&inner_fsets[i]);
            }

            fset.is_onepass = !FirstSet::fsets_clash_value(&inner_fsets);

            fset
        }
    }
}

// Unicode classes are really just big alternatives from the byte
// oriented point of view.
//
// This function translates a unicode class into the
// byte space and checks for intersecting first sets.
//
// Byte classes are always onepass
fn class_is_onepass(cls: &Class) -> bool {
    match cls {
        &Class::Unicode(ref ucls) => {
            let mut seen_char: [bool; 256] = [false; 256];

            for cr in ucls.iter() {
                for br in Utf8Sequences::new(cr.start(), cr.end()) {
                    let first = br.as_slice()[0];
                    for b in first.start..(first.end+1) {
                        if seen_char[b as usize] {
                            return false;
                        }
                        seen_char[b as usize] = true;
                    }
                }
            }
        }
        _ => {}
    }

    true
}

fn concat_is_onepass(es: &[Hir], inner_fsets: &[FirstSet]) -> bool {
    let mut empty_run = vec![];

    for (i, e) in NestedConcat::new(es).enumerate() {
        match e.kind() {
            &HirKind::Anchor(_) | &HirKind::WordBoundary(_) => {
                if i < es.len() - 1 {
                    continue;
                }
            }
            _ => {} // FALLTHROUGH
        }

        if !inner_fsets[i].is_onepass {
            return false;
        }

        let is_real_rep = match e.kind() {
            &HirKind::Repetition(ref rep) => {
                match rep.kind {
                    RepetitionKind::Range(
                        RepetitionRange::Exactly(_)) => false,
                    _ => true,
                }
            },
            _ => false,
        };

        empty_run.push(&inner_fsets[i]);
        if !(inner_fsets[i].accepts_empty || is_real_rep) {
            if FirstSet::fsets_clash_ref(&empty_run) {
                return false;
            }
            empty_run.clear();
        }
    }

    ! FirstSet::fsets_clash_ref(&empty_run)
}

/// The first byte of a unicode code point.
///
/// We only ever care about the first byte of a particular character
/// because the onepass DFA is implemented in the byte space not the
/// character space. This means, for example, that a branch between
/// lowercase delta and uppercase delta is actually non-deterministic.
fn first_byte(c: char) -> u8 {
    let mut b: [u8; 4] = [0; 4];
    c.encode_utf8(&mut b);
    b[0]
}

/// A representation of all the possible ways a word in the language
/// of a regex could begin. ClassBytes has no way to express the empty
/// string, so we add an extra flag to indicate if a FirstSet includes
/// epsilon. Put in a more theoretical way all firstsets are subsets of
/// SIGMA `union` { epsilon }.
#[derive(Debug, PartialEq, Eq, Clone)]
struct FirstSet {
    bytes: ClassBytes,
    accepts_empty: bool,
    is_onepass: bool,
}

/// A macro to define the fsets_clash associated functions,
/// parameterized over the type of the inner slice. This lets
/// us avoid allocating an extra vector when we check
/// alternations for onepassness.
macro_rules! def_fsets_clash {
    ($fun_name:ident, $slice_inner:ty) => {
        /// Check if a list of first sets is incompatible.
        fn $fun_name(fsets: &[$slice_inner]) -> bool {
            let mut seen_so_far = FirstSet::empty();

            for fset in fsets.iter() {
                let mut snapshot = seen_so_far.clone();
                snapshot.intersect(&fset);
                if ! snapshot.is_empty() {
                    return true;
                }

                seen_so_far.union(&fset);
            }

            false
        }
    }
}
impl FirstSet {
    fn empty() -> Self {
        FirstSet {
            bytes: ClassBytes::empty(),
            accepts_empty: false,
            is_onepass: true,
        }
    }

    pub fn new<I>(ranges: I, accepts_empty: bool) -> Self
    where I: IntoIterator<Item=ClassBytesRange>
    {
        FirstSet {
            bytes: ClassBytes::new(ranges),
            accepts_empty: accepts_empty,
            is_onepass: true,
        }
    }

    fn anychar() -> FirstSet {
        let mut f = FirstSet::empty();
        f.push_bytes(ClassBytesRange::new(b'\0', b'\xFF'));
        f
    }

    fn epsilon() -> FirstSet {
        FirstSet {
            bytes: ClassBytes::empty(),
            accepts_empty: true,
            is_onepass: true,
        }
    }

    fn push_bytes(&mut self, byte_range: ClassBytesRange) {
        self.bytes.push(byte_range);
    }

    fn union(&mut self, other: &FirstSet) {
        self.bytes.union(&other.bytes);
        self.accepts_empty = self.accepts_empty || other.accepts_empty;
    }

    fn intersect(&mut self, other: &FirstSet) {
        self.bytes.intersect(&other.bytes);
        self.accepts_empty = self.accepts_empty && other.accepts_empty;
    }

    fn is_empty(&self) -> bool {
        self.bytes.is_empty() && !self.accepts_empty
    }

    def_fsets_clash!(fsets_clash_ref, &FirstSet);
    def_fsets_clash!(fsets_clash_value, FirstSet);
}

/// An iterator over a concatenation of expressions which
/// drills down into other embedded concatenations.
struct NestedConcat<'a>(Vec<(&'a [Hir], usize)>);
impl<'a> NestedConcat<'a> {
    fn new(es: &'a [Hir]) -> Self {
        NestedConcat(vec![(es, 0)])
    }
}
impl<'a> Iterator for NestedConcat<'a> {
    type Item = &'a Hir;

    fn next(&mut self) -> Option<&'a Hir> {
        loop {
            if self.0.len() == 0 {
                return None;
            }

            let tip = self.0.len() - 1;
            let (es, idx) = self.0[tip];

            if idx >= es.len() {
                self.0.pop();
                continue;
            }

            self.0[tip].1 += 1;

            match es[idx].kind() {
                &HirKind::Concat(ref es) => {
                    self.0.push((es, 0));
                    continue;
                }
                _ => return Some(&es[idx]),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use syntax::Parser;
    use syntax::hir::Hir;
    use super::*;

    fn is_intersecting_fset(e1: &Hir, e2: &Hir) -> bool {
        let mut fset = fset_of(e1);
        fset.intersect(&fset_of(e2));
        ! fset.is_empty()
    }

    //
    // First Set intersection smoke tests
    //

    #[test]
    fn fset_lit() {
        let e1 = Parser::new().parse("a").unwrap();
        let e2 = Parser::new().parse("a").unwrap();
        let e3 = Parser::new().parse("b").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_class() {
        let e1 = Parser::new().parse("[a]").unwrap();
        let e2 = Parser::new().parse("[a]").unwrap();
        let e3 = Parser::new().parse("[b]").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_class_n() {
        let e1 = Parser::new().parse("[xamn]").unwrap();
        let e2 = Parser::new().parse("[rlwa]").unwrap();
        let e3 = Parser::new().parse("[bcq]").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_alt() {
        let e1 = Parser::new().parse("ab|bc|ad").unwrap();
        let e2 = Parser::new().parse("yyyy|am|zz").unwrap();
        let e3 = Parser::new().parse("cc|ww").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_group() {
        let e1 = Parser::new().parse("(?:ab)").unwrap();
        let e2 = Parser::new().parse("(?:aq)").unwrap();
        let e3 = Parser::new().parse("(?:m)").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_concat() {
        let e1 = Parser::new().parse("aa(?:nb)").unwrap();
        let e2 = Parser::new().parse("aa(?:rq)").unwrap();
        let e3 = Parser::new().parse("bb(?:m)").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_word_boundary_dropped() {
        let e1 = Parser::new().parse(r"aa").unwrap();
        let e2 = Parser::new().parse(r"\baa").unwrap();
        let e3 = Parser::new().parse(r"\bbb").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_word_boundary_all() {
        let e1 = Parser::new().parse(r"aa").unwrap();
        let e2 = Parser::new().parse(r"\b").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
    }

    #[test]
    fn fset_not_word_boundary_dropped() {
        let e1 = Parser::new().parse(r"aa").unwrap();
        let e2 = Parser::new().parse(r"\Baa").unwrap();
        let e3 = Parser::new().parse(r"\Bbb").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_not_word_boundary_all() {
        let e1 = Parser::new().parse(r"aa").unwrap();
        let e2 = Parser::new().parse(r"\B").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
    }

    #[test]
    fn fset_start_anchor_dropped() {
        let e1 = Parser::new().parse(r"aa").unwrap();
        let e2 = Parser::new().parse(r"^aa").unwrap();
        let e3 = Parser::new().parse(r"^bb").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
        assert!(!is_intersecting_fset(&e1, &e3));
    }

    #[test]
    fn fset_terminal_emptylook_all_1_() {
        let e = Parser::new().parse(r"a*\b").unwrap();
        let mut fset = FirstSet::anychar();
        fset.is_onepass = false;

        assert_eq!(fset, fset_of(&e), "\n\n{:?}\n\n", e);
    }

    #[test]
    fn fset_terminal_emptylook_all_2_() {
        let e = Parser::new().parse(r"(a*)\b").unwrap();
        let mut fset = FirstSet::anychar();
        fset.is_onepass = false;

        assert_eq!(fset, fset_of(&e), "\n\n{:?}\n\n", e);
    }


    #[test]
    fn fset_empty_alt() {
        let e1 = Parser::new().parse(r"(?:a|())b").unwrap();
        let e2 = Parser::new().parse(r"b").unwrap();

        assert!(is_intersecting_fset(&e1, &e2));
    }

    //
    // onepass smoke tests
    //

    macro_rules! test_onepass {
        ($fun_name:ident, $re_str:expr) => {
            #[test]
            fn $fun_name() {
                let e = Parser::new().parse($re_str).unwrap();
                let fset = fset_of(&e);
                assert!(fset.is_onepass, "fset={:?}", fset);
            }
        }
    }

    macro_rules! test_not_onepass {
        ($fun_name:ident, $re_str:expr) => {
            #[test]
            fn $fun_name() {
                let e = Parser::new().parse($re_str).unwrap();
                let fset = fset_of(&e);
                assert!(!fset.is_onepass, "fset={:?}", fset);
            }
        }
    }

    test_onepass!(onepass_smoke_1_, r"[^x]x(.*)");
    test_not_onepass!(onepass_smoke_2_, r"(.*)x(.*)");

    test_not_onepass!(onepass_alt_1_, r"a|b|c|a|d");
    test_not_onepass!(onepass_alt_2_, r"a|b|c|((m|a|x)|g)|d");
    test_onepass!(onepass_alt_3_, r"a|b|c|x|d");
    test_onepass!(onepass_alt_4_, r"a|b|c|((m|x)|g)|d");

    test_not_onepass!(onepass_not_in_rust, r"(\d+)-(\d+)");

    test_onepass!(onepass_empty_alt_1_, r"(a|())b");
    test_not_onepass!(onepass_empty_alt_2_, r"(a|())a");

    test_not_onepass!(onepass_rep_1_, r"a*a");
    test_not_onepass!(onepass_rep_2_, r"a+a");
    test_not_onepass!(onepass_rep_3_, r"a{4,8}a");
    test_not_onepass!(onepass_rep_4_, r"a{4,}a");
    test_onepass!(onepass_rep_5_, r"a{4}a");
    test_not_onepass!(onepass_rep_6_, r"a?a");

    test_onepass!(onepass_rep_7_, r"a*b");
    test_onepass!(onepass_rep_8_, r"a+b");
    test_onepass!(onepass_rep_9_, r"a{4,8}b");
    test_onepass!(onepass_rep_10_, r"a{4,}b");
    test_onepass!(onepass_rep_11_, r"a{4}b");
    test_onepass!(onepass_rep_12_, r"a?b");

    test_not_onepass!(onepass_concat_middle_1_, r"ab?bc");
    test_onepass!(onepass_concat_middle_2_, r"a(?:b|c)dc");

    test_not_onepass!(onepass_unicode_class_1_, r"\d");
    test_not_onepass!(onepass_unicode_class_2_, r"\s");
    test_not_onepass!(onepass_unicode_class_3_, r"\w");
    test_not_onepass!(onepass_unicode_class_4_, r"inthe\wmiddle");

    test_not_onepass!(onepass_unicode_clash_1_, r"Δ|δ");

    test_not_onepass!(onepass_empty_assert_1_, r"a|^a");
    test_onepass!(onepass_empty_assert_2_, r"\ba");
    test_onepass!(onepass_empty_assert_3_, r"^a");
    test_onepass!(onepass_empty_assert_4_, r"a$");

    test_not_onepass!(onepass_naked_empty_assert_1_, r"\w|a");
}
