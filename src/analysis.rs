use syntax::hir::{
    Hir, HirKind, Literal, ClassBytes, ClassBytesRange,
    Class, Visitor, RepetitionRange, RepetitionKind
};
use syntax::hir;
use utf8_ranges::Utf8Sequences;

/// True iff the given expression is one-pass
pub fn is_onepass(expr: &Hir) -> bool {
    hir::visit(expr, IsOnePassVisitor::new()).unwrap()
}

struct IsOnePassVisitor(bool);

impl Visitor for IsOnePassVisitor {
    type Output = bool;
    type Err = ();

    fn finish(self) -> Result<bool, ()> {
        Ok(self.0)
    }

    fn visit_pre(&mut self, hir: &Hir) -> Result<(), ()> {
        if !self.0 {
            return Ok(())
        }

        match hir.kind() {
            &HirKind::Concat(ref es) => self.check_concat(&es),
            &HirKind::Alternation(ref es) => self.check_alternation(&es),
            &HirKind::Repetition(ref rep) => {
                if fset_of(&*rep.hir).is_empty() {
                    self.0 = false;
                }
            }
            &HirKind::Class(ref cls) => self.check_cls(cls),
            _ => ()
        }

        Ok(())
    }
}

impl IsOnePassVisitor {
    fn new() -> Self {
        IsOnePassVisitor(true)
    }

    fn check_concat(&mut self, es: &[Hir]) {
        let mut empty_run = vec![];

        for e in NestedConcat::new(es) {
            let is_rep = match e.kind() {
                &HirKind::Repetition(_) => true,
                _ => false,
            };

            empty_run.push(e);
            if !(accepts_empty(e) || is_rep) {
                self.0 = self.0 && !fsets_clash(&empty_run);
                empty_run.clear();
            }
        }

        if empty_run.len() > 0 {
            self.0 = self.0 && !fsets_clash(&empty_run);
        }
    }

    fn check_alternation(&mut self, es: &[Hir]) {
        self.0 = self.0 && !fsets_clash(&es.iter().collect::<Vec<_>>());
    }

    // Unicode classes are really just big alternatives from the byte
    // oriented point of view.
    //
    // This function translates a unicode class into the 
    // byte space and checks for intersecting first sets.
    fn check_cls(&mut self, cls: &Class) {
        match cls {
            &Class::Unicode(ref ucls) => {
                let mut seen_char: [bool; 256] = [false; 256];

                for cr in ucls.iter() {
                    for br in Utf8Sequences::new(cr.start(), cr.end()) {
                        let first = br.as_slice()[0];
                        for b in first.start..(first.end+1) {
                            if seen_char[b as usize] {
                                self.0 = false;
                                return;
                            }
                            seen_char[b as usize] = true;
                        }
                    }
                }
            }
            _ => {}
        }
    }

}

/// Check if a list of first sets is incompatable.
///
/// O(n^2), but n will usually be quite small.
fn fsets_clash(es: &[&Hir]) -> bool {
    for (i, e1) in es.iter().enumerate() {
        for (j, e2) in es.iter().enumerate() {
            if i != j {
                let mut fset = fset_of(e1);
                let fset2 = fset_of(e2);

                fset.intersect(&fset2);
                if ! fset.is_empty() {
                    return true;
                }
            }
        }
    }
    false
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

    // First compute the set of characters that might begin
    // the expression (ignoring epsilon for now).
    let mut f_char_set = match expr.kind() {
        &HirKind::Empty => FirstSet::epsilon(),
        &HirKind::Literal(ref lit) => {
            match lit {
                &Literal::Unicode(c) => singleton(first_byte(c)),
                &Literal::Byte(b) => singleton(b),
            }
        }
        &HirKind::Class(ref class) => {
            match class {
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
            }
        }

        // When an empty look (Anchor or WordBoundary) is at the start of
        // a concatenation, we conservatively assume that the assertion
        // will pass, so we just drop it. Then we can only get to this
        // point if we are dealing with some sort of naked empty look.
        // For now we just do the most conservative thing and say
        // that such an emptylook could potentially match on any character.
        &HirKind::Anchor(_) | &HirKind::WordBoundary(_) => FirstSet::anychar(),

        &HirKind::Repetition(ref rep) => fset_of(&rep.hir),
        &HirKind::Group(ref group) => fset_of(&group.hir),

        // The most involved case. We need to strip leading empty-looks
        // as well as take the union of the first sets of the first n+1
        // expressions where n is the number of leading expressions which
        // accept the empty string.
        &HirKind::Concat(ref es) => {
            let mut fset = FirstSet::empty();
            for (i, e) in es.iter().enumerate() {
                match e.kind() {
                    &HirKind::Anchor(_) | &HirKind::WordBoundary(_) => {
                        // Ignore any leading emptylooks, but any in tail
                        // position have to be accounted for.
                        if i == es.len() - 1 {
                            fset.union(&FirstSet::anychar());
                        }
                    }
                    _ => {
                        let inner_fset = fset_of(e);
                        fset.union(&inner_fset);

                        if !accepts_empty(e) {
                            // We can stop accumulating after we stop seeing
                            // first sets which contain epsilon.
                            break;
                        }
                    }
                }
            }
            fset
        }
        &HirKind::Alternation(ref es) => {
            let mut fset = FirstSet::empty();
            for e in es {
                fset.union(&fset_of(e));
            }
            fset
        }
    };

    f_char_set.accepts_empty = accepts_empty(expr);
    f_char_set
}

fn accepts_empty(expr: &Hir) -> bool {
    match expr.kind() {
        &HirKind::Empty => true,
        &HirKind::Literal(_) => false,
        &HirKind::Class(_) => false,

        // A naked empty look is a pretty weird thing because we
        // normally strip them from the beginning of concatinations.
        // We are just going to treat them like `.`
        &HirKind::Anchor(_) | &HirKind::WordBoundary(_) => false,

        &HirKind::Repetition(ref rep) => {
            match rep.kind {
                RepetitionKind::ZeroOrOne => true,
                RepetitionKind::ZeroOrMore => true,
                RepetitionKind::OneOrMore => accepts_empty(&rep.hir),
                RepetitionKind::Range(ref range) => {
                    match range {
                        &RepetitionRange::Exactly(0)
                        | &RepetitionRange::AtLeast(0)
                        | &RepetitionRange::Bounded(0, _) => true,
                        _ => accepts_empty(&rep.hir),
                    }
                }
            }
        }

        &HirKind::Group(ref group) => accepts_empty(&group.hir),

        &HirKind::Concat(ref es) => {
            let mut accepts: bool = true;
            for e in es.iter() {
                match e.kind() {
                    &HirKind::Anchor(_) | &HirKind::WordBoundary(_) => {
                        // Ignore any leading emptylooks.
                    }
                    _ => {
                        accepts = accepts && accepts_empty(&e);
                    }
                }

                if !accepts {
                    break;
                }
            }
            accepts
        }

        &HirKind::Alternation(ref es) => es.iter().any(accepts_empty)
    }
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
#[derive(Debug, PartialEq, Eq)]
struct FirstSet {
    bytes: ClassBytes,
    pub accepts_empty: bool,
}

impl FirstSet {
    fn empty() -> Self {
        FirstSet {
            bytes: ClassBytes::empty(),
            accepts_empty: false,
        }
    }

    pub fn new<I>(ranges: I, accepts_empty: bool) -> Self
    where I: IntoIterator<Item=ClassBytesRange>
    {
        FirstSet {
            bytes: ClassBytes::new(ranges),
            accepts_empty: accepts_empty,
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
        if self.0.len() == 0 {
            return None;
        }

        let tip = self.0.len() - 1;
        let (es, idx) = self.0[tip];

        if idx >= es.len() {
            self.0.pop();
            return self.next();
        }

        self.0[tip].1 += 1;

        match es[idx].kind() {
            &HirKind::Concat(ref es) => {
                self.0.push((es, 0));
                self.next()
            }
            _ => Some(&es[idx]),
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
    fn fset_terminal_emptylook_all() {
        let e = Parser::new().parse(r"a*\b").unwrap();

        let mut total_accept = FirstSet::anychar();
        total_accept.accepts_empty = true;

        assert_eq!(total_accept, fset_of(&e));
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

    // This test is pulled right from some of Russ Cox's
    // comments on onepass regex.
    //
    // Note that Russ Cox's other example of a onepass regex
    // (r"(\d+)-(\d+)") is actually not onepass for us because
    // there is byte-level nondeterminism in the \d character
    // class, and we care about things in the byte space rather
    // than the character space. If you do a onepass engine at
    // the character level, Cox's example is indeed onepass.
    #[test]
    fn is_onepass_smoke_test1() {
        let e1 = Parser::new().parse(r"([^x]*)x(.*)").unwrap();
        let e2 = Parser::new().parse(r"(.*)x(.*)").unwrap();

        assert!(is_onepass(&e1));
        assert!(!is_onepass(&e2));
    }

    #[test]
    fn is_onepass_empty_alt() {
        let e1 = Parser::new().parse(r"(a|())b").unwrap();
        let e2 = Parser::new().parse(r"(a|())a").unwrap();

        assert!(is_onepass(&e1));
        assert!(!is_onepass(&e2));
    }
    
    #[test]
    fn is_onepass_rep() {
        let e1 = Parser::new().parse(r"a+a").unwrap();
        let e2 = Parser::new().parse(r"a*a").unwrap();

        assert!(!is_onepass(&e1));
        assert!(!is_onepass(&e2));
    }

    #[test]
    fn is_onepass_clash_in_middle_of_concat() {
        let e = Parser::new().parse(r"ab?b").unwrap();
        assert!(!is_onepass(&e));
    }
}
