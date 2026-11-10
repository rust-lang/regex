// Tests that we can call the PikeVM with more slots
// than is actually in the compiled regex.
#[test]
fn too_many_slots_normal_pattern() {
    use regex_automata::{
        nfa::thompson::pikevm::PikeVM, util::primitives::NonMaxUsize,
        Anchored, Input,
    };

    let expr = PikeVM::new(r"abc").unwrap();
    let s = "abc";
    let input = Input::new(s).span(0..s.len()).anchored(Anchored::Yes);

    let mut cache = expr.create_cache();
    let sentinel = NonMaxUsize::new(123);
    // Reuse the cache with both partial and oversized slot slices.
    for len in [0, 1, 2, 32, 4, 2] {
        let mut slots = vec![sentinel; len];
        let pid = expr.search_slots(&mut cache, &input, &mut slots);
        assert_eq!(pid, Some(regex_automata::PatternID::must(0)));
        let expected = [NonMaxUsize::new(0), NonMaxUsize::new(3)];
        let used = len.min(expected.len());
        assert_eq!(&slots[..used], &expected[..used]);
        assert!(slots[used..].iter().all(|&slot| slot == sentinel));
    }
}

#[test]
fn too_many_slots_capturing_pattern() {
    use regex_automata::{
        nfa::thompson::pikevm::PikeVM, util::primitives::NonMaxUsize, Input,
        PatternID,
    };

    // Include an optional capture and a pattern that can match empty strings.
    for pattern in [r"(a)(b)?", r"(a*)(b)?"] {
        let re = PikeVM::new(pattern).unwrap();
        let mut cache = re.create_cache();
        for (haystack, expected) in [
            ("ab", [Some(0), Some(2), Some(0), Some(1), Some(1), Some(2)]),
            ("a", [Some(0), Some(1), Some(0), Some(1), None, None]),
        ] {
            let expected =
                expected.map(|slot| slot.and_then(NonMaxUsize::new));
            let sentinel = NonMaxUsize::new(123);
            let mut slots = [sentinel; 32];
            assert_eq!(
                re.search_slots(&mut cache, &Input::new(haystack), &mut slots),
                Some(PatternID::ZERO),
            );
            assert_eq!(&slots[..6], &expected);
            assert!(slots[6..].iter().all(|&slot| slot == sentinel));
        }
    }
}
