macro_rules! mat {
    ($name:ident, $res:expr, $text:expr, $($match_index:expr),*) => {
        #[test]
        fn $name() {
            let set = regex_set!($res);
            assert!(set.is_match($text));
            let expected = vec![$($match_index),*];
            let matches = set.matches($text);
            assert!(matches.matched_any());
            let got: Vec<_> = matches.into_iter().collect();
            assert_eq!(expected, got);
        }
    }
}

mat!(set1, &["a", "a"], "a", 0, 1);
mat!(set2, &["a", "a"], "ba", 0, 1);
mat!(set3, &["a", "b"], "a", 0);
mat!(set4, &["a", "b"], "b", 1);
mat!(set5, &["a|b", "b|a"], "b", 0, 1);
mat!(set6, &["foo", "oo"], "foo", 0, 1);
mat!(set7, &["^foo", "bar$"], "foo", 0);
mat!(set8, &["^foo", "bar$"], "foo bar", 0, 1);
mat!(set9, &["^foo", "bar$"], "bar", 1);
mat!(set10, &[r"[a-z]+$", "foo"], "01234 foo", 0, 1);
mat!(set11, &[r"[a-z]+$", "foo"], "foo 01234", 1);
mat!(set12, &[r".*?", "a"], "zzzzzza", 0, 1);
mat!(set13, &[r".*", "a"], "zzzzzza", 0, 1);
mat!(set14, &[r".*", "a"], "zzzzzz", 0);

macro_rules! nomat {
    ($name:ident, $res:expr, $text:expr) => {
        #[test]
        fn $name() {
            let set = regex_set!($res);
            assert!(!set.is_match($text));
            let matches = set.matches($text);
            assert!(!matches.matched_any());
            assert_eq!(0, matches.into_iter().count());
        }
    }
}

nomat!(nset1, &["a", "a"], "b");
nomat!(nset2, &["^foo", "bar$"], "bar foo");
