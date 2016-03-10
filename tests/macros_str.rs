// Macros for use in writing tests generic over &str/&[u8].
macro_rules! text { ($text:expr) => { $text } }
macro_rules! t { ($text:expr) => { text!($text) } }

macro_rules! bytes { ($text:expr) => { $text.as_bytes() } }
macro_rules! b { ($text:expr) => { bytes!($text) } }

macro_rules! u { ($re:expr) => { $re } }

macro_rules! no_expand {
    ($text:expr) => {{
        use regex::NoExpand;
        NoExpand(text!($text))
    }}
}

macro_rules! show { ($text:expr) => { $text } }

// N.B. The expansion API for &str and &[u8] APIs differs slightly for now,
// but they should be unified in 1.0. Then we can move this macro back into
// tests/api.rs where it is used. ---AG
macro_rules! expand {
    ($name:ident, $re:expr, $text:expr, $expand:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let re = regex!($re);
            let cap = re.captures(t!($text)).unwrap();

            let got = cap.expand(t!($expand));
            assert_eq!(show!(t!($expected)), show!(&*got));
        }
    }
}
