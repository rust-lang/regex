mod bytes;
mod bytes_set;
mod string;
mod string_set;

const BLACKLIST: &[&str] = &[
    // CRLF-aware line anchors aren't supported in regex API yet.
    "crlf",
    // Custom line terminators aren't supported in regex API yet.
    "line-terminator",
];

fn suite() -> anyhow::Result<regex_test::RegexTests> {
    let _ = env_logger::try_init();

    let mut tests = regex_test::RegexTests::new();
    macro_rules! load {
        ($name:expr) => {{
            const DATA: &[u8] =
                include_bytes!(concat!("../testdata/", $name, ".toml"));
            tests.load_slice($name, DATA)?;
        }};
    }

    load!("anchored");
    load!("bytes");
    load!("crazy");
    load!("crlf");
    load!("earliest");
    load!("empty");
    load!("expensive");
    load!("flags");
    load!("iter");
    load!("leftmost-all");
    load!("line-terminator");
    load!("misc");
    load!("multiline");
    load!("no-unicode");
    load!("overlapping");
    load!("regression");
    load!("set");
    load!("substring");
    load!("unicode");
    load!("utf8");
    load!("word-boundary");
    load!("fowler/basic");
    load!("fowler/nullsubexpr");
    load!("fowler/repetition");

    Ok(tests)
}
