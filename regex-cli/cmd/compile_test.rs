use std::{
    io::Write,
    path::{Path, PathBuf},
    process::Command,
    time::{Duration, Instant},
};

use {
    anyhow::Context,
    lexopt::{Arg, Parser},
};

use crate::args::{self, Usage};

const REGEX_COMBOS: &[&[&str]] = &[
    &["std", "perf", "unicode"],
    &["std", "perf", "unicode", "perf-dfa-full"],
    &["std"],
    &["std", "perf"],
    &["std", "unicode"],
    &["std", "unicode-case", "unicode-perl"],
];

const REGEX_LITE_COMBOS: &[&[&str]] = &[&["std", "string"]];

const REGEX_AUTOMATA_COMBOS: &[&[&str]] = &[
    &["std", "syntax", "perf", "unicode", "meta", "nfa", "dfa", "hybrid"],
    // Try out some barebones combinations of individual regex engines.
    &["std", "syntax", "nfa-pikevm"],
    &["std", "syntax", "nfa-backtrack"],
    &["std", "syntax", "hybrid"],
    &["std", "syntax", "dfa-onepass"],
    // Now try out some realistic plausible configurations that combine
    // lots (but maybe not all) regex engines.
    //
    // First is dropping 'perf' from the default.
    &["std", "syntax", "unicode", "meta", "nfa", "dfa", "hybrid"],
    // Second is dropping 'dfa', which maybe doesn't carry its weight. We are
    // careful to re-enable the one-pass DFA though.
    &[
        "std",
        "syntax",
        "perf",
        "unicode",
        "meta",
        "nfa",
        "hybrid",
        "dfa-onepass",
    ],
    // This is dropping 'unicode', which comes with a whole bunch of tables.
    &["std", "syntax", "perf", "meta", "nfa", "dfa", "hybrid"],
    // Drop 'unicode' and also 'dfa'. But again, we keep the one-pass DFA
    // around.
    &["std", "syntax", "perf", "meta", "nfa", "hybrid", "dfa-onepass"],
    // "I the meta regex API and don't care about perf, but I want full
    // functionality."
    &["std", "unicode", "meta"],
    // "I just want the meta regex API, but just enough to make it work"
    &["std", "meta"],
];

pub fn run(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = r#"
Runs compilation time and binary size tests on a variety of configurations.

The output of this test includes compilation time, total binary size and
relative binary size. Relative binary size is computed by subtracting the
binary size of a simple "Hello, world!" program from the total size of the
program compiled with calls to the regex (or regex-automata) crate. The purpose
of relative size is to try to capture a metric that tracks the overhead of
the regex crate specifically, and not just the total binary size (which might
fluctuate with changes to the compiler).

The configurations are a combination of several things. 1) Various interesting
feature combinations from regex (and regex-automata when it's enabled). 2)
Debug and release mode Cargo profiles.

Other things, such as lto, are not considered. Instead, we use defaults for
options like lto.

The arguments given to this command are as follows:

The first is a directory containing a checkout of the regex crate repo.

The second is a directory where a bunch of different Cargo projects will be
written.

USAGE:
    regex-cli compile-test <regex-crate-dir> <out-dir>

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
"#;

    let mut config = Config::default();
    args::configure(p, USAGE, &mut [&mut config])?;

    let outdir = config.outdir()?;
    let revision = git_revision_hash(&config.regexdir()?)?;
    let baseline_size_dev = baseline_size(&outdir, Profile::Dev)?;
    let baseline_size_rel = baseline_size(&outdir, Profile::Release)?;

    // We don't bother bringing in a CSV writer just for this. While we don't
    // check it, none of our field values should contain quotes or delimiters.
    // This is still somewhat shady, but not nearly as bad as trying to roll
    // our own CSV parser.
    let mut wtr = std::io::stdout();
    writeln!(wtr, "name,crate,revision,profile,duration,size,relative-size")?;
    for test in config.tests()? {
        let tdir = TestDir::new(&outdir, test)?;
        tdir.write_cargo_toml()?;
        tdir.write_main_rs()?;
        tdir.cargo_clean()?;
        tdir.cargo_fetch()?;
        let m = tdir.cargo_build()?;
        let relative_size = m.size.saturating_sub(match tdir.test.profile {
            Profile::Dev => baseline_size_dev,
            Profile::Release => baseline_size_rel,
        });

        write!(wtr, "{},", tdir.test.name())?;
        if tdir.test.regex_automata {
            write!(wtr, "regex-automata,")?;
        } else {
            write!(wtr, "regex,")?;
        }
        write!(wtr, "{},", revision)?;
        write!(wtr, "{},", tdir.test.profile.as_str())?;
        write!(wtr, "{:?},", m.duration)?;
        write!(wtr, "{:?},", m.size)?;
        write!(wtr, "{:?}", relative_size)?;
        write!(wtr, "\n")?;
    }
    Ok(())
}

#[derive(Debug, Default)]
struct Config {
    regexdir: Option<PathBuf>,
    outdir: Option<PathBuf>,
    regex_lite: bool,
    regex_automata: bool,
}

impl args::Configurable for Config {
    fn configure(
        &mut self,
        _: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("regex-lite") => {
                self.regex_lite = true;
            }
            Arg::Long("regex-automata") => {
                self.regex_automata = true;
            }
            Arg::Value(ref mut v) => {
                if self.regexdir.is_none() {
                    let dir = PathBuf::from(std::mem::take(v));
                    self.regexdir = Some(
                        std::fs::canonicalize(&dir).with_context(|| {
                            format!("could not canonicalize {}", dir.display())
                        })?,
                    );
                } else if self.outdir.is_none() {
                    self.outdir = Some(PathBuf::from(std::mem::take(v)));
                } else {
                    anyhow::bail!("more than 2 arguments were given");
                }
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "--regex-automata",
                "Run tests for regex-automata too.",
                r#"
When enabled, this will run compilation time and binary size tests for
regex-automata too. regex-automata has several interesting configurations, so
this may add quite a bit of time to the tests. But this is useful because it
might provide a way to get better compilation times and binary sizes than is
possible with the 'regex' crate proper. Thus, it is useful to track.
"#,
            ),
            Usage::new(
                "--regex-lite",
                "Run tests for regex-lite too.",
                r#"
When enabled, this will run compilation time and binary size tests for
regex-lite too.
"#,
            ),
        ];
        USAGES
    }
}

impl Config {
    fn tests(&self) -> anyhow::Result<Vec<Test>> {
        let mut tests = vec![];
        for profile in [Profile::Dev, Profile::Release] {
            for features in REGEX_COMBOS.iter() {
                let features =
                    features.iter().map(|f| f.to_string()).collect();
                tests.push(Test {
                    regex_dir: self.regexdir()?.to_path_buf(),
                    profile,
                    features,
                    regex_lite: false,
                    regex_automata: false,
                });
            }
            if self.regex_lite {
                for features in REGEX_LITE_COMBOS.iter() {
                    let features =
                        features.iter().map(|f| f.to_string()).collect();
                    tests.push(Test {
                        regex_dir: self.regexdir()?.to_path_buf(),
                        profile,
                        features,
                        regex_lite: true,
                        regex_automata: false,
                    });
                }
            }
            if self.regex_automata {
                for features in REGEX_AUTOMATA_COMBOS.iter() {
                    let features =
                        features.iter().map(|f| f.to_string()).collect();
                    tests.push(Test {
                        regex_dir: self.regexdir()?.to_path_buf(),
                        profile,
                        features,
                        regex_lite: false,
                        regex_automata: true,
                    });
                }
            }
        }
        Ok(tests)
    }

    fn regexdir(&self) -> anyhow::Result<&Path> {
        self.regexdir
            .as_deref()
            .ok_or_else(|| anyhow::anyhow!("missing <regex-crate-dir>"))
    }

    fn outdir(&self) -> anyhow::Result<&Path> {
        self.outdir
            .as_deref()
            .ok_or_else(|| anyhow::anyhow!("missing <out-dir>"))
    }
}

#[derive(Debug)]
struct TestDir {
    dir: PathBuf,
    test: Test,
}

impl TestDir {
    /// Creates a new test directory for the given test. The directory for the
    /// test is created as a child of `parent_dir`.
    fn new(parent_dir: &Path, test: Test) -> anyhow::Result<TestDir> {
        let dir = parent_dir.join(test.name());
        std::fs::create_dir_all(&dir)
            .with_context(|| dir.display().to_string())?;
        Ok(TestDir { dir, test })
    }

    /// Write the Cargo.toml for this test
    fn write_cargo_toml(&self) -> anyhow::Result<()> {
        let path = self.dir.join("Cargo.toml");
        std::fs::write(&path, self.test.cargo_toml())
            .with_context(|| path.display().to_string())?;
        Ok(())
    }

    /// Write the main.rs for this test.
    fn write_main_rs(&self) -> anyhow::Result<()> {
        let path = self.dir.join("main.rs");
        std::fs::write(&path, self.test.main_rs())
            .with_context(|| path.display().to_string())?;
        Ok(())
    }

    /// Clean the Cargo project in this directory, to ensure we start fresh.
    fn cargo_clean(&self) -> anyhow::Result<()> {
        let status = Command::new("cargo")
            .arg("clean")
            .arg("--manifest-path")
            .arg(self.dir.join("Cargo.toml"))
            .status()
            .with_context(|| {
                format!("'cargo clean' failed for test '{}'", self.test.name())
            })?;
        anyhow::ensure!(
            status.success(),
            "'cargo clean' got an error exit code of {:?} for test '{}'",
            status,
            self.test.name(),
        );
        Ok(())
    }

    /// Fetch all dependencies for the Cargo project in this directory.
    fn cargo_fetch(&self) -> anyhow::Result<()> {
        let status = Command::new("cargo")
            .arg("fetch")
            .arg("--manifest-path")
            .arg(self.dir.join("Cargo.toml"))
            .status()
            .with_context(|| {
                format!("'cargo fetch' failed for test '{}'", self.test.name())
            })?;
        anyhow::ensure!(
            status.success(),
            "'cargo fetch' got an error exit code of {:?} for test '{}'",
            status,
            self.test.name(),
        );
        Ok(())
    }

    /// Build the test and return the time it took and the size of the binary.
    ///
    /// Callers should run 'cargo_fetch()' before this, as this passes the
    /// '--offline' flag. This ensures that our timing measurement only
    /// includes build times.
    fn cargo_build(&self) -> anyhow::Result<Measurement> {
        let start = Instant::now();
        let status = Command::new("cargo")
            .arg("build")
            .arg("--manifest-path")
            .arg(self.dir.join("Cargo.toml"))
            .arg("--offline")
            .arg("--profile")
            .arg(self.test.profile.as_str())
            .status()
            .with_context(|| {
                format!("'cargo build' failed for test '{}'", self.test.name())
            })?;
        anyhow::ensure!(
            status.success(),
            "'cargo build' got an error exit code of {:?} for test '{}'",
            status,
            self.test.name(),
        );
        let duration = start.elapsed();
        let bin = self
            .dir
            .join("target")
            .join(self.test.profile.as_str_for_target_path())
            .join(format!("main{}", std::env::consts::EXE_SUFFIX));
        let size = std::fs::metadata(&bin)
            .with_context(|| bin.display().to_string())?
            .len();
        Ok(Measurement { duration, size })
    }
}

#[derive(Clone, Debug)]
struct Measurement {
    duration: Duration,
    size: u64,
}

#[derive(Debug)]
struct Test {
    /// The path to the directory containing the regex crate.
    regex_dir: PathBuf,
    /// The Cargo profile to use.
    profile: Profile,
    /// The list of crate features to enabled.
    features: Vec<String>,
    /// Whether we're testing the regex-lite crate or not.
    regex_lite: bool,
    /// Whether we're testing the regex-automata crate or not.
    regex_automata: bool,
}

impl Test {
    /// Returns the name for this test.
    ///
    /// The name is meant to be a unique identifier for this test based on its
    /// configuration. At the time of writing, the configuration space is still
    /// somewhat small enough where this is reasonable. But if it blows up in
    /// the future, we might need to reconsider the approach here... But that
    /// would be sad.
    fn name(&self) -> String {
        // Bah, we should probably use an enum for this but I got lazy.
        assert!(!(self.regex_lite && self.regex_automata));
        let krate = if self.regex_lite {
            "regex-lite"
        } else if self.regex_automata {
            "regex-automata"
        } else {
            "regex"
        };
        let profile = self.profile.as_str();
        let features = self.features.join("_");
        format!("{krate}__{profile}__{features}")
    }

    /// Return a string corresponding to the `Cargo.toml` for this test.
    fn cargo_toml(&self) -> String {
        if self.regex_lite {
            self.cargo_toml_regex_lite()
        } else if self.regex_automata {
            self.cargo_toml_regex_automata()
        } else {
            self.cargo_toml_regex()
        }
    }

    /// Return a string corresponding to the `main.rs` for this test.
    fn main_rs(&self) -> String {
        if self.regex_lite {
            self.main_rs_regex_lite()
        } else if self.regex_automata {
            self.main_rs_regex_automata()
        } else {
            self.main_rs_regex()
        }
    }

    fn cargo_toml_regex(&self) -> String {
        let name = self.name();
        let path = self.regex_dir.display();
        let features = self
            .features
            .iter()
            .map(|f| format!(r#""{}""#, f))
            .collect::<Vec<String>>()
            .join(", ");
        format!(
            r#"
[package]
name = "{name}"
version = "0.0.0"
edition = "2021"
publish = false

# This detaches this directory from any workspace
# in a parent directory.
[workspace]

[[bin]]
name = "main"
path = "main.rs"

[dependencies.regex]
path = "{path}"
version = "*"
default-features = false
features = [{features}]

[profile.dev]
strip = "symbols"

[profile.release]
strip = "symbols"
"#
        )
    }

    fn cargo_toml_regex_lite(&self) -> String {
        let name = self.name();
        let path = self.regex_dir.join("regex-lite");
        let path = path.display();
        let features = self
            .features
            .iter()
            .map(|f| format!(r#""{}""#, f))
            .collect::<Vec<String>>()
            .join(", ");
        format!(
            r#"
[package]
name = "{name}"
version = "0.0.0"
edition = "2021"
publish = false

# This detaches this directory from any workspace
# in a parent directory.
[workspace]

[[bin]]
name = "main"
path = "main.rs"

[dependencies.regex-lite]
path = "{path}"
version = "*"
default-features = false
features = [{features}]

[profile.dev]
strip = "symbols"

[profile.release]
strip = "symbols"
"#
        )
    }

    fn cargo_toml_regex_automata(&self) -> String {
        let name = self.name();
        let path = self.regex_dir.join("regex-automata");
        let path = path.display();
        let features = self
            .features
            .iter()
            .map(|f| format!(r#""{}""#, f))
            .collect::<Vec<String>>()
            .join(", ");
        format!(
            r#"
[package]
name = "{name}"
version = "0.0.0"
edition = "2021"
publish = false

# This detaches this directory from any workspace
# in a parent directory.
[workspace]

[[bin]]
name = "main"
path = "main.rs"

[dependencies.regex-automata]
path = "{path}"
default-features = false
features = [{features}]

[profile.dev]
strip = "symbols"

[profile.release]
strip = "symbols"
"#
        )
    }

    fn main_rs_regex(&self) -> String {
        format!(
            r#"
use regex::{{bytes, Regex, RegexSet}};

fn main() {{
    let re = Regex::new("a").unwrap();
    assert!(re.is_match("a"));
    assert_eq!("a", re.find("a").unwrap().as_str());
    assert_eq!("a", &re.captures("a").unwrap()[0]);
    assert_eq!(2, re.find_iter("aa").count());
    assert_eq!(2, re.captures_iter("aa").count());

    let re = bytes::Regex::new("a").unwrap();
    assert!(re.is_match(b"a"));
    assert_eq!(b"a", re.find(b"a").unwrap().as_bytes());
    assert_eq!(b"a", &re.captures(b"a").unwrap()[0]);
    assert_eq!(2, re.find_iter(b"aa").count());
    assert_eq!(2, re.captures_iter(b"aa").count());

    let re = RegexSet::new(&["a", "b"]).unwrap();
    assert!(re.is_match("a"));
    assert_eq!(2, re.matches("acdb").iter().count());

    let re = bytes::RegexSet::new(&["a", "b"]).unwrap();
    assert!(re.is_match(b"a"));
    assert_eq!(2, re.matches(b"acdb").iter().count());
}}
"#
        )
    }

    fn main_rs_regex_lite(&self) -> String {
        format!(
            r#"
use regex_lite::{{Regex}};

fn main() {{
    let re = Regex::new("a").unwrap();
    assert!(re.is_match("a"));
    assert_eq!("a", re.find("a").unwrap().as_str());
    assert_eq!("a", &re.captures("a").unwrap()[0]);
    assert_eq!(2, re.find_iter("aa").count());
    assert_eq!(2, re.captures_iter("aa").count());
}}
"#
        )
    }

    fn main_rs_regex_automata(&self) -> String {
        use std::fmt::Write;

        let mut bufuse = String::new();
        let mut bufmain = String::new();
        if self.contains("nfa") || self.contains("nfa-pikevm") {
            writeln!(
                bufuse,
                r#"
use regex_automata::nfa::thompson::pikevm::PikeVM;
"#
            )
            .unwrap();
            writeln!(
                bufmain,
                r#"
    let re = PikeVM::new("a").unwrap();
    let mut cache = re.create_cache();
    assert!(re.is_match(&mut cache, "a"));
    assert_eq!(0..1, re.find(&mut cache, "a").unwrap().range());
    assert_eq!(2, re.find_iter(&mut cache, "aa").count());
    assert_eq!(2, re.captures_iter(&mut cache, "aa").count());
"#
            )
            .unwrap();
        }
        if self.contains("nfa") || self.contains("nfa-backtrack") {
            writeln!(
                bufuse,
                r#"
use regex_automata::nfa::thompson::backtrack::BoundedBacktracker;
"#
            )
            .unwrap();
            writeln!(
                bufmain,
                r#"
    let re = BoundedBacktracker::new("a").unwrap();
    let mut cache = re.create_cache();
    assert!(re.try_is_match(&mut cache, "a").unwrap());
    assert_eq!(0..1, re.try_find(&mut cache, "a").unwrap().unwrap().range());
    assert_eq!(2, re.try_find_iter(&mut cache, "aa").count());
    assert_eq!(2, re.try_captures_iter(&mut cache, "aa").count());
"#
            )
            .unwrap();
        }
        if self.contains("hybrid") {
            writeln!(
                bufuse,
                r#"
use regex_automata::hybrid;
"#
            )
            .unwrap();
            writeln!(
                bufmain,
                r#"
    let re = hybrid::dfa::DFA::new("a").unwrap();
    let mut cache = re.create_cache();
    let input = regex_automata::Input::new("a");
    assert_eq!(1, re.try_search_fwd(&mut cache, &input).unwrap().unwrap().offset());
"#
            ).unwrap();
        }
        if self.contains("dfa")
            || (self.contains("dfa-build") && self.contains("dfa-search"))
        {
            writeln!(
                bufuse,
                r#"
use regex_automata::dfa::{{Automaton, dense, sparse}};
"#
            )
            .unwrap();
            writeln!(
                bufmain,
                r#"
    let re = dense::DFA::new("a").unwrap();
    let input = regex_automata::Input::new("a");
    assert_eq!(1, re.try_search_fwd(&input).unwrap().unwrap().offset());

    let re = sparse::DFA::new("a").unwrap();
    let input = regex_automata::Input::new("a");
    assert_eq!(1, re.try_search_fwd(&input).unwrap().unwrap().offset());
"#
            )
            .unwrap();
        }
        if self.contains("dfa") || self.contains("dfa-onepass") {
            writeln!(
                bufuse,
                r#"
use regex_automata::dfa::onepass;
"#
            )
            .unwrap();
            writeln!(
                bufmain,
                r#"
    let re = onepass::DFA::new("a").unwrap();
    let mut cache = re.create_cache();
    assert!(re.is_match(&mut cache, "a"));
    let input = regex_automata::Input::new("a");
    let mut caps = re.create_captures();
    assert!(re.try_search(&mut cache, &input, &mut caps).is_ok());
    assert_eq!(0..1, caps.get_match().unwrap().range());
"#
            )
            .unwrap();
        }
        if self.contains("meta") {
            writeln!(
                bufuse,
                r#"
use regex_automata::meta;
"#
            )
            .unwrap();
            writeln!(
                bufmain,
                r#"
    let re = meta::Regex::new("a").unwrap();
    assert!(re.is_match("a"));
    assert_eq!(0..1, re.find("a").unwrap().range());
    let mut caps = re.create_captures();
    re.captures("a", &mut caps);
    assert_eq!(0..1, caps.get_match().unwrap().range());
    assert_eq!(2, re.find_iter("aa").count());
    assert_eq!(2, re.captures_iter("aa").count());
"#
            )
            .unwrap();
        }
        format!(
            r#"
{bufuse}

fn main() {{
{bufmain}
}}
"#
        )
    }

    fn contains(&self, feature_name: &str) -> bool {
        self.features.iter().find(|name| feature_name == &**name).is_some()
    }
}

/// The Cargo profile to use.
#[derive(Clone, Copy, Debug)]
enum Profile {
    Dev,
    Release,
}

impl Profile {
    fn as_str(&self) -> &'static str {
        match *self {
            Profile::Dev => "dev",
            Profile::Release => "release",
        }
    }

    fn as_str_for_target_path(&self) -> &'static str {
        match *self {
            Profile::Dev => "debug",
            Profile::Release => "release",
        }
    }
}

/// Compiles a basic "Hello, world!" program with the given profile and returns
/// the size of the resulting binary.
fn baseline_size(parent_dir: &Path, profile: Profile) -> anyhow::Result<u64> {
    let dir = parent_dir.join(profile.as_str());
    let cargo_toml_path = dir.join("Cargo.toml");
    let main_rs_path = dir.join("main.rs");
    std::fs::create_dir_all(&dir)
        .with_context(|| dir.display().to_string())?;
    std::fs::write(&cargo_toml_path, baseline_cargo_toml())
        .with_context(|| dir.display().to_string())?;
    std::fs::write(
        &main_rs_path,
        r#"fn main() {{ println!("Hello, world!"); }}"#,
    )
    .with_context(|| dir.display().to_string())?;

    let status = Command::new("cargo")
        .arg("clean")
        .arg("--manifest-path")
        .arg(&cargo_toml_path)
        .status()
        .with_context(|| format!("'cargo clean' failed for baseline"))?;
    anyhow::ensure!(
        status.success(),
        "'cargo clean' got an error exit code of {:?} for baseline",
        status,
    );
    let status = Command::new("cargo")
        .arg("build")
        .arg("--manifest-path")
        .arg(dir.join("Cargo.toml"))
        .arg("--offline")
        .arg("--profile")
        .arg(profile.as_str())
        .status()
        .with_context(|| format!("'cargo build' failed for baseline"))?;
    anyhow::ensure!(
        status.success(),
        "'cargo build' got an error exit code of {:?} for baseline",
        status,
    );
    let bin = dir
        .join("target")
        .join(profile.as_str_for_target_path())
        .join(format!("main{}", std::env::consts::EXE_SUFFIX));
    let size = std::fs::metadata(&bin)
        .with_context(|| bin.display().to_string())?
        .len();

    Ok(size)
}

fn baseline_cargo_toml() -> String {
    format!(
        r#"
[package]
name = "baseline"
version = "0.0.0"
edition = "2021"
publish = false

# This detaches this directory from any workspace
# in a parent directory.
[workspace]

[[bin]]
name = "main"
path = "main.rs"

[profile.dev]
strip = "symbols"

[profile.release]
strip = "symbols"
"#
    )
}

fn git_revision_hash(regex_dir: &Path) -> anyhow::Result<String> {
    let output = std::process::Command::new("git")
        .current_dir(regex_dir)
        .args(&["rev-parse", "--short=10", "HEAD"])
        .output()
        .context("failed to run 'git rev-parse'")?;
    let v = String::from_utf8_lossy(&output.stdout).trim().to_string();
    anyhow::ensure!(!v.is_empty(), "got empty output from 'git rev-parse'",);
    Ok(v)
}
