// The code in this module honestly sucks. I did at one point try and make it a
// little more composable, particularly with respect to the stuff that writes
// the Rust code, but it became an unintelligble mess. Instead, I squashed
// it down into four functions: dense DFAs, dense regexes, sparse DFAs and
// sparse regexes. And each of those functions handles the 'regex-automata',
// 'once-cell' and 'lazy-static' variants. So that's 12 different variants.
// There's *some* sharing within each function at least...
//
// With that said, I don't expect this code generation task to expand much.
// We'll probably support std's OnceCell once that stabilizes, but otherwise,
// I think we'll be stuck with just full DFAs for the time being. If and when
// the code generation task expands to other objects (NFAs?), maybe we should
// reconsider how this code is structured.

use std::{
    io::Write,
    path::{Path, PathBuf},
};

use {
    anyhow::Context,
    lexopt::{Arg, Parser, ValueExt},
    regex_automata::dfa::{dense, sparse},
};

use crate::{
    args::{self, Usage},
    util,
};

pub fn run_dense(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Serializes a dense fully compiled DFA (or regex DFA) to disk.

USAGE:
    regex-cli generate serialize dense <command>

ENGINES:
    dfa     Serialize a fully compiled dense DFA.
    regex   Serialize a pair of fully compiled dense DFAs as a regex.
";
    match &*args::next_as_command(USAGE, p)? {
        "dfa" => run_dense_dfa(p),
        "regex" => run_dense_regex(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}

fn run_dense_dfa(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Serializes a dense fully compiled DFA to disk.

USAGE:
    regex-cli generate serialize dense dfa <name> <outdir> [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut config = Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dfa = args::dfa::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            // This needs to come first, as it greedily parses the first
            // two positional parameters, and then 'patterns' takes the rest.
            &mut config,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut dfa,
        ],
    )?;

    let pats = patterns.get()?;
    let asts = syntax.asts(&pats)?;
    let hirs = syntax.hirs(&pats, &asts)?;
    let nfa = thompson.from_hirs(&hirs)?;
    let dfa = dfa.from_nfa(&nfa)?;

    let wtr = config.writer()?;
    wtr.write_dfa_dense_bytes(&dfa, "")?;
    wtr.write_dfa_dense_rust()?;

    Ok(())
}

fn run_dense_regex(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Serializes a dense fully compiled DFA regex to disk.

USAGE:
    regex-cli generate serialize dense regex <name> <outdir> [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut config = Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dfa = args::dfa::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            // This needs to come first, as it greedily parses the first
            // two positional parameters, and then 'patterns' takes the rest.
            &mut config,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut dfa,
        ],
    )?;

    let pats = patterns.get()?;
    let asts = syntax.asts(&pats)?;
    let hirs = syntax.hirs(&pats, &asts)?;
    let nfafwd = thompson.from_hirs(&hirs)?;
    let nfarev = thompson.reversed().from_hirs(&hirs)?;
    let dfafwd = dfa.from_nfa(&nfafwd)?;
    let dfarev = dfa.reversed().from_nfa(&nfarev)?;

    let wtr = config.writer()?;
    wtr.write_dfa_dense_bytes(&dfafwd, "_fwd")?;
    wtr.write_dfa_dense_bytes(&dfarev, "_rev")?;
    wtr.write_regex_dense_rust()?;
    Ok(())
}

pub fn run_sparse(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Serializes a sparse fully compiled DFA (or regex DFA) to disk.

USAGE:
    regex-cli generate serialize sparse <command>

ENGINES:
    dfa     Serialize a fully compiled sparse DFA.
    regex   Serialize a pair of fully compiled sparse DFAs as a regex.
";
    match &*args::next_as_command(USAGE, p)? {
        "dfa" => run_sparse_dfa(p),
        "regex" => run_sparse_regex(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}

fn run_sparse_dfa(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Serializes a sparse fully compiled DFA to disk.

USAGE:
    regex-cli generate serialize sparse dfa <name> <outdir> [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut config = Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dfa = args::dfa::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            // This needs to come first, as it greedily parses the first
            // two positional parameters, and then 'patterns' takes the rest.
            &mut config,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut dfa,
        ],
    )?;

    let pats = patterns.get()?;
    let asts = syntax.asts(&pats)?;
    let hirs = syntax.hirs(&pats, &asts)?;
    let nfa = thompson.from_hirs(&hirs)?;
    let dfa = dfa.from_nfa_sparse(&nfa)?;

    let wtr = config.writer()?;
    wtr.write_dfa_sparse_bytes(&dfa, "")?;
    wtr.write_dfa_sparse_rust()?;

    Ok(())
}

fn run_sparse_regex(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Serializes a sparse fully compiled DFA regex to disk.

USAGE:
    regex-cli generate serialize sparse regex <name> <outdir> [<pattern> ...]

TIP:
    use -h for short docs and --help for long docs

OPTIONS:
%options%
";

    let mut config = Config::default();
    let mut patterns = args::patterns::Config::positional();
    let mut syntax = args::syntax::Config::default();
    let mut thompson = args::thompson::Config::default();
    let mut dfa = args::dfa::Config::default();
    args::configure(
        p,
        USAGE,
        &mut [
            // This needs to come first, as it greedily parses the first
            // two positional parameters, and then 'patterns' takes the rest.
            &mut config,
            &mut patterns,
            &mut syntax,
            &mut thompson,
            &mut dfa,
        ],
    )?;

    let pats = patterns.get()?;
    let asts = syntax.asts(&pats)?;
    let hirs = syntax.hirs(&pats, &asts)?;
    let nfafwd = thompson.from_hirs(&hirs)?;
    let nfarev = thompson.reversed().from_hirs(&hirs)?;
    let dfafwd = dfa.from_nfa_sparse(&nfafwd)?;
    let dfarev = dfa.reversed().from_nfa_sparse(&nfarev)?;

    let wtr = config.writer()?;
    wtr.write_dfa_sparse_bytes(&dfafwd, "_fwd")?;
    wtr.write_dfa_sparse_bytes(&dfarev, "_rev")?;
    wtr.write_regex_sparse_rust()?;
    Ok(())
}

#[derive(Debug, Default)]
struct Config {
    name: Option<String>,
    outdir: Option<PathBuf>,
    safe: bool,
    rust_kind: RustKind,
    rustfmt: bool,
}

impl Config {
    fn writer(&self) -> anyhow::Result<Writer> {
        let varname = self.name()?.to_string();
        let modname = rust_module_name(&varname);
        Ok(Writer {
            varname,
            modname,
            outdir: self.outdir()?.to_path_buf(),
            safe: self.safe,
            rust_kind: self.rust_kind,
            rustfmt: self.rustfmt,
        })
    }

    fn name(&self) -> anyhow::Result<&str> {
        self.name.as_deref().ok_or_else(|| anyhow::anyhow!("missing <name>"))
    }

    fn outdir(&self) -> anyhow::Result<&Path> {
        self.outdir
            .as_deref()
            .ok_or_else(|| anyhow::anyhow!("missing <outdir>"))
    }
}

impl args::Configurable for Config {
    fn configure(
        &mut self,
        p: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Long("safe") => {
                self.safe = true;
            }
            Arg::Long("rust") => {
                self.rust_kind = args::parse(p, "--rust")?;
            }
            Arg::Long("rustfmt") => {
                self.rustfmt = true;
            }
            Arg::Value(ref mut value) => {
                if self.name.is_none() {
                    let v = std::mem::take(value);
                    self.name = Some(
                        v.string().context("<name> must be valid UTF-8")?,
                    );
                } else if self.outdir.is_none() {
                    self.outdir = Some(PathBuf::from(std::mem::take(value)));
                } else {
                    return Ok(false);
                }
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[
            Usage::new(
                "--safe",
                "If possible, only use safe Rust code to deserialize DFAs.",
                r#"
If possible, only use safe Rust code to deserialize DFAs. Generally speaking,
this requires a more expensive deserialization process, as it needs to verify
that every transition in the DFA points to a valid state. In other words, it
does work that is proportional to the size of every transition in the DFA.
Where as using deserialization that isn't safe will execute in constant time.
Whether the extra checks take too much time depends. They probably don't.

With that said, it is always correct to *not* use this flag, as the serialized
DFA is generated by this crate. That is, the only way undefined behavior
can occur is if there is a bug in the implementation of serialization or
deserialization. Therefore, cases where --safe should be used are somewhat
limited. For example, if the risk of a bug in the implementation is too high or
if you're just paranoid.
"#,
            ),
            Usage::new(
                "--rust <kind>",
                "Choose from: regex-automata, once-cell, lazy-static, none.",
                r#"
This flag permits one to specify how the DFA is initially loaded.

The default is regex-automata, which will use its 'util::lazy::Lazy' type.
The 'Lazy' type works in no-std and no-alloc contexts. Otherwise, 'once-cell'
will use the 'Lazy' type from the once_cell crate. 'lazy-static' will use the
'lazy_static!' macro from the lazy_static crate. And 'none' will not generate
any Rust source code at all.
"#,
            ),
            Usage::new(
                "--rustfmt",
                "Run rustfmt on the generated code.",
                r#"
When set, rustfmt is run on the generated code. Without rustfmt, the code
formatting may be arbitrarily bad.
"#,
            ),
        ];
        USAGES
    }
}

#[derive(Clone, Copy, Debug)]
enum RustKind {
    RegexAutomata,
    OnceCell,
    LazyStatic,
    None,
}

impl Default for RustKind {
    fn default() -> RustKind {
        RustKind::RegexAutomata
    }
}

impl std::str::FromStr for RustKind {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<RustKind> {
        match s {
            "regex-automata" => Ok(RustKind::RegexAutomata),
            "once-cell" => Ok(RustKind::OnceCell),
            "lazy-static" => Ok(RustKind::LazyStatic),
            "none" => Ok(RustKind::None),
            unk => anyhow::bail!("unrecognized rust output kind: '{}'", unk),
        }
    }
}

#[derive(Debug)]
struct Writer {
    outdir: PathBuf,
    varname: String,
    modname: String,
    safe: bool,
    rust_kind: RustKind,
    rustfmt: bool,
}

impl Writer {
    fn write_dfa_dense_bytes(
        &self,
        dfa: &dense::DFA<Vec<u32>>,
        name_suffix: &str,
    ) -> anyhow::Result<()> {
        let (big_bytes, _) = dfa.to_bytes_big_endian();
        let (lil_bytes, _) = dfa.to_bytes_little_endian();
        let big_path = self.outdir.join(self.big_name(name_suffix));
        let lil_path = self.outdir.join(self.lil_name(name_suffix));
        std::fs::write(&big_path, &big_bytes)
            .with_context(|| format!("{}", big_path.display()))?;
        std::fs::write(&lil_path, &lil_bytes)
            .with_context(|| format!("{}", lil_path.display()))?;
        Ok(())
    }

    fn write_dfa_sparse_bytes(
        &self,
        dfa: &sparse::DFA<Vec<u8>>,
        name_suffix: &str,
    ) -> anyhow::Result<()> {
        let big_bytes = dfa.to_bytes_big_endian();
        let lil_bytes = dfa.to_bytes_little_endian();
        let big_path = self.outdir.join(self.big_name(name_suffix));
        let lil_path = self.outdir.join(self.lil_name(name_suffix));
        std::fs::write(&big_path, &big_bytes)
            .with_context(|| format!("{}", big_path.display()))?;
        std::fs::write(&lil_path, &lil_bytes)
            .with_context(|| format!("{}", lil_path.display()))?;
        Ok(())
    }

    fn write_dfa_dense_rust(&self) -> anyhow::Result<()> {
        if matches!(self.rust_kind, RustKind::None) {
            return Ok(());
        }
        let outpath = self.outdir.join(format!("{}.rs", self.modname));
        let mut wtr = std::fs::File::create(&outpath)
            .with_context(|| outpath.display().to_string())?;

        let auto_gen_message = self.auto_gen_message().trim().to_string();
        let name = &self.varname;
        let bigname = self.big_name("");
        let lilname = self.lil_name("");
        let from_bytes = if self.safe {
            "DFA::from_bytes(&ALIGNED.bytes)"
        } else {
            "unsafe { DFA::from_bytes_unchecked(&ALIGNED.bytes) }"
        };
        let deserialize = format!(
            r##"
    static ALIGNED: &AlignAs<[u8], u32> = &AlignAs {{
        _align: [],
        #[cfg(target_endian = "big")]
        bytes: *include_bytes!("{bigname}"),
        #[cfg(target_endian = "little")]
        bytes: *include_bytes!("{lilname}"),
    }};
    let (dfa, _) = {from_bytes}.expect("serialized DFA should be valid");
    dfa
"##,
        );
        match self.rust_kind {
            RustKind::RegexAutomata => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use regex_automata::{{
    dfa::dense::DFA,
    util::{{lazy::Lazy, wire::AlignAs}},
}};

pub static {name}: Lazy<DFA<&'static [u32]>> = Lazy::new(|| {{
    {deserialize}
}});
"##,
                )?;
            }
            RustKind::OnceCell => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use {{
    once_cell::sync::Lazy,
    regex_automata::{{
        dfa::dense::DFA,
        util::wire::AlignAs,
    }},
}};

pub static {name}: Lazy<DFA<&'static [u32]>> = Lazy::new(|| {{
    {deserialize}
}});
"##,
                )?;
            }
            RustKind::LazyStatic => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use regex_automata::{{
    dfa::dense::DFA,
    util::wire::AlignAs,
}};

lazy_static::lazy_static! {{
    pub static ref {name}: DFA<&'static [u32]> = {{
        {deserialize}
    }};
}}
"##,
                )?;
            }
            RustKind::None => unreachable!(),
        }
        if self.rustfmt {
            util::rustfmt(&outpath)?;
        }
        Ok(())
    }

    fn write_dfa_sparse_rust(&self) -> anyhow::Result<()> {
        if matches!(self.rust_kind, RustKind::None) {
            return Ok(());
        }
        let outpath = self.outdir.join(format!("{}.rs", self.modname));
        let mut wtr = std::fs::File::create(&outpath)
            .with_context(|| outpath.display().to_string())?;

        let auto_gen_message = self.auto_gen_message().trim().to_string();
        let name = &self.varname;
        let bigname = self.big_name("");
        let lilname = self.lil_name("");
        let from_bytes = if self.safe {
            "DFA::from_bytes(BYTES)"
        } else {
            "unsafe { DFA::from_bytes_unchecked(BYTES) }"
        };
        let deserialize = format!(
            r##"
    #[cfg(target_endian = "big")]
    static BYTES: &'static [u8] = include_bytes!("{bigname}");
    #[cfg(target_endian = "little")]
    static BYTES: &'static [u8] = include_bytes!("{lilname}");
    let (dfa, _) = {from_bytes}.expect("serialized DFA should be valid");
    dfa
"##,
        );
        match self.rust_kind {
            RustKind::RegexAutomata => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use regex_automata::{{
    dfa::sparse::DFA,
    util::lazy::Lazy,
}};

pub static {name}: Lazy<DFA<&'static [u8]>> = Lazy::new(|| {{
    {deserialize}
}});
"##,
                )?;
            }
            RustKind::OnceCell => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use {{
    once_cell::sync::Lazy,
    regex_automata::dfa::sparse::DFA,
}};

pub static {name}: Lazy<DFA<&'static [u8]>> = Lazy::new(|| {{
    {deserialize}
}});
"##,
                )?;
            }
            RustKind::LazyStatic => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use regex_automata::dfa::sparse::DFA;

lazy_static::lazy_static! {{
    pub static ref {name}: DFA<&'static [u8]> = {{
        {deserialize}
    }};
}}
"##,
                )?;
            }
            RustKind::None => unreachable!(),
        }
        if self.rustfmt {
            util::rustfmt(&outpath)?;
        }
        Ok(())
    }

    fn write_regex_dense_rust(&self) -> anyhow::Result<()> {
        if matches!(self.rust_kind, RustKind::None) {
            return Ok(());
        }
        let outpath = self.outdir.join(format!("{}.rs", self.modname));
        let mut wtr = std::fs::File::create(&outpath)
            .with_context(|| outpath.display().to_string())?;

        let auto_gen_message = self.auto_gen_message().trim().to_string();
        let name = &self.varname;
        let fwdbigname = self.big_name("_fwd");
        let fwdlilname = self.lil_name("_fwd");
        let revbigname = self.big_name("_rev");
        let revlilname = self.lil_name("_rev");
        let from_bytes = if self.safe {
            "DFA::from_bytes(&ALIGNED.bytes)"
        } else {
            "unsafe { DFA::from_bytes_unchecked(&ALIGNED.bytes) }"
        };
        let deserialize = format!(
            r##"
    let dfafwd = {{
        static ALIGNED: &AlignAs<[u8], u32> = &AlignAs {{
            _align: [],
            #[cfg(target_endian = "big")]
            bytes: *include_bytes!("{fwdbigname}"),
            #[cfg(target_endian = "little")]
            bytes: *include_bytes!("{fwdlilname}"),
        }};
        {from_bytes}.expect("serialized forward DFA should be valid").0
    }};
    let dfarev = {{
        static ALIGNED: &AlignAs<[u8], u32> = &AlignAs {{
            _align: [],
            #[cfg(target_endian = "big")]
            bytes: *include_bytes!("{revbigname}"),
            #[cfg(target_endian = "little")]
            bytes: *include_bytes!("{revlilname}"),
        }};
        {from_bytes}.expect("serialized reverse DFA should be valid").0
    }};
    Regex::builder().build_from_dfas(dfafwd, dfarev)

"##,
        );
        match self.rust_kind {
            RustKind::RegexAutomata => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use regex_automata::{{
    dfa::{{dense::DFA, regex::Regex}},
    util::{{lazy::Lazy, wire::AlignAs}},
}};

pub static {name}: Lazy<Regex<DFA<&'static [u32]>>> = Lazy::new(|| {{
    {deserialize}
}});
"##,
                )?;
            }
            RustKind::OnceCell => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use {{
    once_cell::sync::Lazy,
    regex_automata::{{
        dfa::{{dense::DFA, regex::Regex}},
        util::wire::AlignAs,
    }},
}};

pub static {name}: Lazy<Regex<DFA<&'static [u32]>>> = Lazy::new(|| {{
    {deserialize}
}});
"##,
                )?;
            }
            RustKind::LazyStatic => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use regex_automata::{{
    dfa::{{dense::DFA, regex::Regex}},
    util::wire::AlignAs,
}};

lazy_static::lazy_static! {{
    pub static ref {name}: Regex<DFA<&'static [u32]>> = {{
        {deserialize}
    }};
}}
"##,
                )?;
            }
            RustKind::None => unreachable!(),
        }
        if self.rustfmt {
            util::rustfmt(&outpath)?;
        }
        Ok(())
    }

    fn write_regex_sparse_rust(&self) -> anyhow::Result<()> {
        if matches!(self.rust_kind, RustKind::None) {
            return Ok(());
        }
        let outpath = self.outdir.join(format!("{}.rs", self.modname));
        let mut wtr = std::fs::File::create(&outpath)
            .with_context(|| outpath.display().to_string())?;

        let auto_gen_message = self.auto_gen_message().trim().to_string();
        let name = &self.varname;
        let fwdbigname = self.big_name("_fwd");
        let fwdlilname = self.lil_name("_fwd");
        let revbigname = self.big_name("_rev");
        let revlilname = self.lil_name("_rev");
        let from_bytes = if self.safe {
            "DFA::from_bytes(BYTES)"
        } else {
            "unsafe { DFA::from_bytes_unchecked(BYTES) }"
        };
        let deserialize = format!(
            r##"
    let dfafwd = {{
        #[cfg(target_endian = "big")]
        static BYTES: &'static [u8] = include_bytes!("{fwdbigname}");
        #[cfg(target_endian = "little")]
        static BYTES: &'static [u8] = include_bytes!("{fwdlilname}");
        {from_bytes}.expect("serialized forward DFA should be valid").0
    }};
    let dfarev = {{
        #[cfg(target_endian = "big")]
        static BYTES: &'static [u8] = include_bytes!("{revbigname}");
        #[cfg(target_endian = "little")]
        static BYTES: &'static [u8] = include_bytes!("{revlilname}");
        {from_bytes}.expect("serialized reverse DFA should be valid").0
    }};
    Regex::builder().build_from_dfas(dfafwd, dfarev)

"##,
        );
        match self.rust_kind {
            RustKind::RegexAutomata => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use regex_automata::{{
    dfa::{{regex::Regex, sparse::DFA}},
    util::lazy::Lazy,
}};

pub static {name}: Lazy<Regex<DFA<&'static [u8]>>> = Lazy::new(|| {{
    {deserialize}
}});
"##,
                )?;
            }
            RustKind::OnceCell => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use {{
    once_cell::sync::Lazy,
    regex_automata::dfa::{{regex::Regex, sparse::DFA}},
}};

pub static {name}: Lazy<Regex<DFA<&'static [u8]>>> = Lazy::new(|| {{
    {deserialize}
}});
"##,
                )?;
            }
            RustKind::LazyStatic => {
                writeln!(
                    wtr,
                    r##"
{auto_gen_message}

use regex_automata::dfa::{{regex::Regex, sparse::DFA}};

lazy_static::lazy_static! {{
    pub static ref {name}: Regex<DFA<&'static [u8]>> = {{
        {deserialize}
    }};
}}
"##,
                )?;
            }
            RustKind::None => unreachable!(),
        }
        if self.rustfmt {
            util::rustfmt(&outpath)?;
        }
        Ok(())
    }

    fn auto_gen_message(&self) -> String {
        let version = env!("CARGO_PKG_VERSION");
        let cmd = std::env::args_os()
            .map(|a| a.to_string_lossy().into_owned())
            .map(|a| {
                if a.contains('\n') {
                    "<snip: arg too long>".to_string()
                } else {
                    a
                }
            })
            .collect::<Vec<String>>()
            .join(" ");
        format!(
            r#"
// DO NOT EDIT THIS FILE. IT WAS AUTOMATICALLY GENERATED BY:
//
//     {cmd}
//
// regex-cli {version} is available on crates.io.
"#
        )
    }

    fn big_name(&self, name_suffix: &str) -> String {
        format!("{}{}.bigendian.dfa", self.modname, name_suffix)
    }

    fn lil_name(&self, name_suffix: &str) -> String {
        format!("{}{}.littleendian.dfa", self.modname, name_suffix)
    }
}

fn rust_module_name(var_name: &str) -> String {
    var_name.to_ascii_lowercase()
}
