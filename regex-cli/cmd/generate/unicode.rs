use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};

use {
    anyhow::Context,
    bstr::BString,
    lexopt::{Arg, Parser},
};

use crate::{
    args::{self, Usage},
    util,
};

pub fn run(p: &mut Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Generates all Unicode tables for the regex project.

Most Unicode tables are generated into the regex-syntax library.

Note that this requires that the 'ucd-generate' tool be installed and in your
PATH. The 'ucd-generate' tool is what is responsible for reading from the
Unicode Character Database (UCD) and converting tables of codepoints into Rust
code that is embedded into the regex library.

ucd-generate can be found here https://github.com/BurntSushi/ucd-generate/
and can be installed with:

    cargo install ucd-generate

And you can get a copy of, for example, UCD 15.0.0 like this:

    $ mkdir /tmp/ucd-15.0.0
    $ cd /tmp/ucd-15.0.0
    $ curl -LO https://www.unicode.org/Public/zipped/15.0.0/UCD.zip
    $ unzip UCD.zip

USAGE:
    regex-cli generate unicode <outdir> <ucddir>

    outdir should be a directory path pointing to the root of the regex
    repository. ucddir should be a directory containing the UCD data
    downloaded from unicode.org, as described in ucd-generate's README:
    https://github.com/BurntSushi/ucd-generate/

";

    let mut config = Config::default();
    args::configure(p, USAGE, &mut [&mut config])?;

    let out = config.outdir()?;
    let ucd = config.ucddir()?;

    // Data tables for regex-syntax.
    let d = out.join("regex-syntax").join("src").join("unicode_tables");
    gen(d.join("age.rs"), &["age", &ucd, "--chars"])?;
    gen(
        d.join("case_folding_simple.rs"),
        &["case-folding-simple", &ucd, "--chars", "--all-pairs"],
    )?;
    gen(
        d.join("general_category.rs"),
        &["general-category", &ucd, "--chars", "--exclude", "surrogate"],
    )?;
    gen(
        d.join("grapheme_cluster_break.rs"),
        &["grapheme-cluster-break", &ucd, "--chars"],
    )?;
    gen(d.join("property_bool.rs"), &["property-bool", &ucd, "--chars"])?;
    gen(d.join("property_names.rs"), &["property-names", &ucd])?;
    gen(
        d.join("property_values.rs"),
        &["property-values", &ucd, "--include", "gc,script,scx,age,gcb,wb,sb"],
    )?;
    gen(d.join("script.rs"), &["script", &ucd, "--chars"])?;
    gen(
        d.join("script_extension.rs"),
        &["script-extension", &ucd, "--chars"],
    )?;
    gen(d.join("sentence_break.rs"), &["sentence-break", &ucd, "--chars"])?;
    gen(d.join("word_break.rs"), &["word-break", &ucd, "--chars"])?;

    // Data tables for regex-automata.
    let d = out
        .join("regex-automata")
        .join("src")
        .join("util")
        .join("unicode_data");
    // Note that this table is only used in cases where the regex-syntax
    // dependency is disabled for regex-automata, but where the
    // unicode-word-boundary feature is still enabled. That is, we do NOT
    // duplicate the perl_word table from regex-syntax. Only one copy will get
    // built.
    gen(d.join("perl_word.rs"), &["perl-word", &ucd, "--chars"])?;

    Ok(())
}

#[derive(Debug, Default)]
struct Config {
    outdir: Option<PathBuf>,
    ucddir: Option<PathBuf>,
}

impl Config {
    fn outdir(&self) -> anyhow::Result<&Path> {
        self.outdir
            .as_deref()
            .ok_or_else(|| anyhow::anyhow!("missing <outdir>"))
    }

    fn ucddir(&self) -> anyhow::Result<&str> {
        self.ucddir
            .as_deref()
            .ok_or_else(|| anyhow::anyhow!("missing <ucddir>"))?
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("ucddir must be valid UTF-8"))
    }
}

impl args::Configurable for Config {
    fn configure(
        &mut self,
        _: &mut Parser,
        arg: &mut Arg,
    ) -> anyhow::Result<bool> {
        match *arg {
            Arg::Value(ref mut value) => {
                if self.outdir.is_none() {
                    self.outdir = Some(PathBuf::from(std::mem::take(value)));
                } else if self.ucddir.is_none() {
                    self.ucddir = Some(PathBuf::from(std::mem::take(value)));
                } else {
                    return Ok(false);
                }
            }
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn usage(&self) -> &[Usage] {
        const USAGES: &'static [Usage] = &[];
        USAGES
    }
}

/// Run 'ucd-generate' with the args given, write its output to the file path
/// given and apply 'rustfmt' to it.
fn gen<P: AsRef<Path>>(dest: P, args: &[&str]) -> anyhow::Result<()> {
    let dest = dest.as_ref();
    ucdgen_to(dest, args)?;
    util::rustfmt(dest)?;
    Ok(())
}

/// Run 'ucd-generate' with the args given and write its output to the file
/// path given.
fn ucdgen_to<P: AsRef<Path>>(dest: P, args: &[&str]) -> anyhow::Result<()> {
    let dest = dest.as_ref();
    let err = || format!("{}", dest.display());
    // The "right" thing would be to connect this to the stdout of
    // ucd-generate, but meh, I got lazy.
    let mut fdest = File::create(dest).with_context(err)?;
    let data = ucdgen(args)?;
    fdest.write_all(&data).with_context(err)?;
    Ok(())
}

/// Run 'ucd-generate' with the args given. Upon success, the contents of
/// stdout are returned. Otherwise, the error will include the contents of
/// stderr.
fn ucdgen(args: &[&str]) -> anyhow::Result<Vec<u8>> {
    let out = Command::new("ucd-generate")
        .args(args)
        .output()
        .context("ucd-generate command failed")?;
    anyhow::ensure!(
        out.status.success(),
        "ucd-generate {}: {}",
        args.join(" "),
        BString::from(out.stderr),
    );
    Ok(out.stdout)
}
