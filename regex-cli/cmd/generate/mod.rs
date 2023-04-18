use crate::args;

mod fowler;
mod serialize;
mod unicode;

const USAGE: &'static str = r#"
A tool for doing various types of generation tasks. This includes things like
serializing DFAs to be compiled into other programs, and generating the Unicode
tables used by the regex project.

USAGE:
    regex-cli generate <command>

COMMANDS:
    fowler      Convert Glenn Fowler's test suite to TOML files.
    serialize   Serialize DFAs and generate Rust code to load them.
    unicode     Generate all Unicode tables required for the regex project.
"#;

pub fn run(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    match &*args::next_as_command(USAGE, p)? {
        "fowler" => fowler::run(p),
        "serialize" => serialize::run(p),
        "unicode" => unicode::run(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}
