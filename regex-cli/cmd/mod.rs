mod compile_test;
mod debug;
mod find;
mod generate;

const USAGE: &'static str = "\
A tool for interacting with Rust's regex crate on the command line.

USAGE:
    regex-cli <command> ...

COMMANDS:
    compile-test  Measure binary size and compile time of various configs.
    debug         Print the debug representation of things from regex-automata.
    find          Search haystacks with one of many different regex engines.
    generate      Various generation tasks, e.g., serializing DFAs.
";

pub fn run(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    let cmd = crate::args::next_as_command(USAGE, p)?;
    match &*cmd {
        "compile-test" => compile_test::run(p),
        "find" => find::run(p),
        "debug" => debug::run(p),
        "generate" => generate::run(p),
        unk => anyhow::bail!("unrecognized command '{unk}'"),
    }
}
