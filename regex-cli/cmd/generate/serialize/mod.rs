use crate::args;

mod dfa;

pub fn run(p: &mut lexopt::Parser) -> anyhow::Result<()> {
    const USAGE: &'static str = "\
Serializes regex objects to disk, and optionally includes generating Rust
source code for embedding and loading those regex objects into your program.

USAGE:
    regex-cli generate serialize <engine>

ENGINES:
    dense    Serialize fully compiled dense DFAs or dense regex DFAs.
    sparse   Serialize fully compiled sparse DFAs or sparse regex DFAs.
";
    match &*args::next_as_command(USAGE, p)? {
        "dense" => dfa::run_dense(p),
        "sparse" => dfa::run_sparse(p),
        unk => anyhow::bail!("unrecognized command '{}'", unk),
    }
}
