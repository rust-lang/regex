extern crate docopt;
extern crate regex;
extern crate regex_syntax as syntax;
extern crate serde;
#[macro_use]
extern crate serde_derive;

use std::error;
use std::io::{self, Write};
use std::process;
use std::result;

use docopt::Docopt;
use regex::internal::{Compiler, LiteralSearcher};
use syntax::hir::literal::Literals;
use syntax::hir::Hir;

const USAGE: &'static str = "
Usage:
    regex-debug [options] ast <pattern>
    regex-debug [options] hir <pattern>
    regex-debug [options] prefixes <patterns> ...
    regex-debug [options] suffixes <patterns> ...
    regex-debug [options] anchors <pattern>
    regex-debug [options] captures <pattern>
    regex-debug [options] compile <patterns> ...
    regex-debug [options] utf8-ranges <class>
    regex-debug --help

Options:
    --help               Show this usage message.
    --size-limit ARG     An approximate size limit on the total size (in bytes)
                         of a compiled regular expression program.
                         [default: 10485760]
    --bytes              Show the instruction codes for byte oriented programs.
                         (As opposed to Unicode oriented programs.)
    --dfa                Show the instruction codes for a DFA.
    --dfa-reverse        Show the instruction codes for a reverse DFA.
                         This implies --dfa.
    -a, --all-literals   Shows all literals extracted.
                         By default, only unambiguous literals are shown.
    --literal-limit ARG  An approximate limit on the total size (in bytes)
                         of all literals extracted. [default: 250]
    --class-limit ARG    A limit on the size of character classes used to
                         extract literals. [default: 10]
    --literal-bytes      Show raw literal bytes instead of Unicode chars.
    --lcp                Show the longest common prefix of all the literals
                         extracted.
    --lcs                Show the longest common suffix of all the literals
                         extracted.
    --searcher           Show the debug output for the literal searcher
                         constructed by the literals found.
";

#[derive(Deserialize)]
struct Args {
    cmd_ast: bool,
    cmd_hir: bool,
    cmd_prefixes: bool,
    cmd_suffixes: bool,
    cmd_anchors: bool,
    cmd_captures: bool,
    cmd_compile: bool,
    cmd_utf8_ranges: bool,

    arg_pattern: String,
    arg_patterns: Vec<String>,
    arg_class: String,

    flag_size_limit: usize,
    flag_bytes: bool,
    flag_dfa: bool,
    flag_dfa_reverse: bool,
    flag_all_literals: bool,
    flag_literal_limit: usize,
    flag_class_limit: usize,
    flag_literal_bytes: bool,
    flag_lcp: bool,
    flag_lcs: bool,
    flag_searcher: bool,
}

type Result<T> = result::Result<T, Box<error::Error + Send + Sync>>;

fn main() {
    let mut args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    if args.flag_dfa_reverse {
        args.flag_dfa = true;
    }
    match run(&args) {
        Ok(_) => process::exit(0),
        Err(err) => {
            let _ = writeln!(&mut io::stderr(), "{}", err);
            process::exit(1)
        }
    }
}

fn run(args: &Args) -> Result<()> {
    if args.cmd_ast {
        cmd_ast(args)
    } else if args.cmd_hir {
        cmd_hir(args)
    } else if args.cmd_prefixes {
        cmd_literals(args)
    } else if args.cmd_suffixes {
        cmd_literals(args)
    } else if args.cmd_anchors {
        cmd_anchors(args)
    } else if args.cmd_captures {
        cmd_captures(args)
    } else if args.cmd_compile {
        cmd_compile(args)
    } else if args.cmd_utf8_ranges {
        cmd_utf8_ranges(args)
    } else {
        unreachable!()
    }
}

fn cmd_ast(args: &Args) -> Result<()> {
    use syntax::ast::parse::Parser;

    let mut parser = Parser::new();
    let ast = parser.parse(&args.arg_pattern)?;
    println!("{:#?}", ast);
    Ok(())
}

fn cmd_hir(args: &Args) -> Result<()> {
    use syntax::ParserBuilder;

    let mut parser = ParserBuilder::new().allow_invalid_utf8(false).build();
    let hir = parser.parse(&args.arg_pattern)?;
    println!("{:#?}", hir);
    Ok(())
}

fn cmd_literals(args: &Args) -> Result<()> {
    let exprs = args.parse_many()?;
    let mut lits = if args.cmd_prefixes {
        args.literals(&exprs, |lits, e| lits.union_prefixes(e))
    } else {
        args.literals(&exprs, |lits, e| lits.union_suffixes(e))
    };
    if !args.flag_all_literals {
        if args.cmd_prefixes {
            lits = lits.unambiguous_prefixes();
        } else {
            lits = lits.unambiguous_suffixes();
        }
    }
    if args.flag_searcher {
        if args.cmd_prefixes {
            println!("{:?}", LiteralSearcher::prefixes(lits))
        } else {
            println!("{:?}", LiteralSearcher::suffixes(lits))
        }
    } else if args.flag_lcp {
        println!("{}", escape_unicode(lits.longest_common_prefix()));
    } else if args.flag_lcs {
        println!("{}", escape_unicode(lits.longest_common_suffix()));
    } else {
        for lit in lits.literals() {
            if args.flag_literal_bytes {
                if lit.is_cut() {
                    println!("Cut({})", escape_bytes(lit));
                } else {
                    println!("Complete({})", escape_bytes(lit));
                }
            } else {
                println!("{:?}", lit);
            }
        }
    }
    Ok(())
}

fn cmd_anchors(args: &Args) -> Result<()> {
    let expr = args.parse_one()?;
    if expr.is_anchored_start() {
        println!("start");
    }
    if expr.is_anchored_end() {
        println!("end");
    }
    Ok(())
}

fn cmd_captures(args: &Args) -> Result<()> {
    let expr = args.parse_one()?;
    let prog = args.compiler().only_utf8(false).compile(&[expr])?;
    for (i, name) in prog.captures.iter().enumerate() {
        match *name {
            None => println!("{}", i),
            Some(ref name) => println!("{}:{}", i, name),
        }
    }
    Ok(())
}

fn cmd_compile(args: &Args) -> Result<()> {
    let exprs = args.parse_many()?;
    let compiler = args
        .compiler()
        .bytes(args.flag_bytes)
        .only_utf8(!args.flag_bytes)
        .dfa(args.flag_dfa)
        .reverse(args.flag_dfa_reverse);
    let prog = compiler.compile(&exprs)?;
    print!("{:?}", prog);
    Ok(())
}

fn cmd_utf8_ranges(args: &Args) -> Result<()> {
    use syntax::hir::{self, HirKind};
    use syntax::utf8::Utf8Sequences;
    use syntax::ParserBuilder;

    let hir = ParserBuilder::new()
        .build()
        .parse(&format!("[{}]", args.arg_class))?;
    let cls = match hir.into_kind() {
        HirKind::Class(hir::Class::Unicode(cls)) => cls,
        _ => {
            return Err(
                format!("unexpected HIR, expected Unicode class").into()
            )
        }
    };
    let mut char_count = 0;
    for (i, range) in cls.iter().enumerate() {
        if i > 0 {
            println!("----------------------------");
        }
        char_count += (range.end() as u32) - (range.start() as u32) + 1;
        for seq in Utf8Sequences::new(range.start(), range.end()) {
            for utf8_range in seq.into_iter() {
                print!("[{:02X}-{:02X}]", utf8_range.start, utf8_range.end);
            }
            println!();
        }
    }
    println!("codepoint count: {}", char_count);
    Ok(())
}

impl Args {
    fn parse_one(&self) -> Result<Hir> {
        parse(&self.arg_pattern)
    }

    fn parse_many(&self) -> Result<Vec<Hir>> {
        self.arg_patterns.iter().map(|s| parse(s)).collect()
    }

    fn literals<F: Fn(&mut Literals, &Hir) -> bool>(
        &self,
        exprs: &[Hir],
        get_literals: F,
    ) -> Literals {
        let mut lits = Some(self.empty_literals());
        for e in exprs {
            lits = lits.and_then(|mut lits| {
                if !get_literals(&mut lits, e) {
                    None
                } else {
                    Some(lits)
                }
            });
        }
        lits.unwrap_or(self.empty_literals())
    }

    fn empty_literals(&self) -> Literals {
        let mut lits = Literals::empty();
        lits.set_limit_size(self.flag_literal_limit);
        lits.set_limit_class(self.flag_class_limit);
        lits
    }

    fn compiler(&self) -> Compiler {
        Compiler::new().size_limit(self.flag_size_limit)
    }
}

fn parse(re: &str) -> Result<Hir> {
    use syntax::ParserBuilder;
    ParserBuilder::new()
        .allow_invalid_utf8(true)
        .build()
        .parse(re)
        .map_err(From::from)
}

fn escape_unicode(bytes: &[u8]) -> String {
    let show = match ::std::str::from_utf8(bytes) {
        Ok(v) => v.to_string(),
        Err(_) => escape_bytes(bytes),
    };
    let mut space_escaped = String::new();
    for c in show.chars() {
        if c.is_whitespace() {
            let escaped = if c as u32 <= 0x7F {
                escape_byte(c as u8)
            } else {
                if c as u32 <= 0xFFFF {
                    format!(r"\u{{{:04x}}}", c as u32)
                } else {
                    format!(r"\U{{{:08x}}}", c as u32)
                }
            };
            space_escaped.push_str(&escaped);
        } else {
            space_escaped.push(c);
        }
    }
    space_escaped
}

fn escape_bytes(bytes: &[u8]) -> String {
    let mut s = String::new();
    for &b in bytes {
        s.push_str(&escape_byte(b));
    }
    s
}

fn escape_byte(byte: u8) -> String {
    use std::ascii::escape_default;

    let escaped: Vec<u8> = escape_default(byte).collect();
    String::from_utf8_lossy(&escaped).into_owned()
}
