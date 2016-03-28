extern crate docopt;
extern crate regex;
extern crate regex_syntax as syntax;
extern crate rustc_serialize;

use std::error;
use std::io::{self, Write};
use std::process;
use std::result;

use docopt::Docopt;
use regex::internal::{Compiler, LiteralSearcher};
use syntax::{ExprBuilder, Expr, Literals};

const USAGE: &'static str = "
Usage:
    regex-debug [options] ast <pattern>
    regex-debug [options] prefixes <patterns> ...
    regex-debug [options] suffixes <patterns> ...
    regex-debug [options] anchors <pattern>
    regex-debug [options] captures <pattern>
    regex-debug [options] compile <patterns> ...
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
    --lcp                Show the longest common prefix of all the literals
                         extracted.
    --lcs                Show the longest common suffix of all the literals
                         extracted.
    --searcher           Show the debug output for the literal searcher
                         constructed by the literals found.
";

#[derive(RustcDecodable)]
struct Args {
    cmd_ast: bool,
    cmd_prefixes: bool,
    cmd_suffixes: bool,
    cmd_anchors: bool,
    cmd_captures: bool,
    cmd_compile: bool,

    arg_pattern: String,
    arg_patterns: Vec<String>,

    flag_size_limit: usize,
    flag_bytes: bool,
    flag_dfa: bool,
    flag_dfa_reverse: bool,
    flag_all_literals: bool,
    flag_literal_limit: usize,
    flag_class_limit: usize,
    flag_lcp: bool,
    flag_lcs: bool,
    flag_searcher: bool,
}

type Result<T> = result::Result<T, Box<error::Error + Send + Sync>>;

fn main() {
    let mut args: Args = Docopt::new(USAGE)
                                .and_then(|d| d.decode())
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
    } else {
        unreachable!()
    }
}

fn cmd_ast(args: &Args) -> Result<()> {
    println!("{:#?}", try!(args.parse_one()));
    Ok(())
}

fn cmd_literals(args: &Args) -> Result<()> {
    let exprs = try!(args.parse_many());
    let mut lits =
        if args.cmd_prefixes {
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
            println!("{:?}", lit);
        }
    }
    Ok(())
}

fn cmd_anchors(args: &Args) -> Result<()> {
    let expr = try!(args.parse_one());
    if expr.is_anchored_start() {
        println!("start");
    }
    if expr.is_anchored_end() {
        println!("end");
    }
    Ok(())
}

fn cmd_captures(args: &Args) -> Result<()> {
    let expr = try!(args.parse_one());
    let prog = try!(args.compiler().only_utf8(false).compile(&[expr]));
    for (i, name) in prog.captures.iter().enumerate() {
        match *name {
            None => println!("{}", i),
            Some(ref name) => println!("{}:{}", i, name),
        }
    }
    Ok(())
}

fn cmd_compile(args: &Args) -> Result<()> {
    let exprs = try!(args.parse_many());
    let compiler =
        args.compiler()
            .bytes(args.flag_bytes)
            .only_utf8(!args.flag_bytes)
            .dfa(args.flag_dfa)
            .reverse(args.flag_dfa_reverse);
    let prog = try!(compiler.compile(&exprs));
    print!("{:?}", prog);
    Ok(())
}

impl Args {
    fn parse_one(&self) -> Result<Expr> {
        parse(&self.arg_pattern)
    }

    fn parse_many(&self) -> Result<Vec<Expr>> {
        self.arg_patterns.iter().map(|s| parse(s)).collect()
    }

    fn literals<F: Fn(&mut Literals, &Expr) -> bool>(
        &self,
        exprs: &[Expr],
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

fn parse(re: &str) -> Result<Expr> {
    ExprBuilder::new().allow_bytes(true).parse(re).map_err(From::from)
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
