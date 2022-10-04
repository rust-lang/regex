use regex_syntax_old::Parser;
use test::Bencher;

use regex_old::internal::Compiler;

#[bench]
fn compile_simple(b: &mut Bencher) {
    b.iter(|| {
        let re = Parser::new().parse(r"^bc(d|e)*$").unwrap();
        Compiler::new().compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_simple_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = Parser::new().parse(r"^bc(d|e)*$").unwrap();
        Compiler::new().bytes(true).compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_simple_full(b: &mut Bencher) {
    b.iter(|| regex!(r"^bc(d|e)*$"));
}

#[bench]
fn compile_small(b: &mut Bencher) {
    b.iter(|| {
        let re = Parser::new().parse(r"\p{L}|\p{N}|\s|.|\d").unwrap();
        Compiler::new().compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_small_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = Parser::new().parse(r"\p{L}|\p{N}|\s|.|\d").unwrap();
        Compiler::new().bytes(true).compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_small_full(b: &mut Bencher) {
    b.iter(|| regex!(r"\p{L}|\p{N}|\s|.|\d"));
}

#[bench]
fn compile_huge(b: &mut Bencher) {
    b.iter(|| {
        let re = Parser::new().parse(r"\p{L}{50}").unwrap();
        Compiler::new().size_limit(1 << 30).compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_huge_bytes(b: &mut Bencher) {
    b.iter(|| {
        let re = Parser::new().parse(r"\p{L}{50}").unwrap();
        Compiler::new().size_limit(1 << 30).bytes(true).compile(&[re]).unwrap()
    });
}

#[bench]
fn compile_huge_full(b: &mut Bencher) {
    b.iter(|| regex!(r"\p{L}{50}"));
}
