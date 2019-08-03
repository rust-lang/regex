use regex_syntax::Expr;
use test::Bencher;

#[bench]
fn parse_simple(b: &mut Bencher) {
    b.iter(|| {
        let re = r"^bc(d|e)*$";
        Expr::parse(re).unwrap()
    });
}

#[bench]
fn parse_simple2(b: &mut Bencher) {
    b.iter(|| {
        let re = r"'[a-zA-Z_][a-zA-Z0-9_]*(')\b";
        Expr::parse(re).unwrap()
    });
}

#[bench]
fn parse_small(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\p{L}|\p{N}|\s|.|\d";
        Expr::parse(re).unwrap()
    });
}

#[bench]
fn parse_huge(b: &mut Bencher) {
    b.iter(|| {
        let re = r"\p{L}{100}";
        Expr::parse(re).unwrap()
    });
}
