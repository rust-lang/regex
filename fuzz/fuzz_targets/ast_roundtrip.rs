#![no_main]

use {
    libfuzzer_sys::{fuzz_target, Corpus},
    regex_syntax::ast::{
        parse::Parser, visit, Ast, Flag, Group, GroupKind, SetFlags, Visitor,
    },
};

#[derive(Eq, PartialEq, arbitrary::Arbitrary)]
struct FuzzData {
    ast: Ast,
}

impl std::fmt::Debug for FuzzData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("FuzzData");
        builder.field("ast", &self.ast);
        builder.field("stringified", &format!("{}", self.ast));
        builder.finish()
    }
}

struct VerboseVisitor;

impl Visitor for VerboseVisitor {
    type Output = ();
    type Err = ();

    fn finish(self) -> Result<Self::Output, Self::Err> {
        Ok(())
    }

    fn visit_pre(&mut self, ast: &Ast) -> Result<Self::Output, Self::Err> {
        match ast {
            Ast::Flags(SetFlags { flags, .. })
            | Ast::Group(Group {
                kind: GroupKind::NonCapturing(flags), ..
            }) if flags
                .flag_state(Flag::IgnoreWhitespace)
                .unwrap_or(false) =>
            {
                Err(())
            }
            _ => Ok(()),
        }
    }
}

fuzz_target!(|data: FuzzData| -> Corpus {
    let _ = env_logger::try_init();

    let pattern = format!("{}", data.ast);
    let Ok(ast) = Parser::new().parse(&pattern) else {
        return Corpus::Keep;
    };
    if visit(&ast, VerboseVisitor).is_err() {
        return Corpus::Reject;
    }
    let ast2 = Parser::new().parse(&ast.to_string()).unwrap();
    assert_eq!(
        ast,
        ast2,
        "Found difference:\
         \nleft:  {:?}\
         \nright: {:?}\
         \nIf these two match, then there was a parsing difference; \
           maybe non-determinism?",
        ast.to_string(),
        ast2.to_string()
    );
    Corpus::Keep
});
