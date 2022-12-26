use crate::library::ast::untyped::{Expr, Op};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar, "/library/parser/grammar.rs");

pub fn get_binop(op: Op) -> impl FnMut(Expr, Expr) -> Expr {
    move |lh, rh| Expr::BinOp(Box::new(lh), op.clone(), Box::new(rh))
}

#[cfg(test)]
mod test {
    use super::grammar::{ExprParser, FunParser, ModParser};
    use crate::library::ast::untyped::*;
    use rstest::*;

    #[fixture]
    fn expr_parser() -> ExprParser {
        ExprParser::new()
    }

    #[rstest]
    #[case::add("a + b", ("a", Op::Add, "b"))]
    #[case::add("a - b", ("a", Op::Sub, "b"))]
    #[case::add("a * b", ("a", Op::Mul, "b"))]
    #[case::add("a / b", ("a", Op::Div, "b"))]
    fn test_parse_binop(
        expr_parser: ExprParser,
        #[case] binop_text: &str,
        #[case] exp_binop_args: (&str, Op, &str),
    ) {
        // when
        let binop_ast = expr_parser.parse(binop_text).unwrap();

        // then
        let (lh, op, rh) = exp_binop_args;
        let exp_binop_ast = Expr::BinOp(
            Box::new(Expr::Value(lh.to_string())),
            op,
            Box::new(Expr::Value(rh.to_string())),
        );
        assert_eq!(binop_ast, exp_binop_ast)
    }

    #[rstest]
    #[case::add_mul(
        "a + b * c",
        Expr::BinOp(
            Box::new(Expr::Value("a".to_string())),
            Op::Add,
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("b".to_string())),
                    Op::Mul,
                    Box::new(Expr::Value("c".to_string())),
                ),
            ),
        ),
    )]
    #[case::mul_add(
        "b * c + a",
        Expr::BinOp(
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("b".to_string())),
                    Op::Mul,
                    Box::new(Expr::Value("c".to_string())),
                )
            ),
            Op::Add,
            Box::new(Expr::Value("a".to_string())),
        ),
    )]
    #[case::sub_div(
        "a - b / c",
        Expr::BinOp(
            Box::new(Expr::Value("a".to_string())),
            Op::Sub,
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("b".to_string())),
                    Op::Div,
                    Box::new(Expr::Value("c".to_string())),
                ),
            ),
        ),
    )]
    #[case::div_sub(
        "a / b - c",
        Expr::BinOp(
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("a".to_string())),
                    Op::Div,
                    Box::new(Expr::Value("b".to_string())),
                )
            ),
            Op::Sub,
            Box::new(Expr::Value("c".to_string())),
        ),
    )]
    #[case::_sub__mul(
        "(a - b) * c",
        Expr::BinOp(
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("a".to_string())),
                    Op::Sub,
                    Box::new(Expr::Value("b".to_string())),
                ),
            ),
            Op::Mul,
            Box::new(Expr::Value("c".to_string())),
        ),
    )]
    #[case::mul__sub_(
        "c * (a - b)",
        Expr::BinOp(
            Box::new(Expr::Value("c".to_string())),
            Op::Mul,
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("a".to_string())),
                    Op::Sub,
                    Box::new(Expr::Value("b".to_string())),
                ),
            ),
        ),
    )]
    #[case::div__add_(
        "a / (b + c)",
        Expr::BinOp(
            Box::new(Expr::Value("a".to_string())),
            Op::Div,
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("b".to_string())),
                    Op::Add,
                    Box::new(Expr::Value("c".to_string())),
                ),
            ),
        ),
    )]
    #[case::div__add_(
        "(b + c) / a",
        Expr::BinOp(
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("b".to_string())),
                    Op::Add,
                    Box::new(Expr::Value("c".to_string())),
                ),
            ),
            Op::Div,
            Box::new(Expr::Value("a".to_string())),
        ),
    )]
    fn test_parse_binop_adv(
        expr_parser: ExprParser,
        #[case] binop_text: &str,
        #[case] exp_binop_ast: Expr,
    ) {
        // when
        let binop_ast = expr_parser.parse(binop_text).unwrap();

        // then
        assert_eq!(binop_ast, exp_binop_ast)
    }

    #[rstest]
    fn test_parse_fun() {
        // given
        let fn_text = r#"fun name (a: Int, b: Int, c: Int): Int { 33 }"#;
        let exp_fn_ast = Fun {
            name: "name".to_string(),
            args: Vec::from([
                ("a".to_string(), Type::Simple("Int".to_string())),
                ("b".to_string(), Type::Simple("Int".to_string())),
                ("c".to_string(), Type::Simple("Int".to_string())),
            ]),
            rt: Type::Simple("Int".to_string()),
            body: (Expr::Lit(Lit::Int(33))),
        };

        // when
        let fn_ast = FunParser::new().parse(fn_text).unwrap();

        // then
        assert_eq!(fn_ast, exp_fn_ast)
    }

    #[rstest]
    fn test_parse() {
        // given
        let text = r#"

        fun f (a: Int): Int { 1 }

        fun g(b:Int, c: Int): Int { 2 }

        fun h (d: Int, e: Int, f: Int): Int{ 3 }

        fun i (a: Int, b: Int, c: Int): Int { a }

        "#;
        let name = "main.hff".to_string();
        let exp_ast = Mod {
            name: "main.hff".to_string(),
            decls: Vec::from([
                Decl::Fun(Fun {
                    name: "f".to_string(),
                    args: Vec::from([(
                        "a".to_string(),
                        Type::Simple("Int".to_string()),
                    )]),
                    rt: Type::Simple("Int".to_string()),
                    body: (Expr::Lit(Lit::Int(1))),
                }),
                Decl::Fun(Fun {
                    name: "g".to_string(),
                    args: Vec::from([
                        ("b".to_string(), Type::Simple("Int".to_string())),
                        ("c".to_string(), Type::Simple("Int".to_string())),
                    ]),
                    rt: Type::Simple("Int".to_string()),
                    body: (Expr::Lit(Lit::Int(2))),
                }),
                Decl::Fun(Fun {
                    name: "h".to_string(),
                    args: Vec::from([
                        ("d".to_string(), Type::Simple("Int".to_string())),
                        ("e".to_string(), Type::Simple("Int".to_string())),
                        ("f".to_string(), Type::Simple("Int".to_string())),
                    ]),
                    rt: Type::Simple("Int".to_string()),
                    body: (Expr::Lit(Lit::Int(3))),
                }),
                Decl::Fun(Fun {
                    name: "i".to_string(),
                    args: Vec::from([
                        ("a".to_string(), Type::Simple("Int".to_string())),
                        ("b".to_string(), Type::Simple("Int".to_string())),
                        ("c".to_string(), Type::Simple("Int".to_string())),
                    ]),
                    rt: Type::Simple("Int".to_string()),
                    body: (Expr::Value("a".to_string())),
                }),
            ]),
            imports: (),
        };

        // when
        let ast = ModParser::new().parse(text).unwrap();

        // then
        assert_eq!(ast, exp_ast.decls)
    }
}
