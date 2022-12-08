/*
use crate::ast::{Expr, Fun, Lit, Mod, Op};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, digit1, multispace0, multispace1};
use nom::combinator::{all_consuming, cond, map, map_res, not};
use nom::error::ParseError;
use nom::multi::{many1, separated_list0};
use nom::sequence::{delimited, tuple};
use nom::{Finish, IResult};
use nom_regex::str::re_find;
use regex::Regex;

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn s1(input: &str) -> IResult<&str, &str> {
    multispace1(input)
}

fn s0(input: &str) -> IResult<&str, &str> {
    multispace0(input)
}

fn parse_id(input: &str) -> IResult<&str, String> {
    let regex = Regex::new(r#"^[a-zA-Z_]+"#).unwrap();
    map(re_find(regex), |id: &str| id.to_string())(input)
}

fn parse_expr0(input: &str) -> IResult<&str, Expr> {
    delimited(tag("("), parse_expr, tag(")"))(input)
}

fn parse_expr1(input: &str) -> IResult<&str, Expr> {
    alt((
        map(
            tuple((tag("val"), s1, parse_id, s1, tag("="), s1, parse_expr)),
            |args| Expr::Assign(args.2, Box::new(args.6)),
        ),
        map(parse_fun, |f| Expr::Function(Box::new(f))),
        parse_expr0,
    ))(input)
}

fn parse_expr2(input: &str) -> IResult<&str, Expr> {
    alt((
        map(parse_value, Expr::Value),
        map(parse_lit, Expr::Lit),
        parse_expr1,
    ))(input)
}

fn parse_expr3(input: &str) -> IResult<&str, Expr> {
    alt((
        map(
            tuple((parse_expr2, s1, tag("*"), s1, parse_expr)),
            get_binop(Op::Mul),
        ),
        map(
            tuple((parse_expr2, s1, tag("/"), s1, parse_expr)),
            get_binop(Op::Div),
        ),
        parse_expr2,
    ))(input)
}

fn parse_expr4(input: &str) -> IResult<&str, Expr> {
    alt((
        map(
            tuple((parse_expr3, s1, tag("+"), s1, parse_expr)),
            get_binop(Op::Add),
        ),
        map(
            tuple((parse_expr3, s1, tag("-"), s1, parse_expr)),
            get_binop(Op::Sub),
        ),
        parse_expr3,
    ))(input)
}

fn parse_expr5(input: &str) -> IResult<&str, Expr> {
    alt((
        map(tuple((parse_expr4, s0, tag(";"), s0, parse_expr)), |args| {
            Expr::Chain(Box::new(args.0), Box::new(args.4))
        }),
        parse_expr4,
    ))(input)
}

fn get_binop<A, B, C>(op: Op) -> impl FnMut((Expr, A, B, C, Expr)) -> Expr {
    move |args| Expr::BinOp(Box::new(args.0), op.clone(), Box::new(args.4))
}

fn parse_num(input: &str) -> IResult<&str, i32> {
    map_res(digit1, str::parse)(input)
}

fn parse_lit(input: &str) -> IResult<&str, Lit> {
    alt((map(parse_num, Lit::Num),))(input)
}

fn parse_value(input: &str) -> IResult<&str, String> {
    map(alpha1, |id: &str| id.to_string())(input)
}

fn parse_expr(input: &str) -> IResult<&str, Expr> {
    parse_expr5(input)
}

fn parse_fun_args(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tag("("), separated_list0(tag(","), ws(parse_id)), tag(")"))(
        input,
    )
}

fn parse_fun(input: &str) -> IResult<&str, Fun> {
    map(
        tuple((
            tag("fun"),
            s1,
            parse_id,
            s0,
            parse_fun_args,
            s0,
            delimited(tag("{"), ws(parse_expr), tag("}")),
        )),
        |args| Fun {
            name: args.2,
            args: args.4,
            body: args.6,
        },
    )(input)
}

fn parse_funcs(input: &str) -> IResult<&str, Vec<Fun>> {
    many1(ws(parse_fun))(input)
}

pub fn parse(name: String, input: &str) -> Result<Mod, String> {
    let result = all_consuming(map(parse_funcs, |funcs| Mod {
        name: name.clone(),
        funcs,
    }))(input)
    .finish();
    match result {
        Ok((_, ast)) => Ok(ast),
        Err(e) => Err(format!("Error when parsing: {e}")),
    }
}

#[cfg(test)]
mod test {

    use crate::ast::*;
    use crate::old_parser::*;
    use rstest::*;

    #[rstest]
    #[case::add("a + b", ("a", Op::Add, "b"))]
    #[case::add("a - b", ("a", Op::Sub, "b"))]
    #[case::add("a * b", ("a", Op::Mul, "b"))]
    #[case::add("a / b", ("a", Op::Div, "b"))]
    fn test_parse_binop(
        #[case] binop_text: &str,
        #[case] exp_binop_args: (&str, Op, &str),
    ) {
        // when
        let (_, binop_ast) = parse_expr(binop_text).unwrap();

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
                )
            ),
        ),
    )]
    /*
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
     */
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
                )
            ),
        ),
    )]
    /*
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
     */
    #[case::_sub__mul(
        "(a - b) * c",
        Expr::BinOp(
            Box::new(
                Expr::BinOp(
                    Box::new(Expr::Value("a".to_string())),
                    Op::Sub,
                    Box::new(Expr::Value("b".to_string())),
                )
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
                )
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
                )
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
                )
            ),
            Op::Div,
            Box::new(Expr::Value("a".to_string())),
        ),
    )]
    fn test_parse_binop_adv(
        #[case] binop_text: &str,
        #[case] exp_binop_ast: Expr,
    ) {
        // when
        let (_, binop_ast) = parse_expr(binop_text).unwrap();

        // then
        assert_eq!(binop_ast, exp_binop_ast)
    }

    #[rstest]
    fn test_parse_fun_args() {
        // given
        let fn_args_text = r#"(a, b, c)"#;
        let exp_fn_args_ast =
            Vec::from(["a".to_string(), "b".to_string(), "c".to_string()]);

        // when
        let (_, fn_args_ast) = parse_fun_args(fn_args_text).unwrap();

        // then
        assert_eq!(fn_args_ast, exp_fn_args_ast);
    }

    #[rstest]
    fn test_parse_fun() {
        // given
        let fn_text = r#"fun name (a, b, c) { 33 }"#;
        let exp_fn_ast = Fun {
            name: "name".to_string(),
            args: Vec::from([
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
            ]),
            body: (Expr::Lit(Lit::Num(33))),
        };

        // when
        let (_, fn_ast) = parse_fun(fn_text).unwrap();

        // then
        assert_eq!(fn_ast, exp_fn_ast)
    }

    #[rstest]
    fn test_parse() {
        // given
        let text = r#"

        fun f (a) { 1 }

        fun g(b, c) { 2 }

        fun h (d, e, f){ 3 }

        fun i (a, b, c) { a }

        "#;
        let name = "main.hff".to_string();
        let exp_ast = Mod {
            name: "main.hff".to_string(),
            funcs: Vec::from([
                Fun {
                    name: "f".to_string(),
                    args: Vec::from(["a".to_string()]),
                    body: (Expr::Lit(Lit::Num(1))),
                },
                Fun {
                    name: "g".to_string(),
                    args: Vec::from(["b".to_string(), "c".to_string()]),
                    body: (Expr::Lit(Lit::Num(2))),
                },
                Fun {
                    name: "h".to_string(),
                    args: Vec::from([
                        "d".to_string(),
                        "e".to_string(),
                        "f".to_string(),
                    ]),
                    body: (Expr::Lit(Lit::Num(3))),
                },
                Fun {
                    name: "i".to_string(),
                    args: Vec::from([
                        "a".to_string(),
                        "b".to_string(),
                        "c".to_string(),
                    ]),
                    body: (Expr::Value("a".to_string())),
                },
            ]),
        };

        // when
        let ast = parse(name, text).unwrap();

        // then
        assert_eq!(ast, exp_ast)
    }
}
 */
