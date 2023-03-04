pub use hoff::{
  decl as parse_decl,
  decls as parse,
  expr as parse_expr,
};
use peg::*;

use crate::library::ast::untyped::*;

fn create_binop(lh: Expr, op: Op, rh: Expr) -> Expr {
  Expr::BinOp(Box::new(lh), op.clone(), Box::new(rh))
}

parser! {
  grammar hoff() for str {
    pub rule decls() -> Vec<Decl>
      = __ ds:(decl() ** __) __ { ds }

    pub rule decl() -> Decl
      = val() / fun() / struct_() / import()

    pub rule val() -> Decl
      = "val" _ tid:typedid() __ "=" __ e:expr() {
        Decl::Val(Val{ name: tid.0, t: tid.1, expr: e})
      }

    pub rule _fun() -> Fun
      = "fun" _ n:id() __ "(" __ a:(typedid() ** (__ "," __)) __ ")" __ ":" __ t:type_() __ "{" __ b:expr() __ "}" {
        Fun { name: n, args: a, rt: t, body: b }
      }

    pub rule fun() -> Decl
      = f:_fun() { Decl::Fun(f) }

    pub rule struct_() -> Decl
      = "type" _ n:tid() __ "{" __ a:(typedid() ** (__ "," __)) __ "}" {
        Decl::Struct(Struct { name: n, args: a })
      }

    pub rule import() -> Decl
      = "from" _ q:(id() ** "::") _ "import" _ n:(id() / tid()) {
        Decl::Import((q, n))
      }

    pub rule expr() -> Expr
      = precedence!{

        x:(@)  __ ";"  __ y:@ { Expr::Chain(Box::new(x), Box::new(y)) }
        --
        i:id() __ "->" __ a:id() { Expr::Attr(i, (), a) }
        --
        x:(@)  __ "&&" __ y:@ { create_binop(x, Op::And, y) }
        x:(@)  __ "||" __ y:@ { create_binop(x, Op::Or, y) }
        --
        x:(@)  __ "<"  __ y:@ { create_binop(x, Op::Lt, y) }
        x:(@)  __ "<=" __ y:@ { create_binop(x, Op::Le, y) }
        x:(@)  __ "!=" __ y:@ { create_binop(x, Op::Ne, y) }
        x:(@)  __ "==" __ y:@ { create_binop(x, Op::Eq, y) }
        x:(@)  __ ">=" __ y:@ { create_binop(x, Op::Ge, y) }
        x:(@)  __ ">"  __ y:@ { create_binop(x, Op::Gt, y) }
        --
        x:(@)  __ "+"  __ y:@ { create_binop(x, Op::Add, y) }
        x:(@)  __ "-"  __ y:@ { create_binop(x, Op::Sub, y) }
        --
        x:(@)  __ "*"  __ y:@ { create_binop(x, Op::Mul, y) }
        x:(@)  __ "/"  __ y:@ { create_binop(x, Op::Div, y) }
        --
        i:if_()      { i }
        c:call()     { c }
        f:fun_expr() { f }
        "val" _ tid:typedid() __ "=" __ e:@ { Expr::Assign(tid, Box::new(e)) }
        n:new()      { n }
        --
        "(" __ e:expr() __ ")" { e }
        s:str_temp() { s }
        l:lit()  { l }
        n:name() { n }
      }

    // String template

    pub rule str_temp() -> Expr
      = "$" s:str() {? Expr::create_string_template(s) }

    // Lit

    pub rule lit() -> Expr
      = l:(lit_int() / lit_float() / lit_bool() / lit_string()) { Expr::Lit(l) }

    pub rule lit_int() -> Lit
      = n:numeric() { Lit::Int(n) }

    pub rule lit_float() -> Lit
      = f:float_num() { Lit::Float(f) }

    pub rule lit_bool() -> Lit
      = b:bool() { Lit::Bool(b) }

    pub rule lit_string() -> Lit
      = s:str() { Lit::String(s) }

    // If

    pub rule if_() -> Expr
      = "if" _ be:expr() __ "{" __ e1:expr() __ "}" __ "else" __ "{" __ e2:expr() __ "}" {
        Expr::If(Box::new(be), Box::new(e1), Box::new(e2))
      }

    // Name

    pub rule name() -> Expr
      = i:id() { Expr::Value(i) }

    // Call

    pub rule call() -> Expr
      = i:id() __ "(" __ a:(expr() ** (__ "," __)) __ ")" {
        Expr::Call(i, a)
      }

    // Fun expr

    pub rule fun_expr() -> Expr
      = f:_fun() { Expr::Function(Box::new(f), ()) }

    // New

    pub rule new() -> Expr
      = t:tid() __ "{" __ a:(expr() ** (__ "," __)) __ "}" {
        Expr::New(t, a)
      }

    // ---- utils ----

    pub rule typedid() -> (String, Type)
      = i:id() __ ":" __ t:type_() { (i, t) }

    pub rule id() -> String
      = alpha:$(['a'..='z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { alpha.to_string() }

    pub rule tid() -> String
      = alpha:$(['A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { alpha.to_string() }

    pub rule type_() -> Type
      = t:tid() { Type::Simple(t) }

    rule _ = quiet!{[' ' | '\n' | '\t']+}
    rule __ = quiet!{[' ' | '\n' | '\t']*}

    pub rule numeric() -> i32
      = num:$(digit()+) { num.parse().unwrap() }

    pub rule float_num() -> String
      = f:$((digit()+ ['.'] digit()*)) { f.to_string() }

    pub rule str() -> String
      = s:$(['"'][^ '"']*['"']) { s[1..s.len()-1].to_string() }

    pub rule digit()
      = ['0'..='9']

    pub rule bool() -> bool
      = b:$("true" / "false") { b.parse().unwrap() }
  }
}

parser! {
  grammar list_parser() for str {
    pub rule number() -> u32
      = n:$(['0'..='9']+) {? n.parse().or(Err("u32")) }

    pub rule list() -> Vec<u32>
      = "[" __ l:(number() ** (__ "," __)) __ "]" { l }

    rule _ = quiet!{[' ' | '\n' | '\t']+}
    rule __ = quiet!{[' ' | '\n' | '\t']*}
  }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod test {
  use rstest::*;

  use super::hoff;
  use crate::library::ast::untyped::*;

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
    let binop_ast = hoff::expr(binop_text).unwrap();

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
    #[case] binop_text: &str,
    #[case] exp_binop_ast: Expr,
  ) {
    // when
    let binop_ast = hoff::expr(binop_text).unwrap();

    // then
    assert_eq!(binop_ast, exp_binop_ast)
  }

  #[rstest]
  #[case::val_chain(
    "val i: Int = 1; 2",
    Expr::Chain(
      Box::new(Expr::Assign(
        ("i".to_string(), Type::Simple("Int".to_string())),
        Box::new(Expr::Lit(Lit::Int(1))),
      )),
      Box::new(
        Expr::Lit(Lit::Int(2))
      )
    )
  )]
  fn test_parse_chain(#[case] chain_text: &str, #[case] exp_chain_ast: Expr) {
    // when
    let chain_ast = hoff::expr(chain_text).unwrap();

    // when
    assert_eq!(chain_ast, exp_chain_ast)
  }

  #[rstest]
  fn test_parse_fun() {
    // given
    let fn_text = r#"fun name (a: Int, b: Int, c: Int): Int { 33 }"#;
    let exp_fn_ast = Decl::Fun(Fun {
      name: "name".to_string(),
      args: Vec::from([
        ("a".to_string(), Type::Simple("Int".to_string())),
        ("b".to_string(), Type::Simple("Int".to_string())),
        ("c".to_string(), Type::Simple("Int".to_string())),
      ]),
      rt: Type::Simple("Int".to_string()),
      body: (Expr::Lit(Lit::Int(33))),
    });

    // when
    let fn_ast = hoff::fun(fn_text).unwrap();

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
    let exp_ast = Vec::from([
      Decl::Fun(Fun {
        name: "f".to_string(),
        args: Vec::from([("a".to_string(), Type::Simple("Int".to_string()))]),
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
    ]);

    // when
    let ast = hoff::decls(text).unwrap();

    // then
    assert_eq!(ast, exp_ast)
  }
}
