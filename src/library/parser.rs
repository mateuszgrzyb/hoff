pub use hoff::{decls as parse, repl as parse_repl};
use peg::*;

use crate::library::ast::untyped::*;

use super::ast::BinOp;

fn binop(lh: Expr, op: Op, rh: Expr) -> Expr {
  Expr::BinOp(Box::new(BinOp { lh, op, rh }))
}

// Precedence
// 0. (),
// 1. let x = ..., fun asdf...
// 2, Value, Lit
// 3. *, /
// 4. +, -
// 5. ;
// 6. f()

parser! {
  grammar hoff() for str {

    // REPL

    pub rule repl() -> Repl
      = repl_decl() / repl_expr()

    pub rule repl_decl() -> Repl
      = __ d:decl() __ { Repl::Def(d) }

    pub rule repl_expr() -> Repl
      = __ e:expr() __ { Repl::Expr(e) }

    // Compile

    pub rule decls() -> Vec<Def>
      = __ ds:(decl() ** __) __ { ds }

    pub rule decl() -> Def
      = val() / fun() / struct_() / import() / class() / impl_()

    pub rule val() -> Def
      = "val" _ tid:typedid() __ "=" __ expr:expr() {
        let (name, t) = tid;
        Def::Val(Val{ name, t, expr })
      }

    pub rule fun_sig() -> FunSig
      = "fun" _ n:id() __ "(" __ a:(typedid() ** (__ "," __)) __ ")" __ ":" __ t:type_() {
        FunSig { name: n, args: a.into_iter().map(|(name, type_)| FunArg { name, type_ }).collect(), rt: t }
      }

    pub rule _fun() -> Fun
      = sig:fun_sig() __ "{" __ body:expr() __ "}" {
        Fun { sig, body }
      }

    pub rule fun() -> Def
      = f:_fun() { Def::Fun(f) }

    pub rule struct_() -> Def
      = "type" _ name:tid() __ "{" __ args:(typedid() ** (__ "," __)) __ "}" {
        Def::Struct(Struct { name, args: args.into_iter().map(|(name, type_)| StructArg { name, type_ }).collect() })
      }

    pub rule import() -> Def
      = "from" _ q:(id() ** "::") _ "import" _ n:(id() / tid()) {
        Def::Import((q, n))
      }

    pub rule class() -> Def
      = "class" _ name:tid() __ "{" __ methods:(fun_sig() ** __) __ "}" {
        Def::Class(Class { name, methods })
      }

    pub rule impl_() -> Def
      = "impl" _ class_name:tid() _ "for" _ t:tid() __ "{" __ impls:(_fun() ** __) __ "}" {
        Def::Impl(Impl { class_name, t, impls })
      }

    // Expr

    pub rule expr() -> Expr
      = precedence!{
        x:(@)  __ ";"  __ y:@ { Expr::Chain(Box::new(Chain { e1: x, e2: y })) }
        --
        "val" _ tid:typedid() __ "=" __ expr:@ { Expr::Assign(Box::new(Assign { name: tid.0, type_: tid.1, expr})) }
        --
        t:@ __ "::" __ m:id() __ "(" __ a:(expr() ** (__ "," __)) __ ")" { Expr::MethodCall(Box::new(MethodCall { this: t, typename: (), methodname: m, args: a })) }
        --
        i:id() __ "->" __ a:id() { Expr::Attr(Attr { name: i, struct_: (), attr: a }) }
        --
        x:(@)  __ "&&" __ y:@ { binop(x, Op::And, y) }
        x:(@)  __ "||" __ y:@ { binop(x, Op::Or, y) }
        --
        x:(@)  __ "<"  __ y:@ { binop(x, Op::Lt, y) }
        x:(@)  __ "<=" __ y:@ { binop(x, Op::Le, y) }
        x:(@)  __ "!=" __ y:@ { binop(x, Op::Ne, y) }
        x:(@)  __ "==" __ y:@ { binop(x, Op::Eq, y) }
        x:(@)  __ ">=" __ y:@ { binop(x, Op::Ge, y) }
        x:(@)  __ ">"  __ y:@ { binop(x, Op::Gt, y) }
        --
        x:(@)  __ "+"  __ y:@ { binop(x, Op::Add, y) }
        x:(@)  __ "-"  __ y:@ { binop(x, Op::Sub, y) }
        --
        x:(@)  __ "*"  __ y:@ { binop(x, Op::Mul, y) }
        x:(@)  __ "/"  __ y:@ { binop(x, Op::Div, y) }
        --
        i:if_()      { i }
        c:call()     { c }
        f:fun_expr() { f }
        n:new()      { n }
        --
        "(" __ e:expr() __ ")" { e }
        s:str_temp() { s }
        l:lit()  { l }
        n:name() { n }
      }

    // String template

    pub rule str_temp() -> Expr
      // = s:__str_temp() { Expr::StringTemplate(s.0, s.1) }
      = s:__str_temp() { Expr::StringTemplate(StringTemplate { string: s.0, args: s.1 }) }

    rule __str_temp() -> (String, Vec<String>)
      = "$\"" s:__str_temp_body() "\""  {
        join_t(s)
      }

    rule __str_temp_body() -> Vec<(String, Option<&'input str>)>
      = (__str_temp_arg() / __str_temp_ch())*

    rule __str_temp_arg() -> (String, Option<&'input str>)
      = "{" s:$([^ '}']*) "}" {
        (format!("{{{}}}", s), Some(s))
      }

    rule __str_temp_ch() -> (String, Option<&'input str>)
      = s:$([^ '{'|'"']+) {
        (s.into(), None)
      }

    // Lit

    pub rule lit() -> Expr
      = l:(lit_float() / lit_int() / lit_bool() / lit_string()) { Expr::Lit(l) }

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
        Expr::If(Box::new(If { if_: be, then: e1, else_: e2 }))
      }

    // Name

    pub rule name() -> Expr
      = i:id() { Expr::Value(Value { name: i }) }

    // Call

    pub rule call() -> Expr
      = i:id() __ "(" __ a:(expr() ** (__ "," __)) __ ")" {
        Expr::Call(Call { name: i, args: a })
      }

    // Fun expr

    pub rule fun_expr() -> Expr
      = f:_fun() { Expr::Function(Box::new(Function { f, closure: () })) }

    // New

    pub rule new() -> Expr
      = t:tid() __ "{" __ a:(expr() ** (__ "," __)) __ "}" {
        Expr::New(New { name: t, args: a })
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

    #[cache]
    rule _ = quiet!{[' ' | '\n' | '\t']+}
    #[cache]
    rule __ = quiet!{[' ' | '\n' | '\t']*}

    pub rule numeric() -> i32
      = num:$(digit()+) { num.parse().unwrap() }

    pub rule float_num() -> String
      = f:$((digit()+ ['.'] digit()*)) { f.to_string() }

    pub rule str() -> String
      = s:$(['"'][^ '"']*['"']) { s[1..s.len()-1].to_string() }

    #[cache]
    pub rule digit()
      = ['0'..='9']

    pub rule bool() -> bool
      = b:$("true" / "false") { b.parse().unwrap() }
  }
}

fn join_t(ts: Vec<(String, Option<&str>)>) -> (String, Vec<String>) {
  let (strings, args): (Vec<_>, Vec<_>) = ts.into_iter().unzip();
  let args = args
    .into_iter()
    .flatten()
    .map(|s| s.to_owned())
    .collect::<Vec<_>>();
  let string = strings.join("");
  (string, args)
}

#[cfg(test)]
#[allow(non_snake_case)]
mod test {
  use rstest::*;

  use super::hoff;
  use crate::library::ast::untyped::*;

  #[rstest]
  #[case::int("1", Lit::Int(1))]
  #[case::float("33.0", Lit::Float("33.0".into()))]
  #[case::bool_t("true", Lit::Bool(true))]
  #[case::bool_f("false", Lit::Bool(false))]
  #[case::string("\"ala\"", Lit::String("ala".into()))]
  fn test_lit(#[case] lit_text: &str, #[case] exp_lit: Lit) {
    // given
    let exp_lit_expr = Expr::Lit(exp_lit);

    // when
    let lit_expr = hoff::lit(lit_text).unwrap();

    // then
    assert_eq!(lit_expr, exp_lit_expr);
  }

  #[rstest]
  #[case::add("a + b", ("a", Op::Add, "b"))]
  #[case::add("a - b", ("a", Op::Sub, "b"))]
  #[case::add("a * b", ("a", Op::Mul, "b"))]
  #[case::add("a / b", ("a", Op::Div, "b"))]
  fn test_binop(#[case] binop_text: &str, #[case] exp_binop_args: (&str, Op, &str)) {
    // given
    let (lh, op, rh) = exp_binop_args;
    let exp_binop_ast = Expr::BinOp(Box::new(BinOp {
      lh: Expr::Value(Value { name: lh.into() }),
      op,
      rh: Expr::Value(Value { name: rh.into() }),
    }));

    // when
    let binop_ast = hoff::expr(binop_text).unwrap();

    // then
    assert_eq!(binop_ast, exp_binop_ast)
  }

  type S = &'static str;

  fn ternop_r(a: S, op1: Op, (b, op2, c): (S, Op, S)) -> Expr {
    Expr::BinOp(Box::new(BinOp {
      lh: Expr::Value(Value { name: a.into() }),
      op: op1,
      rh: Expr::BinOp(Box::new(BinOp {
        lh: Expr::Value(Value { name: b.into() }),
        op: op2,
        rh: Expr::Value(Value { name: c.into() }),
      })),
    }))
  }

  fn ternop_l((a, op1, b): (S, Op, S), op2: Op, c: S) -> Expr {
    Expr::BinOp(Box::new(BinOp {
      lh: Expr::BinOp(Box::new(BinOp {
        lh: Expr::Value(Value { name: a.into() }),
        op: op1,
        rh: Expr::Value(Value { name: b.into() }),
      })),
      op: op2,
      rh: Expr::Value(Value { name: c.into() }),
    }))
  }

  #[rstest]
  #[case::add_mul(
    "a + b * c",
    ternop_r("a", Op::Add, ("b", Op::Mul, "c")),
  )]
  #[case::mul_add(
    "b * c + a",
    ternop_l(("b", Op::Mul, "c"), Op::Add, "a")
  )]
  #[case::sub_div(
    "a - b / c",
    ternop_r("a", Op::Sub, ("b", Op::Div, "c"))
  )]
  #[case::div_sub(
    "a / b - c",
    ternop_l(("a", Op::Div, "b"), Op::Sub, "c")
  )]
  #[case::_sub__mul(
    "(a - b) * c",
    ternop_l(("a", Op::Sub, "b"), Op::Mul, "c")
  )]
  #[case::mul__sub_(
    "c * (a - b)",
    ternop_r("c", Op::Mul, ("a", Op::Sub, "b"))
  )]
  #[case::div__add_(
    "a / (b + c)",
    ternop_r("a", Op::Div, ("b", Op::Add, "c"))
  )]
  #[case::div__add_(
    "(b + c) / a",
    ternop_l(("b", Op::Add, "c"), Op::Div, "a")
  )]
  fn test_binop_adv(#[case] binop_text: &str, #[case] exp_binop_ast: Expr) {
    // when
    let binop_ast = hoff::expr(binop_text).unwrap();

    // then
    assert_eq!(binop_ast, exp_binop_ast)
  }

  #[rstest]
  #[case::val_chain(
    "val i: Int = 1; 2",
    Expr::Chain(
      Box::new(
        Chain {
          e1: Expr::Assign(
            Box::new(
              Assign {
                name: "i".into(),
                type_: Type::Simple("Int".into()),
                expr: Expr::Lit(Lit::Int(1)),
              }
            )
          ),
          e2: Expr::Lit(Lit::Int(2))
        }
      )
    )
  )]
  fn test_chain(#[case] chain_text: &str, #[case] exp_chain_ast: Expr) {
    // when
    let chain_ast = hoff::expr(chain_text).unwrap();

    // when
    assert_eq!(chain_ast, exp_chain_ast)
  }

  #[rstest]
  fn test_fun() {
    // given
    let fn_text = r#"fun name (a: Int, b: Int, c: Int): Int { 33 }"#;
    let exp_fn_ast = Def::Fun(Fun {
      sig: FunSig {
        name: "name".into(),
        args: Vec::from([
          FunArg {
            name: "a".into(),
            type_: Type::Simple("Int".into()),
          },
          FunArg {
            name: "b".into(),
            type_: Type::Simple("Int".into()),
          },
          FunArg {
            name: "c".into(),
            type_: Type::Simple("Int".into()),
          },
        ]),
        rt: Type::Simple("Int".into()),
      },
      body: (Expr::Lit(Lit::Int(33))),
    });

    // when
    let fn_ast = hoff::fun(fn_text).unwrap();

    // then
    assert_eq!(fn_ast, exp_fn_ast)
  }

  #[rstest]
  fn test() {
    // given
    let text = r#"

        fun f (a: Int): Int { 1 }

        fun g(b:Int, c: Int): Int { 2 }

        fun h (d: Int, e: Int, f: Int): Int{ 3 }

        fun i (a: Int, b: Int, c: Int): Int { a }

        "#;
    let exp_ast = Vec::from([
      Def::Fun(Fun {
        sig: FunSig {
          name: "f".into(),
          args: Vec::from([FunArg {
            name: "a".into(),
            type_: Type::Simple("Int".into()),
          }]),
          rt: Type::Simple("Int".into()),
        },
        body: (Expr::Lit(Lit::Int(1))),
      }),
      Def::Fun(Fun {
        sig: FunSig {
          name: "g".into(),
          args: Vec::from([
            FunArg {
              name: "b".into(),
              type_: Type::Simple("Int".into()),
            },
            FunArg {
              name: "c".into(),
              type_: Type::Simple("Int".into()),
            },
          ]),
          rt: Type::Simple("Int".into()),
        },
        body: (Expr::Lit(Lit::Int(2))),
      }),
      Def::Fun(Fun {
        sig: FunSig {
          name: "h".into(),
          args: Vec::from([
            FunArg {
              name: "d".into(),
              type_: Type::Simple("Int".into()),
            },
            FunArg {
              name: "e".into(),
              type_: Type::Simple("Int".into()),
            },
            FunArg {
              name: "f".into(),
              type_: Type::Simple("Int".into()),
            },
          ]),
          rt: Type::Simple("Int".into()),
        },
        body: (Expr::Lit(Lit::Int(3))),
      }),
      Def::Fun(Fun {
        sig: FunSig {
          name: "i".into(),
          args: Vec::from([
            FunArg {
              name: "a".into(),
              type_: Type::Simple("Int".into()),
            },
            FunArg {
              name: "b".into(),
              type_: Type::Simple("Int".into()),
            },
            FunArg {
              name: "c".into(),
              type_: Type::Simple("Int".into()),
            },
          ]),
          rt: Type::Simple("Int".into()),
        },
        body: (Expr::Value(Value { name: "a".into() })),
      }),
    ]);

    // when
    let ast = hoff::decls(text).unwrap();

    // then
    assert_eq!(ast, exp_ast)
  }
}
