#[derive(Debug, PartialEq, Eq, Clone)]

pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

pub fn get_binop(op: Op) -> impl FnMut(Expr, Expr) -> Expr {
    move |lh, rh| Expr::BinOp(Box::new(lh), op.clone(), Box::new(rh))
}

#[derive(Debug, PartialEq, Eq)]

pub enum Lit {
    Num(i32),
}

#[derive(Debug, PartialEq, Eq)]

pub enum Expr {
    BinOp(Box<Expr>, Op, Box<Expr>),
    Lit(Lit),
    Value(String),
    Function(Box<Fun>),
    Assign(String, Box<Expr>),
    Chain(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]

pub struct Fun {
    pub name: String,
    pub args: Vec<String>,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Eq)]

pub struct Mod {
    pub name: String,
    pub funs: Vec<Fun>,
}

/*
Precedence
0. (),
1. let x = ..., fun asdf...
2, Value, Lit
3. *, /
4. +, -
5. ;
6. f()
*/
