#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Lt,
    Le,
    Ne,
    Eq,
    Ge,
    Gt,
}

pub fn get_binop(
    op: Op,
) -> impl FnMut(UntypedExpr, UntypedExpr) -> UntypedExpr {
    move |lh, rh| Expr::BinOp(Box::new(lh), op.clone(), Box::new(rh))
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Lit {
    Int(i32),
    Bool(bool),
    // ??????????????????????/
    Float(String),
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr<T, C, S> {
    BinOp(Box<Expr<T, C, S>>, Op, Box<Expr<T, C, S>>),
    Lit(Lit),
    Value(String),
    Function(Box<Fun<T, C, S>>, C),
    Assign((String, T), Box<Expr<T, C, S>>),
    Chain(Box<Expr<T, C, S>>, Box<Expr<T, C, S>>),
    Call(String, Vec<Expr<T, C, S>>),
    If(Box<Expr<T, C, S>>, Box<Expr<T, C, S>>, Box<Expr<T, C, S>>),
    Attr(String, S, String),
    New(String, Vec<Expr<T, C, S>>),
}

pub type UntypedExpr = Expr<String, (), ()>;
pub type TypedExpr = Expr<Type, Closure, TypedStruct>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fun<T, C, S> {
    pub name: String,
    pub args: Vec<(String, T)>,
    pub rt: T,
    pub body: Expr<T, C, S>,
}

pub type UntypedFun = Fun<String, (), ()>;
pub type TypedFun = Fun<Type, Closure, TypedStruct>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Struct<T> {
    pub name: String,
    pub args: Vec<(String, T)>,
}

pub type UntypedStruct = Struct<String>;
pub type TypedStruct = Struct<Type>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Decl<T, C, S> {
    Fun(Fun<T, C, S>),
    Struct(Struct<T>),
}

pub type UntypedDecl = Decl<String, (), ()>;
pub type TypedDecl = Decl<Type, Closure, TypedStruct>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Mod<T, C, S> {
    pub name: String,
    pub decls: Vec<Decl<T, C, S>>,
}

pub type UntypedMod = Mod<String, (), ()>;
pub type TypedMod = Mod<Type, Closure, TypedStruct>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int,
    Bool,
    Float,
    String,
    Fun(Vec<Type>, Box<Type>),
    Struct(Struct<Type>),
}

pub type Closure = Vec<(String, Type)>;

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
