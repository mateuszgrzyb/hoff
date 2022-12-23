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
) -> impl FnMut(untyped::Expr, untyped::Expr) -> untyped::Expr {
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
    Assign((String, Type<T>), Box<Expr<T, C, S>>),
    Chain(Box<Expr<T, C, S>>, Box<Expr<T, C, S>>),
    Call(String, Vec<Expr<T, C, S>>),
    If(Box<Expr<T, C, S>>, Box<Expr<T, C, S>>, Box<Expr<T, C, S>>),
    Attr(String, S, String),
    New(String, Vec<Expr<T, C, S>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fun<T, C, S> {
    pub name: String,
    pub args: Vec<(String, Type<T>)>,
    pub rt: Type<T>,
    pub body: Expr<T, C, S>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Struct<T> {
    pub name: String,
    pub args: Vec<(String, Type<T>)>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Decl<T, C, S> {
    Fun(Fun<T, C, S>),
    Struct(Struct<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Mod<T, C, S> {
    pub name: String,
    pub decls: Vec<Decl<T, C, S>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type<T> {
    Simple(T),
    Function(Vec<Type<T>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SimpleType {
    Int,
    Bool,
    Float,
    String,
    Struct(Struct<SimpleType>),
}

pub type Closure = Vec<(String, Type<SimpleType>)>;

pub mod typed {
    use crate::ast::{Closure, SimpleType};

    pub type Expr = super::Expr<SimpleType, Closure, Struct>;
    pub type Fun = super::Fun<SimpleType, Closure, Struct>;
    pub type Struct = super::Struct<SimpleType>;
    pub type Decl = super::Decl<SimpleType, Closure, Struct>;
    pub type Mod = super::Mod<SimpleType, Closure, Struct>;
    pub type Type = super::Type<SimpleType>;
}

pub mod untyped {
    pub type Expr = super::Expr<String, (), ()>;
    pub type Fun = super::Fun<String, (), ()>;
    pub type Struct = super::Struct<String>;
    pub type Decl = super::Decl<String, (), ()>;
    pub type Mod = super::Mod<String, (), ()>;
    pub type Type = super::Type<String>;
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
