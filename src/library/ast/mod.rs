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
pub struct FunDecl<T> {
    pub name: String,
    pub args: Vec<(String, Type<T>)>,
    pub rt: Type<T>,
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
pub enum Decl<T, C, S, I> {
    Fun(Fun<T, C, S>),
    Struct(Struct<T>),
    Import(I),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Mod<T, C, S, I, IS> {
    pub name: String,
    pub decls: Vec<Decl<T, C, S, I>>,
    pub imports: IS,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum QualifiedImport<T> {
    Fun(FunDecl<T>),
    Struct(Struct<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Imports {
    pub fundecls: Vec<FunDecl<SimpleType>>,
    pub structs: Vec<Struct<SimpleType>>,
}

impl Imports {
    pub fn create() -> Self {
        Self {
            fundecls: Vec::new(),
            structs: Vec::new(),
        }
    }
}

pub trait NamedModule {
    fn get_name(self) -> String;
}

pub mod untyped {
    use super::NamedModule;
    pub use super::{Lit, Op};

    pub type Expr = super::Expr<String, (), ()>;
    pub type Fun = super::Fun<String, (), ()>;
    pub type FunDecl = super::FunDecl<String>;
    pub type Struct = super::Struct<String>;
    pub type Import = (Vec<String>, String);
    pub type Decl = super::Decl<String, (), (), Import>;
    pub type Mod = super::Mod<String, (), (), Import, ()>;
    pub type Type = super::Type<String>;

    impl NamedModule for Mod {
        fn get_name(self) -> String {
            self.name
        }
    }
}

pub mod qualified {
    pub use super::Imports;
    use super::NamedModule;
    pub use super::{Lit, Op};
    use crate::library::ast::SimpleType;

    pub type Expr = super::Expr<String, (), ()>;
    pub type Fun = super::Fun<String, (), ()>;
    pub type Struct = super::Struct<String>;
    pub type Import = super::QualifiedImport<SimpleType>;
    pub type Decl = super::Decl<String, (), (), Import>;
    pub type Mod = super::Mod<String, (), (), Import, Imports>;
    pub type Type = super::Type<String>;

    impl NamedModule for Mod {
        fn get_name(self) -> String {
            self.name
        }
    }

    impl Mod {
        pub fn empty() -> Self {
            Self {
                name: "".to_string(),
                decls: vec![],
                imports: Imports {
                    fundecls: vec![],
                    structs: vec![],
                },
            }
        }
    }
}

pub mod typed {
    pub use super::Imports;
    use super::NamedModule;
    pub use super::{Closure, SimpleType};
    pub use super::{Lit, Op};

    pub type Expr = super::Expr<SimpleType, Closure, Struct>;
    pub type Fun = super::Fun<SimpleType, Closure, Struct>;
    pub type FunDecl = super::FunDecl<SimpleType>;
    pub type Struct = super::Struct<SimpleType>;
    pub type Import = super::QualifiedImport<SimpleType>;
    pub type Decl = super::Decl<SimpleType, Closure, Struct, Import>;
    pub type Mod = super::Mod<SimpleType, Closure, Struct, Import, Imports>;
    pub type Type = super::Type<SimpleType>;

    impl NamedModule for Mod {
        fn get_name(self) -> String {
            self.name
        }
    }
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
