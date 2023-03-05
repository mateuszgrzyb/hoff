use crate::library::utils::STRING_TEMPLATE_RE;

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
  StringTemplate(String, Vec<String>),
}

impl<T, C, S> Expr<T, C, S> {
  pub fn create_string_template(
    template: String,
  ) -> Result<Self, &'static str> {
    let mut args = Vec::new();

    let mut inside = false;

    for char in template.chars() {
      match (char, inside) {
        ('{', false) => {
          inside = true;
        }
        ('{', true) => return Err("string template error: begin"),
        ('}', false) => return Err("string template error: end"),
        ('}', true) => {
          inside = false;
        }
        _ => {}
      }
    }

    for capture in STRING_TEMPLATE_RE.captures_iter(template.as_str()) {
      if let Some(matched) = capture.get(1) {
        args.push(matched.as_str().to_string());
      }
    }

    Ok(Self::StringTemplate(template, args))
  }
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
pub struct ValDecl<T> {
  pub name: String,
  pub t: Type<T>,
  pub inner_vals: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Val<T, C, S> {
  pub name: String,
  pub t: Type<T>,
  pub expr: Expr<T, C, S>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Decl<T, C, S, I> {
  Val(Val<T, C, S>),
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
pub enum Repl<T, C, S, I> {
  Expr(Expr<T, C, S>),
  Decl(Decl<T, C, S, I>),
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
  Val(ValDecl<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Imports {
  pub fundecls: Vec<FunDecl<SimpleType>>,
  pub structs: Vec<Struct<SimpleType>>,
  pub vals: Vec<ValDecl<SimpleType>>,
}

impl Imports {
  pub fn create() -> Self {
    Self {
      fundecls: Vec::new(),
      structs: Vec::new(),
      vals: Vec::new(),
    }
  }
}

pub mod untyped {
  pub use super::{Lit, Op};
  use crate::library::qualify::Nameable;

  pub type Expr = super::Expr<String, (), ()>;
  pub type Fun = super::Fun<String, (), ()>;
  pub type FunDecl = super::FunDecl<String>;
  pub type Struct = super::Struct<String>;
  pub type Val = super::Val<String, (), ()>;
  pub type ValDecl = super::ValDecl<String>;
  pub type Import = (Vec<String>, String);
  pub type Decl = super::Decl<String, (), (), Import>;
  pub type Mod = super::Mod<String, (), (), Import, ()>;
  pub type Repl = super::Repl<String, (), (), Import>;
  pub type Type = super::Type<String>;

  impl Nameable for Mod {
    fn get_name(&self) -> String {
      self.name.clone()
    }
  }

  impl Nameable for Struct {
    fn get_name(&self) -> String {
      self.name.clone()
    }
  }

  impl Nameable for ValDecl {
    fn get_name(&self) -> String {
      self.name.clone()
    }
  }
}

pub mod qualified {
  pub use super::{Imports, Lit, Op};
  use crate::library::{ast::SimpleType, qualify::Nameable};

  pub type Expr = super::Expr<String, (), ()>;
  pub type Fun = super::Fun<String, (), ()>;
  pub type Struct = super::Struct<String>;
  pub type Val = super::Val<String, (), ()>;
  pub type Import = super::QualifiedImport<SimpleType>;
  pub type Decl = super::Decl<String, (), (), Import>;
  pub type Mod = super::Mod<String, (), (), Import, Imports>;
  pub type Type = super::Type<String>;

  impl Nameable for Mod {
    fn get_name(&self) -> String {
      self.name.clone()
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
          vals: vec![],
        },
      }
    }
  }
}

pub mod typed {
  pub use super::{Closure, Imports, Lit, Op, SimpleType};
  use crate::library::qualify::Nameable;

  pub type Expr = super::Expr<SimpleType, Closure, Struct>;
  pub type Fun = super::Fun<SimpleType, Closure, Struct>;
  pub type FunDecl = super::FunDecl<SimpleType>;
  pub type Struct = super::Struct<SimpleType>;
  pub type Val = super::Val<SimpleType, Closure, Struct>;
  pub type ValDecl = super::ValDecl<SimpleType>;
  pub type Import = super::QualifiedImport<SimpleType>;
  pub type Decl = super::Decl<SimpleType, Closure, Struct, Import>;
  pub type Mod = super::Mod<SimpleType, Closure, Struct, Import, Imports>;
  pub type Type = super::Type<SimpleType>;

  impl Nameable for Mod {
    fn get_name(&self) -> String {
      self.name.clone()
    }
  }
}

// Precedence
// 0. (),
// 1. let x = ..., fun asdf...
// 2, Value, Lit
// 3. *, /
// 4. +, -
// 5. ;
// 6. f()
