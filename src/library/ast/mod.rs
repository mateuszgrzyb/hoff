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
pub struct FunSig<T> {
  pub name: String,
  pub args: Vec<(String, Type<T>)>,
  pub rt: Type<T>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fun<T, C, S> {
  pub sig: FunSig<T>,
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
pub struct Mod<T, C, S, I, IS: Default> {
  pub name: String,
  pub decls: Vec<Decl<T, C, S, I>>,
  pub imports: IS,
}

impl<T, C, S, I, IS: Default> Default for Mod<T, C, S, I, IS> {
  fn default() -> Self {
    Self {
      name: "".to_string(),
      decls: vec![],
      imports: IS::default(),
    }
  }
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
  Fun(FunSig<T>),
  Struct(Struct<T>),
  Val(ValDecl<T>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Imports {
  pub fundecls: Vec<FunSig<SimpleType>>,
  pub structs: Vec<Struct<SimpleType>>,
  pub vals: Vec<ValDecl<SimpleType>>,
}

impl Default for Imports {
  fn default() -> Self {
    Self {
      fundecls: Vec::new(),
      structs: Vec::new(),
      vals: Vec::new(),
    }
  }
}

#[allow(unused_imports, dead_code)]
pub mod untyped {
  use crate::{insert_types, nameable};

  type T = String;
  type C = ();
  type S = ();
  type I = (Vec<String>, String);
  type IS = ();

  insert_types! {}

  nameable! {Mod, Struct, ValDecl}
}

#[allow(unused_imports, dead_code)]
pub mod qualified {
  use crate::{insert_types, nameable};

  type T = String;
  type C = ();
  type S = ();
  type I = QualifiedImport<SimpleType>;
  type IS = Imports;

  insert_types! {}

  nameable! {Mod}
}

#[allow(unused_imports, dead_code)]
pub mod typed {
  use crate::{insert_types, nameable};

  type T = SimpleType;
  type C = Closure;
  type S = Struct;
  type I = QualifiedImport<SimpleType>;
  type IS = Imports;

  insert_types! {}

  nameable! {Mod}
}
