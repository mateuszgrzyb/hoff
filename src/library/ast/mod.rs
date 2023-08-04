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

macro_rules! ast {
  (
    T = $T:ty,
    C = $C:ty,
    S = $S:ty,
    I = $I:ty,
    IS = $IS:ty,
  ) => {
    #[allow(dead_code)]
    pub type Op = super::Op;
    #[allow(dead_code)]
    pub type Lit = super::Lit;
    pub type Expr = super::Expr<$T, $C, $S>;
    #[allow(dead_code)]
    pub type FunDecl = super::FunDecl<$T>;
    pub type Fun = super::Fun<$T, $C, $S>;
    pub type Struct = super::Struct<$T>;
    #[allow(dead_code)]
    pub type ValDecl = super::ValDecl<$T>;
    pub type Val = super::Val<$T, $C, $S>;
    pub type Import = $I;
    #[allow(dead_code)]
    pub type Imports = $IS;
    pub type Decl = super::Decl<$T, $C, $S, $I>;
    pub type Mod = super::Mod<$T, $C, $S, $I, $IS>;
    #[allow(dead_code)]
    pub type Repl = super::Repl<$T, $C, $S, $I>;
    pub type Type = super::Type<$T>;
    #[allow(dead_code)]
    pub type SimpleType = super::SimpleType;
    #[allow(dead_code)]
    pub type Closure = super::Closure;
  };
}

pub mod untyped {
  use crate::library::qualify::Nameable;

  ast!(
    T = String,
    C = (),
    S = (),
    I = (Vec<String>, String),
    IS = (),
  );

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
  use crate::library::qualify::Nameable;

  ast!(
    T = String,
    C = (),
    S = (),
    I = super::QualifiedImport<super::SimpleType>,
    IS = super::Imports,
  );

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
        imports: super::Imports {
          fundecls: vec![],
          structs: vec![],
          vals: vec![],
        },
      }
    }
  }
}

pub mod typed {
  use crate::library::qualify::Nameable;

  ast!(
    T = super::SimpleType,
    C = super::Closure,
    S = super::Struct<super::SimpleType>,
    I = super::QualifiedImport<super::SimpleType>,
    IS = super::Imports,
  );

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
