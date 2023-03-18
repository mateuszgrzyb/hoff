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
pub enum Expr<T, C, S, TN> {
  BinOp(Box<Expr<T, C, S, TN>>, Op, Box<Expr<T, C, S, TN>>),
  Lit(Lit),
  Value(String),
  Function(Box<Fun<T, C, S, TN>>, C),
  Assign((String, Type<T>), Box<Expr<T, C, S, TN>>),
  Chain(Box<Expr<T, C, S, TN>>, Box<Expr<T, C, S, TN>>),
  Call(String, Vec<Expr<T, C, S, TN>>),
  If(
    Box<Expr<T, C, S, TN>>,
    Box<Expr<T, C, S, TN>>,
    Box<Expr<T, C, S, TN>>,
  ),
  Attr(String, S, String),
  New(String, Vec<Expr<T, C, S, TN>>),
  StringTemplate(String, Vec<String>),
  MethodCall(Box<Expr<T, C, S, TN>>, TN, String, Vec<Expr<T, C, S, TN>>),
}

impl<T, C, S, TN> Expr<T, C, S, TN> {
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
pub struct Fun<T, C, S, TN> {
  pub sig: FunSig<T>,
  pub body: Expr<T, C, S, TN>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
pub struct Val<T, C, S, TN> {
  pub name: String,
  pub t: Type<T>,
  pub expr: Expr<T, C, S, TN>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Class<T> {
  pub name: String,
  pub methods: Vec<FunSig<T>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Impl<T, C, S, TN> {
  pub class_name: String,
  pub t: T,
  pub impls: Vec<Fun<T, C, S, TN>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Def<T, C, S, TN, I> {
  Val(Val<T, C, S, TN>),
  Fun(Fun<T, C, S, TN>),
  Struct(Struct<T>),
  Import(I),
  Class(Class<T>),
  Impl(Impl<T, C, S, TN>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Mod<T, C, S, TN, I, IS: Default> {
  pub name: String,
  pub defs: Vec<Def<T, C, S, TN, I>>,
  pub imports: IS,
}

impl<T, C, S, TN, I, IS: Default> Default for Mod<T, C, S, TN, I, IS> {
  fn default() -> Self {
    Self {
      name: "".to_string(),
      defs: vec![],
      imports: IS::default(),
    }
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Repl<T, C, S, TN, I> {
  Expr(Expr<T, C, S, TN>),
  Def(Def<T, C, S, TN, I>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type<T> {
  Simple(T),
  Function(Vec<Type<T>>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum SimpleType {
  Int,
  Bool,
  Float,
  String,
  Struct(Struct<SimpleType>),
  This,
}

impl SimpleType {
  pub fn get_name(&self) -> String {
    match self {
      SimpleType::Int => "Int".into(),
      SimpleType::Bool => "Bool".into(),
      SimpleType::Float => "Float".into(),
      SimpleType::String => "String".into(),
      SimpleType::Struct(s) => format!("Struct<{}>", s.name),
      SimpleType::This => "This".into(),
    }
  }
}

pub type Closure = Vec<(String, Type<SimpleType>)>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Decl<T> {
  Fun(FunSig<T>),
  Struct(Struct<T>),
  Val(ValDecl<T>),
  Class(Class<T>),
}

impl<T> Decl<T> {
  pub fn get_name(&self) -> &String {
    match self {
      Decl::Fun(f) => &f.name,
      Decl::Struct(s) => &s.name,
      Decl::Val(v) => &v.name,
      Decl::Class(c) => &c.name,
    }
  }
}

pub type Decls<T> = Vec<Decl<T>>;

#[allow(unused_imports, dead_code)]
pub mod untyped {
  use crate::nameable;

  type T = String;
  type C = ();
  type S = ();
  type TN = ();
  type I = (Vec<String>, String);
  type IS = ();

  pub use super::*;

  pub type Expr = super::Expr<T, C, S, TN>;
  pub type Fun = super::Fun<T, C, S, TN>;
  pub type FunSig = super::FunSig<T>;
  pub type Struct = super::Struct<T>;
  pub type Val = super::Val<T, C, S, TN>;
  pub type ValDecl = super::ValDecl<T>;
  pub type Class = super::Class<T>;
  pub type Impl = super::Impl<T, C, S, TN>;
  pub type Import = I;
  pub type Def = super::Def<T, C, S, TN, I>;
  pub type Mod = super::Mod<T, C, S, TN, I, IS>;
  pub type Repl = super::Repl<T, C, S, TN, I>;
  pub type Type = super::Type<T>;
  pub type Decl = super::Decl<T>;
  pub type Decls = super::Decls<T>;

  nameable! {Mod, Struct, ValDecl}
}

#[allow(unused_imports, dead_code)]
pub mod qualified {
  use crate::nameable;

  type T = String;
  type C = ();
  type S = ();
  type TN = ();
  type I = Decl<SimpleType>;
  type IS = Decls<SimpleType>;

  pub use super::*;

  pub type Expr = super::Expr<T, C, S, TN>;
  pub type Fun = super::Fun<T, C, S, TN>;
  pub type FunSig = super::FunSig<T>;
  pub type Struct = super::Struct<T>;
  pub type Val = super::Val<T, C, S, TN>;
  pub type ValDecl = super::ValDecl<T>;
  pub type Class = super::Class<T>;
  pub type Impl = super::Impl<T, C, S, TN>;
  pub type Import = I;
  pub type Def = super::Def<T, C, S, TN, I>;
  pub type Mod = super::Mod<T, C, S, TN, I, IS>;
  pub type Repl = super::Repl<T, C, S, TN, I>;
  pub type Type = super::Type<T>;

  nameable! {Mod}
}

#[allow(unused_imports, dead_code)]
pub mod typed {
  use crate::nameable;

  type T = SimpleType;
  type C = Closure;
  type S = Struct;
  type TN = String;
  type I = super::Decl<SimpleType>;
  type IS = super::Decls<SimpleType>;

  pub use super::*;

  pub type Expr = super::Expr<T, C, S, TN>;
  pub type Fun = super::Fun<T, C, S, TN>;
  pub type FunSig = super::FunSig<T>;
  pub type Struct = super::Struct<T>;
  pub type Val = super::Val<T, C, S, TN>;
  pub type ValDecl = super::ValDecl<T>;
  pub type Class = super::Class<T>;
  pub type Impl = super::Impl<T, C, S, TN>;
  pub type Import = I;
  pub type Def = super::Def<T, C, S, TN, I>;
  pub type Mod = super::Mod<T, C, S, TN, I, IS>;
  pub type Repl = super::Repl<T, C, S, TN, I>;
  pub type Type = super::Type<T>;
  pub type Decl = super::Decl<T>;
  pub type Decls = super::Decls<T>;

  nameable! {Mod}

  impl Impl {
    fn instantiate_type(&self, t: Type) -> Type {
      match t {
        Type::Simple(SimpleType::This) => Type::Simple(self.t.clone()),
        _ => t,
      }
    }

    fn instantiate_method(&self, f: Fun) -> Fun {
      let name = f.sig.name;
      let args = f
        .sig
        .args
        .into_iter()
        .map(|(v, t)| (v, self.instantiate_type(t)))
        .collect();
      let rt = self.instantiate_type(f.sig.rt);
      let body = f.body;

      Fun {
        sig: FunSig { name, args, rt },
        body,
      }
    }

    pub fn instantiate(&self) -> Self {
      let class_name = self.class_name.clone();
      let t = self.t.clone();
      let impls = self
        .impls
        .clone()
        .into_iter()
        .map(|i| self.instantiate_method(i))
        .collect();
      Self {
        class_name,
        t,
        impls,
      }
    }
  }

  pub fn get_method_name(t: &SimpleType, name: &String) -> String {
    let type_name = t.get_name();
    let method_name = name.clone();
    format!("{type_name}$(method)${method_name}")
  }

  pub fn get_method_name_by_tname(tname: String, name: String) -> String {
    let type_name = tname.clone();
    let method_name = name.clone();
    format!("{type_name}$(method)${method_name}")
  }
}
