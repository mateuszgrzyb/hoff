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
  BinOp(Box<BinOp<T, C, S, TN>>),
  Lit(Lit),
  Value(Value),
  Function(Box<Function<T, C, S, TN>>),
  Assign(Box<Assign<T, C, S, TN>>),
  Chain(Box<Chain<T, C, S, TN>>),
  Call(Call<T, C, S, TN>),
  If(Box<If<T, C, S, TN>>),
  Attr(Attr<S>),
  New(New<T, C, S, TN>),
  StringTemplate(StringTemplate),
  MethodCall(Box<MethodCall<T, C, S, TN>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinOp<T, C, S, TN> {
  pub lh: Expr<T, C, S, TN>,
  pub op: Op,
  pub rh: Expr<T, C, S, TN>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Value {
  pub name: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function<T, C, S, TN> {
  pub f: Fun<T, C, S, TN>,
  pub closure: C,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assign<T, C, S, TN> {
  pub name: String,
  pub type_: Type<T>,
  pub expr: Expr<T, C, S, TN>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Chain<T, C, S, TN> {
  pub e1: Expr<T, C, S, TN>,
  pub e2: Expr<T, C, S, TN>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call<T, C, S, TN> {
  pub name: String,
  pub args: Vec<Expr<T, C, S, TN>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If<T, C, S, TN> {
  pub if_: Expr<T, C, S, TN>,
  pub then: Expr<T, C, S, TN>,
  pub else_: Expr<T, C, S, TN>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Attr<S> {
  pub name: String,
  pub struct_: S,
  pub attr: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct New<T, C, S, TN> {
  pub name: String,
  pub args: Vec<Expr<T, C, S, TN>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringTemplate {
  pub string: String,
  pub args: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MethodCall<T, C, S, TN> {
  pub this: Expr<T, C, S, TN>,
  pub typename: TN,
  pub methodname: String,
  pub args: Vec<Expr<T, C, S, TN>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunArg<T> {
  pub name: String,
  pub type_: Type<T>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunSig<T> {
  pub name: String,
  pub args: Vec<FunArg<T>>,
  pub rt: Type<T>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fun<T, C, S, TN> {
  pub sig: FunSig<T>,
  pub body: Expr<T, C, S, TN>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StructArg<T> {
  pub name: String,
  pub type_: Type<T>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Struct<T> {
  pub name: String,
  pub args: Vec<StructArg<T>>,
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
pub struct ImplDecl<T> {
  pub class_name: String,
  pub t: T,
  pub impls: Vec<FunSig<T>>,
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
  Function(Vec<Self>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum SimpleType {
  Int,
  Bool,
  Float,
  String,
  Struct(Struct<Self>),
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

pub type Closure = Vec<FunArg<SimpleType>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Decl<T> {
  Fun(FunSig<T>),
  Struct(Struct<T>),
  Val(ValDecl<T>),
  Class(Class<T>),
  Impl(ImplDecl<T>),
}

impl<T> Decl<T> {
  pub fn get_name(&self) -> &String {
    match self {
      Decl::Fun(f) => &f.name,
      Decl::Struct(s) => &s.name,
      Decl::Val(v) => &v.name,
      Decl::Class(c) => &c.name,
      Decl::Impl(i) => &i.class_name,
    }
  }
}

pub type Decls<T> = Vec<Decl<T>>;

macro_rules! publish_type {
  ($T: ident) => {
    pub type $T = super::$T;
  };
  ($T: ident, [$($G: tt),*]) => {
    pub type $T = super::$T<$($G),*>;
  };
}

macro_rules! ast {
  (
    T = $T:ty,
    C = $C:ty,
    S = $S:ty,
    TN = $TN:ty,
    I = $I:ty,
    IS = $IS:ty,
  ) => {
    publish_type!(Op);
    publish_type!(Lit);
    publish_type!(Expr, [$T, $C, $S, $TN]);
    publish_type!(BinOp, [$T, $C, $S, $TN]);
    publish_type!(Function, [$T, $C, $S, $TN]);
    publish_type!(Assign, [$T, $C, $S, $TN]);
    publish_type!(Chain, [$T, $C, $S, $TN]);
    publish_type!(Call, [$T, $C, $S, $TN]);
    publish_type!(If, [$T, $C, $S, $TN]);
    publish_type!(New, [$T, $C, $S, $TN]);
    publish_type!(Attr, [$S]);
    publish_type!(Value);
    publish_type!(StringTemplate);
    publish_type!(MethodCall, [$T, $C, $S, $TN]);
    publish_type!(FunArg, [$T]);
    publish_type!(FunSig, [$T]);
    publish_type!(Fun, [$T, $C, $S, $TN]);
    publish_type!(StructArg, [$T]);
    publish_type!(Struct, [$T]);
    publish_type!(ValDecl, [$T]);
    publish_type!(Val, [$T, $C, $S, $TN]);
    publish_type!(Class, [$T]);
    publish_type!(ImplDecl, [$T]);
    publish_type!(Impl, [$T, $C, $S, $TN]);
    pub type Import = $I;
    #[allow(dead_code)]
    pub type Imports = $IS;
    publish_type!(Def, [$T, $C, $S, $TN, $I]);
    publish_type!(Mod, [$T, $C, $S, $TN, $I, $IS]);
    publish_type!(Repl, [$T, $C, $S, $TN, $I]);
    publish_type!(Type, [$T]);
    publish_type!(SimpleType);
    publish_type!(Closure);
    publish_type!(Decl, [$T]);
    publish_type!(Decls, [$T]);
  };
}

#[allow(unused_imports, dead_code)]
pub mod untyped {
  use crate::library::qualify::Nameable;
  use crate::nameable;

  ast!(
    T = String,
    C = (),
    S = (),
    TN = (),
    I = (Vec<String>, String),
    IS = (),
  );

  nameable! {Mod, Struct, ValDecl}
}

#[allow(unused_imports, dead_code)]
pub mod qualified {
  use crate::library::qualify::Nameable;
  use crate::nameable;

  ast!(
    T = String,
    C = (),
    S = (),
    TN = (),
    I = super::Decl<super::SimpleType>,
    IS = super::Decls<super::SimpleType>,
  );

  nameable! {Mod}
}

#[allow(unused_imports, dead_code)]
pub mod typed {
  use crate::library::qualify::Nameable;
  use crate::nameable;

  ast!(
    T = super::SimpleType,
    C = super::Closure,
    S = super::Struct<super::SimpleType>,
    TN = String,
    I = super::Decl<super::SimpleType>,
    IS = super::Decls<super::SimpleType>,
  );

  nameable! {Mod}
}
