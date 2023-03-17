#[macro_export]
macro_rules! insert_types {
  {} => {
    pub use super::*;

    pub type Expr = super::Expr<T, C, S>;
    pub type Fun = super::Fun<T, C, S>;
    pub type FunSig = super::FunSig<T>;
    pub type Struct = super::Struct<T>;
    pub type Val = super::Val<T, C, S>;
    pub type ValDecl = super::ValDecl<T>;
    pub type Import = I;
    pub type Decl = super::Decl<T, C, S, I>;
    pub type Mod = super::Mod<T, C, S, I, IS>;
    pub type Repl = super::Repl<T, C, S, I>;
    pub type Type = super::Type<T>;
  };
}

#[macro_export]
macro_rules! nameable {
  ( $( $T:ty ),* ) => {
    use $crate::library::qualify::Nameable;

    $(
      impl Nameable for $T {
        fn get_name(&self) -> String {
          self.name.clone()
        }
      }
    )*
  };
}
