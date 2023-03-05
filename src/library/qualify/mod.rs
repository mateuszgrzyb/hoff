mod global_decl_collector;
mod global_decl_typechecker;
mod import_qualifier;

pub use global_decl_collector::GlobalDeclCollector;
pub use global_decl_typechecker::GlobalDeclTypechecker;
pub use import_qualifier::ImportQualifier;

use crate::library::ast::{typed, untyped};

pub trait Nameable {
  fn get_name(&self) -> String;
}

pub struct UntypedGlobalDecls {
  fundecls: Vec<untyped::FunDecl>,
  structs: Vec<untyped::Struct>,
  vals: Vec<untyped::ValDecl>,
}

pub struct TypedGlobalDecls {
  fundecls: Vec<typed::FunDecl>,
  structs: Vec<typed::Struct>,
  vals: Vec<typed::ValDecl>,
}

impl TypedGlobalDecls {
  pub fn create() -> Self {
    Self {
      fundecls: Vec::new(),
      structs: Vec::new(),
      vals: Vec::new(),
    }
  }
}
