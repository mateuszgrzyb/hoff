mod global_decl_collector;
mod global_decl_typechecker;
mod import_qualifier;

pub use global_decl_collector::GlobalDeclCollector;
pub use global_decl_typechecker::GlobalDeclTypechecker;
pub use import_qualifier::ImportQualifier;

pub trait Nameable {
  fn get_name(&self) -> String;
}
