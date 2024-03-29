use once_cell::sync::Lazy;
use regex::Regex;

use crate::library::ast::{
  typed::{FunSig, ImplDecl, ImplDef},
  SimpleType,
};

pub static STRING_TEMPLATE_RE: Lazy<Regex> =
  Lazy::new(|| Regex::new(r"\{\{|\}\}|\{([^}]+)\}").unwrap());

fn _format_method_name(type_name: &str, method_name: &str) -> String {
  format!("{}$(method)${}", type_name, method_name)
}

pub trait MethodNamer {
  fn get_method_name(&self, name: &str) -> String;
  fn get_method_name_by_sig(&self, fs: &FunSig) -> String {
    self.get_method_name(&fs.name)
  }
}

impl MethodNamer for String {
  fn get_method_name(&self, name: &str) -> String {
    let type_name = self;
    let method_name = name;
    _format_method_name(type_name, method_name)
  }
}

impl MethodNamer for SimpleType {
  fn get_method_name(&self, name: &str) -> String {
    let type_name = self.get_name();
    let method_name = name;
    _format_method_name(&type_name, method_name)
  }
}

impl MethodNamer for ImplDef {
  fn get_method_name(&self, name: &str) -> String {
    self.t.get_method_name(name)
  }
}

impl MethodNamer for ImplDecl {
  fn get_method_name(&self, name: &str) -> String {
    self.t.get_method_name(name)
  }
}
