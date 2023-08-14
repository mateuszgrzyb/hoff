use lazy_static::lazy_static;
use regex::Regex;

use crate::library::ast::{
  typed::{FunSig, Impl, ImplDecl},
  SimpleType,
};

lazy_static! {
  pub static ref STRING_TEMPLATE_RE: Regex =
    Regex::new(r"\{\{|\}\}|\{([^}]+)\}").unwrap();
}

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

impl MethodNamer for Impl {
  fn get_method_name(&self, name: &str) -> String {
    self.t.get_method_name(name)
  }
}

impl MethodNamer for ImplDecl {
  fn get_method_name(&self, name: &str) -> String {
    self.t.get_method_name(name)
  }
}
