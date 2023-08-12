mod sorter;

use std::collections::HashMap;

use crate::library::ast::Decl;
use crate::library::{
  ast::{typed, untyped, SimpleType},
  qualify::global_decl_typechecker::sorter::Sorter,
};

use anyhow::{bail, Result};

type TypeCheckResult<V> = Result<V>;

pub struct GlobalDeclTypechecker {
  types: HashMap<String, SimpleType>,
}

impl GlobalDeclTypechecker {
  pub fn create() -> Self {
    Self {
      types: Default::default(),
    }
  }

  pub fn check(
    &mut self,
    untyped_decls: untyped::Decls,
  ) -> TypeCheckResult<typed::Decls> {
    self.populate_types();

    let mut structs = Vec::new();
    let mut classes = Vec::new();
    let mut fundecls = Vec::new();
    let mut vals = Vec::new();

    for d in untyped_decls {
      match d {
        Decl::Struct(s) => structs.push(s),
        Decl::Class(c) => classes.push(c),
        Decl::Fun(f) => fundecls.push(f),
        Decl::Val(v) => vals.push(v),
      }
    }

    let structs = self.check_structs(structs)?;
    let classes = self.check_classes(classes)?;
    let fundecls = self.check_fundecls(fundecls)?;
    let vals = self.check_vals(vals)?;

    let typed_decls = structs
      .into_iter()
      .map(|s| Decl::Struct(s))
      .chain(classes.into_iter().map(|c| Decl::Class(c)))
      .chain(fundecls.into_iter().map(|f| Decl::Fun(f)))
      .chain(vals.into_iter().map(|v| Decl::Val(v)))
      .collect();

    Ok(typed_decls)
  }

  fn populate_types(&mut self) {
    self.types.insert("Int".to_string(), SimpleType::Int);
    self.types.insert("Bool".to_string(), SimpleType::Bool);
    self.types.insert("Float".to_string(), SimpleType::Float);
    self.types.insert("String".to_string(), SimpleType::String);
    self.types.insert("This".to_string(), SimpleType::This);
  }

  fn check_args(
    &self,
    args: Vec<(String, untyped::Type)>,
  ) -> TypeCheckResult<Vec<(String, typed::Type)>> {
    args
      .into_iter()
      .map(|(n, t)| Ok((n, self.get_type(t)?)))
      .collect::<TypeCheckResult<_>>()
  }

  fn check_structs(
    &mut self,
    ss: Vec<untyped::Struct>,
  ) -> TypeCheckResult<Vec<typed::Struct>> {
    let mut type_sorter = Sorter::create(ss);
    let ss = type_sorter.sort()?;

    ss.into_iter()
      .map(|s| {
        let ts = self.check_struct(s)?;
        self
          .types
          .insert(ts.name.clone(), SimpleType::Struct(ts.clone()));
        Ok(ts)
      })
      .collect()
  }

  fn check_struct(
    &self,
    s: untyped::Struct,
  ) -> TypeCheckResult<typed::Struct> {
    let name = s.name;
    let args = self.check_args(s.args)?;

    Ok(typed::Struct { name, args })
  }

  fn check_fundecls(
    &self,
    fds: Vec<untyped::FunSig>,
  ) -> TypeCheckResult<Vec<typed::FunSig>> {
    fds.into_iter().map(|fd| self.check_fundecl(fd)).collect()
  }

  fn check_fundecl(
    &self,
    fd: untyped::FunSig,
  ) -> TypeCheckResult<typed::FunSig> {
    let name = fd.name;
    let args = self.check_args(fd.args)?;
    let rt = self.get_type(fd.rt)?;

    Ok(typed::FunSig { name, args, rt })
  }

  fn check_vals(
    &self,
    vds: Vec<untyped::ValDecl>,
  ) -> TypeCheckResult<Vec<typed::ValDecl>> {
    let mut name_sorter = Sorter::create(vds);
    let vds = name_sorter.sort()?;

    vds.into_iter().map(|fd| self.check_val(fd)).collect()
  }

  fn check_val(
    &self,
    vd: untyped::ValDecl,
  ) -> TypeCheckResult<typed::ValDecl> {
    let name = vd.name;
    let t = self.get_type(vd.t)?;
    let inner_vals = vd.inner_vals;

    Ok(typed::ValDecl {
      name,
      t,
      inner_vals,
    })
  }

  fn check_classes(
    &self,
    cs: Vec<untyped::Class>,
  ) -> TypeCheckResult<Vec<typed::Class>> {
    cs.into_iter().map(|c| self.check_class(c)).collect()
  }

  fn check_class(&self, c: untyped::Class) -> TypeCheckResult<typed::Class> {
    let name = c.name;
    let methods = c
      .methods
      .into_iter()
      .map(|m| self.check_fundecl(m))
      .collect::<Result<_, _>>()?;

    Ok(typed::Class { name, methods })
  }

  fn get_type(&self, t: untyped::Type) -> TypeCheckResult<typed::Type> {
    match t {
      untyped::Type::Simple(s) => {
        let Some(t) = self.types.get(s.as_str()) else {
          bail!("Unknown type: {s}")
        };
        Ok(typed::Type::Simple(t.clone()))
      }
      untyped::Type::Function(ts) => {
        let ts = ts
          .into_iter()
          .map(|t| self.get_type(t))
          .collect::<Result<_, _>>()?;
        Ok(typed::Type::Function(ts))
      }
    }
  }
}
