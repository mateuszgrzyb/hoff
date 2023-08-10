use std::rc::Rc;

use crate::library::{
  ast::{
    qualified,
    typed::{FunDecl, Struct, ValDecl},
    untyped, Decl, Mod,
  },
  qualify::TypedGlobalDecls,
};
use anyhow::anyhow;
use anyhow::Result;

type QualifyResult<V> = Result<V>;

pub struct ImportQualifier {
  global_decls: Rc<TypedGlobalDecls>,
  fundecls: Vec<FunDecl>,
  structs: Vec<Struct>,
  vals: Vec<ValDecl>,
}

impl ImportQualifier {
  pub fn create(global_decls: Rc<TypedGlobalDecls>) -> Self {
    Self {
      global_decls,
      fundecls: Vec::new(),
      structs: Vec::new(),
      vals: Vec::new(),
    }
  }

  pub fn qualify(&mut self, m: untyped::Mod) -> QualifyResult<qualified::Mod> {
    let name = m.name;
    let decls = self.qualify_decls(m.decls)?;
    let imports = qualified::Imports {
      fundecls: self.fundecls.clone(),
      structs: self.structs.clone(),
      vals: self.vals.clone(),
    };

    Ok(Mod {
      name,
      decls,
      imports,
    })
  }

  pub fn qualify_decls(
    &mut self,
    decls: Vec<untyped::Decl>,
  ) -> QualifyResult<Vec<qualified::Decl>> {
    decls
      .into_iter()
      .map(|d| self.qualify_decl(d))
      .collect::<Result<Vec<_>, _>>()
  }

  fn qualify_decl(
    &mut self,
    d: untyped::Decl,
  ) -> QualifyResult<qualified::Decl> {
    match d {
      Decl::Fun(f) => self.qualify_fun(f).map(Decl::Fun),
      Decl::Struct(s) => self.qualify_struct(s).map(Decl::Struct),
      Decl::Val(v) => self.qualify_val(v).map(Decl::Val),
      Decl::Import(i) => self.qualify_import(i).map(Decl::Import),
    }
  }

  fn qualify_fun(&mut self, f: untyped::Fun) -> QualifyResult<qualified::Fun> {
    Ok(f)
  }

  fn qualify_struct(
    &mut self,
    s: untyped::Struct,
  ) -> QualifyResult<qualified::Struct> {
    Ok(s)
  }

  fn qualify_val(&mut self, v: untyped::Val) -> QualifyResult<qualified::Val> {
    Ok(v)
  }

  fn qualify_import(
    &mut self,
    i: untyped::Import,
  ) -> QualifyResult<qualified::Import> {
    let (_, name) = i;

    if let Some(s) = self
      .global_decls
      .structs
      .iter()
      .find(|s| s.name == name)
      .cloned()
    {
      self.structs.push(s.clone());
      return Ok(qualified::Import::Struct(s));
    };

    if let Some(fd) = self
      .global_decls
      .fundecls
      .iter()
      .find(|fd| fd.name == name)
      .cloned()
    {
      self.fundecls.push(fd.clone());
      return Ok(qualified::Import::Fun(fd));
    };

    if let Some(v) = self
      .global_decls
      .vals
      .iter()
      .find(|v| v.name == name)
      .cloned()
    {
      self.vals.push(v.clone());
      return Ok(qualified::Import::Val(v));
    }

    Err(anyhow!(format!("{} cannot be imported", name)))
  }
}
