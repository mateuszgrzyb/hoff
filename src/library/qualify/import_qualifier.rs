use std::error::Error;

use crate::library::{
  ast::{
    qualified,
    typed::{
      FunDecl,
      Struct,
      ValDecl,
    },
    untyped,
    Decl,
    Mod,
  },
  qualify::TypedGlobalDecls,
};

type QualifyResult<V> = Result<V, Box<dyn Error>>;

pub struct ImportQualifier<'init> {
  global_decls: &'init TypedGlobalDecls,
  fundecls: Vec<FunDecl>,
  structs: Vec<Struct>,
  vals: Vec<ValDecl>,
}

impl<'init> ImportQualifier<'init> {
  pub fn create(global_decls: &'init TypedGlobalDecls) -> Self {
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
      fundecls: self
        .fundecls
        .clone(),
      structs: self
        .structs
        .clone(),
      vals: self
        .vals
        .clone(),
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
      Decl::Fun(f) => self
        .qualify_fun(f)
        .map(|f| Decl::Fun(f)),
      Decl::Struct(s) => self
        .qualify_struct(s)
        .map(|s| Decl::Struct(s)),
      Decl::Val(v) => self
        .qualify_val(v)
        .map(|v| Decl::Val(v)),
      Decl::Import(i) => self
        .qualify_import(i)
        .map(|i| Decl::Import(i)),
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
      self
        .structs
        .push(s.clone());
      return Ok(qualified::Import::Struct(s));
    };

    if let Some(fd) = self
      .global_decls
      .fundecls
      .iter()
      .find(|fd| fd.name == name)
      .cloned()
    {
      self
        .fundecls
        .push(fd.clone());
      return Ok(qualified::Import::Fun(fd));
    };

    if let Some(v) = self
      .global_decls
      .vals
      .iter()
      .find(|v| v.name == name)
      .cloned()
    {
      self
        .vals
        .push(v.clone());
      return Ok(qualified::Import::Val(v));
    }

    Err(format!("{} cannot be imported", name).into())
  }
}
