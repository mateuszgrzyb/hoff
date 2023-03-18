use std::error::Error;

use crate::library::ast::{qualified, typed, untyped, Def, Mod};

type QualifyResult<V> = Result<V, Box<dyn Error>>;

pub struct ImportQualifier<'init> {
  global_decls: &'init typed::Decls,
  decls: typed::Decls,
}

impl<'init> ImportQualifier<'init> {
  pub fn create(global_decls: &'init typed::Decls) -> Self {
    Self {
      global_decls,
      decls: Vec::new(),
    }
  }

  pub fn qualify(&mut self, m: untyped::Mod) -> QualifyResult<qualified::Mod> {
    let name = m.name;
    let decls = self.qualify_decls(m.defs)?;
    let imports = self.decls.clone();

    Ok(Mod {
      name,
      defs: decls,
      imports,
    })
  }

  pub fn qualify_decls(
    &mut self,
    decls: Vec<untyped::Def>,
  ) -> QualifyResult<Vec<qualified::Def>> {
    decls
      .into_iter()
      .map(|d| self.qualify_decl(d))
      .collect::<Result<Vec<_>, _>>()
  }

  fn qualify_decl(
    &mut self,
    d: untyped::Def,
  ) -> QualifyResult<qualified::Def> {
    match d {
      Def::Import(i) => self.qualify_import(i).map(|i| Def::Import(i)),

      Def::Fun(f) => Ok(Def::Fun(f)),
      Def::Struct(s) => Ok(Def::Struct(s)),
      Def::Val(v) => Ok(Def::Val(v)),
      Def::Class(c) => Ok(Def::Class(c)),
      Def::Impl(i) => Ok(Def::Impl(i)),
    }
  }

  fn qualify_import(
    &mut self,
    i: untyped::Import,
  ) -> QualifyResult<qualified::Import> {
    let (_, name) = i;

    let Some(d) = self.global_decls.iter().find(|d| d.get_name() == &name).cloned() else {
      return Err(format!("{} cannot be imported", name).into())
    };

    self.decls.push(d.clone());

    let i = match d {
      typed::Decl::Fun(f) => qualified::Import::Fun(f),
      typed::Decl::Struct(s) => qualified::Import::Struct(s),
      typed::Decl::Val(v) => qualified::Import::Val(v),
      typed::Decl::Class(c) => qualified::Import::Class(c),
    };

    Ok(i)
  }
}
