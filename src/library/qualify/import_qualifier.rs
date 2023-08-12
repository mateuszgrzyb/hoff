use std::rc::Rc;

use crate::library::ast::{qualified, typed, untyped};
use anyhow::bail;
use anyhow::Result;

type QualifyResult<V> = Result<V>;
pub struct ImportQualifier {
  global_decls: Rc<typed::Decls>,
  decls: typed::Decls,
}

impl ImportQualifier {
  pub fn create(global_decls: Rc<typed::Decls>) -> Self {
    Self {
      global_decls,
      decls: Vec::new(),
    }
  }

  pub fn qualify(&mut self, m: untyped::Mod) -> QualifyResult<qualified::Mod> {
    let name = m.name;
    let decls = self.qualify_decls(m.defs)?;
    let imports = self.decls.clone();

    Ok(qualified::Mod {
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
      untyped::Def::Import(i) => {
        self.qualify_import(i).map(|i| qualified::Def::Import(i))
      }
      untyped::Def::Fun(f) => Ok(qualified::Def::Fun(f)),
      untyped::Def::Struct(s) => Ok(qualified::Def::Struct(s)),
      untyped::Def::Val(v) => Ok(qualified::Def::Val(v)),
      untyped::Def::Class(c) => Ok(qualified::Def::Class(c)),
      untyped::Def::Impl(i) => Ok(qualified::Def::Impl(i)),
    }
  }

  fn qualify_import(
    &mut self,
    i: untyped::Import,
  ) -> QualifyResult<qualified::Import> {
    let (_, name) = i;

    let Some(d) = self.global_decls.iter().find(|d| d.get_name() == &name).cloned() else {
      bail!("{} cannot be imported", name);
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
