use std::sync::{Arc, Mutex};

use crate::library::ast::{qualified, typed, untyped};
use anyhow::{anyhow, bail, Result};
use macros::lock;
use rayon::prelude::*;
pub struct ImportQualifier {
  global_decls: Arc<typed::Decls>,
  decls: Arc<Mutex<typed::Decls>>,
}

impl ImportQualifier {
  pub fn create(global_decls: Arc<typed::Decls>) -> Self {
    Self {
      global_decls,
      decls: Arc::new(Mutex::new(Vec::new())),
    }
  }

  pub fn qualify(&self, m: untyped::Mod) -> Result<qualified::Mod> {
    let name = m.name;
    let defs = self.qualify_defs(m.defs)?;
    let imports = self._get_decls()?;

    Ok(qualified::Mod {
      name,
      defs,
      imports,
    })
  }

  pub fn qualify_defs(&self, defs: Vec<untyped::Def>) -> Result<Vec<qualified::Def>> {
    defs
      .into_par_iter()
      .map(|d| self.qualify_def(d))
      .collect::<Result<Vec<_>, _>>()
  }

  fn qualify_def(&self, d: untyped::Def) -> Result<qualified::Def> {
    match d {
      untyped::Def::Import(i) => self.qualify_import(i).map(qualified::Def::Import),
      untyped::Def::Fun(f) => Ok(qualified::Def::Fun(f)),
      untyped::Def::Struct(s) => Ok(qualified::Def::Struct(s)),
      untyped::Def::Val(v) => Ok(qualified::Def::Val(v)),
      untyped::Def::Class(c) => Ok(qualified::Def::Class(c)),
      untyped::Def::Impl(i) => Ok(qualified::Def::Impl(i)),
    }
  }

  fn qualify_import(&self, i: untyped::Import) -> Result<qualified::Import> {
    let (_, name) = i;

    let Some(d) = self
      .global_decls
      .iter()
      .find(|d| d.get_name() == &name)
      .cloned()
    else {
      bail!("{} cannot be imported", name);
    };

    // auto import all impls if class is imported
    if let typed::Decl::Class(c) = &d {
      let class_impls = self
        .global_decls
        .iter()
        .filter_map(|d| match d {
          typed::Decl::Impl(i) => Some(i),
          _ => None,
        })
        .filter(|i| i.class_name == c.name)
        .map(|i| typed::Decl::Impl(i.clone()))
        .collect::<Vec<_>>();
      // TODO: OPTIMIZE PLZ, WHAT EVEN IS THIS YOU LAZY !!!
      for class_impl in class_impls {
        self._push_decl_if_not_exist(class_impl)?
      }
    };

    self._push_decl(d.clone())?;

    let i = match d {
      typed::Decl::Fun(f) => qualified::Import::Fun(f),
      typed::Decl::Struct(s) => qualified::Import::Struct(s),
      typed::Decl::Val(v) => qualified::Import::Val(v),
      typed::Decl::Class(c) => qualified::Import::Class(c),
      typed::Decl::Impl(i) => qualified::Import::Impl(i),
    };

    Ok(i)
  }

  fn _push_decl_if_not_exist(&self, d: typed::Decl) -> Result<()> {
    lock!(mut decls);
    if !decls.contains(&d) {
      decls.push(d);
    };

    Ok(())
  }

  fn _push_decl(&self, d: typed::Decl) -> Result<()> {
    lock!(mut decls);
    decls.push(d);
    Ok(())
  }

  fn _get_decls(&self) -> Result<typed::Decls> {
    lock!(decls);
    Ok(decls.clone())
  }
}
