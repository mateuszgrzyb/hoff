use std::{collections::HashSet, sync::Arc};

use crate::library::ast::{qualified as to, typed, untyped as from};
use anyhow::{bail, Result};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
pub struct ImportQualifier {
  global_decls: Arc<typed::Decls>,
}

impl ImportQualifier {
  pub fn create(global_decls: Arc<typed::Decls>) -> Self {
    Self { global_decls }
  }

  pub fn qualify(&self, m: from::Mod) -> Result<to::Mod> {
    let name = m.name;
    let (defs, imports) = self._qualify_defs_and_get_decls(m.defs)?;

    Ok(to::Mod {
      name,
      defs,
      imports,
    })
  }

  fn _qualify_defs_and_get_decls(
    &self,
    defs: Vec<from::Def>,
  ) -> Result<(Vec<to::Def>, typed::Decls)> {
    let (defs, imports) = defs
      .into_par_iter()
      .map(|d| self._qualify_def_and_get_decl(d))
      .collect::<Result<(Vec<_>, Vec<_>)>>()?;
    let imports = imports
      .into_par_iter()
      .flat_map(|i| i)
      .collect::<HashSet<_>>();
    let imports = imports.into_iter().collect();
    Ok((defs, imports))
  }

  fn _qualify_def_and_get_decl(
    &self,
    def: from::Def,
  ) -> Result<(to::Def, typed::Decls)> {
    match def {
      from::Def::Import(i) => {
        let (i, decls) = self._qualify_import(i)?;
        Ok((to::Def::Import(i), decls))
      }
      from::Def::Fun(f) => Ok((to::Def::Fun(f), vec![])),
      from::Def::Struct(s) => Ok((to::Def::Struct(s), vec![])),
      from::Def::Val(v) => Ok((to::Def::Val(v), vec![])),
      from::Def::Class(c) => Ok((to::Def::Class(c), vec![])),
      from::Def::Impl(i) => Ok((to::Def::Impl(i), vec![])),
    }
  }

  fn _qualify_import(
    &self,
    i: from::Import,
  ) -> Result<(to::Import, typed::Decls)> {
    let (_, name) = i;

    let Some(d) = self.global_decls.iter().find(|d| d.get_name() == &name).cloned() else {
      bail!("{} cannot be imported", name);
    };

    let mut decls = vec![];

    // auto import all impls if class is imported
    if let typed::Decl::Class(c) = &d {
      let mut class_impls = self
        .global_decls
        .iter()
        .filter_map(|d| match d {
          typed::Decl::Impl(i) => Some(i),
          _ => None,
        })
        .filter(|i| i.class_name == c.name)
        .map(|i| typed::Decl::Impl(i.clone()))
        .collect::<Vec<_>>();

      decls.append(&mut class_impls);
    };

    decls.push(d.clone());

    let i = match d {
      typed::Decl::Fun(f) => to::Import::Fun(f),
      typed::Decl::Struct(s) => to::Import::Struct(s),
      typed::Decl::Val(v) => to::Import::Val(v),
      typed::Decl::Class(c) => to::Import::Class(c),
      typed::Decl::Impl(i) => to::Import::Impl(i),
    };

    Ok((i, decls))
  }

  pub fn qualify_defs(&self, defs: Vec<from::Def>) -> Result<Vec<to::Def>> {
    let (defs, _) = self._qualify_defs_and_get_decls(defs)?;
    Ok(defs)
  }
}
