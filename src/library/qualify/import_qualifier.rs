use std::sync::{Arc, Mutex};

use crate::library::ast::{qualified, typed, untyped};
use anyhow::{anyhow, bail, Result};
use macros::lock;
use rayon::prelude::*;
pub struct ImportQualifier {
  global_decls: Arc<typed::Decls>,
  decls: Arc<Mutex<typed::Decls>>,
}

pub trait ProcessImportQualifierNode {
  type R;
  fn process(self, ctx: &ImportQualifier) -> Self::R;
}

impl ProcessImportQualifierNode for untyped::Mod {
  type R = Result<qualified::Mod>;

  fn process(self, ctx: &ImportQualifier) -> Self::R {
    let name = self.name;
    let defs = self.defs.process(ctx)?;
    let imports = ctx._get_decls()?;

    Ok(qualified::Mod {
      name,
      defs,
      imports,
    })
  }
}

impl ProcessImportQualifierNode for Vec<untyped::Def> {
  type R = Result<Vec<qualified::Def>>;

  fn process(self, ctx: &ImportQualifier) -> Self::R {
    self
      .into_par_iter()
      .map(|d| d.process(ctx))
      .collect::<Result<Vec<_>, _>>()
  }
}

impl ProcessImportQualifierNode for untyped::Def {
  type R = Result<qualified::Def>;

  fn process(self, ctx: &ImportQualifier) -> Self::R {
    match self {
      untyped::Def::Import(i) => i.process(ctx).map(qualified::Def::Import),
      untyped::Def::Fun(f) => Ok(qualified::Def::Fun(f)),
      untyped::Def::Struct(s) => Ok(qualified::Def::Struct(s)),
      untyped::Def::Val(v) => Ok(qualified::Def::Val(v)),
      untyped::Def::Class(c) => Ok(qualified::Def::Class(c)),
      untyped::Def::Impl(i) => Ok(qualified::Def::Impl(i)),
    }
  }
}

impl ProcessImportQualifierNode for untyped::Import {
  type R = Result<qualified::Import>;

  fn process(self, ctx: &ImportQualifier) -> Self::R {
    let (_, name) = self;

    let Some(d) = ctx
      .global_decls
      .iter()
      .find(|d| d.get_name() == &name)
      .cloned()
    else {
      bail!("{} cannot be imported", name);
    };

    // auto import all impls if class is imported
    if let typed::Decl::Class(c) = &d {
      let class_impls = ctx
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
        ctx._push_decl_if_not_exist(class_impl)?
      }
    };

    ctx._push_decl(d.clone())?;

    let i = match d {
      typed::Decl::Fun(f) => qualified::Import::Fun(f),
      typed::Decl::Struct(s) => qualified::Import::Struct(s),
      typed::Decl::Val(v) => qualified::Import::Val(v),
      typed::Decl::Class(c) => qualified::Import::Class(c),
      typed::Decl::Impl(i) => qualified::Import::Impl(i),
    };

    Ok(i)
  }
}

impl ImportQualifier {
  pub fn create(global_decls: Arc<typed::Decls>) -> Self {
    Self {
      global_decls,
      decls: Arc::new(Mutex::new(Vec::new())),
    }
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
