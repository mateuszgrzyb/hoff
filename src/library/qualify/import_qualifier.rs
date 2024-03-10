use std::sync::Arc;

use crate::library::ast::{qualified, typed, untyped};
use rayon::prelude::*;
pub struct ImportQualifier {
  global_decls: Arc<typed::Decls>,
}

#[derive(Debug, Clone)]
pub enum ImportQualifierError {
  CannotImport(String),
}

pub trait ProcessImportQualifierNode {
  type R;
  fn process(self, ctx: &ImportQualifier) -> Self::R;
}

impl ProcessImportQualifierNode for untyped::Mod {
  type R = Result<qualified::Mod, ImportQualifierError>;

  fn process(self, ctx: &ImportQualifier) -> Self::R {
    let name = self.name;

    let (defs, imports) = self.defs.process(ctx)?;

    Ok(qualified::Mod {
      name,
      defs,
      imports,
    })
  }
}

impl ProcessImportQualifierNode for Vec<untyped::Def> {
  type R = Result<(Vec<qualified::Def>, typed::Decls), ImportQualifierError>;

  fn process(self, ctx: &ImportQualifier) -> Self::R {
    let (defs, decls) = self
      .into_par_iter()
      .map(|d| d.process(ctx))
      .collect::<Result<Vec<_>, _>>()?
      .into_iter()
      .unzip::<_, _, Vec<_>, Vec<_>>();

    Ok((defs, decls.into_iter().flatten().collect()))
  }
}

impl ProcessImportQualifierNode for untyped::Def {
  type R = Result<(qualified::Def, typed::Decls), ImportQualifierError>;

  fn process(self, ctx: &ImportQualifier) -> Self::R {
    match self {
      untyped::Def::Import(i) => {
        i.process(ctx).map(|(a, b)| (qualified::Def::Import(a), b))
      }
      untyped::Def::Fun(f) => Ok((qualified::Def::Fun(f), Vec::new())),
      untyped::Def::Struct(s) => Ok((qualified::Def::Struct(s), Vec::new())),
      untyped::Def::Val(v) => Ok((qualified::Def::Val(v), Vec::new())),
      untyped::Def::Class(c) => Ok((qualified::Def::Class(c), Vec::new())),
      untyped::Def::Impl(i) => Ok((qualified::Def::Impl(i), Vec::new())),
    }
  }
}

impl ProcessImportQualifierNode for untyped::Import {
  type R = Result<(qualified::Import, typed::Decls), ImportQualifierError>;

  fn process(self, ctx: &ImportQualifier) -> Self::R {
    let mut decls = Vec::new();

    let (_, name) = self;

    let Some(d) = ctx
      .global_decls
      .iter()
      .find(|d| d.get_name() == &name)
      .cloned()
    else {
      return Err(ImportQualifierError::CannotImport(name));
    };

    // auto import all impls if class is imported
    if let typed::Decl::Class(c) = &d {
      let mut class_impls = ctx
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
      typed::Decl::Fun(f) => qualified::Import::Fun(f),
      typed::Decl::Struct(s) => qualified::Import::Struct(s),
      typed::Decl::Val(v) => qualified::Import::Val(v),
      typed::Decl::Class(c) => qualified::Import::Class(c),
      typed::Decl::Impl(i) => qualified::Import::Impl(i),
    };

    Ok((i, decls))
  }
}

impl ImportQualifier {
  pub fn create(global_decls: Arc<typed::Decls>) -> Self {
    Self { global_decls }
  }
}
