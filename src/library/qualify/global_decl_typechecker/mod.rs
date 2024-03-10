mod sorter;

use std::collections::HashMap;

use crate::library::ast::Decl;
use crate::library::{
  ast::{typed, untyped, SimpleType},
  qualify::global_decl_typechecker::sorter::Sorter,
  typecheck::instantiate::Instantiate,
};

use macros::define_map_result;

use self::sorter::SorterError;

pub struct GlobalDeclTypechecker {
  types: HashMap<String, SimpleType>,
}

#[derive(Debug, Clone)]
pub enum GdtError {
  UnknownType(String),
  NameSorterError(SorterError),
  TypeSorterError(SorterError),
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
  ) -> Result<typed::Decls, GdtError> {
    self.populate_types();

    let mut structs = Vec::new();
    let mut classes = Vec::new();
    let mut fundecls = Vec::new();
    let mut vals = Vec::new();
    let mut impls = Vec::new();

    for d in untyped_decls {
      match d {
        Decl::Struct(s) => structs.push(s),
        Decl::Class(c) => classes.push(c),
        Decl::Fun(f) => fundecls.push(f),
        Decl::Val(v) => vals.push(v),
        Decl::Impl(i) => impls.push(i),
      }
    }

    let structs = self.check_structs(structs)?;
    let classes = self.check_classes(classes)?;
    let fundecls = self.check_funsigs(fundecls)?;
    let vals = self.check_vals(vals)?;
    let impls = self.check_impls(impls)?;

    let typed_decls = structs
      .into_iter()
      .map(Decl::Struct)
      .chain(classes.into_iter().map(Decl::Class))
      .chain(fundecls.into_iter().map(Decl::Fun))
      .chain(vals.into_iter().map(Decl::Val))
      .chain(impls.into_iter().map(Decl::Impl))
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

  fn check_funargs(
    &self,
    args: Vec<untyped::FunArg>,
  ) -> Result<Vec<typed::FunArg>, GdtError> {
    args
      .into_iter()
      .map(|arg| {
        Ok(typed::FunArg {
          name: arg.name,
          type_: self.get_type(arg.type_)?,
        })
      })
      .collect::<Result<_, _>>()
  }

  fn check_structargs(
    &self,
    args: Vec<untyped::StructArg>,
  ) -> Result<Vec<typed::StructArg>, GdtError> {
    args
      .into_iter()
      .map(|arg| {
        Ok(typed::StructArg {
          name: arg.name,
          type_: self.get_type(arg.type_)?,
        })
      })
      .collect::<Result<_, _>>()
  }

  fn check_structs(
    &mut self,
    ss: Vec<untyped::StructDef>,
  ) -> Result<Vec<typed::StructDef>, GdtError> {
    let mut type_sorter = Sorter::create(ss);
    let ss = type_sorter.sort().map_err(GdtError::TypeSorterError)?;

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
    s: untyped::StructDef,
  ) -> Result<typed::StructDef, GdtError> {
    let name = s.name;
    let args = self.check_structargs(s.args)?;

    Ok(typed::StructDef { name, args })
  }

  define_map_result!(check_funsig, GdtError, untyped::FunSig, typed::FunSig);

  fn check_funsig(
    &self,
    fd: untyped::FunSig,
  ) -> Result<typed::FunSig, GdtError> {
    let name = fd.name;
    let args = self.check_funargs(fd.args)?;
    let rt = self.get_type(fd.rt)?;

    Ok(typed::FunSig { name, args, rt })
  }

  fn check_vals(
    &self,
    vds: Vec<untyped::ValDecl>,
  ) -> Result<Vec<typed::ValDecl>, GdtError> {
    let mut name_sorter = Sorter::create(vds);
    let vds = name_sorter.sort().map_err(GdtError::NameSorterError)?;

    vds.into_iter().map(|fd| self.check_val(fd)).collect()
  }

  fn check_val(
    &self,
    vd: untyped::ValDecl,
  ) -> Result<typed::ValDecl, GdtError> {
    let name = vd.name;
    let t = self.get_type(vd.t)?;
    let inner_vals = vd.inner_vals;

    Ok(typed::ValDecl {
      name,
      t,
      inner_vals,
    })
  }

  define_map_result!(
    check_class,
    GdtError,
    untyped::ClassDef,
    typed::ClassDef
  );

  fn check_class(
    &self,
    c: untyped::ClassDef,
  ) -> Result<typed::ClassDef, GdtError> {
    let name = c.name;
    let methods = c
      .methods
      .into_iter()
      .map(|m| self.check_funsig(m))
      .collect::<Result<_, _>>()?;

    Ok(typed::ClassDef { name, methods })
  }

  define_map_result!(check_impl, GdtError, untyped::ImplDecl, typed::ImplDecl);

  fn check_impl(
    &self,
    i: untyped::ImplDecl,
  ) -> Result<typed::ImplDecl, GdtError> {
    let class_name = i.class_name.clone();
    let t = self.get_simple_type(i.t.clone())?;
    let impls = self.check_funsigs(i.impls)?;

    Ok(
      typed::ImplDecl {
        class_name,
        t,
        impls,
      }
      .instantiate(),
    )
  }

  fn get_type(&self, t: untyped::Type) -> Result<typed::Type, GdtError> {
    match t {
      untyped::Type::Simple(s) => {
        Ok(typed::Type::Simple(self.get_simple_type(s)?))
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

  fn get_simple_type(&self, s: String) -> Result<typed::SimpleType, GdtError> {
    let Some(t) = self.types.get(s.as_str()) else {
      return Err(GdtError::UnknownType(s));
    };
    Ok(t.clone())
  }
}
