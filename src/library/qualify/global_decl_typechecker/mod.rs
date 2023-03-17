mod sorter;

use std::{collections::HashMap, error::Error};

use crate::library::{
  ast::{typed, untyped, SimpleType},
  qualify::{
    global_decl_typechecker::sorter::Sorter, TypedGlobalDecls,
    UntypedGlobalDecls,
  },
};

type TypeCheckResult<V> = Result<V, Box<dyn Error>>;

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
    untyped_global_decls: UntypedGlobalDecls,
  ) -> TypeCheckResult<TypedGlobalDecls> {
    self.populate_types();

    let structs = self.check_structs(untyped_global_decls.structs)?;

    let fundecls = self.check_fundecls(untyped_global_decls.fundecls)?;

    let vals = self.check_vals(untyped_global_decls.vals)?;

    Ok(TypedGlobalDecls {
      fundecls,
      structs,
      vals,
    })
  }

  fn populate_types(&mut self) {
    self.types.insert("Int".to_string(), SimpleType::Int);
    self.types.insert("Bool".to_string(), SimpleType::Bool);
    self.types.insert("Float".to_string(), SimpleType::Float);
    self.types.insert("String".to_string(), SimpleType::String);
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

    let result = ss
      .into_iter()
      .map(|s| {
        let ts = self.check_struct(s)?;
        self
          .types
          .insert(ts.name.clone(), SimpleType::Struct(ts.clone()));
        Ok(ts)
      })
      .collect();

    result
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

  fn get_type(&self, t: untyped::Type) -> TypeCheckResult<typed::Type> {
    match t {
      untyped::Type::Simple(s) => {
        let Some(t) = self.types.get(s.as_str()) else {
                    return Err(format!("Unknown type: {s}").into())
                };
        Ok(typed::Type::Simple(t.clone()))
      }
      untyped::Type::Function(ts) => {
        let ts = ts
          .into_iter()
          .map(|t| self.get_type(t))
          .collect::<Result<Vec<_>, _>>()?;
        return Ok(typed::Type::Function(ts));
      }
    }
  }
}
