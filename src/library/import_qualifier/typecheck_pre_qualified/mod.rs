mod sorter;
mod type_sorter;

use crate::library::ast::{typed, untyped, SimpleType};
use crate::library::import_qualifier::typecheck_pre_qualified::type_sorter::{
    get_name_sorter, get_type_sorter,
};
use std::collections::HashMap;
use std::error::Error;

type TypeCheckResult<V> = Result<V, Box<dyn Error>>;

pub struct TypeCheckPreQualified {
    types: HashMap<String, SimpleType>,
}

impl TypeCheckPreQualified {
    pub fn create() -> Self {
        Self {
            types: Default::default(),
        }
    }

    pub fn run(
        &mut self,
        fundecls: Vec<untyped::FunDecl>,
        structs: Vec<untyped::Struct>,
        valdecls: Vec<untyped::ValDecl>,
    ) -> TypeCheckResult<(
        Vec<typed::FunDecl>,
        Vec<typed::Struct>,
        Vec<typed::ValDecl>,
    )> {
        self.populate_types();

        let typed_structs = self.check_structs(structs)?;

        let typed_fundecls = self.check_fundecls(fundecls)?;

        let typed_valdecls = self.check_valdecls(valdecls)?;

        Ok((typed_fundecls, typed_structs, typed_valdecls))
    }

    fn populate_types(&mut self) {
        self.types.insert("Int".to_string(), SimpleType::Int);
        self.types.insert("Bool".to_string(), SimpleType::Bool);
        self.types.insert("Float".to_string(), SimpleType::Float);
        self.types.insert("String".to_string(), SimpleType::String);
    }

    fn check_structs(
        &mut self,
        ss: Vec<untyped::Struct>,
    ) -> TypeCheckResult<Vec<typed::Struct>> {
        let mut type_sorter = get_type_sorter(ss);
        let ss = type_sorter.sort()?;

        let result = ss
            .into_iter()
            .map(|s| {
                let ts = self.check_struct(s)?;
                self.types
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
        let args = s
            .args
            .into_iter()
            .map(|(n, t)| self.get_type(t).map(|t| (n, t)))
            .collect::<TypeCheckResult<_>>()?;

        Ok(typed::Struct { name, args })
    }

    fn check_fundecls(
        &self,
        fds: Vec<untyped::FunDecl>,
    ) -> TypeCheckResult<Vec<typed::FunDecl>> {
        fds.into_iter().map(|fd| self.check_fundecl(fd)).collect()
    }

    fn check_fundecl(
        &self,
        fd: untyped::FunDecl,
    ) -> TypeCheckResult<typed::FunDecl> {
        let name = fd.name;
        let args = fd
            .args
            .into_iter()
            .map(|(n, t)| self.get_type(t).map(|t| (n, t)))
            .collect::<TypeCheckResult<_>>()?;
        let rt = self.get_type(fd.rt)?;

        Ok(typed::FunDecl { name, args, rt })
    }

    fn check_valdecls(
        &self,
        vds: Vec<untyped::ValDecl>,
    ) -> TypeCheckResult<Vec<typed::ValDecl>> {
        let mut name_sorter = get_name_sorter(vds);
        let vds = name_sorter.sort()?;

        vds.into_iter().map(|fd| self.check_valdecl(fd)).collect()
    }

    fn check_valdecl(
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
