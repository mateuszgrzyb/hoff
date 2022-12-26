mod type_sorter;

use crate::library::ast::{typed, untyped, SimpleType};
use std::collections::HashMap;
use std::error::Error;
use type_sorter::TypeSorter;

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
    ) -> TypeCheckResult<(Vec<typed::FunDecl>, Vec<typed::Struct>)> {
        self.populate_types();

        let typed_structs = self.check_structs(structs)?;

        let typed_fundecls = self.check_fundecls(fundecls)?;

        Ok((typed_fundecls, typed_structs))
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
        let mut type_sorter = TypeSorter::create(ss);
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

    fn get_type(&self, t: untyped::Type) -> TypeCheckResult<typed::Type> {
        match t {
            untyped::Type::Simple(s) => {
                let Some(t) = self.types.get(s.as_str()) else {
                    return Err(format!("Unknown type: {s}").into())
                };
                Ok(typed::Type::Simple((*t).clone()))
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
