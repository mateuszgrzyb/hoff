use crate::library::ast::typed::{FunDecl, Struct};
use crate::library::ast::{qualified, untyped, Decl, Mod};

type QualifyResult<V> = Result<V, String>;

pub struct ImportQualifier<'init> {
    global_fundecls: &'init Vec<FunDecl>,
    global_structs: &'init Vec<Struct>,
    fundecls: Vec<FunDecl>,
    structs: Vec<Struct>,
}

impl<'init> ImportQualifier<'init> {
    pub fn create(
        fundecls: &'init Vec<FunDecl>,
        structs: &'init Vec<Struct>,
    ) -> Self {
        Self {
            global_fundecls: fundecls,
            global_structs: structs,
            fundecls: Vec::new(),
            structs: Vec::new(),
        }
    }

    pub fn qualify(
        &mut self,
        m: untyped::Mod,
    ) -> QualifyResult<qualified::Mod> {
        let name = m.name;
        let decls = self.qualify_decls(m.decls)?;
        let imports = qualified::Imports {
            fundecls: self.fundecls.clone(),
            structs: self.structs.clone(),
        };

        Ok(Mod {
            name,
            decls,
            imports,
        })
    }

    pub fn qualify_decls(
        &mut self,
        decls: Vec<untyped::Decl>,
    ) -> QualifyResult<Vec<qualified::Decl>> {
        decls
            .into_iter()
            .map(|d| self.qualify_decl(d))
            .collect::<Result<Vec<_>, String>>()
    }

    fn qualify_decl(
        &mut self,
        d: untyped::Decl,
    ) -> QualifyResult<qualified::Decl> {
        match d {
            Decl::Fun(f) => self.qualify_fun(f).map(|f| Decl::Fun(f)),
            Decl::Struct(s) => self.qualify_struct(s).map(|s| Decl::Struct(s)),
            Decl::Import(i) => self.qualify_import(i).map(|i| Decl::Import(i)),
        }
    }

    fn qualify_fun(
        &mut self,
        f: untyped::Fun,
    ) -> QualifyResult<qualified::Fun> {
        Ok(f)
    }

    fn qualify_struct(
        &mut self,
        s: untyped::Struct,
    ) -> QualifyResult<qualified::Struct> {
        Ok(s)
    }

    fn qualify_import(
        &mut self,
        i: untyped::Import,
    ) -> QualifyResult<qualified::Import> {
        let (_, name) = i;

        if let Some(s) = self
            .global_structs
            .into_iter()
            .find(|s| s.name == name)
            .cloned()
        {
            self.structs.push(s.clone());
            return Ok(qualified::Import::Struct(s));
        };

        if let Some(fd) = self
            .global_fundecls
            .into_iter()
            .find(|fd| fd.name == name)
            .cloned()
        {
            self.fundecls.push(fd.clone());
            return Ok(qualified::Import::Fun(fd));
        };

        Err(format!("{} cannot be imported", name))
    }
}
