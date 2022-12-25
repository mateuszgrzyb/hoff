use crate::ast::{qualified, untyped, Decl, Mod};

type QualifyResult<V> = Result<V, String>;

pub struct ImportQualifier {}

impl ImportQualifier {
    pub fn create() -> Self {
        Self {}
    }

    pub fn qualify(
        &mut self,
        m: untyped::Mod,
    ) -> QualifyResult<qualified::Mod> {
        let name = m.name;
        let decls = m
            .decls
            .into_iter()
            .map(|d| self.qualify_decl(d))
            .collect::<Result<_, String>>()?;

        Ok(Mod { name, decls })
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
        todo!();
    }
}
