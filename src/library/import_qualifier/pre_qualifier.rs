use crate::library::ast::untyped::*;

pub struct ImportPreQualifier {
    fundecls: Vec<FunDecl>,
    structs: Vec<Struct>,
}

impl ImportPreQualifier {
    pub fn create() -> Self {
        Self {
            fundecls: Vec::new(),
            structs: Vec::new(),
        }
    }

    pub fn pre_qualify(
        &mut self,
        ms: &Vec<Mod>,
    ) -> (Vec<FunDecl>, Vec<Struct>) {
        ms.into_iter().for_each(|m| self.pre_qualify_mod(m));
        (self.fundecls.clone(), self.structs.clone())
    }

    fn pre_qualify_mod(&mut self, m: &Mod) {
        for d in &m.decls {
            self.pre_qualify_decl(d)
        }
    }

    fn pre_qualify_decl(&mut self, d: &Decl) {
        match d {
            Decl::Fun(f) => {
                let fundecl = FunDecl {
                    name: f.name.clone(),
                    args: f.args.clone(),
                    rt: f.rt.clone(),
                };
                self.fundecls.push(fundecl)
            }
            Decl::Struct(s) => self.structs.push(s.clone()),
            Decl::Import(_) => {}
        }
    }
}
