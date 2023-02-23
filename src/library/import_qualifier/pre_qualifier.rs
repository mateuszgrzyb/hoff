use crate::library::ast::untyped::*;

pub struct ImportPreQualifier {
    fundecls: Vec<FunDecl>,
    structs: Vec<Struct>,
    vals: Vec<ValDecl>,
}

impl ImportPreQualifier {
    pub fn create() -> Self {
        Self {
            fundecls: Vec::new(),
            structs: Vec::new(),
            vals: Vec::new(),
        }
    }

    pub fn pre_qualify(
        &mut self,
        ms: &Vec<Mod>,
    ) -> (Vec<FunDecl>, Vec<Struct>, Vec<ValDecl>) {
        ms.into_iter().for_each(|m| self.pre_qualify_mod(m));
        (
            self.fundecls.clone(),
            self.structs.clone(),
            self.vals.clone(),
        )
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
            Decl::Val(v) => {
                let valdecl = ValDecl {
                    name: v.name.clone(),
                    t: v.t.clone(),
                    inner_vals: self.get_inner_vals(v.expr.clone()),
                };
                self.vals.push(valdecl)
            }
            Decl::Import(_) => {}
        }
    }

    fn get_inner_vals(&mut self, expr: Expr) -> Vec<String> {
        match expr {
            Expr::BinOp(lh, _, rh) => self._map(Vec::from([*lh, *rh])),
            Expr::Lit(_) => Vec::new(),
            Expr::Value(v) => Vec::from([v]),
            Expr::Function(_, _) => Vec::new(),
            Expr::Assign(_, expr) => self.get_inner_vals(*expr),
            Expr::Chain(lh, rh) => self._map(Vec::from([*lh, *rh])),
            Expr::Call(_, args) => self._map(args),
            Expr::If(ie, e1, e2) => self._map(Vec::from([*ie, *e1, *e2])),
            Expr::Attr(_, _, _) => Vec::new(),
            Expr::New(_, args) => self._map(args),
        }
    }

    fn _map(&mut self, es: Vec<Expr>) -> Vec<String> {
        es.into_iter()
            .flat_map(|arg| self.get_inner_vals(arg))
            .collect()
    }
}
