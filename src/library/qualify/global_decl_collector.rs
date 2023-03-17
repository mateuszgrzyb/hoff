use crate::library::{ast::untyped::*, qualify::UntypedGlobalDecls};

pub struct GlobalDeclCollector {
  fundecls: Vec<FunSig>,
  structs: Vec<Struct>,
  vals: Vec<ValDecl>,
}

impl GlobalDeclCollector {
  pub fn create() -> Self {
    Self {
      fundecls: Vec::new(),
      structs: Vec::new(),
      vals: Vec::new(),
    }
  }

  pub fn collect(&mut self, ms: &Vec<Mod>) -> UntypedGlobalDecls {
    ms.into_iter().for_each(|m| self.process_mod(m));

    UntypedGlobalDecls {
      fundecls: self.fundecls.clone(),
      structs: self.structs.clone(),
      vals: self.vals.clone(),
    }
  }

  fn process_mod(&mut self, m: &Mod) {
    for d in &m.decls {
      self.process_decl(d)
    }
  }

  fn process_decl(&mut self, d: &Decl) {
    match d {
      Decl::Fun(f) => {
        let fundecl = FunSig {
          name: f.sig.name.clone(),
          args: f.sig.args.clone(),
          rt: f.sig.rt.clone(),
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
      Expr::StringTemplate(_, args) => args,
    }
  }

  fn _map(&mut self, es: Vec<Expr>) -> Vec<String> {
    es.into_iter()
      .flat_map(|arg| self.get_inner_vals(arg))
      .collect()
  }
}
