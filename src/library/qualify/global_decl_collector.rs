use crate::library::ast::untyped::*;

pub struct GlobalDeclCollector {
  decls: Decls,
}

impl GlobalDeclCollector {
  pub fn create() -> Self {
    Self { decls: Vec::new() }
  }

  pub fn collect<MS>(&mut self, ms: MS) -> Decls
  where
    MS: Iterator<Item = Mod>,
  {
    ms.for_each(|m| self.process_mod(&m));
    self.decls.clone()
  }

  fn process_mod(&mut self, m: &Mod) {
    for d in &m.defs {
      self.process_decl(d)
    }
  }

  fn process_decl(&mut self, d: &Def) {
    match d {
      Def::Fun(f) => {
        let fundecl = self.get_fun_sig(f);
        self.decls.push(Decl::Fun(fundecl))
      }
      Def::Struct(s) => self.decls.push(Decl::Struct(s.clone())),
      Def::Val(v) => {
        let valdecl = ValDecl {
          name: v.name.clone(),
          t: v.t.clone(),
          inner_vals: self.get_inner_vals(v.expr.clone()),
        };
        self.decls.push(Decl::Val(valdecl))
      }
      Def::Import(_) => {}
      Def::Class(c) => {
        let class = Class {
          name: c.name.clone(),
          methods: c.methods.clone(),
        };
        self.decls.push(Decl::Class(class))
      }
      Def::Impl(_) => {}
    }
  }

  fn get_fun_sig(&self, f: &Fun) -> FunSig {
    FunSig {
      name: f.sig.name.clone(),
      args: f.sig.args.clone(),
      rt: f.sig.rt.clone(),
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
      Expr::MethodCall(this, _, _, args) => {
        let mut args = args.clone();
        args.insert(0, *this);
        self._map(args)
      }
    }
  }

  fn _map(&mut self, es: Vec<Expr>) -> Vec<String> {
    es.into_iter()
      .flat_map(|arg| self.get_inner_vals(arg))
      .collect()
  }
}
