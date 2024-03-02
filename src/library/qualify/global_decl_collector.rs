use std::sync::{Arc, Mutex};

use macros::lock;
use rayon::prelude::*;

use anyhow::{anyhow, Result};

use crate::library::ast::untyped::*;

pub struct GlobalDeclCollector {
  decls: Arc<Mutex<Decls>>,
}

impl GlobalDeclCollector {
  pub fn create() -> Self {
    Self {
      decls: Arc::new(Mutex::new(Vec::new())),
    }
  }

  pub fn collect<MS>(&self, ms: MS) -> Result<Decls>
  where
    MS: ParallelIterator<Item = Mod>,
  {
    ms.try_for_each(|m| self.process_mod(&m))?;

    self._return_decls()
  }

  fn process_mod(&self, m: &Mod) -> Result<()> {
    for d in &m.defs {
      self.process_decl(d)?;
    }

    Ok(())
  }

  fn process_decl(&self, d: &Def) -> Result<()> {
    match d {
      Def::Fun(f) => {
        let fundecl = self.get_fun_sig(f);
        self._push_decl(Decl::Fun(fundecl))
      }
      Def::Struct(s) => self._push_decl(Decl::Struct(s.clone())),
      Def::Val(v) => {
        let valdecl = ValDecl {
          name: v.name.clone(),
          t: v.t.clone(),
          inner_vals: self.get_inner_vals(v.expr.clone()),
        };
        self._push_decl(Decl::Val(valdecl))
      }
      Def::Import(_) => Ok(()),
      Def::Class(c) => {
        let class = Class {
          name: c.name.clone(),
          methods: c.methods.clone(),
        };
        self._push_decl(Decl::Class(class))
      }
      Def::Impl(i) => {
        let impldecl = ImplDecl {
          class_name: i.class_name.clone(),
          t: i.t.clone(),
          impls: i.impls.clone().into_iter().map(|i| i.sig).collect(),
        };
        self._push_decl(Decl::Impl(impldecl))
      }
    }
  }

  fn get_fun_sig(&self, f: &Fun) -> FunSig {
    f.sig.clone()
  }

  fn get_inner_vals(&self, expr: Expr) -> Vec<String> {
    match expr {
      Expr::BinOp(binop) => self._map(Vec::from([binop.lh, binop.rh])),
      Expr::Lit(_) => Vec::new(),
      Expr::Value(v) => Vec::from([v.name]),
      Expr::Function(_) => Vec::new(),
      Expr::Assign(assign) => self.get_inner_vals(assign.expr),
      Expr::Chain(chain) => self._map(Vec::from([chain.e1, chain.e2])),
      Expr::Call(call) => self._map(call.args),
      Expr::If(if_) => self._map(Vec::from([if_.if_, if_.then, if_.else_])),
      Expr::Attr(_) => Vec::new(),
      Expr::New(new) => self._map(new.args),
      Expr::StringTemplate(stringtemplate) => stringtemplate.args,
      Expr::MethodCall(methodcall) => {
        let mut args = methodcall.args;
        args.insert(0, methodcall.this);
        self._map(args)
      }
    }
  }

  fn _map(&self, es: Vec<Expr>) -> Vec<String> {
    es.into_iter()
      .flat_map(|arg| self.get_inner_vals(arg))
      .collect()
  }

  fn _push_decl(&self, decl: Decl) -> Result<()> {
    lock!(mut decls);
    decls.push(decl);
    Ok(())
  }

  fn _return_decls(&self) -> Result<Decls> {
    lock!(decls);
    Ok(decls.clone())
  }
}
