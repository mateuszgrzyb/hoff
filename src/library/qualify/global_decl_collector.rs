use std::sync::{Arc, Mutex};

use rayon::prelude::*;

use crate::library::ast::untyped::*;

pub struct GlobalDeclCollector {
  decls: Arc<Mutex<Decls>>,
}

#[derive(Debug, Clone)]
pub enum GdcError {
  CannotLock,
}

pub trait ProcessGDCNode {
  type R;
  fn process(self, ctx: &GlobalDeclCollector) -> Self::R;
}

impl ProcessGDCNode for &Mod {
  type R = Result<(), GdcError>;

  fn process(self, ctx: &GlobalDeclCollector) -> Self::R {
    for d in &self.defs {
      d.process(ctx)?;
    }

    Ok(())
  }
}

impl ProcessGDCNode for &Def {
  type R = Result<(), GdcError>;

  fn process(self, ctx: &GlobalDeclCollector) -> Self::R {
    match self {
      Def::Fun(f) => f.process(ctx),
      Def::Struct(s) => s.process(ctx),
      Def::Val(v) => v.process(ctx),
      Def::Import(_) => Ok(()),
      Def::Class(c) => c.process(ctx),
      Def::Impl(i) => i.process(ctx),
    }
  }
}

impl ProcessGDCNode for &FunDef {
  type R = Result<(), GdcError>;

  fn process(self, ctx: &GlobalDeclCollector) -> Self::R {
    let fundecl = ctx.get_fun_sig(self);
    ctx._push_decl(Decl::Fun(fundecl))
  }
}

impl ProcessGDCNode for &StructDef {
  type R = Result<(), GdcError>;

  fn process(self, ctx: &GlobalDeclCollector) -> Self::R {
    ctx._push_decl(Decl::Struct(self.clone()))
  }
}

impl ProcessGDCNode for &ValDef {
  type R = Result<(), GdcError>;

  fn process(self, ctx: &GlobalDeclCollector) -> Self::R {
    let valdecl = ValDecl {
      name: self.name.clone(),
      t: self.t.clone(),
      inner_vals: ctx.get_inner_vals(self.expr.clone()),
    };
    ctx._push_decl(Decl::Val(valdecl))
  }
}

impl ProcessGDCNode for &ClassDef {
  type R = Result<(), GdcError>;

  fn process(self, ctx: &GlobalDeclCollector) -> Self::R {
    let class = ClassDef {
      name: self.name.clone(),
      methods: self.methods.clone(),
    };
    ctx._push_decl(Decl::Class(class))
  }
}

impl ProcessGDCNode for &ImplDef {
  type R = Result<(), GdcError>;

  fn process(self, ctx: &GlobalDeclCollector) -> Self::R {
    let impldecl = ImplDecl {
      class_name: self.class_name.clone(),
      t: self.t.clone(),
      impls: self.impls.clone().into_iter().map(|i| i.sig).collect(),
    };
    ctx._push_decl(Decl::Impl(impldecl))
  }
}

impl GlobalDeclCollector {
  pub fn create() -> Self {
    Self {
      decls: Arc::new(Mutex::new(Vec::new())),
    }
  }

  pub fn collect<MS>(&self, ms: MS) -> Result<Decls, GdcError>
  where
    MS: ParallelIterator<Item = Mod>,
  {
    ms.try_for_each(|m| m.process(self))?;

    self._return_decls()
  }

  fn get_fun_sig(&self, f: &FunDef) -> FunSig {
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

  fn _push_decl(&self, decl: Decl) -> Result<(), GdcError> {
    let mut decls = self.decls.lock().map_err(|_| GdcError::CannotLock)?;
    decls.push(decl);
    Ok(())
  }

  fn _return_decls(&self) -> Result<Decls, GdcError> {
    let decls = self.decls.lock().map_err(|_| GdcError::CannotLock)?;
    Ok(decls.clone())
  }
}
