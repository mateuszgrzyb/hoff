use rayon::prelude::*;

use crate::library::ast::untyped::*;

pub struct GlobalDeclCollector {}

pub trait ProcessGDCNode<'ctx> {
  type R;
  fn process(self, ctx: &'ctx GlobalDeclCollector) -> Self::R;
}

impl<'ctx> ProcessGDCNode<'ctx> for Mod {
  type R = impl ParallelIterator<Item = Decl> + 'ctx;

  fn process(self, ctx: &'ctx GlobalDeclCollector) -> Self::R {
    self.defs.into_par_iter().filter_map(|d| d.process(ctx))
  }
}

impl<'ctx> ProcessGDCNode<'ctx> for &'ctx Def {
  type R = Option<Decl>;

  fn process(self, ctx: &'ctx GlobalDeclCollector) -> Self::R {
    match self {
      Def::Fun(f) => f.process(ctx),
      Def::Struct(s) => s.process(ctx),
      Def::Val(v) => v.process(ctx),
      Def::Import(_) => None,
      Def::Class(c) => c.process(ctx),
      Def::Impl(i) => i.process(ctx),
    }
  }
}

impl<'ctx> ProcessGDCNode<'ctx> for &'ctx FunDef {
  type R = Option<Decl>;

  fn process(self, ctx: &'ctx GlobalDeclCollector) -> Self::R {
    let fundecl = ctx.get_fun_sig(self);
    Some(Decl::Fun(fundecl))
  }
}

impl<'ctx> ProcessGDCNode<'ctx> for &'ctx StructDef {
  type R = Option<Decl>;

  fn process(self, _ctx: &'ctx GlobalDeclCollector) -> Self::R {
    Some(Decl::Struct(self.clone()))
  }
}

impl<'ctx> ProcessGDCNode<'ctx> for &'ctx ValDef {
  type R = Option<Decl>;

  fn process(self, ctx: &'ctx GlobalDeclCollector) -> Self::R {
    let valdecl = ValDecl {
      name: self.name.clone(),
      t: self.t.clone(),
      inner_vals: ctx.get_inner_vals(self.expr.clone()),
    };
    Some(Decl::Val(valdecl))
  }
}

impl<'ctx> ProcessGDCNode<'ctx> for &'ctx ClassDef {
  type R = Option<Decl>;

  fn process(self, _ctx: &'ctx GlobalDeclCollector) -> Self::R {
    let class = ClassDef {
      name: self.name.clone(),
      methods: self.methods.clone(),
    };
    Some(Decl::Class(class))
  }
}

impl<'ctx> ProcessGDCNode<'ctx> for &'ctx ImplDef {
  type R = Option<Decl>;

  fn process(self, _ctx: &'ctx GlobalDeclCollector) -> Self::R {
    let impldecl = ImplDecl {
      class_name: self.class_name.clone(),
      t: self.t.clone(),
      impls: self.impls.clone().into_iter().map(|i| i.sig).collect(),
    };
    Some(Decl::Impl(impldecl))
  }
}

impl GlobalDeclCollector {
  pub fn create() -> Self {
    Self {}
  }

  pub fn collect<MS>(&self, ms: MS) -> Decls
  where
    MS: ParallelIterator<Item = Mod>,
  {
    ms.flat_map(|m| m.process(self)).collect()
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
}
