use rayon::{iter::once, prelude::*};

use crate::library::ast::untyped::*;

pub fn collect_global_declarations<MS>(ms: MS) -> Decls
where
  MS: ParallelIterator<Item = Mod>,
{
  ms.flat_map(process_mod).collect::<Vec<_>>()
}

fn process_mod(m: Mod) -> impl ParallelIterator<Item = Decl> {
  m.defs.into_par_iter().flat_map(process_decl)
}

fn process_decl(d: Def) -> impl ParallelIterator<Item = Decl> {
  once(d).filter_map(convert_def_to_decl)
}

fn convert_def_to_decl(d: Def) -> Option<Decl> {
  match d {
    Def::Fun(Fun { sig, .. }) => Some(Decl::Fun(sig)),
    Def::Struct(s) => Some(Decl::Struct(s)),
    Def::Val(Val { name, t, expr }) => {
      let valdecl = ValDecl {
        name,
        t,
        inner_vals: utils::get_inner_vals(expr),
      };
      Some(Decl::Val(valdecl))
    }
    Def::Import(_) => None,
    Def::Class(c) => Some(Decl::Class(c)),
    Def::Impl(Impl {
      class_name,
      t,
      impls,
    }) => {
      let impldecl = ImplDecl {
        class_name,
        t,
        impls: impls.into_iter().map(|i| i.sig).collect(),
      };
      Some(Decl::Impl(impldecl))
    }
  }
}

mod utils {
  use super::*;

  pub fn get_inner_vals(expr: Expr) -> Vec<String> {
    match expr {
      Expr::BinOp(lh, _, rh) => map(Vec::from([*lh, *rh])),
      Expr::Lit(_) => Vec::new(),
      Expr::Value(v) => Vec::from([v]),
      Expr::Function(_, _) => Vec::new(),
      Expr::Assign(_, expr) => get_inner_vals(*expr),
      Expr::Chain(lh, rh) => map(Vec::from([*lh, *rh])),
      Expr::Call(_, args) => map(args),
      Expr::If(ie, e1, e2) => map(Vec::from([*ie, *e1, *e2])),
      Expr::Attr(_, _, _) => Vec::new(),
      Expr::New(_, args) => map(args),
      Expr::StringTemplate(_, args) => args,
      Expr::MethodCall(this, _, _, args) => {
        let mut args = args;
        args.insert(0, *this);
        map(args)
      }
    }
  }

  fn map(es: Vec<Expr>) -> Vec<String> {
    es.into_iter().flat_map(get_inner_vals).collect()
  }
}
