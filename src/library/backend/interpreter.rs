use std::sync::Arc;

use anyhow::{bail, Result};
use inkwell::{context::Context, execution_engine::ExecutionEngine};

use crate::library::{
  ast::{typed, untyped, SimpleType},
  backend::get_opt_level,
  codegen::{Codegen, ProcessCodegenNode as _},
  qualify::{ImportQualifier, ProcessImportQualifierNode as _},
  typecheck::{ProcessTypecheckerNode as _, Typechecker},
};

pub struct Interpreter<'ctx> {
  typechecker: Typechecker,
  qualifier: ImportQualifier,
  codegen: Codegen<'ctx>,
  execution_engine: ExecutionEngine<'ctx>,
  defs: Vec<untyped::Def>,
}

impl<'ctx> Interpreter<'ctx> {
  pub fn create(
    global_decls: Arc<typed::Decls>,
    context: &'ctx Context,
    opt_level: u8,
  ) -> Self {
    let opt_level = get_opt_level(opt_level);
    let codegen = Codegen::create(context, true, false);
    let execution_engine = codegen
      .module
      .create_jit_execution_engine(opt_level)
      .unwrap();

    let import_qualifier = ImportQualifier::create(global_decls);

    Self {
      typechecker: Typechecker::create_empty(),
      qualifier: import_qualifier,
      codegen,
      execution_engine,
      defs: Vec::new(),
    }
  }

  pub fn eval(&mut self, repl: untyped::Repl) -> Result<String> {
    match repl {
      untyped::Repl::Expr(expr) => self.eval_expr(expr),
      untyped::Repl::Def(decl) => self.eval_decl(decl),
    }
  }

  fn eval_expr(&mut self, expr: untyped::Expr) -> Result<String> {
    let (body, rt) = self.typechecker.get_type_of_expr(expr)?;

    let main_mod = self.create_main(body, rt.clone())?;

    main_mod.process(&mut self.codegen);

    let return_value_repr = unsafe {
      let Ok(main) = self.execution_engine.get_function_value("main") else {
        bail!("some error happened")
      };

      let return_value = self.execution_engine.run_function(main, &[]);

      let ft = self.codegen.types.float;
      match rt {
        typed::Type::Simple(SimpleType::Int) => {
          format!("{:?}", return_value.as_int(true))
        }
        typed::Type::Simple(SimpleType::Bool) => {
          format!("{:?}", return_value.as_int(true) == 0)
        }
        typed::Type::Simple(SimpleType::Float) => {
          format!("{:?}", return_value.as_float(&ft))
        }
        _ => todo!(),
      }
    };

    Ok(return_value_repr)
  }

  fn eval_decl(&mut self, def: untyped::Def) -> Result<String> {
    let sig = format!("{:?}", def);
    self.defs.push(def);
    Ok(sig)
  }

  fn create_main(
    &mut self,
    body: typed::Expr,
    rt: typed::Type,
  ) -> Result<typed::Mod> {
    let main = typed::FunDef {
      sig: typed::FunSig {
        name: "main".to_string(),
        args: Vec::new(),
        rt,
      },
      body,
    };

    let defs = self.defs.clone();
    let defs = defs.process(&self.qualifier)?;
    let mut defs = defs.process(&mut self.typechecker)?;

    defs.push(typed::Def::Fun(main));

    Ok(typed::Mod {
      name: "repl".to_string(),
      defs,
      imports: typed::Decls::default(),
    })
  }
}
