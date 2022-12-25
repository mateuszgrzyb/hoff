use crate::library::ast::{typed, untyped, SimpleType};
use crate::library::backend::get_opt_level;
use crate::library::codegen::CodeGen;
use crate::library::typecheck::Typechecker;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;

pub struct REPL<'ctx> {
    typechecker: Typechecker,
    codegen: CodeGen<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> REPL<'ctx> {
    pub fn create(context: &'ctx Context, opt_level: u32) -> Self {
        let opt_level = get_opt_level(opt_level);
        let codegen = CodeGen::create(&context, true);
        let execution_engine = codegen
            .module
            .create_jit_execution_engine(opt_level)
            .unwrap();
        Self {
            typechecker: Typechecker::create(),
            codegen,
            execution_engine,
        }
    }

    pub fn eval(&mut self, expr: untyped::Expr) -> Result<String, String> {
        let (body, rt) = self.typechecker.get_type_of_expr(expr.clone())?;

        let main_mod = self.create_main(body, rt.clone())?;

        self.codegen.compile_module(main_mod);

        let return_value_repr = unsafe {
            let Ok(main) = self.execution_engine.get_function_value("main") else {
                return Err("some error happened".to_string())
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

    fn create_main(
        &mut self,
        body: typed::Expr,
        rt: typed::Type,
    ) -> Result<typed::Mod, String> {
        let main = typed::Fun {
            name: "main".to_string(),
            args: Vec::new(),
            rt,
            body,
        };

        Ok(typed::Mod {
            name: "repl".to_string(),
            decls: Vec::from([typed::Decl::Fun(main)]),
        })
    }
}
