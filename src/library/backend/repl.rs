use crate::library::ast::{typed, untyped, SimpleType};
use crate::library::backend::get_opt_level;
use crate::library::codegen::CodeGen;
use crate::library::import_qualifier::ImportQualifier;
use crate::library::typecheck::TypeChecker;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;

pub struct REPL<'init, 'ctx> {
    typechecker: TypeChecker,
    qualifier: ImportQualifier<'init>,
    codegen: CodeGen<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
    decls: Vec<untyped::Decl>,
}

impl<'init, 'ctx> REPL<'init, 'ctx> {
    pub fn create(
        fds: &'init Vec<typed::FunDecl>,
        ss: &'init Vec<typed::Struct>,
        context: &'ctx Context,
        opt_level: u32,
    ) -> Self {
        let opt_level = get_opt_level(opt_level);
        let codegen = CodeGen::create(&context, true);
        let execution_engine = codegen
            .module
            .create_jit_execution_engine(opt_level)
            .unwrap();

        let import_qualifier = ImportQualifier::create(&fds, &ss);

        Self {
            typechecker: TypeChecker::create(),
            qualifier: import_qualifier,
            codegen,
            execution_engine,
            decls: Vec::new(),
        }
    }

    pub fn eval(&mut self, repl: untyped::Repl) -> Result<String, String> {
        match repl {
            untyped::Repl::Expr(expr) => self.eval_expr(expr),
            untyped::Repl::Decl(decl) => self.eval_decl(decl),
        }
    }

    fn eval_expr(&mut self, expr: untyped::Expr) -> Result<String, String> {
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

    fn eval_decl(&mut self, decl: untyped::Decl) -> Result<String, String> {
        self.decls.push(decl);
        Ok("".to_string())
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

        let decls = self.decls.clone();
        println!("{:?}", decls);
        let decls = self.qualifier.qualify_decls(decls)?;
        let mut decls = self.typechecker.typecheck_decls(decls)?;

        decls.push(typed::Decl::Fun(main));

        Ok(typed::Mod {
            name: "repl".to_string(),
            decls,
            imports: typed::Imports::create(),
        })
    }
}
