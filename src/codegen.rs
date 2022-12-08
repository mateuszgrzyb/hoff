use crate::ast::{Expr, Fun, Lit, Mod, Op};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, FloatType, IntType};
use inkwell::values::{BasicValueEnum, IntValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;

pub struct Types<'ctx> {
    pub int: IntType<'ctx>,
    pub float: FloatType<'ctx>,
    pub bool: IntType<'ctx>,
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub parent_basic_block: Option<BasicBlock<'ctx>>,
    pub types: Types<'ctx>,
}

type Namespace<'ctx> = HashMap<String, IntValue<'ctx>>;

impl<'ctx> CodeGen<'ctx> {
    pub fn compile_module(&mut self, m: Mod) -> Result<(), Box<dyn Error>> {
        self.module.set_name(m.name.as_str());
        let mut module_name = m.name.clone();
        module_name.push_str(".ir");
        self.module.set_source_file_name(module_name.as_str());

        for f in m.funs.into_iter() {
            self.compile_fun(&mut HashMap::new(), f);
        }

        Ok(())
    }

    fn compile_fun(
        &mut self,
        _namespace: &mut Namespace<'ctx>,
        f: Fun,
    ) -> IntValue<'ctx> {
        let fn_type = self.types.int.fn_type(
            &f.args
                .iter()
                .map(|_| self.types.int.into())
                .collect::<Vec<BasicMetadataTypeEnum>>()[..],
            false,
        );
        let function =
            self.module.add_function(f.name.as_str(), fn_type, None);
        let args = function.get_param_iter().zip(f.args).map(|(p, n)| {
            p.set_name(n.as_str());
            (n, p)
        });
        let basic_block = self.context.append_basic_block(function, "entry");
        let parent_basic_block = self.parent_basic_block.clone();
        self.builder.position_at_end(basic_block);

        let mut new_namespace: Namespace<'ctx> = args
            .map(
                |(name, param)| -> Result<(String, IntValue<'ctx>), Box<dyn Error>> {
                    let BasicValueEnum::IntValue(i) = param else {
                    panic!("impossible state")
                };
                    Ok((name, i))
                },
            )
            .collect::<Result<Namespace<'ctx>, _>>().unwrap();

        self.parent_basic_block = Some(basic_block);
        let value = self.compile_expr(&mut new_namespace, f.body);

        self.builder.position_at_end(basic_block);
        self.builder.build_return(Some(&value));
        if let Some(pbb) = parent_basic_block {
            self.builder.position_at_end(pbb)
        };

        self.types.int.const_int(0, false)
    }

    fn compile_expr(
        &mut self,
        namespace: &mut Namespace<'ctx>,
        e: Expr,
    ) -> IntValue<'ctx> {
        match e {
            Expr::BinOp(lh, op, rh) => {
                self.compile_binop(namespace, *lh, op, *rh)
            }
            Expr::Lit(l) => self.compile_lit(namespace, l),
            Expr::Value(name) => self.compile_value(namespace, name),
            Expr::Assign(name, val) => {
                self.compile_assign(namespace, name, *val)
            }
            Expr::Chain(lh, rh) => self.compile_chain(namespace, *lh, *rh),
            Expr::Function(f) => self.compile_fun(namespace, *f),
        }
    }

    fn compile_binop(
        &mut self,
        namespace: &mut Namespace<'ctx>,
        lh: Expr,
        op: Op,
        rh: Expr,
    ) -> IntValue<'ctx> {
        let lh = self.compile_expr(namespace, lh);
        let rh = self.compile_expr(namespace, rh);
        match op {
            Op::Add => self.builder.build_int_add(lh, rh, "add"),
            Op::Sub => self.builder.build_int_sub(lh, rh, "sub"),
            Op::Mul => self.builder.build_int_mul(lh, rh, "mul"),
            Op::Div => self.builder.build_int_signed_div(lh, rh, "div"),
        }
    }

    fn compile_lit(
        &self,
        _namespace: &mut Namespace<'ctx>,
        l: Lit,
    ) -> IntValue<'ctx> {
        match l {
            Lit::Num(i) => self.types.int.const_int(i as u64, false),
        }
    }

    fn compile_value(
        &self,
        namespace: &mut Namespace<'ctx>,
        name: String,
    ) -> IntValue<'ctx> {
        *namespace
            .get(name.as_str())
            .unwrap_or_else(|| panic!("unknown value {name}"))
    }

    fn compile_assign(
        &mut self,
        namespace: &mut Namespace<'ctx>,
        name: String,
        val: Expr,
    ) -> IntValue<'ctx> {
        let val = self.compile_expr(namespace, val);
        namespace.insert(name, val);
        val
    }

    fn compile_chain(
        &mut self,
        namespace: &mut Namespace<'ctx>,
        lh: Expr,
        rh: Expr,
    ) -> IntValue<'ctx> {
        self.compile_expr(namespace, lh);
        self.compile_expr(namespace, rh)
    }

    pub fn create(context: &'ctx Context) -> Result<Self, Box<dyn Error>> {
        let module = context.create_module("sum");
        let execution_engine =
            module.create_jit_execution_engine(OptimizationLevel::None)?;
        let builder = context.create_builder();
        let types = Types {
            int: context.i32_type(),
            float: context.f32_type(),
            bool: context.bool_type(),
        };
        let int = context.i32_type();
        let parent_basic_block = None;
        let codegen = Self {
            context,
            module,
            builder,
            execution_engine,
            types,
            parent_basic_block,
        };
        Ok(codegen)
    }
}
