use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::types::StructType;
use inkwell::OptimizationLevel;
use rstest::*;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

#[rstest]
fn test_struct() {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    let builder = context.create_builder();

    let f32_type = context.f32_type();
    let f32_zero = f32_type.const_float(0.);
    let struct_type: StructType =
        context.struct_type(&[f32_type.into()], false);
    let struct_val = struct_type.const_named_struct(&[f32_zero.into()]);

    let mt = context.i32_type().fn_type(&[], false);
    let m = module.add_function("main", mt, None);

    let basic_block = context.append_basic_block(m, "entry");
    builder.position_at_end(basic_block);
    builder.build_malloc(struct_type, "").unwrap();
}
