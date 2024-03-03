mod types;

use std::collections::HashMap;

use inkwell::{
  basic_block::BasicBlock,
  builder::Builder,
  context::Context,
  module::Module,
  types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
  values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
  },
  AddressSpace, FloatPredicate, IntPredicate,
};
use FloatPredicate::{OEQ, OGE, OGT, OLE, OLT, ONE};
use IntPredicate::{EQ, NE, SGE, SGT, SLE, SLT};

use crate::library::{
  ast::typed::*, codegen::types::Types, utils::MethodNamer,
};

use super::ast::FunArg;

pub struct Codegen<'ctx> {
  context: &'ctx Context,
  sort_decls: bool,
  pub module: Module<'ctx>,
  builder: Builder<'ctx>,
  parent_basic_block: Option<BasicBlock<'ctx>>,
  pub types: Types<'ctx>,
  functions: HashMap<String, FunctionValue<'ctx>>,
  values: HashMap<String, BasicValueEnum<'ctx>>,
  closure: HashMap<String, BasicValueEnum<'ctx>>,
  closures: HashMap<String, Vec<String>>,
  current_function: Option<FunctionValue<'ctx>>,
}

type V<'ctx> = BasicValueEnum<'ctx>;

pub trait ProcessCodegenNode<'ctx> {
  type R;
  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R;
}

impl<'ctx> ProcessCodegenNode<'ctx> for Mod {
  type R = ();

  fn process(mut self, ctx: &mut Codegen<'ctx>) -> Self::R {
    // TODO: Fix if tests are flaky again...
    if ctx.sort_decls {
      self.imports.sort_by_key(|i| i.get_name().clone());
    }

    for i in &self.imports {
      if let Decl::Struct(s) = i {
        s.clone().process(ctx)
      }
    }

    for i in self.imports {
      match i {
        Decl::Struct(_) => {}
        Decl::Fun(f) => f.process(ctx),
        Decl::Val(v) => v.process(ctx),
        Decl::Class(_) => { /* noop */ }
        Decl::Impl(i) => {
          for method_sig in &i.impls {
            let mut method_sig = method_sig.clone();
            method_sig.name = i.get_method_name_by_sig(&method_sig);
            method_sig.process(ctx);
          }
        }
      }
    }

    ctx.module.set_name(self.name.as_str());
    let mut module_name = self.name.clone();
    module_name.push_str(".ir");
    ctx.module.set_source_file_name(module_name.as_str());

    for d in self.defs.into_iter() {
      d.process(ctx);
    }
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for FunSig {
  type R = ();

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    ctx.compile_fun_sig(self, Vec::new());
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for ValDecl {
  type R = ();

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let f = FunSig {
      name: self.name,
      args: Vec::new(),
      rt: self.t,
    };
    f.process(ctx)
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Def {
  type R = ();

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    match self {
      Def::Fun(f) => {
        Function {
          f,
          closure: Vec::new(),
        }
        .process(ctx);
      }
      Def::Struct(s) => {
        s.process(ctx);
      }
      Def::Val(v) => {
        v.process(ctx);
      }
      Def::Import(_) => {
        // imports are compiled before all declarations
      }
      Def::Class(_) => {
        // noop
      }
      Def::Impl(i) => {
        i.process(ctx);
      }
    }
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Struct {
  type R = ();

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let name = self.name;
    let args = self
      .args
      .into_iter()
      .map(|arg| ctx.get_type(&arg.type_))
      .collect::<Vec<BasicTypeEnum>>();

    ctx
      .context
      .get_struct_type(name.as_str())
      .unwrap_or_else(|| {
        let s = ctx.context.opaque_struct_type(name.as_str());
        s.set_body(&args[..], true);
        s
      });
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Function {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let function =
      ctx.compile_fun_sig(self.f.sig.clone(), self.closure.clone());
    ctx.compile_fun_body(self.f, self.closure, function)
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Val {
  type R = ();

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    Function {
      f: Fun {
        sig: FunSig {
          name: self.name,
          args: Vec::new(),
          rt: self.t,
        },
        body: self.expr,
      },
      closure: Vec::new(),
    }
    .process(ctx);
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Impl {
  type R = ();

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    for method in self.impls.clone() {
      let method_name = self.get_method_name_by_sig(&method.sig);
      let function = ctx.functions.get(&method_name).unwrap();
      ctx.compile_fun_body(method, Vec::new(), *function);
    }
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Expr {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    match self {
      Expr::BinOp(binop) => binop.process(ctx),
      Expr::Lit(l) => l.process(ctx),
      Expr::Value(value) => value.process(ctx),
      Expr::Assign(assign) => assign.process(ctx),
      Expr::Chain(chain) => chain.process(ctx),
      Expr::Function(function) => function.process(ctx),
      Expr::Call(call) => call.process(ctx),
      Expr::If(if_) => if_.process(ctx),
      Expr::Attr(attr) => attr.process(ctx),
      Expr::New(new) => new.process(ctx),
      Expr::StringTemplate(stringtemplate) => stringtemplate.process(ctx),
      Expr::MethodCall(methodcall) => methodcall.process(ctx),
    }
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for BinOp {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let lh = self.lh.process(ctx);
    let rh = self.rh.process(ctx);

    let b = &mut ctx.builder;

    match (lh.get_type(), self.op) {
      (BasicTypeEnum::IntType(i), op) if i == ctx.types.bool => {
        let lh = lh.into_int_value();
        let rh = rh.into_int_value();
        let result = match op {
          Op::And => ctx.builder.build_and(lh, rh, "and"),
          Op::Or => ctx.builder.build_or(lh, rh, "and"),
          _ => panic!("Invalid state"),
        };
        result.unwrap().as_basic_value_enum()
      }
      (BasicTypeEnum::IntType(_), op) => {
        let lh = lh.into_int_value();
        let rh = rh.into_int_value();
        let result = match op {
          Op::Add => b.build_int_add(lh, rh, "add"),
          Op::Sub => b.build_int_sub(lh, rh, "sub"),
          Op::Mul => b.build_int_mul(lh, rh, "mul"),
          Op::Div => b.build_int_signed_div(lh, rh, "div"),
          Op::Lt => b.build_int_compare(SLT, lh, rh, "ilt"),
          Op::Le => b.build_int_compare(SLE, lh, rh, "ile"),
          Op::Ne => b.build_int_compare(NE, lh, rh, "ine"),
          Op::Eq => b.build_int_compare(EQ, lh, rh, "ieq"),
          Op::Ge => b.build_int_compare(SGE, lh, rh, "ige"),
          Op::Gt => b.build_int_compare(SGT, lh, rh, "igt"),
          Op::And | Op::Or => panic!("Invalid state"),
        };
        result.unwrap().as_basic_value_enum()
      }
      (
        BasicTypeEnum::FloatType(_),
        op @ (Op::Add | Op::Sub | Op::Mul | Op::Div),
      ) => {
        let lh = lh.into_float_value();
        let rh = rh.into_float_value();
        let result = match op {
          Op::Add => b.build_float_add(lh, rh, "fadd"),
          Op::Sub => b.build_float_sub(lh, rh, "fsub"),
          Op::Mul => b.build_float_mul(lh, rh, "fmul"),
          Op::Div => b.build_float_div(lh, rh, "fdiv"),
          _ => panic!("Invalid state"),
        };
        result.unwrap().as_basic_value_enum()
      }

      (BasicTypeEnum::FloatType(_), op) => {
        let lh = lh.into_float_value();
        let rh = rh.into_float_value();
        let result = match op {
          Op::Lt => b.build_float_compare(OLT, lh, rh, "flt"),
          Op::Le => b.build_float_compare(OLE, lh, rh, "fle"),
          Op::Ne => b.build_float_compare(ONE, lh, rh, "fne"),
          Op::Eq => b.build_float_compare(OEQ, lh, rh, "feq"),
          Op::Ge => b.build_float_compare(OGE, lh, rh, "fge"),
          Op::Gt => b.build_float_compare(OGT, lh, rh, "fgt"),
          _ => panic!("Invalid state"),
        };
        result.unwrap().as_basic_value_enum()
      }
      _ => panic!("Invalid state"),
    }
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Lit {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let ts = &ctx.types;
    match self {
      Lit::Int(i) => ts.int.const_int(i as u64, false).as_basic_value_enum(),
      Lit::Bool(b) => ts.bool.const_int(b.into(), false).as_basic_value_enum(),
      Lit::Float(f) => ts
        .float
        .const_float(f.parse().unwrap())
        .as_basic_value_enum(),
      Lit::String(s) => ctx
        .builder
        .build_global_string_ptr(&s, "")
        .unwrap()
        .as_basic_value_enum(),
    }
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Value {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let n = self.name.as_str();

    if let Some(value) = ctx.closure.get(n) {
      return *value;
    }

    if let Some(value) = ctx.values.get(n) {
      return *value;
    }

    if let Some(f) = ctx.functions.get(n) {
      return ctx
        .builder
        .build_call(*f, &[], "call")
        .unwrap()
        .try_as_basic_value()
        .left()
        .unwrap();
    }

    panic!("unknown value {}", self.name)
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Assign {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let val = self.expr.process(ctx);
    ctx.values.insert(self.name, val);
    val
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Chain {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    self.e1.process(ctx);
    self.e2.process(ctx)
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Call {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let f = *ctx.functions.get(self.name.as_str()).unwrap();
    let closure = ctx
      .closures
      .get(self.name.as_str())
      .unwrap_or(&Vec::new())
      .clone();

    let standard_args = self
      .args
      .into_iter()
      .map(|a| a.process(ctx).into())
      .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();

    let closure = closure
      .into_iter()
      .map(|n| (*ctx.values.get(n.as_str()).unwrap()).into())
      .collect::<Vec<_>>();

    let args = standard_args
      .clone()
      .into_iter()
      .chain(closure.clone())
      .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();

    // println!("
    // name {}
    // sa {}
    // c {}
    // a {}
    // f {}
    // ",
    // name,
    // standard_args.len(),
    // closure.len(),
    // args.len(),
    // f.count_params(),
    // );
    //

    ctx
      .builder
      .build_call(f, &args[..], "call")
      .unwrap()
      .try_as_basic_value()
      .left()
      .unwrap()

    // if f.count_params() < args.len() as u32 {
    // panic!("invalid state")
    // } else if f.count_params() == args.len() as u32 {
    // self.builder
    // .build_call(f, &args[..], "call")
    // .try_as_basic_value()
    // .left()
    // .unwrap()
    // } else {
    // let partial_ft = Vec::from([
    // Type::Simple(SimpleType::Int),
    // Type::Simple(SimpleType::Int),
    // ]);
    // let mut partial_ft = partial_ft
    // .into_iter()
    // .map(|t| self.get_type(&t))
    // .collect::<Vec<_>>();
    // let partial_rt = partial_ft.pop().unwrap();
    // let ft = partial_rt.fn_type(
    // &partial_ft
    // .into_iter()
    // .map(|t| t.into())
    // .collect::<Vec<BasicMetadataTypeEnum>>()[..],
    // false,
    // );
    // let partial_f = self.module.add_function("", ft, None);
    //
    // let bb = self.context.append_basic_block(partial_f, "entry");
    // self.builder.position_at_end(bb);
    // let partial_rt = self
    // .builder
    // .build_call(
    // f,
    // &(args
    // .into_iter()
    // .chain(partial_f.get_param_iter().map(|p| p.into()))
    // .collect::<Vec<_>>())[..],
    // "call",
    // )
    // .try_as_basic_value()
    // .left()
    // .unwrap();
    // self.builder.build_return(Some(&partial_rt));
    //
    // self.builder
    // .position_at_end(self.parent_basic_block.unwrap());
    //
    // partial_f
    // .as_global_value()
    // .as_pointer_value()
    // .as_basic_value_enum()
    // }
    //
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for If {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let if_ = self.if_.process(ctx);
    let then = self.then.process(ctx);
    let else_ = self.else_.process(ctx);
    ctx
      .builder
      .build_select(if_.into_int_value(), then, else_, "select")
      .unwrap()
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for Attr {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let ptr = ctx
      .values
      .get(self.name.as_str())
      .unwrap()
      .into_pointer_value();

    let (i, arg) = self
      .struct_
      .args
      .into_iter()
      .enumerate()
      .find(|(_, arg)| arg.name == self.attr)
      .unwrap();

    let attr_ptr =
      ctx.builder.build_struct_gep(ptr, i as u32, "attr").unwrap();

    match arg.type_ {
      Type::Function(_) | Type::Simple(SimpleType::Struct(_)) => {
        attr_ptr.as_basic_value_enum()
      }
      _ => ctx.builder.build_load(attr_ptr, "load").unwrap(),
    }
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for New {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let struct_type = ctx.context.get_struct_type(self.name.as_str()).unwrap();

    let struct_args = self
      .args
      .into_iter()
      .map(|a| a.process(ctx))
      .collect::<Vec<_>>();

    let struct_value = struct_type.const_named_struct(&struct_args);

    let b = &ctx.builder;

    let struct_ptr = b.build_malloc(struct_type, "malloc").unwrap();

    b.build_store(struct_ptr, struct_value).unwrap();

    struct_ptr.as_basic_value_enum()
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for StringTemplate {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let mut values: Vec<BasicMetadataValueEnum<'ctx>> = self
      .args
      .into_iter()
      .map(|value| Value { name: value }.process(ctx).into())
      .collect::<Vec<_>>();

    let b = &ctx.builder;

    let template_value = b
      .build_global_string_ptr(&self.string, "str_tmpl")
      .unwrap()
      .as_basic_value_enum();

    let template_value_result = b
      .build_global_string_ptr("", "str_tmpl_rstl")
      .unwrap()
      .as_basic_value_enum();

    let mut sprintf_args =
      Vec::from([template_value_result.into(), template_value.into()]);
    sprintf_args.append(&mut values);

    let sprintf = ctx.functions.get("sprintf").unwrap();

    b.build_call(*sprintf, &sprintf_args[..], "run_str_tmpl")
      .unwrap()
      .try_as_basic_value()
      .left()
      .unwrap();

    template_value_result
  }
}

impl<'ctx> ProcessCodegenNode<'ctx> for MethodCall {
  type R = V<'ctx>;

  fn process(self, ctx: &mut Codegen<'ctx>) -> Self::R {
    let name = self.typename.get_method_name(&self.methodname);
    let mut args = self.args;
    args.insert(0, self.this);
    Call { name, args }.process(ctx)
  }
}

impl<'ctx> Codegen<'ctx> {
  pub fn create(
    context: &'ctx Context,
    add_stdlib: bool,
    sort_decls: bool,
  ) -> Self {
    let module = context.create_module("sum");
    let types = Types {
      int: context.i32_type(),
      float: context.f32_type(),
      bool: context.bool_type(),
      string: context.i8_type().ptr_type(AddressSpace::default()),
    };

    let functions = if add_stdlib {
      Self::create_stdlib(context, &module)
    } else {
      HashMap::new()
    };

    Self {
      context,
      module,
      builder: context.create_builder(),
      types,
      parent_basic_block: None,
      functions,
      values: HashMap::new(),
      closure: HashMap::new(),
      closures: HashMap::new(),
      current_function: None,
      sort_decls,
    }
  }

  fn create_stdlib(
    context: &'ctx Context,
    module: &Module<'ctx>,
  ) -> HashMap<String, FunctionValue<'ctx>> {
    [
      (
        "puts",
        context.i32_type().fn_type(
          &[BasicMetadataTypeEnum::from(
            context.i8_type().ptr_type(AddressSpace::default()),
          )],
          false,
        ),
      ),
      (
        "sprintf",
        context.i32_type().fn_type(
          &[
            BasicMetadataTypeEnum::from(
              context.i8_type().ptr_type(AddressSpace::default()),
            ),
            BasicMetadataTypeEnum::from(
              context.i8_type().ptr_type(AddressSpace::default()),
            ),
          ],
          true,
        ),
      ),
    ]
    .into_iter()
    .map(|(name, f_type)| {
      let f = module.add_function(name, f_type, None);
      (name.to_string(), f)
    })
    .collect()
  }

  fn get_simple_type(&self, st: &SimpleType) -> BasicTypeEnum<'ctx> {
    match st {
      SimpleType::Int => self.types.int.into(),
      SimpleType::Bool => self.types.bool.into(),
      SimpleType::Float => self.types.float.into(),
      SimpleType::String => self.types.string.into(),
      SimpleType::This => {
        panic!("this type should not be instantiated")
      }
      SimpleType::Struct(s) => self
        .context
        .get_struct_type(&s.name)
        .unwrap()
        .ptr_type(AddressSpace::default())
        .into(),
    }
  }

  fn get_function_type(&self, ts: &[Type]) -> BasicTypeEnum<'ctx> {
    let mut args = ts.to_owned();
    let rt = args.pop().unwrap();
    let rt = self.get_type(&rt);
    let args = args
      .into_iter()
      .map(|a| self.get_type(&a).into())
      .collect::<Vec<BasicMetadataTypeEnum>>();
    let ft = match rt {
      BasicTypeEnum::ArrayType(at) => at.fn_type(&args[..], false),
      BasicTypeEnum::FloatType(ft) => ft.fn_type(&args[..], false),
      BasicTypeEnum::IntType(it) => it.fn_type(&args[..], false),
      BasicTypeEnum::PointerType(pt) => pt.fn_type(&args[..], false),
      BasicTypeEnum::StructType(st) => st.fn_type(&args[..], false),
      BasicTypeEnum::VectorType(vt) => vt.fn_type(&args[..], false),
    };

    ft.ptr_type(AddressSpace::default()).into()
  }

  fn get_type(&self, t: &Type) -> BasicTypeEnum<'ctx> {
    match t {
      Type::Simple(st) => self.get_simple_type(st),
      Type::Function(ft) => self.get_function_type(ft),
    }
  }

  fn get_fun_args_with_closure(
    &mut self,
    f: FunSig,
    closure: Closure,
  ) -> (Vec<String>, Vec<BasicMetadataTypeEnum<'ctx>>) {
    f.args
      .into_iter()
      .chain(closure)
      .map(|FunArg { name, type_ }| (name, self.get_type(&type_).into()))
      .unzip::<String, BasicMetadataTypeEnum, Vec<String>, Vec<BasicMetadataTypeEnum>>()
  }

  fn compile_fun_sig(
    &mut self,
    f: FunSig,
    closure: Closure,
  ) -> FunctionValue<'ctx> {
    // prepare function type with closure

    let (arg_names, arg_types) =
      self.get_fun_args_with_closure(f.clone(), closure);

    let fn_type = self.get_type(&f.rt).fn_type(&arg_types[..], false);

    // create function value

    let function = self.module.add_function(f.name.as_str(), fn_type, None);

    // add names to parameters

    for (n, p) in arg_names.iter().zip(function.get_param_iter()) {
      p.set_name(n.as_str());
    }

    self.functions.insert(f.name, function);

    function
  }

  fn compile_fun_body(
    &mut self,
    f: Fun,
    closure: Closure,
    function: FunctionValue<'ctx>,
  ) -> V<'ctx> {
    let (arg_names, _) =
      self.get_fun_args_with_closure(f.sig.clone(), closure.clone());

    let basic_block = self.context.append_basic_block(function, "entry");
    let parent_basic_block = self.parent_basic_block;
    self.builder.position_at_end(basic_block);

    // find whether LLVM argument is a regular function argument or closure

    for (n, p) in arg_names.iter().zip(function.get_param_iter()) {
      if closure.clone().into_iter().any(|arg| &arg.name == n) {
        self.closure.insert(n.clone(), p);
      } else {
        self.values.insert(n.clone(), p);
      }
    }

    // register closure

    self.closures.insert(
      f.sig.name.clone(),
      closure.into_iter().map(|arg| arg.name).collect(),
    );

    self.parent_basic_block = Some(basic_block);

    //self.functions.insert(f.sig.name, function);

    self.current_function = Some(function);

    // compile body

    let value = f.body.process(self);

    // fix builder position

    self.builder.position_at_end(basic_block);
    self.builder.build_return(Some(&value)).unwrap();
    if let Some(pbb) = parent_basic_block {
      self.builder.position_at_end(pbb)
    };

    function
      .as_global_value()
      .as_pointer_value()
      .as_basic_value_enum()
  }
}

#[cfg(test)]
mod test {
  use indoc::indoc;
  use rstest::*;

  use super::*;

  #[fixture]
  fn context() -> &'static Context {
    Box::leak(Box::new(Context::create()))
  }

  #[fixture]
  fn codegen(context: &'static Context) -> Codegen<'static> {
    let codegen = Codegen::create(context, false, true);
    codegen
  }

  #[rstest]
  fn test_1(mut codegen: Codegen) {
    // given
    let m = Mod {
      name: "test_1".to_string(),
      defs: Vec::from([
        Def::Struct(Struct {
          name: "Foo".to_string(),
          args: Vec::from([
            StructArg {
              name: "a".to_string(),
              type_: Type::Simple(SimpleType::Int),
            },
            StructArg {
              name: "b".to_string(),
              type_: Type::Simple(SimpleType::Bool),
            },
            StructArg {
              name: "c".to_string(),
              type_: Type::Simple(SimpleType::String),
            },
          ]),
        }),
        Def::Fun(Fun {
          sig: FunSig {
            name: "f1".to_string(),
            args: Vec::from([FunArg {
              name: "a".to_string(),
              type_: Type::Simple(SimpleType::Struct(Struct {
                name: "Foo".to_string(),
                args: Vec::from([
                  StructArg {
                    name: "a".to_string(),
                    type_: Type::Simple(SimpleType::Int),
                  },
                  StructArg {
                    name: "b".to_string(),
                    type_: Type::Simple(SimpleType::Bool),
                  },
                  StructArg {
                    name: "c".to_string(),
                    type_: Type::Simple(SimpleType::String),
                  },
                ]),
              })),
            }]),
            rt: Type::Simple(SimpleType::Int),
          },
          body: Expr::Lit(Lit::Int(32)),
        }),
      ]),
      imports: Decls::default(),
    };
    let expected_module_str = indoc! {r#"
        ; ModuleID = 'test_1'
        source_filename = "test_1.ir"

        %Foo = type <{ i32, i1, i8* }>

        define i32 @f1(%Foo* %a) {
        entry:
          ret i32 32
        }
    "#};

    // when
    m.process(&mut codegen);

    // then
    assert_eq!(codegen.module.to_string(), expected_module_str);
  }
}
