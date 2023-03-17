mod types;

use std::collections::HashMap;

use inkwell::{
  basic_block::BasicBlock,
  builder::Builder,
  context::Context,
  module::Module,
  types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
  values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
  },
  AddressSpace, FloatPredicate, IntPredicate,
};
use FloatPredicate::{OEQ, OGE, OGT, OLE, OLT, ONE};
use IntPredicate::{EQ, NE, SGE, SGT, SLE, SLT};

use crate::library::{ast::typed::*, codegen::types::Types};

pub struct CodeGen<'ctx> {
  context: &'ctx Context,
  pub module: Module<'ctx>,
  builder: Builder<'ctx>,
  parent_basic_block: Option<BasicBlock<'ctx>>,
  pub types: Types<'ctx>,
  structs: HashMap<String, (Struct, StructType<'ctx>)>,
  functions: HashMap<String, FunctionValue<'ctx>>,
  values: HashMap<String, BasicValueEnum<'ctx>>,
  closure: HashMap<String, BasicValueEnum<'ctx>>,
  closures: HashMap<String, Vec<String>>,
  current_function: Option<FunctionValue<'ctx>>,
}

type Value<'ctx> = BasicValueEnum<'ctx>;

impl<'ctx> CodeGen<'ctx> {
  pub fn create(context: &'ctx Context, add_stdlib: bool) -> Self {
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
      structs: HashMap::new(),
      functions,
      values: HashMap::new(),
      closure: HashMap::new(),
      closures: HashMap::new(),
      current_function: None,
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

  fn get_type(&self, t: &Type) -> BasicTypeEnum<'ctx> {
    match t {
      Type::Simple(SimpleType::Int) => self.types.int.into(),
      Type::Simple(SimpleType::Bool) => self.types.bool.into(),
      Type::Simple(SimpleType::Float) => self.types.float.into(),
      Type::Simple(SimpleType::String) => self.types.string.into(),
      Type::Function(ts) => {
        let mut args = ts.clone();
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
      Type::Simple(SimpleType::Struct(s)) => self
        .structs
        .get(s.name.as_str())
        .unwrap()
        .1
        .ptr_type(AddressSpace::default())
        .into(),
    }
  }

  pub fn compile_module(&mut self, m: Mod) {
    for s in m.imports.structs {
      self.compile_struct(s)
    }

    for fd in m.imports.fundecls {
      self.compile_import_fun(fd)
    }

    for v in m.imports.vals {
      self.compile_import_val(v)
    }

    self.module.set_name(m.name.as_str());
    let mut module_name = m.name.clone();
    module_name.push_str(".ir");
    self.module.set_source_file_name(module_name.as_str());

    for d in m.decls.into_iter() {
      self.compile_decl(d);
    }
  }

  fn compile_decl(&mut self, d: Decl) {
    match d {
      Decl::Fun(f) => {
        self.compile_fun(f, Vec::new());
      }
      Decl::Struct(s) => {
        self.compile_struct(s);
      }
      Decl::Val(v) => {
        self.compile_val(v);
      }
      Decl::Import(_) => {
        // imports are compiled before all declarations
      }
    }
  }

  fn compile_import_fun(&mut self, f: FunSig) {
    let (arg_names, arg_types) = &f.args
            .clone()
            .into_iter()
            .map(|(n, t)| (n, self.get_type(&t).into()))
            .unzip::<String, BasicMetadataTypeEnum, Vec<String>, Vec<BasicMetadataTypeEnum>>();

    let fn_type = self.get_type(&f.rt).fn_type(&arg_types[..], false);

    let function = self.module.add_function(f.name.as_str(), fn_type, None);

    for (n, p) in arg_names.into_iter().zip(function.get_param_iter()) {
      p.set_name(n.as_str());
    }

    self.functions.insert(f.name, function);
  }

  fn compile_fun(
    &mut self,
    f: Fun,
    closure: Vec<(String, Type)>,
  ) -> Value<'ctx> {
    // prepare function type with closure

    let (arg_names, arg_types) = &f.sig.args
            .clone()
            .into_iter()
            .chain(closure.clone())
            .map(|(n, t)| (n, self.get_type(&t).into()))
            .unzip::<String, BasicMetadataTypeEnum, Vec<String>, Vec<BasicMetadataTypeEnum>>();

    let fn_type = self.get_type(&f.sig.rt).fn_type(&arg_types[..], false);

    // create function value

    let function =
      self.module.add_function(f.sig.name.as_str(), fn_type, None);

    // add names to parameters

    let basic_block = self.context.append_basic_block(function, "entry");
    let parent_basic_block = self.parent_basic_block.clone();
    self.builder.position_at_end(basic_block);

    for (n, p) in arg_names.into_iter().zip(function.get_param_iter()) {
      p.set_name(n.as_str());
      if closure.clone().into_iter().any(|(n1, _)| &n1 == n) {
        self.closure.insert(n.clone(), p);
      } else {
        self.values.insert(n.clone(), p);
      }
    }

    // register closure

    self.closures.insert(
      f.sig.name.clone(),
      closure.into_iter().map(|(n, _)| n).collect(),
    );

    self.parent_basic_block = Some(basic_block);

    self.functions.insert(f.sig.name, function);
    self.current_function = Some(function);

    let value = self.compile_expr(f.body);

    self.builder.position_at_end(basic_block);
    self.builder.build_return(Some(&value));
    if let Some(pbb) = parent_basic_block {
      self.builder.position_at_end(pbb)
    };

    function
      .as_global_value()
      .as_pointer_value()
      .as_basic_value_enum()
  }

  fn compile_struct(&mut self, struct_: Struct) {
    let name = struct_.clone().name;
    let args = struct_
      .clone()
      .args
      .into_iter()
      .map(|(_, t)| self.get_type(&t))
      .collect::<Vec<BasicTypeEnum>>();

    let s = self.context.opaque_struct_type(name.as_str());
    s.set_body(&args[..], true);

    self.structs.insert(name, (struct_.clone(), s));
  }

  fn compile_import_val(&mut self, value: ValDecl) {
    let f = FunSig {
      name: value.name,
      args: Vec::new(),
      rt: value.t,
    };
    self.compile_import_fun(f);
  }

  fn compile_val(&mut self, value: Val) {
    let f = Fun {
      sig: FunSig {
        name: value.name,
        args: Vec::new(),
        rt: value.t,
      },
      body: value.expr,
    };
    self.compile_fun(f, Vec::new());
  }

  fn compile_expr(&mut self, e: Expr) -> Value<'ctx> {
    match e {
      Expr::BinOp(lh, op, rh) => self.compile_binop(*lh, op, *rh),
      Expr::Lit(l) => self.compile_lit(l),
      Expr::Value(name) => self.compile_value(name),
      Expr::Assign((name, type_), val) => {
        self.compile_assign(name, type_, *val)
      }
      Expr::Chain(lh, rh) => self.compile_chain(*lh, *rh),
      Expr::Function(f, closure) => self.compile_fun(*f, closure),
      Expr::Call(name, args) => self.compile_call(name, args),
      Expr::If(be, e1, e2) => self.compile_if(*be, *e1, *e2),
      Expr::Attr(name, t, attr) => self.compile_attr(name, t, attr),
      Expr::New(name, args) => self.compile_new(name, args),
      Expr::StringTemplate(template, args) => {
        self.compile_string_template(template, args)
      }
    }
  }

  fn compile_binop(&mut self, lh: Expr, op: Op, rh: Expr) -> Value<'ctx> {
    let lh = self.compile_expr(lh);
    let rh = self.compile_expr(rh);
    match (lh.get_type(), op) {
      (BasicTypeEnum::IntType(i), op) if i == self.types.bool => {
        let lh = lh.into_int_value();
        let rh = rh.into_int_value();
        let result = match op {
          Op::And => self.builder.build_and(lh, rh, "and"),
          Op::Or => self.builder.build_or(lh, rh, "and"),
          _ => panic!("Invalid state"),
        };
        result.as_basic_value_enum()
      }
      (BasicTypeEnum::IntType(_), op) => {
        let lh = lh.into_int_value();
        let rh = rh.into_int_value();
        let result = match op {
          Op::Add => self.builder.build_int_add(lh, rh, "add"),
          Op::Sub => self.builder.build_int_sub(lh, rh, "sub"),
          Op::Mul => self.builder.build_int_mul(lh, rh, "mul"),
          Op::Div => self.builder.build_int_signed_div(lh, rh, "div"),
          Op::Lt => self.builder.build_int_compare(SLT, lh, rh, "ilt"),
          Op::Le => self.builder.build_int_compare(SLE, lh, rh, "ile"),
          Op::Ne => self.builder.build_int_compare(NE, lh, rh, "ine"),
          Op::Eq => self.builder.build_int_compare(EQ, lh, rh, "ieq"),
          Op::Ge => self.builder.build_int_compare(SGE, lh, rh, "ige"),
          Op::Gt => self.builder.build_int_compare(SGT, lh, rh, "igt"),
          Op::And | Op::Or => panic!("Invalid state"),
        };
        result.as_basic_value_enum()
      }
      (
        BasicTypeEnum::FloatType(_),
        op @ (Op::Add | Op::Sub | Op::Mul | Op::Div),
      ) => {
        let lh = lh.into_float_value();
        let rh = rh.into_float_value();
        let result = match op {
          Op::Add => self.builder.build_float_add(lh, rh, "fadd"),
          Op::Sub => self.builder.build_float_sub(lh, rh, "fsub"),
          Op::Mul => self.builder.build_float_mul(lh, rh, "fmul"),
          Op::Div => self.builder.build_float_div(lh, rh, "fdiv"),
          _ => panic!("Invalid state"),
        };
        result.as_basic_value_enum()
      }

      (BasicTypeEnum::FloatType(_), op) => {
        let lh = lh.into_float_value();
        let rh = rh.into_float_value();
        let result = match op {
          Op::Lt => self.builder.build_float_compare(OLT, lh, rh, "flt"),
          Op::Le => self.builder.build_float_compare(OLE, lh, rh, "fle"),
          Op::Ne => self.builder.build_float_compare(ONE, lh, rh, "fne"),
          Op::Eq => self.builder.build_float_compare(OEQ, lh, rh, "feq"),
          Op::Ge => self.builder.build_float_compare(OGE, lh, rh, "fge"),
          Op::Gt => self.builder.build_float_compare(OGT, lh, rh, "fgt"),
          _ => panic!("Invalid state"),
        };
        result.as_basic_value_enum()
      }
      _ => panic!("Invalid state"),
    }
  }

  fn compile_lit(&self, l: Lit) -> Value<'ctx> {
    match l {
      Lit::Int(i) => self
        .types
        .int
        .const_int(i as u64, false)
        .as_basic_value_enum(),
      Lit::Bool(b) => self
        .types
        .bool
        .const_int(b.into(), false)
        .as_basic_value_enum(),
      Lit::Float(f) => self
        .types
        .float
        .const_float(f.parse().unwrap())
        .as_basic_value_enum(),
      Lit::String(s) => self
        .builder
        .build_global_string_ptr(&s, "")
        .as_basic_value_enum(),
    }
  }

  fn compile_value(&self, name: String) -> Value<'ctx> {
    if let Some(value) = self.closure.get(name.as_str()) {
      return value.clone();
    }

    if let Some(value) = self.values.get(name.as_str()) {
      return value.clone();
    }

    if let Some(f) = self.functions.get(name.as_str()) {
      return self
        .builder
        .build_call(f.clone(), &[], "call")
        .try_as_basic_value()
        .left()
        .unwrap();
    }

    panic!("unknown value {name}")
  }

  fn compile_assign(
    &mut self,
    name: String,
    _: Type,
    val: Expr,
  ) -> Value<'ctx> {
    let val = self.compile_expr(val);
    self.values.insert(name, val);
    val
  }

  fn compile_chain(&mut self, lh: Expr, rh: Expr) -> Value<'ctx> {
    self.compile_expr(lh);
    self.compile_expr(rh)
  }

  fn compile_call(&mut self, name: String, args: Vec<Expr>) -> Value<'ctx> {
    let f = *self.functions.get(name.as_str()).unwrap();
    let closure = self
      .closures
      .get(name.as_str())
      .unwrap_or(&Vec::new())
      .clone();

    let standard_args = args
      .into_iter()
      .map(|a| self.compile_expr(a).into())
      .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();

    let closure = closure
      .into_iter()
      .map(|n| (*self.values.get(n.as_str()).unwrap()).into())
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

    self
      .builder
      .build_call(f, &args[..], "call")
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

  fn compile_if(&mut self, be: Expr, e1: Expr, e2: Expr) -> Value<'ctx> {
    let be = self.compile_expr(be);
    let e1 = self.compile_expr(e1);
    let e2 = self.compile_expr(e2);
    self
      .builder
      .build_select(be.into_int_value(), e1, e2, "select")
  }

  fn compile_attr(
    &self,
    name: String,
    t: Struct,
    attr: String,
  ) -> Value<'ctx> {
    let ptr = self.values.get(&*name).unwrap().into_pointer_value();

    let (i, (_, t)) = t
      .args
      .into_iter()
      .enumerate()
      .find(|(_, (a, _))| a == &attr)
      .unwrap();
    let attr_ptr = self
      .builder
      .build_struct_gep(ptr, i as u32, "attr")
      .unwrap();

    match t {
      Type::Function(_) | Type::Simple(SimpleType::Struct(_)) => {
        attr_ptr.as_basic_value_enum()
      }
      _ => self.builder.build_load(attr_ptr, "load"),
    }
  }

  fn compile_new(&mut self, name: String, args: Vec<Expr>) -> Value<'ctx> {
    let structs = self.structs.clone();
    let (_, struct_type) = structs.get(name.as_str()).unwrap();

    let struct_args = args
      .into_iter()
      .map(|a| self.compile_expr(a))
      .collect::<Vec<_>>();

    let struct_value = struct_type.const_named_struct(&struct_args);

    let struct_ptr =
      self.builder.build_malloc(*struct_type, "malloc").unwrap();

    self.builder.build_store(struct_ptr, struct_value);

    struct_ptr.as_basic_value_enum()
  }

  fn compile_string_template(
    &mut self,
    template: String,
    values: Vec<String>,
  ) -> Value<'ctx> {
    let mut values: Vec<BasicMetadataValueEnum<'ctx>> = values
      .into_iter()
      .map(|value| self.compile_value(value).into())
      .collect::<Vec<_>>();

    let template_value = self
      .builder
      .build_global_string_ptr(&template, "str_tmpl")
      .as_basic_value_enum();

    let template_value_result = self
      .builder
      .build_global_string_ptr("", "str_tmpl_rstl")
      .as_basic_value_enum();

    let mut sprintf_args =
      Vec::from([template_value_result.clone().into(), template_value.into()]);
    sprintf_args.append(&mut values);

    let sprintf = self.functions.get("sprintf").unwrap();

    self
      .builder
      .build_call(*sprintf, &sprintf_args[..], "run_str_tmpl")
      .try_as_basic_value()
      .left()
      .unwrap();

    template_value_result
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
  fn codegen(context: &'static Context) -> CodeGen<'static> {
    let codegen = CodeGen::create(&context, false);
    codegen
  }

  #[rstest]
  fn test_1(mut codegen: CodeGen) {
    // given
    let m = Mod {
      name: "test_1".to_string(),
      decls: Vec::from([
        Decl::Struct(Struct {
          name: "Foo".to_string(),
          args: Vec::from([
            ("a".to_string(), Type::Simple(SimpleType::Int)),
            ("b".to_string(), Type::Simple(SimpleType::Bool)),
            ("c".to_string(), Type::Simple(SimpleType::String)),
          ]),
        }),
        Decl::Fun(Fun {
          sig: FunSig {
            name: "f1".to_string(),
            args: Vec::from([(
              "a".to_string(),
              Type::Simple(SimpleType::Struct(Struct {
                name: "Foo".to_string(),
                args: Vec::from([
                  ("a".to_string(), Type::Simple(SimpleType::Int)),
                  ("b".to_string(), Type::Simple(SimpleType::Bool)),
                  ("c".to_string(), Type::Simple(SimpleType::String)),
                ]),
              })),
            )]),
            rt: Type::Simple(SimpleType::Int),
          },
          body: Expr::Lit(Lit::Int(32)),
        }),
      ]),
      imports: Imports::default(),
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
    codegen.compile_module(m);

    // then
    assert_eq!(codegen.module.to_string(), expected_module_str);
  }
}
