use crate::ast::{
    Decl, Expr, Fun, Lit, Op, Struct, Type, TypedDecl, TypedExpr, TypedFun,
    TypedMod, TypedStruct,
};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{
    BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType,
    PointerType, StructType,
};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use std::collections::HashMap;
use std::error::Error;
use FloatPredicate::{OEQ, OGE, OGT, OLE, OLT, ONE};
use IntPredicate::{EQ, NE, SGE, SGT, SLE, SLT};

pub struct Types<'ctx> {
    pub int: IntType<'ctx>,
    pub float: FloatType<'ctx>,
    pub bool: IntType<'ctx>,
    pub string: PointerType<'ctx>,
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub parent_basic_block: Option<BasicBlock<'ctx>>,
    pub types: Types<'ctx>,
    pub structs: HashMap<String, StructType<'ctx>>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    pub values: HashMap<String, BasicValueEnum<'ctx>>,
    pub closures: HashMap<String, Vec<(String, BasicValueEnum<'ctx>)>>,
    pub current_function: Option<FunctionValue<'ctx>>,
}

pub struct CompError {
    message: String,
}

impl CompError {
    fn from_message<V>(message: String) -> Result<V, Self> {
        Err(Self { message })
    }
}

type Value<'ctx> = BasicValueEnum<'ctx>;

impl<'ctx> CodeGen<'ctx> {
    pub fn create(
        context: &'ctx Context,
        add_stdlib: bool,
    ) -> Result<Self, Box<dyn Error>> {
        let module = context.create_module("sum");
        let types = Types {
            int: context.i32_type(),
            float: context.f32_type(),
            bool: context.bool_type(),
            string: context.i8_type().ptr_type(AddressSpace::Generic),
        };
        let mut functions = HashMap::new();

        if add_stdlib {
            let puts = module.add_function(
                "puts",
                context.i32_type().fn_type(
                    &[BasicMetadataTypeEnum::from(
                        context.i8_type().ptr_type(AddressSpace::Generic),
                    )],
                    false,
                ),
                None,
            );

            functions.insert("puts".to_string(), puts);
        };

        Ok(Self {
            context,
            module,
            builder: context.create_builder(),
            types,
            parent_basic_block: None,
            structs: HashMap::new(),
            functions,
            values: HashMap::new(),
            closures: HashMap::new(),
            current_function: None,
        })
    }

    fn get_type(&self, t: &Type) -> BasicTypeEnum<'ctx> {
        match t {
            Type::Int => self.types.int.into(),
            Type::Bool => self.types.bool.into(),
            Type::Float => self.types.float.into(),
            Type::String => self.types.string.into(),
            Type::Fun(args, rt) => {
                let rt = self.get_type(rt);
                let args = args
                    .into_iter()
                    .map(|a| self.get_type(a).into())
                    .collect::<Vec<BasicMetadataTypeEnum>>();
                let ft = match rt {
                    BasicTypeEnum::ArrayType(at) => {
                        at.fn_type(&args[..], false)
                    }
                    BasicTypeEnum::FloatType(ft) => {
                        ft.fn_type(&args[..], false)
                    }
                    BasicTypeEnum::IntType(it) => it.fn_type(&args[..], false),
                    BasicTypeEnum::PointerType(pt) => {
                        pt.fn_type(&args[..], false)
                    }
                    BasicTypeEnum::StructType(st) => {
                        st.fn_type(&args[..], false)
                    }
                    BasicTypeEnum::VectorType(vt) => {
                        vt.fn_type(&args[..], false)
                    }
                };

                ft.ptr_type(AddressSpace::Const).into()
            }
            Type::Struct(s) => self
                .structs
                .get(&*s.name)
                .unwrap()
                .ptr_type(AddressSpace::Generic)
                .into(),
        }
    }

    pub fn compile_module(
        &mut self,
        m: TypedMod,
    ) -> Result<(), Box<dyn Error>> {
        self.module.set_name(m.name.as_str());
        let mut module_name = m.name.clone();
        module_name.push_str(".ir");
        self.module.set_source_file_name(module_name.as_str());

        for d in m.decls.into_iter() {
            self.compile_decl(d);
        }

        Ok(())
    }

    fn compile_decl(&mut self, d: TypedDecl) {
        match d {
            Decl::Fun(f) => {
                self.compile_fun(f, Vec::new());
            }
            Decl::Struct(s) => {
                self.compile_struct(s);
            }
        }
    }

    fn compile_fun(
        &mut self,
        f: TypedFun,
        closure: Vec<(String, Type)>,
    ) -> Value<'ctx> {
        // prepare function type with closure

        let (arg_names, arg_types) = &f.args
            .clone()
            .into_iter()
            .chain(closure.clone())
            .map(|(n, t)| (n, self.get_type(&t).into()))
            .unzip::<String, BasicMetadataTypeEnum, Vec<String>, Vec<BasicMetadataTypeEnum>>();
        //.collect::<Vec<BasicMetadataTypeEnum>>();

        let fn_type = self.get_type(&f.rt).fn_type(&arg_types[..], false);

        // create function value

        let function =
            self.module.add_function(f.name.as_str(), fn_type, None);

        // add names to parameters

        let basic_block = self.context.append_basic_block(function, "entry");
        let parent_basic_block = self.parent_basic_block.clone();
        self.builder.position_at_end(basic_block);

        for ((n, t), p) in f.args.into_iter().zip(function.get_param_iter()) {
            p.set_name(n.as_str());
            let alloca = self
                .builder
                .build_alloca::<BasicTypeEnum>(self.get_type(&t), "alloca");
            self.builder.build_store(alloca, p);
            self.values.insert(n.clone(), p);
        }

        let closure = closure
            .into_iter()
            .map(|(n, _)| -> (String, BasicValueEnum) {
                (n.clone(), *self.values.get(&*n).unwrap())
            })
            .collect();
        // register closure

        self.closures.insert(f.name.clone(), closure);

        self.parent_basic_block = Some(basic_block);

        self.functions.insert(f.name, function);
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

    fn compile_struct(&mut self, struct_: TypedStruct) {
        let name = struct_.name;
        let args = struct_
            .args
            .into_iter()
            .map(|(_, t)| self.get_type(&t))
            .collect::<Vec<BasicTypeEnum>>();

        let s = self.context.opaque_struct_type(name.as_str());
        s.set_body(&args[..], true);

        self.structs.insert(name, s);
    }

    fn compile_expr(&mut self, e: TypedExpr) -> Value<'ctx> {
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
        }
    }

    fn compile_binop(
        &mut self,
        lh: TypedExpr,
        op: Op,
        rh: TypedExpr,
    ) -> Value<'ctx> {
        let lh = self.compile_expr(lh);
        let rh = self.compile_expr(rh);
        match (lh.get_type(), op) {
            (BasicTypeEnum::IntType(i), op) if i == self.types.bool => {
                let lh = lh.into_int_value();
                let rh = rh.into_int_value();
                let result = match op {
                    Op::And => self.builder.build_and(lh, rh, "and"),
                    Op::Or => self.builder.build_or(lh, rh, "and"),
                    _ => panic!(""),
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
                    Op::Div => {
                        self.builder.build_int_signed_div(lh, rh, "div")
                    }
                    Op::Lt => {
                        self.builder.build_int_compare(SLT, lh, rh, "ilt")
                    }
                    Op::Le => {
                        self.builder.build_int_compare(SLE, lh, rh, "ile")
                    }
                    Op::Ne => {
                        self.builder.build_int_compare(NE, lh, rh, "ine")
                    }
                    Op::Eq => {
                        self.builder.build_int_compare(EQ, lh, rh, "ieq")
                    }
                    Op::Ge => {
                        self.builder.build_int_compare(SGE, lh, rh, "ige")
                    }
                    Op::Gt => {
                        self.builder.build_int_compare(SGT, lh, rh, "igt")
                    }
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
                    Op::Lt => {
                        self.builder.build_float_compare(OLT, lh, rh, "flt")
                    }
                    Op::Le => {
                        self.builder.build_float_compare(OLE, lh, rh, "fle")
                    }
                    Op::Ne => {
                        self.builder.build_float_compare(ONE, lh, rh, "fne")
                    }
                    Op::Eq => {
                        self.builder.build_float_compare(OEQ, lh, rh, "feq")
                    }
                    Op::Ge => {
                        self.builder.build_float_compare(OGE, lh, rh, "fge")
                    }
                    Op::Gt => {
                        self.builder.build_float_compare(OGT, lh, rh, "fgt")
                    }
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
        *self
            .values
            .get(name.as_str())
            .unwrap_or_else(|| panic!("unknown value {name}"))
    }

    fn compile_assign(
        &mut self,
        name: String,
        type_: Type,
        val: TypedExpr,
    ) -> Value<'ctx> {
        let val = self.compile_expr(val);
        self.values.insert(name, val);
        val
    }

    fn compile_chain(&mut self, lh: TypedExpr, rh: TypedExpr) -> Value<'ctx> {
        self.compile_expr(lh);
        self.compile_expr(rh)
    }

    fn compile_call(
        &mut self,
        name: String,
        args: Vec<TypedExpr>,
    ) -> Value<'ctx> {
        let f = *self.functions.get(name.as_str()).unwrap();
        let closure = self.closures.get(&*name).unwrap().clone();
        let standard_args =
            args.into_iter().map(|a| self.compile_expr(a).into());
        //.collect::<Vec<BasicMetadataValueEnum<'ctx>>>();
        let closure = closure.into_iter().map(|(_, c)| c.into());

        let args = standard_args
            .chain(closure)
            .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();

        self.builder
            .build_call(f, &args[..], "call")
            .try_as_basic_value()
            .left()
            .unwrap()
    }

    fn compile_if(
        &mut self,
        be: TypedExpr,
        e1: TypedExpr,
        e2: TypedExpr,
    ) -> Value<'ctx> {
        let be = self.compile_expr(be);
        let e1 = self.compile_expr(e1);
        let e2 = self.compile_expr(e2);
        self.builder
            .build_select(be.into_int_value(), e1, e2, "select")
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::codegen::*;
    use rstest::*;

    #[rstest]
    fn test_1() {
        // given
        let context = Context::create();
        let mut codegen = CodeGen::create(&context, false).unwrap();

        let m = Mod {
            name: "test_1".to_string(),
            decls: Vec::from([
                Decl::Struct(Struct {
                    name: "Foo".to_string(),
                    args: Vec::from([
                        ("a".to_string(), Type::Int),
                        ("b".to_string(), Type::Bool),
                        ("c".to_string(), Type::String),
                    ]),
                }),
                Decl::Fun(Fun {
                    name: "f1".to_string(),
                    args: Vec::from([(
                        "a".to_string(),
                        Type::Struct(Struct {
                            name: "Foo".to_string(),
                            args: Vec::from([
                                ("a".to_string(), Type::Int),
                                ("b".to_string(), Type::Bool),
                                ("c".to_string(), Type::String),
                            ]),
                        }),
                    )]),
                    rt: Type::Int,
                    body: Expr::Lit(Lit::Int(32)),
                }),
            ]),
        };
        let expected_module_str = r#"; ModuleID = 'test_1'
source_filename = "test_1.ir"

%Foo = type <{ i32, i1, i8* }>

define i32 @f1(%Foo* %a) {
entry:
  ret i32 32
}
"#;

        // when
        codegen.compile_module(m).unwrap();

        // then
        // TODO: fix
        // assert_eq!(codegen.module.to_string(), expected_module_str)
    }
}
