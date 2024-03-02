mod closure_manager;
pub mod instantiate;
mod namespace;

use anyhow::{anyhow, bail, ensure, Result};
use std::collections::HashMap;

use closure_manager::ClosureManager;
use namespace::Namespace;
use regex::Captures;

use crate::library::typecheck::instantiate::Instantiate;

use crate::library::{
  ast::{qualified, typed, *},
  utils::STRING_TEMPLATE_RE,
};

use crate::library::utils::MethodNamer;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedValue<V, T> {
  v: V,
  t: T,
}

impl<V, T> TypedValue<V, T> {
  fn get(v: V, t: T) -> CheckResult<V, T> {
    Ok(Self { v, t })
  }
}

type ValueResult<V> = Result<V>;
type CheckResult<V, T = typed::Type> = Result<TypedValue<V, T>>;

pub struct TypeChecker {
  values: HashMap<String, typed::Type>,
  functions: HashMap<String, (Vec<typed::Type>, typed::Type)>,
  types: HashMap<String, Vec<(String, typed::Type)>>,
  classes: HashMap<String, typed::Class>,
  methods: HashMap<String, typed::FunSig>,
  closure: Vec<(String, typed::Type)>,
  closure_manager: ClosureManager,
  namespace: Namespace,
  module: qualified::Mod,
  type_impl: Option<SimpleType>,
}

impl TypeChecker {
  pub fn create(m: qualified::Mod) -> Self {
    Self {
      values: HashMap::new(),
      functions: HashMap::from([(
        "puts".to_string(),
        (
          Vec::from([Type::Simple(SimpleType::String)]),
          Type::Simple(SimpleType::Int),
        ),
      )]),
      types: HashMap::new(),
      classes: HashMap::new(),
      methods: HashMap::new(),
      closure: Vec::new(),
      closure_manager: ClosureManager::new(),
      namespace: Namespace::new(),
      module: m,
      type_impl: None,
    }
  }

  pub fn create_empty() -> Self {
    Self::create(Mod::default())
  }

  pub fn get_type_of_expr(&mut self, expr: qualified::Expr) -> Result<(typed::Expr, typed::Type)> {
    self.typecheck_expr(expr).map(|tv| (tv.v, tv.t))
  }

  fn get_struct(&self, name: String) -> Result<typed::Struct> {
    if let Some(args) = self.types.get(name.as_str()) {
      return Ok(Struct {
        name: name.to_string(),
        args: args
          .iter()
          .cloned()
          .map(|(name, type_)| typed::StructArg { name, type_ })
          .collect(),
      });
    }

    if let Some(Decl::Struct(s)) = self.module.imports.iter().find(|s| s.get_name() == &name) {
      return Ok(s.clone());
    }

    bail!("Cant find struct {}", name)
  }

  fn get_function(&self, name: String) -> Result<(Vec<typed::Type>, typed::Type)> {
    if let Some(f) = self.functions.get(name.as_str()) {
      return Ok(f.clone());
    }

    if let Some(Decl::Fun(f)) = self.module.imports.iter().find(|vd| vd.get_name() == &name) {
      let arg_types = f.args.clone().into_iter().map(|a| a.type_).collect();
      let rt = f.rt.clone();
      return Ok((arg_types, rt));
    }

    bail!("Cant find function {}", name)
  }

  fn get_value(&self, name: String) -> Result<typed::Type> {
    if let Some(ts) = self.values.get(name.as_str()) {
      return Ok(ts.clone());
    }

    if let Some(Decl::Val(ts)) = self.module.imports.iter().find(|i| i.get_name() == &name) {
      return Ok(ts.t.clone());
    }

    bail!("Cant find value {}", name)
  }

  fn get_class(&self, name: &str) -> Result<typed::Class> {
    if let Some(c) = self.classes.get(name) {
      return Ok(c.clone());
    }

    if let Some(c) = self
      .module
      .imports
      .iter()
      .filter_map(|d| match d {
        Decl::Class(c) => Some(c),
        _ => None,
      })
      .find(|c| c.name.as_str() == name)
    {
      return Ok(c.clone());
    }

    bail!("Cant find class {}", name)
  }

  fn get_impl_method(
    &self,
    this: TypedValue<typed::Expr, typed::Type>,
    method: String,
  ) -> Result<typed::FunSig> {
    let Type::Simple(t) = &this.t else {
      bail!("Functional types cannot have methods: {:?}", this.t);
    };

    if let Some(m) = self.methods.get(t.get_method_name(&method).as_str()) {
      return Ok(m.clone());
    };

    if let Some((_, m)) = self
      .module
      .imports
      .iter()
      .filter_map(|d| match d {
        Decl::Impl(i) => Some(i),
        _ => None,
      })
      .flat_map(|i| i.impls.iter().map(|e| (&i.t, e)))
      .find(|(i_t, i_fs)| i_t == &t && i_fs.name == method)
    {
      return Ok(m.clone());
    };

    bail!("No method `{}` defined for type `{:?}`.", method, this.t,);
  }

  fn get_simple_type(&self, t: String) -> Result<SimpleType> {
    match t.as_str() {
      "Int" => Ok(SimpleType::Int),
      "Bool" => Ok(SimpleType::Bool),
      "Float" => Ok(SimpleType::Float),
      "String" => Ok(SimpleType::String),
      "This" => Ok(SimpleType::This),
      name => {
        let s = self.get_struct(name.to_string())?;
        Ok(SimpleType::Struct(s))
      }
    }
  }

  fn get_type(&self, s: qualified::Type) -> Result<typed::Type> {
    match s {
      Type::Function(ts) => {
        let ts = ts
          .into_iter()
          .map(|t| self.get_type(t))
          .collect::<Result<Vec<_>, _>>()?;
        Ok(Type::Function(ts))
      }
      Type::Simple(s) => Ok(Type::Simple(self.get_simple_type(s)?)),
    }
  }

  pub fn check(&mut self) -> Result<typed::Mod> {
    let name = self.module.name.clone();
    let defs = self.typecheck_defs(self.module.defs.clone())?;
    let imports = self.module.imports.clone();

    Ok(Mod {
      name,
      defs,
      imports,
    })
  }

  pub fn typecheck_defs(&mut self, ds: Vec<qualified::Def>) -> Result<Vec<typed::Def>> {
    ds.into_iter()
      .map(|d| self.typecheck_def(d))
      .collect::<Result<_, _>>()
  }

  fn typecheck_def(&mut self, d: qualified::Def) -> ValueResult<typed::Def> {
    match d {
      Def::Fun(f) => self
        .typecheck_fun(Function { f, closure: () })
        .map(|tv| Def::Fun(tv.v.f)),
      Def::Struct(s) => self.typecheck_struct(s).map(Def::Struct),
      Def::Val(v) => self.typecheck_val(v).map(Def::Val),
      Def::Import(i) => self.typecheck_import(i).map(Def::Import),
      Def::Class(c) => self.typecheck_class(c).map(Def::Class),
      Def::Impl(i) => self.typecheck_impl(i).map(Def::Impl),
    }
  }

  fn typecheck_funsig(
    &mut self,
    fs: qualified::FunSig,
    sig_only: bool,
  ) -> CheckResult<typed::FunSig> {
    let name = self.namespace.create_qualified_name(fs.name);
    let args = self.typecheck_fun_args(fs.args)?;
    let rt = self.get_type(fs.rt)?;

    if sig_only {
      self.namespace.drop_qualified_name();
    }

    let mut t = args.t;
    t.push(rt.clone());

    TypedValue::get(
      FunSig {
        name,
        args: args.v,
        rt,
      },
      Type::Function(t),
    )
  }

  fn typecheck_fun(&mut self, f: qualified::Function) -> CheckResult<typed::Function> {
    let TypedValue { v: sig, t } = self.typecheck_funsig(f.f.sig, false)?;

    sig.args.iter().for_each(|arg| {
      self.values.insert(arg.name.clone(), arg.type_.clone());
    });

    self.functions.insert(
      sig.name.clone(),
      (
        sig.args.clone().into_iter().map(|arg| arg.type_).collect(),
        (sig.rt.clone()),
      ),
    );

    let body = self.typecheck_expr(f.f.body)?;

    self.namespace.drop_qualified_name();

    ensure!(
      sig.rt == body.t,
      "Return type and body type does not match: {:?} != {:?}",
      sig.rt,
      body.t,
    );

    Ok(TypedValue {
      v: Function {
        f: Fun { sig, body: body.v },
        closure: Vec::new(),
      },
      t,
    })
  }

  fn typecheck_struct(&mut self, struct_: qualified::Struct) -> ValueResult<typed::Struct> {
    let name = struct_.name;
    let args = struct_
      .args
      .into_iter()
      .map(|arg| {
        self.get_type(arg.type_).map(|type_| StructArg {
          name: arg.name,
          type_,
        })
      })
      .collect::<Result<Vec<_>, _>>()?;

    self.types.insert(
      name.clone(),
      args
        .iter()
        .cloned()
        .map(|arg| (arg.name, arg.type_))
        .collect(),
    );

    let struct_ = Struct { name, args };

    Ok(struct_)
  }

  fn typecheck_val(&mut self, value: qualified::Val) -> ValueResult<typed::Val> {
    let name = value.name;
    let t = self.get_type(value.t)?;
    let expr = self.typecheck_expr(value.expr)?;

    if t != expr.t {
      bail!(
        "Declared type and value type does not match: {:?} != {:?}",
        t,
        expr.t,
      );
    }

    self.values.insert(name.clone(), expr.t.clone());

    Ok(Val {
      name,
      t,
      expr: expr.v,
    })
  }

  fn typecheck_import(&mut self, import_: qualified::Import) -> ValueResult<typed::Import> {
    Ok(import_)
  }

  fn typecheck_class(&mut self, class: qualified::Class) -> ValueResult<typed::Class> {
    let name = class.name.clone();
    let methods = class
      .methods
      .into_iter()
      .map(|m| self.typecheck_funsig(m, true).map(|m| m.v))
      .collect::<Result<_, _>>()?;

    let class = Class { name, methods };

    self.classes.insert(class.name.clone(), class.clone());

    Ok(class)
  }

  fn typecheck_impl(&mut self, impl_: qualified::Impl) -> ValueResult<typed::Impl> {
    let class_name = impl_.class_name.clone();
    // TODO: this should always be SimpleType::Struct
    let t = self.get_simple_type(impl_.t.clone())?;
    self.type_impl = Some(t.clone());

    let impls: Vec<typed::Fun> = impl_
      .impls
      .into_iter()
      .map(|i| {
        self
          .typecheck_fun(Function { f: i, closure: () })
          .map(|i| i.v.f)
      })
      .collect::<Result<_, _>>()?;

    let Ok(class) = self.get_class(class_name.as_str()) else {
      bail!("No class with name {}", class_name);
    };

    let mut class_sigs = class.methods;
    class_sigs.sort_by_key(|m| m.name.clone());

    let mut impl_sigs = impls.clone().into_iter().map(|i| i.sig).collect::<Vec<_>>();
    impl_sigs.sort_by_key(|m| m.name.clone());

    if class_sigs != impl_sigs {
      bail!("Wrong implementations: {:?}, {:?}", class_sigs, impl_sigs,);
    };

    for i in impls.clone() {
      self.methods.insert(t.get_method_name(&i.sig.name), i.sig);
    }

    self.type_impl = None;

    Ok(
      Impl {
        class_name,
        t,
        impls,
      }
      .instantiate(),
    )
  }

  fn typecheck_fun_args(
    &mut self,
    args: Vec<qualified::FunArg>,
  ) -> CheckResult<Vec<typed::FunArg>, Vec<typed::Type>> {
    let v = args
      .into_iter()
      .map(|arg| {
        self.get_type(arg.type_).map(|type_| FunArg {
          name: arg.name,
          type_,
        })
      })
      .collect::<Result<Vec<_>, _>>()?;

    self.closure_manager.append(v.clone());

    let t = v.clone().into_iter().map(|arg| arg.type_).collect();

    Ok(TypedValue { v, t })
  }

  fn typecheck_expr(&mut self, expr: qualified::Expr) -> CheckResult<typed::Expr> {
    match expr {
      Expr::BinOp(binop) => {
        let TypedValue { v, t } = self.typecheck_binop(*binop)?;
        TypedValue::get(Expr::BinOp(Box::new(v)), t)
      }
      Expr::Lit(l) => {
        let TypedValue { v, t } = self.typecheck_lit(l)?;
        TypedValue::get(Expr::Lit(v), t)
      }
      Expr::Value(name) => {
        let TypedValue { v, t } = self.typecheck_value(name)?;
        TypedValue::get(Expr::Value(v), t)
      }
      Expr::Assign(assign) => {
        let TypedValue { v, t } = self.typecheck_assign(*assign)?;
        TypedValue::get(Expr::Assign(Box::new(v)), t)
      }
      Expr::Chain(chain) => {
        let TypedValue { v, t } = self.typecheck_chain(*chain)?;
        TypedValue::get(Expr::Chain(Box::new(v)), t)
      }
      Expr::Function(function) => {
        let closure = self.closure_manager.clone();
        self.closure_manager.push_layer();
        let mut f = self.typecheck_fun(*function)?;
        self.closure_manager.pop_layer();
        f.v.closure = closure;
        TypedValue::get(Expr::Function(Box::new(f.v)), f.t)
      }
      Expr::Call(call) => self.typecheck_call(call),
      Expr::If(if_) => {
        let TypedValue { v, t } = self.typecheck_if(*if_)?;
        TypedValue::get(Expr::If(Box::new(v)), t)
      }
      Expr::Attr(attr) => {
        let TypedValue { v, t } = self.typecheck_attr(attr)?;
        TypedValue::get(Expr::Attr(v), t)
      }
      Expr::New(new) => {
        let TypedValue { v, t } = self.typecheck_new(new)?;
        TypedValue::get(Expr::New(v), t)
      }
      Expr::StringTemplate(stringtemplate) => {
        let TypedValue { v, t } = self.typecheck_string_template(stringtemplate)?;
        TypedValue::get(Expr::StringTemplate(v), t)
      }
      Expr::MethodCall(methodcall) => {
        let TypedValue { v, t } = self.typecheck_method_call(*methodcall)?;
        TypedValue::get(Expr::MethodCall(Box::new(v)), t)
      }
    }
  }

  fn typecheck_binop(&mut self, binop: qualified::BinOp) -> CheckResult<typed::BinOp> {
    let lh = self.typecheck_expr(binop.lh)?;
    let rh = self.typecheck_expr(binop.rh)?;

    ensure!(
      lh.t == rh.t,
      "Unequal binop types: {:?} != {:?}",
      lh.t,
      rh.t,
    );

    let (Type::Simple(lht), Type::Simple(rht)) = (lh.t.clone(), rh.t.clone()) else {
      bail!(
        "Cannot run binary operation on functions: lh: {:?}, rh: {:?}",
        lh.t,
        rh.t,
      );
    };

    match (lht, binop.op) {
      (SimpleType::Int | SimpleType::Float, op @ (Op::Add | Op::Sub | Op::Mul | Op::Div)) => {
        TypedValue::get(
          BinOp {
            lh: lh.v,
            op,
            rh: rh.v,
          },
          Type::Simple(rht),
        )
      }
      (
        SimpleType::Int | SimpleType::Float,
        op @ (Op::Lt | Op::Le | Op::Ne | Op::Eq | Op::Ge | Op::Gt),
      ) => TypedValue::get(
        BinOp {
          lh: lh.v,
          op,
          rh: rh.v,
        },
        Type::Simple(SimpleType::Bool),
      ),
      (SimpleType::Bool, op @ (Op::And | Op::Or)) => TypedValue::get(
        BinOp {
          lh: lh.v,
          op,
          rh: rh.v,
        },
        Type::Simple(rht),
      ),
      (lht, op) => Err(anyhow!(
        "Invalid operation type: {:?} {:?} {:?}",
        lht,
        op,
        rh.t
      )),
    }
  }

  fn typecheck_lit(&self, lit: Lit) -> CheckResult<typed::Lit> {
    let t = match lit {
      Lit::Int(_) => SimpleType::Int,
      Lit::Bool(_) => SimpleType::Bool,
      Lit::Float(_) => SimpleType::Float,
      Lit::String(_) => SimpleType::String,
    };

    TypedValue::get(lit, Type::Simple(t))
  }

  fn typecheck_value(&mut self, value: qualified::Value) -> CheckResult<typed::Value> {
    let type_ = self.get_value(value.name.clone())?;

    TypedValue::get(typed::Value { name: value.name }, type_)
  }

  fn typecheck_assign(&mut self, assign: qualified::Assign) -> CheckResult<typed::Assign> {
    let type_ = self.get_type(assign.type_)?;
    let expr = self.typecheck_expr(assign.expr)?;

    ensure!(
      type_ == expr.t,
      "Unequal assign types: {:?} != {:?}",
      type_,
      expr.t,
    );

    self.values.insert(assign.name.clone(), type_.clone());
    self
      .closure_manager
      .push(assign.name.clone(), type_.clone());

    TypedValue::get(
      Assign {
        name: assign.name,
        type_: type_.clone(),
        expr: expr.v,
      },
      type_,
    )
  }

  fn typecheck_chain(&mut self, chain: qualified::Chain) -> CheckResult<typed::Chain> {
    let lh = self.typecheck_expr(chain.e1)?;
    let rh = self.typecheck_expr(chain.e2)?;

    TypedValue::get(Chain { e1: lh.v, e2: rh.v }, rh.t)
  }

  fn typecheck_call(&mut self, call: qualified::Call) -> CheckResult<typed::Expr> {
    let (arg_values, arg_types): (Vec<typed::Expr>, Vec<typed::Type>) = call
      .args
      .into_iter()
      .map(|a| self.typecheck_expr(a))
      .collect::<Result<Vec<TypedValue<typed::Expr, typed::Type>>, _>>()?
      .into_iter()
      .map(|TypedValue { v, t }| (v, t))
      .unzip();
    let name = self.namespace.get_qualified_name(call.name);
    let (exp_arg_types, rt) = self.get_function(name.clone())?;

    let exp_no_of_args = exp_arg_types.len();
    let no_of_args = arg_types.len();

    if exp_no_of_args < no_of_args {
      bail!(
        "Invalid number of arguments: expected: {:?}, actual: {:?}",
        exp_arg_types.len(),
        arg_types.len()
      )
    }

    for (exp_arg_type, arg_type) in exp_arg_types.iter().zip::<Vec<typed::Type>>(arg_types) {
      ensure!(
        *exp_arg_type == arg_type,
        "Invalid argument type: {:?} != {:?}",
        exp_arg_type,
        arg_type,
      );
    }

    if exp_no_of_args == no_of_args {
      // regular function
      TypedValue::get(
        Expr::Call(Call {
          name,
          args: arg_values,
        }),
        rt,
      )
    } else {
      // partial function application
      let mut exp_arg_types = exp_arg_types;
      let mut exp_rt = exp_arg_types.split_off(no_of_args);

      exp_rt.push(rt.clone());

      let partial_name = format!("partial-{}", name);
      let args = exp_rt
        .clone()
        .into_iter()
        .enumerate()
        .map(|(i, t)| FunArg {
          name: i.to_string(),
          type_: t,
        })
        .collect();
      let body = Expr::Call(Call {
        name,
        args: arg_values
          .into_iter()
          .chain(
            exp_rt
              .clone()
              .into_iter()
              .enumerate()
              .map(|(i, _)| {
                Expr::Value(Value {
                  name: i.to_string(),
                })
              })
              .collect::<Vec<_>>(),
          )
          .collect(),
      });

      TypedValue::get(
        Expr::Function(Box::new(Function {
          f: Fun {
            sig: FunSig {
              name: partial_name,
              args,
              rt,
            },
            body,
          },
          closure: self
            .closure
            .iter()
            .cloned()
            .map(|(name, type_)| FunArg { name, type_ })
            .collect(),
        })),
        Type::Function(exp_rt),
      )
    }
  }

  fn typecheck_if(&mut self, if_: qualified::If) -> CheckResult<typed::If> {
    let be = self.typecheck_expr(if_.if_)?;
    let e1 = self.typecheck_expr(if_.then)?;
    let e2 = self.typecheck_expr(if_.else_)?;

    if be.t != Type::Simple(SimpleType::Bool) || e1.t != e2.t {
      bail!(
        "Invalid if expression types: {:?} {:?} {:?}",
        be.t,
        e1.t,
        e2.t
      )
    };

    TypedValue::get(
      If {
        if_: be.v,
        then: e1.v,
        else_: e2.v,
      },
      e1.t,
    )
  }

  fn typecheck_attr(&self, attr: qualified::Attr) -> CheckResult<typed::Attr> {
    let struct_ = match (self.get_value(attr.name.clone())?, &self.type_impl) {
      (Type::Simple(SimpleType::Struct(struct_)), _) => struct_,
      (Type::Simple(SimpleType::This), Some(SimpleType::Struct(struct_))) => struct_.clone(),
      _ => {
        bail!("Struct `{}` does not exist.", attr.name);
      }
    };

    let Some(StructArg { type_, .. }) = struct_.clone().args.into_iter().find(|arg| arg.name == attr.attr) else {
      bail!(
        "Struct `{}` does not have attribute `{}`.",
        attr.name,
        attr.attr,
      );
    };

    TypedValue::get(
      typed::Attr {
        name: attr.name,
        struct_,
        attr: attr.attr,
      },
      type_,
    )
  }

  fn typecheck_new(&mut self, new: qualified::New) -> CheckResult<typed::New> {
    let args = new
      .args
      .into_iter()
      .map(|e| self.typecheck_expr(e).map(|tv| tv.v))
      .collect::<Result<Vec<typed::Expr>, _>>()?;
    let struct_args = self.get_struct(new.name.clone()).map(|s| s.args)?;

    TypedValue::get(
      New {
        name: new.name.clone(),
        args,
      },
      Type::Simple(SimpleType::Struct(Struct {
        name: new.name,
        args: struct_args,
      })),
    )
  }

  fn typecheck_string_template(
    &mut self,
    stringtemplate: qualified::StringTemplate,
  ) -> CheckResult<typed::StringTemplate> {
    let mut typed_args = Vec::new();
    let mut types_by_value_map = HashMap::new();

    for value in stringtemplate.args {
      let TypedValue { v, t } = self.typecheck_value(Value { name: value })?;
      typed_args.push(v.name.clone());
      types_by_value_map.insert(v.name, t);
    }

    let typed_template = STRING_TEMPLATE_RE
      .replace_all(stringtemplate.string.as_str(), |caps: &Captures| {
        let a = &caps[1];
        match types_by_value_map[a] {
          Type::Simple(SimpleType::Int) => "%d",
          Type::Simple(SimpleType::Bool) => "%d",
          Type::Simple(SimpleType::Float) => "%F",
          Type::Simple(SimpleType::String) => "%s",
          Type::Simple(SimpleType::This) => todo!(),
          Type::Simple(SimpleType::Struct(_)) => todo!(),
          Type::Function(_) => todo!(),
        }
      })
      .into_owned();

    TypedValue::get(
      typed::StringTemplate {
        string: typed_template,
        args: typed_args,
      },
      Type::Simple(SimpleType::String),
    )
  }

  fn typecheck_method_call(
    &mut self,
    methodcall: qualified::MethodCall,
  ) -> CheckResult<typed::MethodCall> {
    let this = self.typecheck_expr(methodcall.this)?;
    let methodname = methodcall.methodname;
    let args = methodcall
      .args
      .into_iter()
      .map(|a| self.typecheck_expr(a).map(|a| a.v))
      .collect::<Result<_, _>>()?;

    let fs = self.get_impl_method(this.clone(), methodname.clone())?;

    let Type::Simple(t) = this.t else {
      bail!(
        "Only simple types can have methods: `{:?}`",
        this.t,
      );
    };

    TypedValue::get(
      typed::MethodCall {
        this: this.v,
        typename: t.get_name(),
        methodname: methodname.clone(),
        args,
      },
      fs.rt,
    )
  }
}

#[cfg(test)]
mod test {
  use rstest::*;

  use super::*;

  #[fixture]
  fn typechecker() -> TypeChecker {
    TypeChecker::create_empty()
  }

  // typecheck_lit
  #[rstest]
  #[case(Lit::Int(32), SimpleType::Int)]
  #[case(Lit::Bool(true), SimpleType::Bool)]
  #[case(Lit::Float("3.2".to_string()), SimpleType::Float)]
  #[case(Lit::String("foo".to_string()), SimpleType::String)]
  fn test_typecheck_lit(typechecker: TypeChecker, #[case] lit: Lit, #[case] type_: SimpleType) {
    // given
    let expected_result = TypedValue {
      v: lit.clone(),
      t: Type::Simple(type_),
    };

    // when
    let result = typechecker.typecheck_lit(lit).unwrap();

    // then
    assert_eq!(result, expected_result)
  }

  // typecheck_value

  #[rstest]
  fn test_typecheck_value_ok(mut typechecker: TypeChecker) {
    // given
    let value = Value {
      name: "foo".to_string(),
    };
    let type_ = Type::Simple(SimpleType::String);
    typechecker.values.insert(value.name.clone(), type_.clone());
    let expected_result = TypedValue {
      v: value.clone(),
      t: type_.clone(),
    };

    // when
    let result = typechecker.typecheck_value(value).unwrap();

    // then
    assert_eq!(result, expected_result)
  }

  #[rstest]
  fn test_typecheck_value_error(mut typechecker: TypeChecker) {
    // given
    let name = Value {
      name: "foo".to_string(),
    };

    // when
    let result = typechecker.typecheck_value(name);

    // then
    assert!(result.err().is_some())
  }

  // typecheck_assign

  #[rstest]
  fn test_typecheck_assign_ok(mut typechecker: TypeChecker) {
    // given
    let name = "foo".to_string();
    let type_ = Type::Simple("Int".to_string());
    let expr = Expr::Lit(Lit::Int(32));
    let expected_result = TypedValue {
      v: Assign {
        name: name.clone(),
        type_: Type::Simple(SimpleType::Int),
        expr: Expr::Lit(Lit::Int(32)),
      },
      t: Type::Simple(SimpleType::Int),
    };
    let assign = Assign { name, type_, expr };

    // when
    let result = typechecker.typecheck_assign(assign).unwrap();

    // then
    assert_eq!(result, expected_result);
    assert_eq!(
      typechecker.values.get("foo").unwrap(),
      &Type::Simple(SimpleType::Int)
    )
  }

  #[rstest]
  fn test_typecheck_assign_error(mut typechecker: TypeChecker) {
    let name = "foo".to_string();
    let type_ = Type::Simple("Int".to_string());
    let expr = Expr::Lit(Lit::Bool(false));
    let assign = Assign { name, type_, expr };

    // when
    let result = typechecker.typecheck_assign(assign);

    // then
    assert!(result.err().is_some());
    assert!(typechecker.values.get("foo").is_none())
  }

  // typecheck_chain

  #[rstest]
  fn test_typecheck_chain_ok(mut typechecker: TypeChecker) {
    // given
    let e1 = Expr::Lit(Lit::Int(32));
    let e2 = Expr::Lit(Lit::Float("32.0".to_string()));
    let expected_result = TypedValue {
      v: Chain {
        e1: Expr::Lit(Lit::Int(32)),
        e2: Expr::Lit(Lit::Float("32.0".to_string())),
      },
      t: Type::Simple(SimpleType::Float),
    };
    let chain = Chain { e1, e2 };

    // when
    let result = typechecker.typecheck_chain(chain).unwrap();

    // then
    assert_eq!(result, expected_result)
  }

  // typecheck_call

  #[rstest]
  fn test_typecheck_call_ok(mut typechecker: TypeChecker) {
    // given
    let name = "foo".to_string();
    let args = Vec::from([
      Expr::Lit(Lit::Int(1)),
      Expr::Lit(Lit::Float("1.2".to_string())),
      Expr::Lit(Lit::Bool(true)),
    ]);
    typechecker.functions.insert(
      name.clone(),
      (
        Vec::from([
          Type::Simple(SimpleType::Int),
          Type::Simple(SimpleType::Float),
          Type::Simple(SimpleType::Bool),
        ]),
        Type::Simple(SimpleType::String),
      ),
    );
    let expected_result = TypedValue {
      v: Expr::Call(Call {
        name: name.clone(),
        args: Vec::from([
          Expr::Lit(Lit::Int(1)),
          Expr::Lit(Lit::Float("1.2".to_string())),
          Expr::Lit(Lit::Bool(true)),
        ]),
      }),
      t: Type::Simple(SimpleType::String),
    };
    let call = Call { name, args };

    // when
    let result = typechecker.typecheck_call(call).unwrap();

    // then
    assert_eq!(result, expected_result)
  }

  #[rstest]
  fn test_typecheck_call_error_not_found(mut typechecker: TypeChecker) {
    // given
    let name = "foo".to_string();
    let args = Vec::new();
    let call = Call { name, args };

    // when
    let result = typechecker.typecheck_call(call);

    // then
    assert!(result.err().is_some())
  }

  #[rstest]
  #[case::bad_int(
        Vec::from([
            Expr::Lit(Lit::String("bad".to_string())),
            Expr::Lit(Lit::Float("1.2".to_string())),
            Expr::Lit(Lit::Bool(true)),
        ])
    )]
  #[case::bad_float(
        Vec::from([
            Expr::Lit(Lit::Int(1)),
            Expr::Lit(Lit::String("bad".to_string())),
            Expr::Lit(Lit::Bool(true)),
        ])
    )]
  #[case::bad_bool(
        Vec::from([
            Expr::Lit(Lit::Int(1)),
            Expr::Lit(Lit::Float("1.2".to_string())),
            Expr::Lit(Lit::String("bad".to_string())),
        ])
    )]
  fn test_typecheck_call_error_invalid_arg_types(
    mut typechecker: TypeChecker,
    #[case] args: Vec<qualified::Expr>,
  ) {
    // given
    let name = "foo".to_string();
    typechecker.functions.insert(
      name.clone(),
      (
        Vec::from([
          Type::Simple(SimpleType::Int),
          Type::Simple(SimpleType::Float),
          Type::Simple(SimpleType::Bool),
        ]),
        Type::Simple(SimpleType::String),
      ),
    );
    let call = Call { name, args };

    // when
    let result = typechecker.typecheck_call(call);

    // then
    assert!(result.err().is_some())
  }

  // typecheck_if

  #[rstest]
  fn test_typecheck_if_ok(mut typechecker: TypeChecker) {
    // given
    let if_ = Expr::Lit(Lit::Bool(true));
    let then = Expr::Lit(Lit::Int(1));
    let else_ = Expr::Lit(Lit::Int(2));
    let expected_result = TypedValue {
      v: If {
        if_: Expr::Lit(Lit::Bool(true)),
        then: Expr::Lit(Lit::Int(1)),
        else_: Expr::Lit(Lit::Int(2)),
      },
      t: Type::Simple(SimpleType::Int),
    };
    let if_ = If { if_, then, else_ };

    // when
    let result = typechecker.typecheck_if(if_).unwrap();

    // then
    assert_eq!(result, expected_result)
  }

  #[rstest]
  #[case::invalid_if(Lit::Int(3), Lit::Int(1), Lit::Int(2))]
  #[case::unequal_branches(
        Lit::Bool(false),
        Lit::Int(1),
        Lit::Float("3.4".to_string()),
    )]
  fn test_typecheck_if_error(
    mut typechecker: TypeChecker,
    #[case] be: Lit,
    #[case] e1: Lit,
    #[case] e2: Lit,
  ) {
    // given
    let if_ = Expr::Lit(be);
    let then = Expr::Lit(e1);
    let else_ = Expr::Lit(e2);
    let if_ = If { if_, then, else_ };

    // when
    let result = typechecker.typecheck_if(if_);

    // then
    assert!(result.err().is_some())
  }

  // get_type

  #[rstest]
  #[case("Int", SimpleType::Int)]
  #[case("Bool", SimpleType::Bool)]
  #[case("Float", SimpleType::Float)]
  #[case("String", SimpleType::String)]
  fn test_get_type_ok_simple(typechecker: TypeChecker, #[case] s: &str, #[case] t: SimpleType) {
    // given
    let s = Type::Simple(s.to_string());

    // when
    let result = typechecker.get_type(s).unwrap();

    // then
    assert_eq!(result, Type::Simple(t))
  }

  #[rstest]
  fn test_get_type_ok_user_defined(mut typechecker: TypeChecker) {
    // given
    let s = "Foo".to_string();
    let args = Vec::from([
      StructArg {
        name: "bar".to_string(),
        type_: Type::Simple(SimpleType::Int),
      },
      StructArg {
        name: "baz".to_string(),
        type_: Type::Simple(SimpleType::Float),
      },
      StructArg {
        name: "qux".to_string(),
        type_: Type::Simple(SimpleType::Bool),
      },
    ]);
    typechecker.types.insert(
      s.clone(),
      args
        .iter()
        .cloned()
        .map(|arg| (arg.name, arg.type_))
        .collect(),
    );
    let expected_result = Type::Simple(SimpleType::Struct(Struct {
      name: s.clone(),
      args,
    }));

    // when
    let result = typechecker.get_type(Type::Simple(s)).unwrap();

    // then
    assert_eq!(result, expected_result)
  }

  #[rstest]
  fn test_get_type_error(typechecker: TypeChecker) {
    // given
    let s = Type::Simple("Foo".to_string());

    // when
    let result = typechecker.get_type(s);

    // then
    assert!(result.err().is_some())
  }
}
