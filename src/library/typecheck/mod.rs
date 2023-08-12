mod closure_manager;
mod instantiate;
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

  pub fn get_type_of_expr(
    &mut self,
    expr: qualified::Expr,
  ) -> Result<(typed::Expr, typed::Type)> {
    self.typecheck_expr(expr).map(|tv| (tv.v, tv.t))
  }

  fn get_struct(&self, name: String) -> Result<typed::Struct> {
    if let Some(args) = self.types.get(name.as_str()) {
      return Ok(Struct {
        name: name.to_string(),
        args: args.clone(),
      });
    }

    if let Some(Decl::Struct(s)) =
      self.module.imports.iter().find(|s| s.get_name() == &name)
    {
      return Ok(s.clone());
    }

    bail!("Cant find struct {}", name)
  }

  fn get_function(
    &self,
    name: String,
  ) -> Result<(Vec<typed::Type>, typed::Type)> {
    if let Some(f) = self.functions.get(name.as_str()) {
      return Ok(f.clone());
    }

    if let Some(Decl::Fun(f)) =
      self.module.imports.iter().find(|vd| vd.get_name() == &name)
    {
      let arg_types = f.args.clone().into_iter().map(|a| a.1).collect();
      let rt = f.rt.clone();
      return Ok((arg_types, rt));
    }

    bail!("Cant find function {}", name)
  }

  fn get_value(&self, name: String) -> Result<typed::Type> {
    if let Some(ts) = self.values.get(name.as_str()) {
      return Ok(ts.clone());
    }

    if let Some(Decl::Val(ts)) =
      self.module.imports.iter().find(|i| i.get_name() == &name)
    {
      return Ok(ts.t.clone());
    }

    bail!("Cant find value {}", name)
  }

  fn get_class(&self, name: &str) -> Result<typed::Class> {
    if let Some(c) = self.classes.get(name) {
      return Ok(c.clone());
    }

    if let Some(Decl::Class(c)) = self
      .module
      .imports
      .iter()
      .find(|c| c.get_name().as_str() == name)
    {
      return Ok(c.clone());
    }

    bail!("Cant find class {}", name)
  }

  fn get_impl_method(
    &self,
    this: TypedValue<typed::Expr, typed::Type>,
    method: String,
  ) -> Option<typed::FunSig> {
    let Type::Simple(t) = this.t else { return None };
    self
      .methods
      .get(t.get_method_name(&method).as_str())
      .cloned()
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

  pub fn typecheck_defs(
    &mut self,
    ds: Vec<qualified::Def>,
  ) -> Result<Vec<typed::Def>> {
    ds.into_iter()
      .map(|d| self.typecheck_def(d))
      .collect::<Result<_, _>>()
  }

  fn typecheck_def(&mut self, d: qualified::Def) -> ValueResult<typed::Def> {
    match d {
      Def::Fun(f) => self.typecheck_fun(f).map(|f| Def::Fun(f.v)),
      Def::Struct(s) => self.typecheck_struct(s).map(|s| Def::Struct(s)),
      Def::Val(v) => self.typecheck_val(v).map(|v| Def::Val(v)),
      Def::Import(i) => self.typecheck_import(i).map(|i| Def::Import(i)),
      Def::Class(c) => self.typecheck_class(c).map(|c| Def::Class(c)),
      Def::Impl(i) => self.typecheck_impl(i).map(|i| Def::Impl(i)),
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

  fn typecheck_fun(&mut self, f: qualified::Fun) -> CheckResult<typed::Fun> {
    let TypedValue { v: sig, t } = self.typecheck_funsig(f.sig, false)?;

    sig.args.iter().for_each(|(n, t)| {
      self.values.insert(n.clone(), t.clone());
    });

    self.functions.insert(
      sig.name.clone(),
      (
        sig.args.clone().into_iter().map(|(_, t)| t).collect(),
        (sig.rt.clone()),
      ),
    );

    let body = self.typecheck_expr(f.body)?;

    self.namespace.drop_qualified_name();

    ensure!(
      sig.rt == body.t,
      "Return type and body type does not match: {:?} != {:?}",
      sig.rt,
      body.t,
    );

    Ok(TypedValue {
      v: Fun { sig, body: body.v },
      t,
    })
  }

  fn typecheck_struct(
    &mut self,
    struct_: qualified::Struct,
  ) -> ValueResult<typed::Struct> {
    let name = struct_.name;
    let args = struct_
      .args
      .into_iter()
      .map(|(n, t)| self.get_type(t).map(|t| (n, t)))
      .collect::<Result<Vec<_>, _>>()?;

    self.types.insert(name.clone(), args.clone());

    let struct_ = Struct { name, args };

    Ok(struct_.clone())
  }

  fn typecheck_val(
    &mut self,
    value: qualified::Val,
  ) -> ValueResult<typed::Val> {
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

  fn typecheck_import(
    &mut self,
    import_: qualified::Import,
  ) -> ValueResult<typed::Import> {
    Ok(import_)
  }

  fn typecheck_class(
    &mut self,
    class: qualified::Class,
  ) -> ValueResult<typed::Class> {
    let name = class.name.clone();
    let methods = class
      .methods
      .clone()
      .into_iter()
      .map(|m| self.typecheck_funsig(m, true).map(|m| m.v))
      .collect::<Result<_, _>>()?;

    let class = Class { name, methods };

    self.classes.insert(class.name.clone(), class.clone());

    Ok(class)
  }

  fn typecheck_impl(
    &mut self,
    impl_: qualified::Impl,
  ) -> ValueResult<typed::Impl> {
    let class_name = impl_.class_name.clone();
    // TODO: this should always be SimpleType::Struct
    let t = self.get_simple_type(impl_.t.clone())?;
    self.type_impl = Some(t.clone());

    let impls: Vec<typed::Fun> = impl_
      .impls
      .clone()
      .into_iter()
      .map(|i| self.typecheck_fun(i).map(|i| i.v))
      .collect::<Result<_, _>>()?;

    let Ok(class) = self.get_class(class_name.as_str()) else {
      bail!("No class with name {}", class_name);
    };

    let mut class_sigs = class
      .methods
      .clone()
      .into_iter()
      .map(|m| m)
      .collect::<Vec<_>>();
    class_sigs.sort_by_key(|m| m.name.clone());
    let mut impl_sigs =
      impls.clone().into_iter().map(|i| i.sig).collect::<Vec<_>>();
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
    args: Vec<(String, qualified::Type)>,
  ) -> CheckResult<Vec<(String, typed::Type)>, Vec<typed::Type>> {
    let v = args
      .into_iter()
      .map(|(n, t)| self.get_type(t).map(|t| (n, t)))
      .collect::<Result<Vec<_>, _>>()?;

    self.closure_manager.append(v.clone());

    let t = v.clone().into_iter().map(|(_, t)| t).collect();

    Ok(TypedValue { v, t })
  }

  fn typecheck_expr(
    &mut self,
    expr: qualified::Expr,
  ) -> CheckResult<typed::Expr> {
    match expr {
      Expr::BinOp(lh, op, rh) => {
        let TypedValue { v: (lh, op, rh), t } =
          self.typecheck_binop(*lh, op, *rh)?;
        TypedValue::get(Expr::BinOp(Box::new(lh), op, Box::new(rh)), t)
      }
      Expr::Lit(l) => {
        let TypedValue { v, t } = self.typecheck_lit(l)?;
        TypedValue::get(Expr::Lit(v), t)
      }
      Expr::Value(name) => {
        let TypedValue { v, t } = self.typecheck_value(name)?;
        TypedValue::get(Expr::Value(v), t)
      }
      Expr::Assign((name, type_), val) => {
        let TypedValue {
          v: (name, type_, val),
          t,
        } = self.typecheck_assign(name, type_, *val)?;
        TypedValue::get(Expr::Assign((name, type_), Box::new(val)), t)
      }
      Expr::Chain(lh, rh) => {
        let TypedValue { v: (lh, rh), t } = self.typecheck_chain(*lh, *rh)?;
        TypedValue::get(Expr::Chain(Box::new(lh), Box::new(rh)), t)
      }
      Expr::Function(f, _) => {
        let closure = self.closure_manager.clone();
        self.closure_manager.push_layer();
        let f = self.typecheck_fun(*f)?;
        self.closure_manager.pop_layer();
        TypedValue::get(Expr::Function(Box::new(f.v), closure), f.t)
      }
      Expr::Call(name, args) => self.typecheck_call(name, args),
      Expr::If(be, e1, e2) => {
        let TypedValue { v: (be, e1, e2), t } =
          self.typecheck_if(*be, *e1, *e2)?;
        TypedValue::get(Expr::If(Box::new(be), Box::new(e1), Box::new(e2)), t)
      }
      Expr::Attr(name, _, attr) => {
        let TypedValue {
          v: (name, struct_, attr),
          t,
        } = self.typecheck_attr(name, attr)?;
        TypedValue::get(Expr::Attr(name, struct_, attr), t)
      }
      Expr::New(name, args) => {
        let TypedValue { v: (name, args), t } =
          self.typecheck_new(name, args)?;
        TypedValue::get(Expr::New(name, args), t)
      }
      Expr::StringTemplate(template, values) => {
        let TypedValue {
          v: (template, values),
          t,
        } = self.typecheck_string_template(template, values)?;
        TypedValue::get(Expr::StringTemplate(template, values), t)
      }
      Expr::MethodCall(this, _, method, args) => {
        let TypedValue {
          v: (this, tname, method, args),
          t,
        } = self.typecheck_method_call(*this, method, args)?;
        TypedValue::get(
          Expr::MethodCall(Box::new(this), tname, method, args),
          t,
        )
      }
    }
  }

  fn typecheck_binop(
    &mut self,
    lh: qualified::Expr,
    op: Op,
    rh: qualified::Expr,
  ) -> CheckResult<(typed::Expr, Op, typed::Expr)> {
    let lh = self.typecheck_expr(lh)?;
    let rh = self.typecheck_expr(rh)?;

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

    match (lht, op) {
      (
        SimpleType::Int | SimpleType::Float,
        op @ (Op::Add | Op::Sub | Op::Mul | Op::Div),
      ) => TypedValue::get((lh.v, op, rh.v), Type::Simple(rht)),
      (
        SimpleType::Int | SimpleType::Float,
        op @ (Op::Lt | Op::Le | Op::Ne | Op::Eq | Op::Ge | Op::Gt),
      ) => TypedValue::get((lh.v, op, rh.v), Type::Simple(SimpleType::Bool)),
      (SimpleType::Bool, op @ (Op::And | Op::Or)) => {
        TypedValue::get((lh.v, op, rh.v), Type::Simple(rht))
      }
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

  fn typecheck_value(&mut self, name: String) -> CheckResult<String> {
    let type_ = self.get_value(name.clone())?;

    TypedValue::get(name, type_)
  }

  fn typecheck_assign(
    &mut self,
    name: String,
    type_: qualified::Type,
    expr: qualified::Expr,
  ) -> CheckResult<(String, typed::Type, typed::Expr)> {
    let type_ = self.get_type(type_)?;
    let expr = self.typecheck_expr(expr)?;

    ensure!(
      type_ == expr.t,
      "Unequal assign types: {:?} != {:?}",
      type_,
      expr.t,
    );

    self.values.insert(name.clone(), type_.clone());
    self.closure_manager.push(name.clone(), type_.clone());

    TypedValue::get((name, type_.clone(), expr.v), type_)
  }

  fn typecheck_chain(
    &mut self,
    lh: qualified::Expr,
    rh: qualified::Expr,
  ) -> CheckResult<(typed::Expr, typed::Expr)> {
    let lh = self.typecheck_expr(lh)?;
    let rh = self.typecheck_expr(rh)?;

    TypedValue::get((lh.v, rh.v), rh.t)
  }

  fn typecheck_call(
    &mut self,
    name: String,
    args: Vec<qualified::Expr>,
  ) -> CheckResult<typed::Expr> {
    let (arg_values, arg_types): (Vec<typed::Expr>, Vec<typed::Type>) = args
      .into_iter()
      .map(|a| self.typecheck_expr(a))
      .collect::<Result<Vec<TypedValue<typed::Expr, typed::Type>>, _>>()?
      .into_iter()
      .map(|TypedValue { v, t }| (v, t))
      .unzip();
    let name = self.namespace.get_qualified_name(name);
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

    for (exp_arg_type, arg_type) in
      exp_arg_types.iter().zip::<Vec<typed::Type>>(arg_types)
    {
      ensure!(
        *exp_arg_type == arg_type,
        "Invalid argument type: {:?} != {:?}",
        exp_arg_type,
        arg_type,
      );
    }

    if exp_no_of_args == no_of_args {
      // regular function
      TypedValue::get(Expr::Call(name, arg_values), rt)
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
        .map(|(i, t)| (i.to_string(), t))
        .collect();
      let body = Expr::Call(
        name,
        arg_values
          .into_iter()
          .chain(
            exp_rt
              .clone()
              .into_iter()
              .enumerate()
              .map(|(i, _)| Expr::Value(i.to_string()))
              .collect::<Vec<_>>(),
          )
          .collect(),
      );

      TypedValue::get(
        Expr::Function(
          Box::new(Fun {
            sig: FunSig {
              name: partial_name,
              args,
              rt,
            },
            body,
          }),
          self.closure.clone(),
        ),
        Type::Function(exp_rt),
      )
    }
  }

  fn typecheck_if(
    &mut self,
    be: qualified::Expr,
    e1: qualified::Expr,
    e2: qualified::Expr,
  ) -> CheckResult<(typed::Expr, typed::Expr, typed::Expr)> {
    let be = self.typecheck_expr(be)?;
    let e1 = self.typecheck_expr(e1)?;
    let e2 = self.typecheck_expr(e2)?;

    if be.t != Type::Simple(SimpleType::Bool) || e1.t != e2.t {
      bail!(
        "Invalid if expression types: {:?} {:?} {:?}",
        be.t,
        e1.t,
        e2.t
      )
    };

    TypedValue::get((be.v, e1.v, e2.v), e1.t)
  }

  fn typecheck_attr(
    &self,
    name: String,
    attr: String,
  ) -> CheckResult<(String, typed::Struct, String)> {
    let struct_ = match (self.get_value(name.clone())?, &self.type_impl) {
      (Type::Simple(SimpleType::Struct(struct_)), _) => struct_,
      (Type::Simple(SimpleType::This), Some(SimpleType::Struct(struct_))) => {
        struct_.clone()
      }
      _ => {
        bail!("Struct `{}` does not exist.", name);
      }
    };

    let Some((_, type_)) = struct_.clone().args.into_iter().find(|(n, _)| n == &attr) else {
      bail!(
        "Struct `{}` does not have attribute `{}`.",
        name,
        attr,
      );
    };

    TypedValue::get((name, struct_.clone(), attr), type_)
  }

  fn typecheck_new(
    &mut self,
    name: String,
    args: Vec<qualified::Expr>,
  ) -> CheckResult<(String, Vec<typed::Expr>)> {
    let args = args
      .into_iter()
      .map(|e| self.typecheck_expr(e).map(|tv| tv.v))
      .collect::<Result<Vec<typed::Expr>, _>>()?;
    let struct_args = self.get_struct(name.clone()).map(|s| s.args)?;

    TypedValue::get(
      (name.clone(), args),
      Type::Simple(SimpleType::Struct(Struct {
        name,
        args: struct_args,
      })),
    )
  }

  fn typecheck_string_template(
    &mut self,
    template: String,
    values: Vec<String>,
  ) -> CheckResult<(String, Vec<String>)> {
    let mut typed_args = Vec::new();
    let mut types_by_value_map = HashMap::new();

    for value in values {
      let TypedValue { v: name, t } = self.typecheck_value(value)?;
      typed_args.push(name.clone());
      types_by_value_map.insert(name, t);
    }

    let typed_template = STRING_TEMPLATE_RE
      .replace_all(template.as_str(), |caps: &Captures| {
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
      (typed_template, typed_args),
      Type::Simple(SimpleType::String),
    )
  }

  fn typecheck_method_call(
    &mut self,
    this: qualified::Expr,
    method: String,
    args: Vec<qualified::Expr>,
  ) -> CheckResult<(typed::Expr, String, String, Vec<typed::Expr>)> {
    let this = self.typecheck_expr(this)?;
    let method = method;
    let args = args
      .into_iter()
      .map(|a| self.typecheck_expr(a).map(|a| a.v))
      .collect::<Result<_, _>>()?;

    let Some(fs) = self.get_impl_method(this.clone(), method.clone()) else {
      bail!(
        "No method `{}` defined for type `{:?}`.",
        method,
        this.t,
      );
    };

    let Type::Simple(t) = this.t else {
      bail!(
        "Only simple types can have methods: `{:?}`",
        this.t,
      );
    };

    TypedValue::get((this.v, t.get_name(), method, args), fs.rt)
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
  fn test_typecheck_lit(
    typechecker: TypeChecker,
    #[case] lit: Lit,
    #[case] type_: SimpleType,
  ) {
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
    let name = "foo".to_string();
    let type_ = Type::Simple(SimpleType::String);
    typechecker.values.insert(name.clone(), type_.clone());
    let expected_result = TypedValue {
      v: name.clone(),
      t: type_.clone(),
    };

    // when
    let result = typechecker.typecheck_value(name).unwrap();

    // then
    assert_eq!(result, expected_result)
  }

  #[rstest]
  fn test_typecheck_value_error(mut typechecker: TypeChecker) {
    // given
    let name = "foo".to_string();

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
      v: (
        name.clone(),
        Type::Simple(SimpleType::Int),
        Expr::Lit(Lit::Int(32)),
      ),
      t: Type::Simple(SimpleType::Int),
    };

    // when
    let result = typechecker.typecheck_assign(name, type_, expr).unwrap();

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

    // when
    let result = typechecker.typecheck_assign(name, type_, expr);

    // then
    assert!(result.err().is_some());
    assert!(typechecker.values.get("foo").is_none())
  }

  // typecheck_chain

  #[rstest]
  fn test_typecheck_chain_ok(mut typechecker: TypeChecker) {
    // given
    let lh = Expr::Lit(Lit::Int(32));
    let rh = Expr::Lit(Lit::Float("32.0".to_string()));
    let expected_result = TypedValue {
      v: (
        Expr::Lit(Lit::Int(32)),
        Expr::Lit(Lit::Float("32.0".to_string())),
      ),
      t: Type::Simple(SimpleType::Float),
    };

    // when
    let result = typechecker.typecheck_chain(lh, rh).unwrap();

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
      v: Expr::Call(
        name.clone(),
        Vec::from([
          Expr::Lit(Lit::Int(1)),
          Expr::Lit(Lit::Float("1.2".to_string())),
          Expr::Lit(Lit::Bool(true)),
        ]),
      ),
      t: Type::Simple(SimpleType::String),
    };

    // when
    let result = typechecker.typecheck_call(name, args).unwrap();

    // then
    assert_eq!(result, expected_result)
  }

  #[rstest]
  fn test_typecheck_call_error_not_found(mut typechecker: TypeChecker) {
    // given
    let name = "foo".to_string();
    let args = Vec::new();

    // when
    let result = typechecker.typecheck_call(name, args);

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

    // when
    let result = typechecker.typecheck_call(name, args);

    // then
    assert!(result.err().is_some())
  }

  // typecheck_if

  #[rstest]
  fn test_typecheck_if_ok(mut typechecker: TypeChecker) {
    // given
    let be = Expr::Lit(Lit::Bool(true));
    let e1 = Expr::Lit(Lit::Int(1));
    let e2 = Expr::Lit(Lit::Int(2));
    let expected_result = TypedValue {
      v: (
        Expr::Lit(Lit::Bool(true)),
        Expr::Lit(Lit::Int(1)),
        Expr::Lit(Lit::Int(2)),
      ),
      t: Type::Simple(SimpleType::Int),
    };

    // when
    let result = typechecker.typecheck_if(be, e1, e2).unwrap();

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
    let be = Expr::Lit(be);
    let e1 = Expr::Lit(e1);
    let e2 = Expr::Lit(e2);

    // when
    let result = typechecker.typecheck_if(be, e1, e2);

    // then
    assert!(result.err().is_some())
  }

  // get_type

  #[rstest]
  #[case("Int", SimpleType::Int)]
  #[case("Bool", SimpleType::Bool)]
  #[case("Float", SimpleType::Float)]
  #[case("String", SimpleType::String)]
  fn test_get_type_ok_simple(
    typechecker: TypeChecker,
    #[case] s: &str,
    #[case] t: SimpleType,
  ) {
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
      ("bar".to_string(), Type::Simple(SimpleType::Int)),
      ("baz".to_string(), Type::Simple(SimpleType::Float)),
      ("qux".to_string(), Type::Simple(SimpleType::Bool)),
    ]);
    typechecker.types.insert(s.clone(), args.clone());
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
