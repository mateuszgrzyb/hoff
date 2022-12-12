use crate::ast::{
    Decl, Expr, Fun, Lit, Mod, Op, Struct, Type, TypedDecl, TypedExpr,
    TypedFun, TypedMod, TypedStruct, UntypedDecl, UntypedExpr, UntypedFun,
    UntypedMod, UntypedStruct,
};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

pub struct CheckError {
    message: String,
}

impl CheckError {
    fn from_message<V>(message: String) -> Result<V, CheckError> {
        Err(Self { message })
    }
}

impl Debug for CheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Display for CheckError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for CheckError {}

#[derive(Debug, PartialEq, Eq)]
pub struct TypedValue<V, T> {
    v: V,
    t: T,
}

impl<V, T> TypedValue<V, T> {
    fn get(v: V, t: T) -> CheckResult<V, T> {
        Ok(Self { v, t })
    }
}

type CheckResult<V, T = Type> = Result<TypedValue<V, T>, CheckError>;

pub struct Typechecker {
    pub values: HashMap<String, Type>,
    pub functions: HashMap<String, (Vec<Type>, Type)>,
    pub types: HashMap<String, Vec<(String, Type)>>,
    pub closure: Vec<(String, Type)>,
    pub nested_name: Vec<String>,
    pub qualified_names: HashMap<String, String>,
}

impl Typechecker {
    pub fn create() -> Self {
        Self {
            values: HashMap::new(),
            functions: HashMap::from([(
                "puts".to_string(),
                (Vec::from([Type::String]), Type::Int),
            )]),
            types: HashMap::new(),
            closure: Vec::new(),
            nested_name: Vec::new(),
            qualified_names: Default::default(),
        }
    }

    fn get_type(&self, s: String) -> Result<Type, CheckError> {
        match s.as_str() {
            "Int" => Ok(Type::Int),
            "Bool" => Ok(Type::Bool),
            "Float" => Ok(Type::Float),
            "String" => Ok(Type::String),
            s => {
                let Some(t) = self.types.get(s) else {
                    return CheckError::from_message(format!("Unknown type: {s}"))
                };
                Ok(Type::Struct(Struct {
                    name: s.to_string(),
                    args: (*t).clone(),
                }))
            }
        }
    }

    fn create_qualified_name(&mut self, name: String) -> String {
        self.nested_name.push(name.clone());
        let qualifled_name = self.nested_name.join("$$$");
        self.qualified_names.insert(name, qualifled_name.clone());
        qualifled_name
    }

    fn get_qualified_name(&self, name: String) -> String {
        (*self.qualified_names.get(&*name).unwrap_or(&name)).clone()
    }

    pub fn typecheck(
        &mut self,
        m: UntypedMod,
    ) -> Result<TypedMod, CheckError> {
        let name = m.name;
        let decls = m
            .decls
            .into_iter()
            .map(|d| Ok(self.typecheck_decl(d)?.v))
            .collect::<Result<_, _>>()?;

        Ok(Mod { name, decls })
    }

    fn typecheck_decl(&mut self, d: UntypedDecl) -> CheckResult<TypedDecl> {
        match d {
            Decl::Fun(f) => {
                let f = self.typecheck_fun(f)?;
                TypedValue::get(Decl::Fun(f.v), f.t)
            }
            Decl::Struct(s) => {
                let s = self.typecheck_struct(s)?;
                TypedValue::get(Decl::Struct(s.v), s.t)
            }
        }
    }

    fn typecheck_fun(&mut self, f: UntypedFun) -> CheckResult<TypedFun> {
        let name = self.create_qualified_name(f.name);
        let args = self.typecheck_fun_args(f.args)?;
        let rt = self.get_type(f.rt)?;

        args.v.iter().for_each(|(n, t)| {
            self.values.insert((*n).clone(), (*t).clone());
        });

        self.functions.insert(
            name.clone(),
            (
                args.v.clone().into_iter().map(|(_, t)| t).collect(),
                (rt.clone()),
            ),
        );

        let body = self.typecheck_expr(f.body)?;

        self.nested_name.pop();

        if rt != body.t {
            return CheckError::from_message(format!(
                "Return type and body type does not match: {:?} != {:?}",
                rt, body.t,
            ));
        }

        Ok(TypedValue {
            v: Fun {
                name,
                args: args.v,
                rt,
                body: body.v,
            },
            t: Type::Fun(args.t, Box::new(body.t)),
        })
    }

    fn typecheck_struct(
        &mut self,
        struct_: UntypedStruct,
    ) -> CheckResult<TypedStruct> {
        let name = struct_.name;
        let args = struct_
            .args
            .into_iter()
            .map(|(n, t)| Ok((n, self.get_type(t)?)))
            .collect::<Result<Vec<(String, Type)>, _>>()?;

        self.types.insert(name.clone(), args.clone());

        let struct_ = Struct { name, args };

        TypedValue::get(struct_.clone(), Type::Struct(struct_))
    }

    fn typecheck_fun_args(
        &mut self,
        args: Vec<(String, String)>,
    ) -> CheckResult<Vec<(String, Type)>, Vec<Type>> {
        let v = args
            .into_iter()
            .map(|(n, t)| Ok((n, self.get_type(t)?)))
            .collect::<Result<Vec<(String, Type)>, _>>()?;

        self.closure.append(&mut v.clone());

        let t = v.clone().into_iter().map(|(_, t)| t).collect();

        Ok(TypedValue { v, t })
    }

    fn typecheck_expr(&mut self, expr: UntypedExpr) -> CheckResult<TypedExpr> {
        match expr {
            Expr::BinOp(lh, op, rh) => self.typecheck_binop(*lh, op, *rh),
            Expr::Lit(l) => self.typecheck_lit(l),
            Expr::Value(name) => self.typecheck_value(name),
            Expr::Assign((name, type_), val) => {
                self.typecheck_assign(name, type_, *val)
            }
            Expr::Chain(lh, rh) => self.typecheck_chain(*lh, *rh),
            Expr::Function(f, _) => {
                let closure = self.closure.clone();
                let f = self.typecheck_fun(*f)?;
                TypedValue::get(Expr::Function(Box::new(f.v), closure), f.t)
            }
            Expr::Call(name, args) => self.typecheck_call(name, args),
            Expr::If(be, e1, e2) => self.typecheck_if(*be, *e1, *e2),
            Expr::Attr(name, _, attr) => self.typecheck_attr(name, attr),
            Expr::New(name, args) => self.typecheck_new(name, args),
        }
    }

    fn typecheck_binop(
        &mut self,
        lh: UntypedExpr,
        op: Op,
        rh: UntypedExpr,
    ) -> CheckResult<TypedExpr> {
        let lh = self.typecheck_expr(lh)?;
        let rh = self.typecheck_expr(rh)?;

        if lh.t != rh.t {
            return CheckError::from_message(format!(
                "Unequal binop types: {:?} != {:?}",
                lh.t, rh.t
            ));
        }

        match (lh.t, op) {
            (
                Type::Int | Type::Float,
                op @ (Op::Add | Op::Sub | Op::Mul | Op::Div),
            ) => TypedValue::get(
                Expr::BinOp(Box::new(lh.v), op, Box::new(rh.v)),
                rh.t,
            ),
            (
                Type::Int | Type::Float,
                op @ (Op::Lt | Op::Le | Op::Ne | Op::Eq | Op::Ge | Op::Gt),
            ) => TypedValue::get(
                Expr::BinOp(Box::new(lh.v), op, Box::new(rh.v)),
                Type::Bool,
            ),
            (Type::Bool, op @ (Op::And | Op::Or)) => TypedValue::get(
                Expr::BinOp(Box::new(lh.v), op, Box::new(rh.v)),
                rh.t,
            ),
            (lht, op) => CheckError::from_message(format!(
                "Invalid operation type: {:?} {:?} {:?}",
                lht, op, rh.t
            )),
        }
    }

    fn typecheck_lit(&self, lit: Lit) -> CheckResult<TypedExpr> {
        let t = match lit {
            Lit::Int(_) => Type::Int,
            Lit::Bool(_) => Type::Bool,
            Lit::Float(_) => Type::Float,
            Lit::String(_) => Type::String,
        };
        let v = Expr::Lit(lit);

        TypedValue::get(v, t)
    }

    fn typecheck_value(&mut self, name: String) -> CheckResult<TypedExpr> {
        let type_ = self.values.get(name.as_str());

        let Some(type_) = type_ else {
            return CheckError::from_message(format!("Value with name {name} does not exist"))
        };

        TypedValue::get(Expr::Value(name), (*type_).clone())
    }

    fn typecheck_assign(
        &mut self,
        name: String,
        type_: String,
        expr: UntypedExpr,
    ) -> CheckResult<TypedExpr> {
        let type_ = self.get_type(type_)?;
        let expr = self.typecheck_expr(expr)?;

        if type_ != expr.t {
            return CheckError::from_message(format!(
                "Unequal assign types: {:?} != {:?}",
                type_, expr.t
            ));
        };

        self.values.insert(name.clone(), type_.clone());
        self.closure.push((name.clone(), type_.clone()));

        TypedValue::get(
            Expr::Assign((name, type_.clone()), Box::new(expr.v)),
            type_,
        )
    }

    fn typecheck_chain(
        &mut self,
        lh: UntypedExpr,
        rh: UntypedExpr,
    ) -> CheckResult<TypedExpr> {
        let lh = self.typecheck_expr(lh)?;
        let rh = self.typecheck_expr(rh)?;

        TypedValue::get(Expr::Chain(Box::new(lh.v), Box::new(rh.v)), rh.t)
    }

    fn typecheck_call(
        &mut self,
        name: String,
        args: Vec<UntypedExpr>,
    ) -> CheckResult<TypedExpr> {
        let (argvs, argts) = args
            .into_iter()
            .map(|a| self.typecheck_expr(a))
            .collect::<Result<Vec<TypedValue<TypedExpr, Type>>, _>>()?
            .into_iter()
            .map(|TypedValue { v, t }| (v, t))
            .unzip();
        let name = self.get_qualified_name(name);
        let f = self.functions.get(&*name);

        let Some((exp_args, rt)) = f else {
            return CheckError::from_message(
                format!("Function {name} not found")
            )
        };

        for (exp_arg, arg) in exp_args.iter().zip::<Vec<Type>>(argts) {
            if *exp_arg != arg {
                return CheckError::from_message(format!(
                    "Invalid argument type: {:?} != {:?}",
                    exp_arg, arg
                ));
            }
        }

        TypedValue::get(Expr::Call(name.clone(), argvs), (*rt).clone())
    }

    fn typecheck_if(
        &mut self,
        be: UntypedExpr,
        e1: UntypedExpr,
        e2: UntypedExpr,
    ) -> CheckResult<TypedExpr> {
        let be = self.typecheck_expr(be)?;
        let e1 = self.typecheck_expr(e1)?;
        let e2 = self.typecheck_expr(e2)?;

        if be.t != Type::Bool || e1.t != e2.t {
            return CheckError::from_message(format!(
                "Invalid if expression types: {:?} {:?} {:?}",
                be.t, e1.t, e2.t
            ));
        };

        TypedValue::get(
            Expr::If(Box::new(be.v), Box::new(e1.v), Box::new(e2.v)),
            e1.t,
        )
    }

    fn typecheck_attr(
        &self,
        name: String,
        attr: String,
    ) -> CheckResult<TypedExpr> {
        let Some(Type::Struct(struct_)) = self.values.get(&*name) else {
            return CheckError::from_message(format!(
                "Struct `{name}` does not exist.",
            ));
        };

        let Some((_, type_)) = struct_.clone().args.into_iter().find(|(n, _)| n == &attr) else {
            return CheckError::from_message(format!(
                "Struct `{name}` does not have attribute `{attr}`."
            ));
        };

        TypedValue::get(Expr::Attr(name, struct_.clone(), attr), type_.clone())
    }
    fn typecheck_new(
        &mut self,
        name: String,
        args: Vec<UntypedExpr>,
    ) -> CheckResult<TypedExpr> {
        let args = args
            .into_iter()
            .map(|e| Ok(self.typecheck_expr(e)?.v))
            .collect::<Result<Vec<TypedExpr>, _>>()?;
        let struct_args = self.types.get(&*name).unwrap();
        TypedValue::get(
            Expr::New(name.clone(), args),
            Type::Struct(Struct {
                name: name.clone(),
                args: struct_args.clone(),
            }),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::typecheck::*;
    use rstest::*;

    #[fixture]
    fn typechecker() -> Typechecker {
        Typechecker {
            values: Default::default(),
            functions: Default::default(),
            types: Default::default(),
            closure: Default::default(),
            nested_name: Default::default(),
            qualified_names: Default::default(),
        }
    }

    // typecheck_lit
    #[rstest]
    #[case(Lit::Int(32), Type::Int)]
    #[case(Lit::Bool(true), Type::Bool)]
    #[case(Lit::Float("3.2".to_string()), Type::Float)]
    #[case(Lit::String("foo".to_string()), Type::String)]
    fn test_typecheck_lit(
        typechecker: Typechecker,
        #[case] lit: Lit,
        #[case] type_: Type,
    ) {
        // given
        let expected_result = TypedValue {
            v: Expr::Lit(lit.clone()),
            t: type_,
        };

        // when
        let result = typechecker.typecheck_lit(lit).unwrap();

        // then
        assert_eq!(result, expected_result)
    }

    // typecheck_value

    #[rstest]
    fn test_typecheck_value_ok(mut typechecker: Typechecker) {
        // given
        let name = "foo".to_string();
        let type_ = Type::String;
        typechecker.values.insert(name.clone(), type_.clone());
        let expected_result = TypedValue {
            v: Expr::Value(name.clone()),
            t: type_.clone(),
        };

        // when
        let result = typechecker.typecheck_value(name).unwrap();

        // then
        assert_eq!(result, expected_result)
    }

    #[rstest]
    fn test_typecheck_value_error(mut typechecker: Typechecker) {
        // given
        let name = "foo".to_string();

        // when
        let result = typechecker.typecheck_value(name);

        // then
        assert!(result.err().is_some())
    }

    // typecheck_assign

    #[rstest]
    fn test_typecheck_assign_ok(mut typechecker: Typechecker) {
        // given
        let name = "foo".to_string();
        let type_ = "Int".to_string();
        let expr = Expr::Lit(Lit::Int(32));
        let expected_result = TypedValue {
            v: Expr::Assign(
                (name.clone(), Type::Int),
                Box::new(Expr::Lit(Lit::Int(32))),
            ),
            t: Type::Int,
        };

        // when
        let result = typechecker.typecheck_assign(name, type_, expr).unwrap();

        // then
        assert_eq!(result, expected_result);
        assert_eq!(typechecker.values.get("foo").unwrap(), &Type::Int)
    }

    #[rstest]
    fn test_typecheck_assing_error(mut typechecker: Typechecker) {
        let name = "foo".to_string();
        let type_ = "Int".to_string();
        let expr = Expr::Lit(Lit::Bool(false));

        // when
        let result = typechecker.typecheck_assign(name, type_, expr);

        // then
        assert!(result.err().is_some());
        assert!(typechecker.values.get("foo").is_none())
    }

    // typecheck_chain

    #[rstest]
    fn test_typecheck_chain_ok(mut typechecker: Typechecker) {
        // given
        let lh = Expr::Lit(Lit::Int(32));
        let rh = Expr::Lit(Lit::Float("32.0".to_string()));
        let expected_result = TypedValue {
            v: Expr::Chain(
                Box::new(Expr::Lit(Lit::Int(32))),
                Box::new(Expr::Lit(Lit::Float("32.0".to_string()))),
            ),
            t: Type::Float,
        };

        // when
        let result = typechecker.typecheck_chain(lh, rh).unwrap();

        // then
        assert_eq!(result, expected_result)
    }

    // typecheck_call

    #[rstest]
    fn test_typecheck_call_ok(mut typechecker: Typechecker) {
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
                Vec::from([Type::Int, Type::Float, Type::Bool]),
                Type::String,
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
            t: Type::String,
        };

        // when
        let result = typechecker.typecheck_call(name, args).unwrap();

        // then
        assert_eq!(result, expected_result)
    }

    #[rstest]
    fn test_typecheck_call_error_not_found(mut typechecker: Typechecker) {
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
        mut typechecker: Typechecker,
        #[case] args: Vec<UntypedExpr>,
    ) {
        // given
        let name = "foo".to_string();
        typechecker.functions.insert(
            name.clone(),
            (
                Vec::from([Type::Int, Type::Float, Type::Bool]),
                Type::String,
            ),
        );

        // when
        let result = typechecker.typecheck_call(name, args);

        // then
        assert!(result.err().is_some())
    }

    // typecheck_if

    #[rstest]
    fn test_typecheck_if_ok(mut typechecker: Typechecker) {
        // given
        let be = Expr::Lit(Lit::Bool(true));
        let e1 = Expr::Lit(Lit::Int(1));
        let e2 = Expr::Lit(Lit::Int(2));
        let expected_result = TypedValue {
            v: Expr::If(
                Box::new(Expr::Lit(Lit::Bool(true))),
                Box::new(Expr::Lit(Lit::Int(1))),
                Box::new(Expr::Lit(Lit::Int(2))),
            ),
            t: Type::Int,
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
        mut typechecker: Typechecker,
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
    #[case("Int", Type::Int)]
    #[case("Bool", Type::Bool)]
    #[case("Float", Type::Float)]
    #[case("String", Type::String)]
    fn test_get_type_ok_simple(
        typechecker: Typechecker,
        #[case] s: &str,
        #[case] t: Type,
    ) {
        // given
        let s = s.to_string();

        // when
        let result = typechecker.get_type(s).unwrap();

        // then
        assert_eq!(result, t)
    }

    #[rstest]
    fn test_get_type_ok_user_defined(mut typechecker: Typechecker) {
        // given
        let s = "Foo".to_string();
        let args = Vec::from([
            ("bar".to_string(), Type::Int),
            ("baz".to_string(), Type::Float),
            ("qux".to_string(), Type::Bool),
        ]);
        typechecker.types.insert(s.clone(), args.clone());
        let expected_result = Type::Struct(Struct {
            name: s.clone(),
            args,
        });

        // when
        let result = typechecker.get_type(s).unwrap();

        // then
        assert_eq!(result, expected_result)
    }

    #[rstest]
    fn test_get_type_error(typechecker: Typechecker) {
        // given
        let s = "Foo".to_string();

        // when
        let result = typechecker.get_type(s);

        // then
        assert!(result.err().is_some())
    }
}
