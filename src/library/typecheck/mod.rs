mod closure_manager;
mod namespace;

use crate::library::ast::{
    qualified, typed, Decl, Expr, Fun, FunDecl, Imports, Lit, Mod, Op,
    SimpleType, Struct, Type, Val,
};
use closure_manager::ClosureManager;
use namespace::Namespace;
use std::collections::HashMap;

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

type CheckResult<V, T = typed::Type> = Result<TypedValue<V, T>, String>;

pub struct TypeChecker {
    values: HashMap<String, typed::Type>,
    functions: HashMap<String, (Vec<typed::Type>, typed::Type)>,
    types: HashMap<String, Vec<(String, typed::Type)>>,
    closure: Vec<(String, typed::Type)>,
    closure_manager: ClosureManager,
    namespace: Namespace,
    module: qualified::Mod,
}

impl TypeChecker {
    pub fn create() -> Self {
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
            closure: Vec::new(),
            closure_manager: ClosureManager::new(),
            namespace: Namespace::new(),
            module: qualified::Mod::empty(),
        }
    }

    pub fn get_type_of_expr(
        &mut self,
        expr: qualified::Expr,
    ) -> Result<(typed::Expr, typed::Type), String> {
        self.typecheck_expr(expr).map(|tv| (tv.v, tv.t))
    }

    fn get_struct(&self, name: String) -> Result<typed::Struct, String> {
        match self.types.get(name.as_str()) {
            Some(args) => Ok(Struct {
                name: name.to_string(),
                args: (*args).clone(),
            }),
            None => {
                match self
                    .module
                    .imports
                    .structs
                    .iter()
                    .find(|s| s.name == name)
                {
                    Some(s) => Ok((*s).clone()),
                    None => Err(format!("Cant find struct {}", name)),
                }
            }
        }
    }

    fn get_function(
        &self,
        name: String,
    ) -> Result<(Vec<typed::Type>, typed::Type), String> {
        if let Some(f) = self.functions.get(name.as_str()) {
            return Ok(f.clone());
        }

        if let Some(f) = self
            .module
            .imports
            .fundecls
            .iter()
            .find(|vd| vd.name == name)
        {
            let argts = f.args.clone().into_iter().map(|a| a.1).collect();
            let rt = f.rt.clone();
            return Ok((argts, rt));
        }

        Err(format!("Cant find function {}", name))
    }

    fn get_value(&self, name: String) -> Result<typed::Type, String> {
        if let Some(ts) = self.values.get(name.as_str()) {
            return Ok(ts.clone());
        }

        if let Some(ts) =
            self.module.imports.vals.iter().find(|vd| vd.name == name)
        {
            return Ok(ts.t.clone());
        }

        Err(format!("Cant find value {}", name))
    }

    fn get_type(&self, s: qualified::Type) -> Result<typed::Type, String> {
        match s {
            Type::Function(ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| self.get_type(t))
                    .collect::<Result<Vec<_>, _>>()?;
                return Ok(Type::Function(ts));
            }
            Type::Simple(s) => match s.as_str() {
                "Int" => Ok(Type::Simple(SimpleType::Int)),
                "Bool" => Ok(Type::Simple(SimpleType::Bool)),
                "Float" => Ok(Type::Simple(SimpleType::Float)),
                "String" => Ok(Type::Simple(SimpleType::String)),
                name => {
                    let s = self.get_struct(name.to_string())?;
                    Ok(Type::Simple(SimpleType::Struct(s)))
                }
            },
        }
    }

    pub fn check(&mut self, m: qualified::Mod) -> Result<typed::Mod, String> {
        self.module = m;

        let name = self.module.name.clone();
        let decls = self.typecheck_decls(self.module.decls.clone())?;
        let imports = self.module.imports.clone();

        Ok(Mod {
            name,
            decls,
            imports,
        })
    }

    pub fn typecheck_decls(
        &mut self,
        ds: Vec<qualified::Decl>,
    ) -> Result<Vec<typed::Decl>, String> {
        ds.into_iter()
            .map(|d| self.typecheck_decl(d).map(|tv| tv.v))
            .collect::<Result<_, _>>()
    }

    fn typecheck_decl(
        &mut self,
        d: qualified::Decl,
    ) -> CheckResult<typed::Decl> {
        match d {
            Decl::Fun(f) => self.typecheck_fun(f).map(|f| TypedValue {
                v: Decl::Fun(f.v),
                t: f.t,
            }),
            Decl::Struct(s) => self.typecheck_struct(s).map(|s| TypedValue {
                v: Decl::Struct(s.v),
                t: s.t,
            }),
            Decl::Val(v) => self.typecheck_val(v).map(|v| TypedValue {
                v: Decl::Val(v.v),
                t: v.t,
            }),
            Decl::Import(i) => self.typecheck_import(i).map(|i| TypedValue {
                v: Decl::Import(i.v),
                t: i.t,
            }),
        }
    }

    fn typecheck_fun(&mut self, f: qualified::Fun) -> CheckResult<typed::Fun> {
        let name = self.namespace.create_qualified_name(f.name);
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

        self.namespace.drop_qualified_name();

        if rt != body.t {
            return Err(format!(
                "Return type and body type does not match: {:?} != {:?}",
                rt, body.t,
            ));
        }

        let mut t = args.t;
        t.push(body.t);

        Ok(TypedValue {
            v: Fun {
                name,
                args: args.v,
                rt,
                body: body.v,
            },
            t: Type::Function(t),
        })
    }

    fn typecheck_struct(
        &mut self,
        struct_: qualified::Struct,
    ) -> CheckResult<typed::Struct> {
        let name = struct_.name;
        let args = struct_
            .args
            .into_iter()
            .map(|(n, t)| self.get_type(t).map(|t| (n, t)))
            .collect::<Result<Vec<_>, String>>()?;

        self.types.insert(name.clone(), args.clone());

        let struct_ = Struct { name, args };

        TypedValue::get(
            struct_.clone(),
            Type::Simple(SimpleType::Struct(struct_)),
        )
    }

    fn typecheck_val(
        &mut self,
        value: qualified::Val,
    ) -> CheckResult<typed::Val> {
        let name = value.name;
        let t = self.get_type(value.t)?;
        let expr = self.typecheck_expr(value.expr)?;

        if t != expr.t {
            return Err(format!(
                "Declared type and value type does not match: {:?} != {:?}",
                t, expr.t,
            ));
        }

        self.values.insert(name.clone(), expr.t.clone());

        Ok(TypedValue {
            v: Val {
                name,
                t,
                expr: expr.v,
            },
            t: expr.t,
        })
    }

    fn typecheck_import(
        &mut self,
        import_: qualified::Import,
    ) -> CheckResult<typed::Import> {
        let type_ = match import_.clone() {
            typed::Import::Fun(fd) => {
                let mut ft =
                    fd.args.into_iter().map(|a| a.1).collect::<Vec<_>>();
                ft.push(fd.rt);
                Type::Function(ft)
            }
            typed::Import::Struct(s) => Type::Simple(SimpleType::Struct(s)),
            typed::Import::Val(v) => v.t,
        };
        TypedValue::get(import_, type_)
    }

    fn typecheck_fun_args(
        &mut self,
        args: Vec<(String, qualified::Type)>,
    ) -> CheckResult<Vec<(String, typed::Type)>, Vec<typed::Type>> {
        let v = args
            .into_iter()
            .map(|(n, t)| self.get_type(t).map(|t| (n, t)))
            .collect::<Result<Vec<_>, String>>()?;

        self.closure_manager.append(v.clone());

        let t = v.clone().into_iter().map(|(_, t)| t).collect();

        Ok(TypedValue { v, t })
    }

    fn typecheck_expr(
        &mut self,
        expr: qualified::Expr,
    ) -> CheckResult<typed::Expr> {
        match expr {
            Expr::BinOp(lh, op, rh) => self.typecheck_binop(*lh, op, *rh),
            Expr::Lit(l) => self.typecheck_lit(l),
            Expr::Value(name) => self.typecheck_value(name),
            Expr::Assign((name, type_), val) => {
                self.typecheck_assign(name, type_, *val)
            }
            Expr::Chain(lh, rh) => self.typecheck_chain(*lh, *rh),
            Expr::Function(f, _) => {
                let closure = self.closure_manager.clone();
                self.closure_manager.push_layer();
                let f = self.typecheck_fun(*f)?;
                self.closure_manager.pop_layer();
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
        lh: qualified::Expr,
        op: Op,
        rh: qualified::Expr,
    ) -> CheckResult<typed::Expr> {
        let lh = self.typecheck_expr(lh)?;
        let rh = self.typecheck_expr(rh)?;

        if lh.t != rh.t {
            return Err(format!(
                "Unequal binop types: {:?} != {:?}",
                lh.t, rh.t
            ));
        }

        let (Type::Simple(lht), Type::Simple(rht)) = (lh.t.clone(), rh.t.clone()) else {
            return Err(format!(
                "Cannot run binary operation on functions: lh: {:?}, rh: {:?}",
                lh.t, rh.t
            ));
        };

        match (lht, op) {
            (
                SimpleType::Int | SimpleType::Float,
                op @ (Op::Add | Op::Sub | Op::Mul | Op::Div),
            ) => TypedValue::get(
                Expr::BinOp(Box::new(lh.v), op, Box::new(rh.v)),
                Type::Simple(rht),
            ),
            (
                SimpleType::Int | SimpleType::Float,
                op @ (Op::Lt | Op::Le | Op::Ne | Op::Eq | Op::Ge | Op::Gt),
            ) => TypedValue::get(
                Expr::BinOp(Box::new(lh.v), op, Box::new(rh.v)),
                Type::Simple(SimpleType::Bool),
            ),
            (SimpleType::Bool, op @ (Op::And | Op::Or)) => TypedValue::get(
                Expr::BinOp(Box::new(lh.v), op, Box::new(rh.v)),
                Type::Simple(rht),
            ),
            (lht, op) => Err(format!(
                "Invalid operation type: {:?} {:?} {:?}",
                lht, op, rh.t
            )),
        }
    }

    fn typecheck_lit(&self, lit: Lit) -> CheckResult<typed::Expr> {
        let t = match lit {
            Lit::Int(_) => SimpleType::Int,
            Lit::Bool(_) => SimpleType::Bool,
            Lit::Float(_) => SimpleType::Float,
            Lit::String(_) => SimpleType::String,
        };
        let v = Expr::Lit(lit);

        TypedValue::get(v, Type::Simple(t))
    }

    fn typecheck_value(&mut self, name: String) -> CheckResult<typed::Expr> {
        let type_ = self.get_value(name.clone())?;

        TypedValue::get(Expr::Value(name), type_.clone())
    }

    fn typecheck_assign(
        &mut self,
        name: String,
        type_: qualified::Type,
        expr: qualified::Expr,
    ) -> CheckResult<typed::Expr> {
        let type_ = self.get_type(type_)?;
        let expr = self.typecheck_expr(expr)?;

        if type_ != expr.t {
            return Err(format!(
                "Unequal assign types: {:?} != {:?}",
                type_, expr.t
            ));
        };

        self.values.insert(name.clone(), type_.clone());
        self.closure_manager.push(name.clone(), type_.clone());

        TypedValue::get(
            Expr::Assign((name, type_.clone()), Box::new(expr.v)),
            type_,
        )
    }

    fn typecheck_chain(
        &mut self,
        lh: qualified::Expr,
        rh: qualified::Expr,
    ) -> CheckResult<typed::Expr> {
        let lh = self.typecheck_expr(lh)?;
        let rh = self.typecheck_expr(rh)?;

        TypedValue::get(Expr::Chain(Box::new(lh.v), Box::new(rh.v)), rh.t)
    }

    fn typecheck_call(
        &mut self,
        name: String,
        args: Vec<qualified::Expr>,
    ) -> CheckResult<typed::Expr> {
        let (argvs, argts): (Vec<typed::Expr>, Vec<typed::Type>) = args
            .into_iter()
            .map(|a| self.typecheck_expr(a))
            .collect::<Result<Vec<TypedValue<typed::Expr, typed::Type>>, _>>()?
            .into_iter()
            .map(|TypedValue { v, t }| (v, t))
            .unzip();
        let name = self.namespace.get_qualified_name(name);
        let (exp_argts, rt) = self.get_function(name.clone())?;

        if exp_argts.len() < argts.len() {
            return Err(format!(
                "Invalid number of arguments: expected: {:?}, actual: {:?}",
                exp_argts.len(),
                argts.len()
            ));
        } else if exp_argts.len() == argts.len() {
            // regular function
            for (exp_argt, argt) in
                exp_argts.iter().zip::<Vec<typed::Type>>(argts)
            {
                if *exp_argt != argt {
                    return Err(format!(
                        "Invalid argument type: {:?} != {:?}",
                        exp_argt, argt
                    ));
                }
            }

            return TypedValue::get(
                Expr::Call(name.clone(), argvs),
                rt.clone(),
            );
        } else {
            // partial function application
            let mut exp_argts = exp_argts.clone();
            let mut exp_rt = exp_argts.split_off(argts.len());

            for (exp_argt, argt) in
                exp_argts.iter().zip::<Vec<typed::Type>>(argts)
            {
                if *exp_argt != argt {
                    return Err(format!(
                        "Invalid argument type: {:?} != {:?}",
                        exp_argt, argt
                    ));
                }
            }

            exp_rt.push(rt.clone());

            let partial_name = format!("partial-{}", name);
            let args = exp_rt
                .clone()
                .into_iter()
                .enumerate()
                .map(|p| (p.0.to_string(), p.1))
                .collect();
            let body = Expr::Call(
                name,
                argvs
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

            return TypedValue::get(
                Expr::Function(
                    Box::new(Fun {
                        name: partial_name,
                        args,
                        rt,
                        body,
                    }),
                    self.closure.clone(),
                ),
                Type::Function(exp_rt),
            );
        }
    }

    fn typecheck_if(
        &mut self,
        be: qualified::Expr,
        e1: qualified::Expr,
        e2: qualified::Expr,
    ) -> CheckResult<typed::Expr> {
        let be = self.typecheck_expr(be)?;
        let e1 = self.typecheck_expr(e1)?;
        let e2 = self.typecheck_expr(e2)?;

        if be.t != Type::Simple(SimpleType::Bool) || e1.t != e2.t {
            return Err(format!(
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
    ) -> CheckResult<typed::Expr> {
        let Some(Type::Simple(SimpleType::Struct(struct_))) = self.values.get(&*name) else {
            return Err(format!(
                "Struct `{name}` does not exist.",
            ));
        };

        let Some((_, type_)) = struct_.clone().args.into_iter().find(|(n, _)| n == &attr) else {
            return Err(format!(
                "Struct `{name}` does not have attribute `{attr}`."
            ));
        };

        TypedValue::get(Expr::Attr(name, struct_.clone(), attr), type_.clone())
    }

    fn typecheck_new(
        &mut self,
        name: String,
        args: Vec<qualified::Expr>,
    ) -> CheckResult<typed::Expr> {
        let args = args
            .into_iter()
            .map(|e| self.typecheck_expr(e).map(|tv| tv.v))
            .collect::<Result<Vec<typed::Expr>, String>>()?;

        let struct_args = self.get_struct(name.clone()).map(|s| s.args)?;

        TypedValue::get(
            Expr::New(name.clone(), args),
            Type::Simple(SimpleType::Struct(Struct {
                name: name.clone(),
                args: struct_args.clone(),
            })),
        )
    }
}
/*
#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    fn typechecker(module: qualified::Mod) -> TypeChecker {
        TypeChecker {
            values: Default::default(),
            functions: Default::default(),
            types: Default::default(),
            closure: Default::default(),
            closure_manager: ClosureManager::new(),
            namespace: Namespace::new(),
            module
        }
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
            v: Expr::Lit(lit.clone()),
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
            v: Expr::Value(name.clone()),
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
            v: Expr::Assign(
                (name.clone(), Type::Simple(SimpleType::Int)),
                Box::new(Expr::Lit(Lit::Int(32))),
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
    fn test_typecheck_assing_error(mut typechecker: TypeChecker) {
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
            v: Expr::Chain(
                Box::new(Expr::Lit(Lit::Int(32))),
                Box::new(Expr::Lit(Lit::Float("32.0".to_string()))),
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
            v: Expr::If(
                Box::new(Expr::Lit(Lit::Bool(true))),
                Box::new(Expr::Lit(Lit::Int(1))),
                Box::new(Expr::Lit(Lit::Int(2))),
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


 */