mod ast;
mod cli;
mod codegen;
mod interpreter;
mod parser;
mod typecheck;

use clap::Parser;
use inkwell::context::Context;
use lalrpop_util::{lalrpop_mod, ParseError};
use std::error::Error;
use std::io::stdin;

use crate::ast::{Decl, Expr, Fun, Lit, Mod, Type, TypedMod};
use crate::cli::{Args, EmitLLVMTarget};
use crate::codegen::CodeGen;
use crate::grammar::{ExprParser, ModParser};
use crate::interpreter::Interpreter;
use crate::typecheck::Typechecker;

lalrpop_mod!(pub grammar);

fn main() -> Result<(), Box<dyn Error>> {
    let args: Args = Args::parse();

    if args.repl {
        repl(args)
    } else {
        compile(args)
    }
}

fn repl(args: Args) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let mod_parser = ModParser::new();
    let expr_parser = ExprParser::new();
    let mut typechecker = Typechecker::create();

    // let mut decls = Vec::new();

    loop {
        let mut input = String::new();
        while !input.contains(";;") {
            stdin().read_line(&mut input).unwrap();
        }
        let input = input.split(";;").next().unwrap_or("").to_string();

        let mut expr = expr_parser.parse(&input).unwrap();

        // decls.append(&mut ds);

        let typed_decls = typechecker.typecheck(Mod {
            name: "repl".to_string(),
            decls: Vec::from([Decl::Fun(Fun {
                name: "main".to_string(),
                args: Vec::new(),
                rt: "Int".to_string(),
                body: expr,
                /*
                body: Expr::Chain(
                    Box::new(expr),
                    Box::new(Expr::Lit(Lit::Int(0))),
                ),
                 */
            })]),
        })?;

        let mut codegen = CodeGen::create(&context, true)?;

        codegen.compile_module(typed_decls)?;

        println!("{}", codegen.module.to_string());

        continue;

        let interpreter = Interpreter::create(codegen.module, args.O);

        interpreter.run()?;
    }
}

pub fn compile(args: Args) -> Result<(), Box<dyn Error>> {
    let files = args
        .files
        .into_iter()
        .map(|name| {
            let file = std::fs::read_to_string(name.clone())?;
            Ok((name, file))
        })
        .collect::<Result<Vec<(String, String)>, Box<dyn Error>>>()?;

    let parser = ModParser::new();

    let modules = files.into_iter().map(|(name, file)| Mod {
        name,
        decls: parser.parse(&file).unwrap(),
    });

    if args.dump_ast && !args.dump_typed_ast {
        for module in modules {
            println!("{module:#?}")
        }
        return Ok(());
    };

    let typed_modules = modules
        .into_iter()
        .map(|module| {
            let mut typechecker = Typechecker::create();
            typechecker.typecheck(module)
        })
        .collect::<Result<Vec<TypedMod>, _>>()?;

    if args.dump_typed_ast {
        for module in typed_modules {
            println!("{module:#?}")
        }
        return Ok(());
    };

    let mut contexts = Vec::new();
    for _ in 0..typed_modules.len() {
        contexts.push(Context::create())
    }

    let codegens = contexts
        .iter()
        .zip(typed_modules)
        .map(|(context, module)| {
            let mut codegen = CodeGen::create(context, true)?;

            codegen.compile_module(module)?;
            if !args.no_verify_llvm {
                codegen.module.verify()?;
            }

            Ok(codegen)
        })
        .collect::<Result<Vec<CodeGen>, Box<dyn Error>>>()?;

    if args.emit_llvm {
        for codegen in codegens {
            let filename = codegen.module.get_source_file_name().to_str()?;

            match args.emit_llvm_target {
                EmitLLVMTarget::IrFile => {
                    codegen.module.print_to_file(filename)?
                }
                EmitLLVMTarget::StdOut => {
                    println!("{}", codegen.module.to_string())
                }
            }
        }
        return Ok(());
    }

    let m = codegens
        .into_iter()
        .map(|codegen| codegen.module)
        .reduce(|m1, m2| {
            m1.link_in_module(m2).unwrap();
            m1
        })
        .ok_or_else(|| format!("No files were compiled"))?;

    let interpreter = Interpreter::create(m, args.O);
    interpreter.run()?;

    Ok(())
}
