extern crate core;

mod ast;
mod backend;
mod cli;
mod codegen;
mod import_qualifier;
mod parser;
mod typecheck;

use clap::Parser;
use inkwell::context::Context;
use std::error::Error;
use std::io::stdin;

use crate::ast::typed::*;
use crate::ast::untyped;
use crate::backend::{Backend, Compiler, Interpreter, REPL};
use crate::cli::{Args, DumpMode, DumpTarget, RunMode};
use crate::codegen::CodeGen;
use crate::import_qualifier::ImportQualifier;
use crate::parser::grammar::{ExprParser, ModParser};
use crate::typecheck::Typechecker;

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
    let mut repl = REPL::create(&context);
    let expr_parser = ExprParser::new();

    loop {
        let mut input = String::new();
        while !input.contains(";;") {
            stdin().read_line(&mut input).unwrap();
        }
        let input = input.split(";;").next().unwrap_or("").to_string();

        let expr = match expr_parser.parse(&input) {
            Err(err) => {
                println!("Parse error: {}", err);
                continue;
            }
            Ok(expr) => expr,
        };

        let result = match repl.eval(expr) {
            Err(err) => {
                println!("Eval error: {}", err);
                continue;
            }
            Ok(result) => result,
        };

        println!("{}", result)
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

    let modules = files.into_iter().map(|(name, file)| untyped::Mod {
        name,
        decls: parser.parse(&file).unwrap(),
    });

    if let DumpMode::Ast = args.dump_mode {
        for module in modules {
            println!("{module:#?}")
        }
        return Ok(());
    };

    let qualified_modules = modules
        .into_iter()
        .map(|module| {
            let mut qualifier = ImportQualifier::create();
            qualifier.qualify(module)
        })
        .collect::<Result<Vec<_>, _>>()?;

    let typed_modules = qualified_modules
        .into_iter()
        .map(|module| {
            let mut typechecker = Typechecker::create();
            typechecker.typecheck(module)
        })
        .collect::<Result<Vec<Mod>, _>>()?;

    if let DumpMode::TypedAst = args.dump_mode {
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
            let mut codegen = CodeGen::create(context, true);

            codegen.compile_module(module);
            if !args.no_verify_llvm {
                codegen.module.verify()?;
            }

            Ok(codegen)
        })
        .collect::<Result<Vec<CodeGen>, Box<dyn Error>>>()?;

    if let DumpMode::LlvmIr = args.dump_mode {
        for codegen in codegens {
            let filename = codegen.module.get_source_file_name().to_str()?;

            match args.dump_target {
                DumpTarget::File => codegen.module.print_to_file(filename)?,
                DumpTarget::StdOut => {
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

    let backend: Box<dyn Backend> = match args.mode {
        RunMode::JIT => Box::new(Interpreter::create(m, args.o)),
        RunMode::Compile => Box::new(Compiler::create()),
    };

    backend.run()?;

    Ok(())
}
