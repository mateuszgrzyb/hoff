mod ast;
mod cli;
mod codegen;
mod old_parser;
mod parser;

use crate::ast::Mod;
use crate::cli::Args;
use crate::codegen::CodeGen;
use clap::Parser;
use inkwell::context::Context;
use lalrpop_util::lalrpop_mod;
use std::error::Error;

lalrpop_mod!(pub grammar);

use crate::grammar::ModParser;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Args = Args::parse();

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
        funs: parser.parse(file.as_str()).unwrap(),
    });

    if args.dump_ast {
        for module in modules {
            println!("{module:#?}")
        }
        return Ok(());
    };

    for module in modules {
        let context = Context::create();
        let mut codegen = CodeGen::create(&context)?;
        codegen.compile_module(module)?;
        codegen.module.verify()?;
        let filename = codegen.module.get_source_file_name().to_str()?;
        codegen.module.print_to_file(filename)?;
    }

    Ok(())
}
