use crate::library::ast::NamedModule;
use crate::library::cli::{Args, DumpTarget};
use crate::library::codegen::CodeGen;
use inkwell::context::Context;
use std::error::Error;
use std::fmt::Debug;
use std::fs::write;

pub fn dump<I, T>(args: &Args, ms: I) -> Result<(), Box<dyn Error>>
where
    T: Debug + NamedModule,
    I: Iterator<Item = T>,
{
    for m in ms {
        let contents = format!("{:?}", m);
        match args.dump_target {
            DumpTarget::File => write(m.get_name(), contents)?,
            DumpTarget::StdOut => println!("{contents}"),
        }
    }

    Ok(())
}

pub fn dump_llvm<'ctx>(
    args: &Args,
    codegens: Vec<CodeGen<'ctx>>,
) -> Result<(), Box<dyn Error>> {
    for codegen in codegens {
        let filename = codegen.module.get_source_file_name().to_str()?;

        match args.dump_target {
            DumpTarget::File => codegen.module.print_to_file(filename)?,
            DumpTarget::StdOut => {
                println!("{}", codegen.module.to_string())
            }
        }
    }

    Ok(())
}

pub fn initialize_contexts(n: usize) -> Vec<Context> {
    let mut contexts = Vec::new();
    for _ in 0..n {
        contexts.push(Context::create())
    }
    contexts
}
