use std::{error::Error, fmt::Debug, fs::write};

use inkwell::context::Context;

use crate::library::{
  cli::{Args, DumpTarget},
  codegen::CodeGen,
  qualify::Nameable,
};

pub fn dump<I, T>(args: &Args, ms: I) -> Result<(), Box<dyn Error>>
where
  T: Debug + Nameable,
  I: Iterator<Item = T>,
{
  for m in ms {
    let contents = format!("{:#?}", m);
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
