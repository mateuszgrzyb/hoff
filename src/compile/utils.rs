use std::{fmt::Debug, fs::write};

use inkwell::context::Context;

use anyhow::anyhow;
use anyhow::Result;

use crate::library::{
  cli::{Args, DumpTarget},
  codegen::CodeGen,
  qualify::Nameable,
};

pub fn dump<I, IS>(args: &Args, is: IS) -> Result<()>
where
  I: Debug + Nameable,
  IS: Iterator<Item = Result<I>>,
{
  for i in is {
    let i = i?;
    let contents = format!("{:#?}", i);
    match args.dump_target {
      DumpTarget::File => write(i.get_name(), contents)?,
      DumpTarget::StdOut => println!("{contents}"),
    }
  }

  Ok(())
}

pub fn dump_llvm<'ctx, CGS>(args: &Args, codegens: CGS) -> Result<()>
where
  CGS: Iterator<Item = Result<CodeGen<'ctx>>>,
{
  for codegen in codegens {
    let codegen = codegen?;
    let filename = codegen.module.get_source_file_name().to_str()?;

    match args.dump_target {
      DumpTarget::File => codegen
        .module
        .print_to_file(filename)
        .map_err(|e| anyhow!(e.to_string()))?,
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
