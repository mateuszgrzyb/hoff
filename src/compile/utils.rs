use std::{fmt::Debug, fs::write};

use anyhow::{anyhow, Result};

use rayon::prelude::*;

use crate::library::{
  cli::{Args, DumpTarget},
  codegen::CodeGen,
  qualify::Nameable,
};

pub fn dump<I, IS>(args: &Args, is: IS) -> Result<()>
where
  I: Debug + Nameable,
  IS: ParallelIterator<Item = Result<I>>,
{
  is.try_for_each(|i: Result<I>| -> Result<()> {
    let i = i?;
    let contents = format!("{:#?}", i);
    match args.dump_target {
      DumpTarget::File => write(i.get_name(), contents)?,
      DumpTarget::StdOut => println!("{contents}"),
    }

    Ok(())
  })?;

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
