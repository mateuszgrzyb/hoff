use std::{
  fmt::Debug,
  fs::{read_to_string, write},
};

use anyhow::{anyhow, bail, Result};

use rayon::prelude::*;

use crate::library::{
  backend::compiler::obj_file_compiler::ObjFileCompiler,
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

pub fn dump_obj_files<'ctx, CGS>(args: &Args, codegens: CGS) -> Result<()>
where
  CGS: Iterator<Item = Result<CodeGen<'ctx>>>,
{
  if let DumpTarget::StdOut = args.dump_target {
    bail!("Standart Output dumping not supported for object files.")
  };

  for codegen in codegens {
    let codegen = codegen?;
    let filename = codegen.module.get_source_file_name().to_str()?.to_string();

    let ofc = ObjFileCompiler::create(codegen.module, args.o, filename)?;

    ofc.create_object_file()?
  }

  Ok(())
}

#[derive(Clone)]
pub struct InputFile {
  pub name: String,
  pub contents: String,
}

impl InputFile {
  pub fn par_read_files(
    paths: Vec<String>,
  ) -> impl ParallelIterator<Item = Result<Self>> + Clone {
    paths.into_par_iter().map(Self::read_file)
  }

  fn read_file(path: String) -> Result<Self> {
    let contents = read_to_string(&path)?;

    Ok(Self {
      name: path,
      contents,
    })
  }
}
