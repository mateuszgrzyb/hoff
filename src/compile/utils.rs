use std::{fmt::Debug, fs::{write, read_dir, read_to_string}};

use anyhow::{anyhow, Result};

use itertools::Itertools;
use rayon::{prelude::*, iter::once};

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


#[derive(Clone)]
pub struct InputFile {
  pub name: String,
  pub contents: String,
}

impl InputFile {
  pub fn read_files(paths: Vec<String>) -> impl ParallelIterator<Item = Result<Self>> + Clone {
    Self::_read_files(paths.into_par_iter().map(Ok)).map(|i| i.map_err(|e| anyhow!(e)))
  }

  fn _read_files<PS>(
    paths: PS,
  ) -> impl ParallelIterator<Item = Result<Self, String>> + Clone 
  where
    PS: ParallelIterator<Item = Result<String, String>> + Clone
  {
    paths.flat_map(|path| Self::_read_file(path))
  }

  fn _read_file(path: Result<String, String>) -> impl ParallelIterator<Item = Result<Self, String>> + Clone {
    let file: Result<Self, String> = 
      path.and_then(|path| -> Result<Self, String> {
        match read_to_string(&path) {
          Err(e) => Err(e.to_string()),
          Ok(contents) => Ok(InputFile { name: path, contents }),
        }
      });

    once(file)
  }

  fn _read_dir(path: String) -> impl ParallelIterator<Item = Result<Self, String>> + Clone {
    let sub_paths = read_dir(&path);

    let sub_paths = match sub_paths {
        Ok(o) => o.into_iter().map(|e| e.map_err(|e| format!("{:?}", e))).collect_vec(),
        Err(e) => vec![Err(e.to_string())],
    };

    let sub_paths = sub_paths
      .into_iter()
      .map(|p| 
        Ok(
          p
            .map_err(|e| format!("{:?}", e))?
            .path()
            .into_os_string()
            .into_string()
            .map_err(|e| format!("{:?}", e))?
          )
        )
      .collect_vec();

    Self::_read_files(sub_paths.into_par_iter())
  }
}
