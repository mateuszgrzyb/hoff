use std::{
  fmt::Debug,
  fs::{read_dir, read_to_string, write},
};

use itertools::Itertools;
use rayon::{iter::once, prelude::*};

use crate::library::{
  cli::{Args, DumpTarget},
  codegen::Codegen,
  qualify::Nameable,
};

use derive_more::From;

pub fn dump<I, IS, E>(args: &Args, is: IS) -> Result<(), E>
where
  I: Debug + Nameable,
  IS: ParallelIterator<Item = Result<I, E>>,
  E: Send,
{
  is.try_for_each(|i: Result<I, E>| -> Result<(), E> {
    let i = i?;
    let contents = format!("{:#?}", i);
    match args.dump_target {
      //DumpTarget::File => write(i.get_name(), contents)?,
      DumpTarget::File => write(i.get_name(), contents).unwrap(),
      DumpTarget::StdOut => println!("{contents}"),
    }

    Ok(())
  })?;

  Ok(())
}

pub fn dump_llvm<'ctx, CGS, E>(args: &Args, codegens: CGS) -> Result<(), E>
where
  CGS: Iterator<Item = Result<Codegen<'ctx>, E>>,
{
  for codegen in codegens {
    let codegen = codegen?;
    //let filename = codegen.module.get_source_file_name().to_str()?;
    let filename = codegen.module.get_source_file_name().to_str().unwrap();

    match args.dump_target {
      DumpTarget::File => codegen.module.print_to_file(filename).unwrap(),
      //.map_err(|e| anyhow!(e.to_string()))?,
      DumpTarget::StdOut => {
        println!("{}", codegen.module.to_string())
      }
    }
  }

  Ok(())
}

#[derive(Debug, From)]
pub enum InputFileError {
  IOError(std::io::Error),
  OSError(std::ffi::OsString),
}

impl Clone for InputFileError {
  fn clone(&self) -> Self {
    todo!()
    // match self {
    //   Self::IOError(arg0) => Self::IOError(arg0.clone()),
    // }
  }
}

#[derive(Clone)]
pub struct InputFile {
  pub name: String,
  pub contents: String,
}

impl InputFile {
  pub fn read_files(
    paths: Vec<String>,
  ) -> impl ParallelIterator<Item = Result<Self, InputFileError>> + Clone {
    Self::_read_files(paths.into_par_iter().map(Ok))
  }

  fn _read_files<PS>(
    paths: PS,
  ) -> impl ParallelIterator<Item = Result<Self, InputFileError>> + Clone
  where
    PS: ParallelIterator<Item = Result<String, InputFileError>> + Clone,
  {
    paths.flat_map(Self::_read_file)
  }

  fn _read_file(
    path: Result<String, InputFileError>,
  ) -> impl ParallelIterator<Item = Result<Self, InputFileError>> + Clone {
    let file: Result<InputFile, InputFileError> = path.and_then(|path| {
      let contents = read_to_string(&path)?;
      Ok(InputFile {
        name: path,
        contents,
      })
    });

    once(file)
  }

  fn _read_dir(
    path: String,
  ) -> impl ParallelIterator<Item = Result<Self, InputFileError>> + Clone {
    let sub_paths = read_dir(path);

    let sub_paths = match sub_paths {
      Ok(o) => o.into_iter().collect_vec(),
      Err(e) => vec![Err(e)],
    };

    let sub_paths = sub_paths
      .into_iter()
      .map(|p| {
        Ok(p?.path().into_os_string().into_string()?)
        //.map_err(|e| format!("{:?}", e))
      })
      .collect_vec();

    Self::_read_files(sub_paths.into_par_iter())
  }
}
