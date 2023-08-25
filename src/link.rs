use anyhow::Result;

use crate::library::{
  backend::compiler::obj_file_linker::ObjFileLinker, cli::Args,
};
pub struct Linker {
  args: Args,
}

impl Linker {
  pub fn create(args: Args) -> Self {
    Self { args }
  }

  pub fn link(&self) -> Result<()> {
    let linker =
      ObjFileLinker::create(self.args.paths.clone(), "main.exe".into())?;
    linker.link()?;

    Ok(())
  }
}
