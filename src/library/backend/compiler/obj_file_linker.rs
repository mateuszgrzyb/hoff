use std::{fs::remove_file, rc::Rc};

use super::{
  target_configuration::TargetConfiguration,
  target_configuration_manager::TargetConfigurationManager,
};

use anyhow::{ensure, Result};

pub struct ObjFileLinker {
  pub obj_files: Vec<String>,
  pub output_file: String,
  pub target_machine: Rc<TargetConfiguration>,
}

impl ObjFileLinker {
  pub fn create(obj_files: Vec<String>, output_file: String) -> Result<Self> {
    let target_machine = TargetConfigurationManager::create().get_config()?;

    Ok(Self {
      obj_files,
      output_file,
      target_machine,
    })
  }

  pub fn link(&self) -> Result<()> {
    let command = self.target_machine.link_command;

    let output = command(&self.obj_files[..], &self.output_file).output()?;

    let error = String::from_utf8(output.stderr)?;

    for file in &self.obj_files {
      remove_file(file)?;
    }

    ensure!(error.is_empty(), error);

    Ok(())
  }
}
