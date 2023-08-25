use std::{path::Path, rc::Rc};

use inkwell::{
  module::Module,
  targets::{CodeModel, FileType, RelocMode, Target},
  OptimizationLevel,
};

use crate::library::backend::get_opt_level;

use super::{
  target_configuration::TargetConfiguration,
  target_configuration_manager::TargetConfigurationManager,
};

use anyhow::{anyhow, Result};

pub struct ObjFileCompiler<'ctx> {
  pub module: Module<'ctx>,
  pub opt_level: OptimizationLevel,
  pub obj_file: String,
  pub target_machine: Rc<TargetConfiguration>,
}

impl<'ctx> ObjFileCompiler<'ctx> {
  pub fn create(
    module: Module<'ctx>,
    opt_level: u8,
    obj_file: String,
  ) -> Result<Self> {
    let opt_level = get_opt_level(opt_level);
    let target_machine = TargetConfigurationManager::create().get_config()?;

    Ok(Self {
      module,
      opt_level,
      obj_file,
      target_machine,
    })
  }

  pub fn create_object_file(&self) -> Result<()> {
    let target_triple = &self.target_machine.target_triple;
    let target = Target::from_triple(target_triple)
      .map_err(|e| anyhow!(e.to_string()))?;

    let target_machine = target
      .create_target_machine(
        target_triple,
        "",
        "",
        self.opt_level,
        RelocMode::Default,
        CodeModel::Default,
      )
      .ok_or(anyhow!("Cannot create target machine"))?;

    target_machine
      .write_to_file(&self.module, FileType::Object, Path::new(&self.obj_file))
      .map_err(|e| anyhow!(e.to_string()))?;

    Ok(())
  }
}
