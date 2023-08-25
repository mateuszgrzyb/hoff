use anyhow::Result;
use inkwell::module::Module;

use self::{
  obj_file_compiler::ObjFileCompiler, obj_file_linker::ObjFileLinker,
  target_configuration_manager::TargetConfigurationManager,
};

use super::{get_opt_level, Backend};

pub mod obj_file_compiler;
pub mod obj_file_linker;
pub mod target_configuration;
pub mod target_configuration_manager;

pub struct Compiler<'ctx> {
  obj_file_compiler: ObjFileCompiler<'ctx>,
  obj_file_linker: ObjFileLinker,
}

impl<'ctx> Backend for Compiler<'ctx> {
  fn run(&self) -> Result<()> {
    self.obj_file_compiler.create_object_file()?;
    self.obj_file_linker.link()?;
    Ok(())
  }
}

impl<'ctx> Compiler<'ctx> {
  pub fn create(module: Module<'ctx>, opt_level: u8) -> Result<Self> {
    let mut manager = TargetConfigurationManager::create();

    let obj_file_compiler = ObjFileCompiler {
      module,
      opt_level: get_opt_level(opt_level),
      obj_file: "main.o".into(),
      target_machine: manager.get_config()?,
    };

    let obj_file_linker = ObjFileLinker {
      obj_files: vec!["main.o".into()],
      output_file: "main.exe".into(),
      target_machine: manager.get_config()?,
    };

    Ok(Self {
      obj_file_compiler,
      obj_file_linker,
    })
  }
}
