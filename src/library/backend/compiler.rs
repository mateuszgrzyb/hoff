use std::{
  collections::HashMap, fs::remove_file, path::Path, process::Command,
  string::FromUtf8Error, sync::Arc,
};

use current_platform::CURRENT_PLATFORM;
use derive_more::From;
use inkwell::{
  module::Module,
  targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
  },
  OptimizationLevel,
};

use super::Backend;
use crate::library::backend::get_opt_level;

pub struct Compiler<'ctx> {
  module: Module<'ctx>,
  opt_level: OptimizationLevel,
  obj_file: String,
  output_file: String,
  target_machines: HashMap<&'static str, TargetConfiguration>,
}

struct TargetConfiguration {
  target_triple: TargetTriple,
  link_command: fn(&str, &str) -> Command,
}

impl TargetConfiguration {
  fn create(triple: &str, link: fn(&str, &str) -> Command) -> (&str, Self) {
    (
      triple,
      TargetConfiguration {
        target_triple: TargetTriple::create(triple),
        link_command: link,
      },
    )
  }
}

#[derive(Debug, From, Clone)]
pub enum CompilerError {
  #[from(ignore)]
  UnknownTarget(&'static str),
  #[from(ignore)]
  TargetTriple(String),
  CreateTargetMachine,
  #[from(ignore)]
  WriteTargetMachineToFile(String),
  IO(Arc<std::io::Error>),
  Utf8(FromUtf8Error),
  #[from(ignore)]
  Command(String),
}

impl<'ctx> Backend for Compiler<'ctx> {
  type E = CompilerError;

  fn run(&self) -> Result<(), Self::E> {
    let target_config = self
      .target_machines
      .get(CURRENT_PLATFORM)
      .ok_or(CompilerError::UnknownTarget(CURRENT_PLATFORM))?;

    let target = Target::from_triple(&target_config.target_triple)
      .map_err(|e| CompilerError::TargetTriple(e.to_string()))?;

    let target_machine = target
      .create_target_machine(
        &target_config.target_triple,
        "",
        "",
        self.opt_level,
        RelocMode::Default,
        CodeModel::Default,
      )
      .ok_or(CompilerError::CreateTargetMachine)?;

    target_machine
      .write_to_file(&self.module, FileType::Object, Path::new(&self.obj_file))
      .map_err(|e| CompilerError::WriteTargetMachineToFile(e.to_string()))?;

    let command = target_config.link_command;

    let output = command(&self.obj_file, &self.output_file)
      .output()
      .map_err(Arc::new)?;

    let error = String::from_utf8(output.stderr)?;

    if !error.is_empty() {
      return Err(CompilerError::Command(error));
    }

    remove_file(&self.obj_file).map_err(Arc::new)?;

    Ok(())
  }
}

impl<'ctx> Compiler<'ctx> {
  pub fn create(module: Module<'ctx>, opt_level: u8) -> Self {
    let config = InitializationConfig::default();

    Self {
      module,
      opt_level: get_opt_level(opt_level),
      obj_file: "main.o".to_string(),
      output_file: "main.exe".to_string(),
      target_machines: Self::get_target_machines(&config),
    }
  }

  fn get_target_machines(
    config: &InitializationConfig,
  ) -> HashMap<&'static str, TargetConfiguration> {
    Target::initialize_aarch64(config);
    Target::initialize_x86(config);

    let mac_m1_triple = "aarch64-apple-darwin";
    let linux_x86_triple = "x86_64-unknown-linux-gnu";

    fn clang_link(input: &str, output: &str) -> Command {
      let mut c = Command::new("gcc");
      c.args(["-no-pie", "-o", output, input]);
      c
    }

    HashMap::from([
      TargetConfiguration::create(mac_m1_triple, clang_link),
      TargetConfiguration::create(linux_x86_triple, clang_link),
    ])
  }
}
