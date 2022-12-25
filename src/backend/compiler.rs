use super::Backend;
use crate::backend::get_opt_level;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;
use std::fs::remove_file;
use std::path::Path;
use std::process::Command;

pub struct Compiler<'ctx> {
    module: Module<'ctx>,
    opt_level: OptimizationLevel,
    target_triple: TargetTriple,
    obj_file: String,
    output_file: String,
}

impl<'ctx> Backend for Compiler<'ctx> {
    fn run(&self) -> Result<(), String> {
        let target = Target::from_triple(&self.target_triple)
            .map_err(|err| err.to_string())?;
        let target_machine = target
            .create_target_machine(
                &self.target_triple,
                "",
                "",
                self.opt_level,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or("target triple")?;

        target_machine
            .write_to_file(
                &self.module,
                FileType::Object,
                Path::new(&self.obj_file),
            )
            .map_err(|err| err.to_string())?;

        //Command::new("ld64.lld")
        //.args(["-execute", "-o", "main.exe", &self.obj_file])
        Command::new("clang")
            .args([&self.obj_file, "-o", &self.output_file])
            .output()
            .map_err(|err| err.to_string())
            .and_then(|output| {
                let error = String::from_utf8(output.stderr)
                    .map_err(|err| err.to_string())?;

                if !error.is_empty() {
                    return Err(error);
                }

                Ok(())
            })?;

        remove_file(&self.obj_file).map_err(|err| err.to_string())?;

        Ok(())
    }
}

impl<'ctx> Compiler<'ctx> {
    pub fn create(module: Module<'ctx>, opt_level: u32) -> Self {
        let config = InitializationConfig::default();
        Target::initialize_all(&config);

        Self {
            module,
            opt_level: get_opt_level(opt_level),
            target_triple: TargetTriple::create("arm64-apple-macosx13.0.0"),
            obj_file: "main.o".to_string(),
            output_file: "main.exe".to_string(),
        }
    }
}
