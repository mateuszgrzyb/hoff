use super::Backend;
use crate::library::backend::get_opt_level;
use current_platform::CURRENT_PLATFORM;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::fs::remove_file;
use std::path::Path;
use std::process::Command;

pub struct Compiler<'ctx> {
    module: Module<'ctx>,
    opt_level: OptimizationLevel,
    obj_file: String,
    output_file: String,
    target_machines: HashMap<&'static str, TargetConfiguration>,
}

struct TargetConfiguration {
    target_triple: TargetTriple,
    link_command: fn(&String, &String) -> Command,
}

impl TargetConfiguration {
    fn create(
        triple: &str,
        link: fn(&String, &String) -> Command,
    ) -> (&str, Self) {
        (
            triple,
            TargetConfiguration {
                target_triple: TargetTriple::create(triple),
                link_command: link,
            },
        )
    }
}

impl<'ctx> Backend for Compiler<'ctx> {
    fn run(&self) -> Result<(), String> {
        let target_config = self
            .target_machines
            .get(CURRENT_PLATFORM)
            .ok_or(format!("Unknown target: {}", CURRENT_PLATFORM))?;

        let target = Target::from_triple(&target_config.target_triple)
            .map_err(|err| err.to_string())?;

        let target_machine = target
            .create_target_machine(
                &target_config.target_triple,
                "",
                "",
                self.opt_level,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or("Cannot create target machine")?;

        target_machine
            .write_to_file(
                &self.module,
                FileType::Object,
                Path::new(&self.obj_file),
            )
            .map_err(|err| err.to_string())?;

        let command = target_config.link_command;
        command(&self.obj_file, &self.output_file)
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
        let mac_m1_triple = "aarch64-apple-darwin";
        fn mac_m1_link(input: &String, output: &String) -> Command {
            let mut c = Command::new("clang");
            c.args([&input, "-o", &output]);
            c
        }

        HashMap::from([TargetConfiguration::create(
            mac_m1_triple,
            mac_m1_link,
        )])
    }
}
