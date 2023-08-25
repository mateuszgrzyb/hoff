use std::{collections::HashMap, process::Command, rc::Rc};

use current_platform::CURRENT_PLATFORM;
use inkwell::targets::{InitializationConfig, Target};

use super::target_configuration::TargetConfiguration;

use anyhow::{bail, Result};

pub struct TargetConfigurationManager {
  configs: HashMap<&'static str, Rc<TargetConfiguration>>,
}

impl TargetConfigurationManager {
  pub fn create() -> Self {
    let config = InitializationConfig::default();
    Target::initialize_aarch64(&config);
    Target::initialize_x86(&config);

    let mac_m1_triple = "aarch64-apple-darwin";
    let linux_x86_triple = "x86_64-unknown-linux-gnu";

    fn clang_link(input: &[String], output: &str) -> Command {
      let mut c = Command::new("gcc");
      c.args(["-no-pie", "-o", output]);
      c.args(input);
      c
    }

    let configs = HashMap::from([
      TargetConfiguration::create(mac_m1_triple, clang_link),
      TargetConfiguration::create(linux_x86_triple, clang_link),
    ]);

    Self { configs }
  }

  pub fn get_config(&mut self) -> Result<Rc<TargetConfiguration>> {
    let Some(config) = self.configs.get(CURRENT_PLATFORM) else {
      bail!("Invalid target triple: {}", CURRENT_PLATFORM)
    };

    Ok(config.clone())
  }
}
