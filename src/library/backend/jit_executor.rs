use std::error::Error;

use inkwell::{
  execution_engine::ExecutionEngine,
  module::Module,
};

use super::Backend;
use crate::library::backend::get_opt_level;

pub struct JITExecutor<'ctx> {
  execution_engine: ExecutionEngine<'ctx>,
}

type MainFunc = unsafe extern "C" fn() -> u32;

impl<'ctx> JITExecutor<'ctx> {
  pub fn create(module: Module<'ctx>, opt_level: u32) -> Self {
    let opt_level = get_opt_level(opt_level);

    Self {
      execution_engine: module.create_jit_execution_engine(opt_level).unwrap(),
    }
  }
}

impl<'ctx> Backend for JITExecutor<'ctx> {
  fn run(&self) -> Result<(), Box<dyn Error>> {
    unsafe {
      let main = self.execution_engine.get_function::<MainFunc>("main");

      let Ok(main) = main else {
        return Err("Main function was not declared".into())
      };

      main.call();
    }

    Ok(())
  }
}