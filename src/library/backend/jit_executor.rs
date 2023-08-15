use anyhow::bail;
use anyhow::Result;
use inkwell::{execution_engine::ExecutionEngine, module::Module};

use super::Backend;
use crate::library::backend::get_opt_level;

pub struct JITExecutor<'ctx> {
  execution_engine: ExecutionEngine<'ctx>,
}

type MainFunc = unsafe extern "C" fn() -> u32;

impl<'ctx> JITExecutor<'ctx> {
  pub fn create(module: Module<'ctx>, opt_level: u8) -> Self {
    let opt_level = get_opt_level(opt_level);

    Self {
      execution_engine: module.create_jit_execution_engine(opt_level).unwrap(),
    }
  }
}

impl<'ctx> Backend for JITExecutor<'ctx> {
  fn run(&self) -> Result<()> {
    unsafe {
      let main = self.execution_engine.get_function::<MainFunc>("main");

      let Ok(main) = main else {
        bail!("Main function was not declared")
      };

      main.call();
    }

    Ok(())
  }
}
