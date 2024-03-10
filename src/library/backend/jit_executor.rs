use inkwell::{execution_engine::ExecutionEngine, module::Module};

use super::Backend;
use crate::library::backend::get_opt_level;

pub struct JitExecutor<'ctx> {
  execution_engine: ExecutionEngine<'ctx>,
}

#[derive(Debug, Clone)]
pub enum JitExecutorError {
  MainNotDeclaredError,
}

type MainFunc = unsafe extern "C" fn() -> u32;

impl<'ctx> JitExecutor<'ctx> {
  pub fn create(module: Module<'ctx>, opt_level: u8) -> Self {
    let opt_level = get_opt_level(opt_level);

    Self {
      execution_engine: module.create_jit_execution_engine(opt_level).unwrap(),
    }
  }
}

impl<'ctx> Backend for JitExecutor<'ctx> {
  type E = JitExecutorError;

  fn run(&self) -> Result<(), Self::E> {
    unsafe {
      let main = self.execution_engine.get_function::<MainFunc>("main");

      let Ok(main) = main else {
        return Err(JitExecutorError::MainNotDeclaredError);
      };

      main.call();
    }

    Ok(())
  }
}
