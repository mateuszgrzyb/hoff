use super::Backend;
use crate::library::backend::get_opt_level;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;

pub struct Interpreter<'ctx> {
    execution_engine: ExecutionEngine<'ctx>,
}

type MainFunc = unsafe extern "C" fn() -> u32;

impl<'ctx> Interpreter<'ctx> {
    pub fn create(module: Module<'ctx>, opt_level: u32) -> Self {
        let a: String = String::from("aal");

        let opt_level = get_opt_level(opt_level);

        Self {
            execution_engine: module
                .create_jit_execution_engine(opt_level)
                .unwrap(),
        }
    }
}

impl<'ctx> Backend for Interpreter<'ctx> {
    fn run(&self) -> Result<(), String> {
        unsafe {
            let main = self.execution_engine.get_function::<MainFunc>("main");

            let Ok(main) = main else {
                return Err("Main function was not declared".to_string())
            };

            main.call();
        }

        Ok(())
    }
}
