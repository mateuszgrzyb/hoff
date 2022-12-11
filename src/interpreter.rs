use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::OptimizationLevel;

pub struct Interpreter<'ctx> {
    pub execution_engine: ExecutionEngine<'ctx>,
}

type MainFunc = unsafe extern "C" fn() -> u32;

impl<'ctx> Interpreter<'ctx> {
    pub fn create(module: Module<'ctx>, opt_level: u32) -> Self {
        let opt_level = match opt_level {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Less,
            2 => OptimizationLevel::Default,
            3 => OptimizationLevel::Aggressive,
            _ => OptimizationLevel::Default,
        };

        Self {
            execution_engine: module
                .create_jit_execution_engine(opt_level)
                .unwrap(),
        }
    }

    pub fn run(self) -> Result<(), String> {
        unsafe {
            let main = self.execution_engine.get_function::<MainFunc>("main");

            let Ok(main) = main else {
                return Err("Main function was not declared".to_string())
            };

            main.call();

            Ok(())
        }
    }
}
