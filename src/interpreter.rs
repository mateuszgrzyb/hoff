use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use std::mem::transmute;

pub struct Interpreter<'ctx> {
    pub execution_engine: ExecutionEngine<'ctx>,
}

type MainFunc = unsafe extern "C" fn() -> u32;

impl<'ctx> Interpreter<'ctx> {
    pub fn create(module: Module<'ctx>, opt_level: u32) -> Self {
        let opt_level = Self::get_opt_level(opt_level);

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
        }

        Ok(())
    }

    fn get_opt_level(opt_level: u32) -> OptimizationLevel {
        if opt_level > OptimizationLevel::Aggressive as u32 {
            return OptimizationLevel::Default;
        }
        unsafe { transmute::<u32, OptimizationLevel>(opt_level) }
    }
}

#[cfg(test)]
mod test {
    use crate::interpreter::Interpreter;
    use inkwell::*;
    use rstest::*;

    #[rstest]
    #[case(0, OptimizationLevel::None)]
    #[case(1, OptimizationLevel::Less)]
    #[case(2, OptimizationLevel::Default)]
    #[case(3, OptimizationLevel::Aggressive)]
    #[case(4, OptimizationLevel::Default)]
    #[case(5, OptimizationLevel::Default)]
    #[case(5, OptimizationLevel::Default)]
    #[case(6, OptimizationLevel::Default)]
    #[case(7, OptimizationLevel::Default)]
    #[case(8, OptimizationLevel::Default)]
    #[case(9, OptimizationLevel::Default)]
    #[case(10, OptimizationLevel::Default)]
    #[case(11, OptimizationLevel::Default)]
    #[case(12, OptimizationLevel::Default)]
    fn test_get_opt_level(
        #[case] int_opt_level: u32,
        #[case] exp_opt_level: OptimizationLevel,
    ) {
        // when
        let opt_level = Interpreter::get_opt_level(int_opt_level);

        // then
        assert_eq!(opt_level, exp_opt_level)
    }
}
