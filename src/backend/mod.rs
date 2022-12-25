mod compiler;
mod interpreter;
mod repl;

pub use compiler::Compiler;
use inkwell::OptimizationLevel;
pub use interpreter::Interpreter;
pub use repl::REPL;
use std::mem::transmute;

pub trait Backend {
    fn run(&self) -> Result<(), String>;
}

fn get_opt_level(opt_level: u32) -> OptimizationLevel {
    if opt_level > OptimizationLevel::Aggressive as u32 {
        return OptimizationLevel::Default;
    }
    unsafe { transmute::<u32, OptimizationLevel>(opt_level) }
}

#[cfg(test)]
mod test {
    use super::*;
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
        let opt_level = get_opt_level(int_opt_level);

        // then
        assert_eq!(opt_level, exp_opt_level)
    }
}
