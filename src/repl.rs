use crate::library::ast::untyped::Repl;
use crate::library::backend::REPL;
use crate::library::cli::Args;
use crate::library::parser::grammar::{DeclParser, ExprParser};
use inkwell::context::Context;
use std::io::stdin;

pub fn repl(args: Args) -> ! {
    let context = Context::create();

    let fds = Vec::new();
    let ss = Vec::new();

    let expr_parser = ExprParser::new();
    let decl_parser = DeclParser::new();

    'repl: loop {
        print!(">>>");
        let mut input = String::new();
        while !input.contains(";;") {
            let Ok(_) = stdin()
                .read_line(&mut input)
                .map_err(|err| println!("Invalid input: {}", err))
            else { continue 'repl };
        }

        let input = input.split(";;").next().unwrap_or("").to_string();

        let Ok(expr) = decl_parser
            .parse(&input)
            .map(|decl| Repl::Decl(decl))
            .or(expr_parser.parse(&input).map(|expr| Repl::Expr(expr)))
            .map_err(|err| println!("Parse error: {}", err))
        else { continue };

        let mut repl = REPL::create(&fds, &ss, &context, args.o);

        let Ok(result) = repl.eval(expr)
            .map_err(|err| println!("Eval error: {}", err))
        else { continue };

        println!("{}", result)
    }
}
