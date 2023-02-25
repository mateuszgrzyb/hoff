use crate::library::ast::untyped::Repl;
use crate::library::ast::ValDecl;
use crate::library::backend::REPL;
use crate::library::cli::Args;
use crate::library::parser::grammar::{DeclParser, ExprParser};
use inkwell::context::Context;
use std::error::Error;
use std::io::{stdin, stdout, Write};

fn parse_repl_line(
    expr_parser: &ExprParser,
    decl_parser: &DeclParser,
    input: String,
) -> Result<Repl, Box<dyn Error>> {
    if let Ok(decl) = decl_parser.parse(&input) {
        return Ok(Repl::Decl(decl));
    }

    if let Ok(expr) = expr_parser.parse(&input) {
        return Ok(Repl::Expr(expr));
    }

    return Err("Parse error".into());
}

fn print_and_flush(text: &str) {
    print!("{}", text);
    stdout()
        .flush()
        .unwrap_or_else(|err| panic!("flush error: {}", err));
}

pub fn repl(args: Args) -> ! {
    let context = Context::create();

    let fds = Vec::new();
    let ss = Vec::new();
    let vs = Vec::new();

    let expr_parser = ExprParser::new();
    let decl_parser = DeclParser::new();

    'repl: loop {
        print_and_flush(">>>");
        let mut input = String::new();
        while !input.contains(";;") {
            if let Err(err) = stdin().read_line(&mut input) {
                println!("Invalid input: {}", err);
                continue 'repl;
            }
            print_and_flush("...");
        }

        let input = input.split(";;").next().unwrap().to_string();

        let Ok(expr) = parse_repl_line(&expr_parser, &decl_parser, input)
            .map_err(|err| println!("Parse error: {}", err))
        else { continue };

        let mut repl = REPL::create(&fds, &ss, &vs, &context, args.o);

        let Ok(result) = repl.eval(expr)
            .map_err(|err| println!("Eval error: {}", err))
        else { continue };

        println!("{}", result)
    }
}
