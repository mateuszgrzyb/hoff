use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    pub files: Vec<String>,
    #[arg(long, short, default_value_t = false)]
    pub dump_ast: bool,
}
