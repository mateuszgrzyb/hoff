use rstest::*;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::process::Command;

#[fixture]
fn command() -> Command {
    let mut path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    path.push("target/debug/hoff");
    Command::new(path)
}

#[fixture]
fn cli(mut command: Command) -> Command {
    command.args(["--emit-llvm", "--emit-llvm-target", "stdout"]);
    command
}

#[rstest]
#[case("test1/input.hff", "test1/output.ir")]
#[case("test2/input.hff", "test2/output.ir")]
#[case("test3/input.hff", "test3/output.ir")]
fn test_compile(
    mut cli: Command,
    #[case] input_file: &str,
    #[case] output_file: &str,
) {
    // given
    let output_file = "tests/data/".to_string() + output_file;
    let expected_output = read_to_string(output_file)
        .map(|s| s.trim_end_matches("\n").to_string())
        .unwrap();

    // when
    let input_file = "tests/data/".to_string() + input_file;
    let output = cli
        .arg(input_file)
        .output()
        .expect("Failed to execute command");

    // then
    let stdout = String::from_utf8(output.stdout)
        .map(|s| s.trim_end_matches("\n").to_string())
        .unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();

    println!("{}", stdout);
    println!("{}", stderr);

    assert!(stderr.is_empty());
    assert_eq!(stdout, expected_output);
}
