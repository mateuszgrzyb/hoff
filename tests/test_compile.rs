use rstest::*;
use std::fs::{read_dir, read_to_string};
use std::path::PathBuf;
use std::process::Command;

#[fixture]
fn command() -> Command {
    let mut path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    path.push("target/debug/testlang");
    Command::new(path)
}

#[fixture]
fn cli(mut command: Command) -> Command {
    command.args(["--emit-llvm", "--emit-llvm-target", "stdout"]);
    command
}

#[rstest]
#[case("tests/data/test1/input.hff", "tests/data/test1/output.ir")]
#[case("tests/data/test2/input.hff", "tests/data/test2/output.ir")]
#[case("tests/data/test3/input.hff", "tests/data/test3/output.ir")]
fn test_compile(
    mut cli: Command,
    #[case] input_file: String,
    #[case] output_file: String,
) {
    // configure all test cases
    for f in read_dir("tests/data").unwrap() {
        println!("{:?}", f)
    }

    // given
    let expected_output = read_to_string(output_file)
        .map(|s| s.trim_end_matches("\n").to_string())
        .unwrap();

    // when
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
