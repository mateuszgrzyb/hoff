use rstest::*;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::process::Command;

#[fixture]
fn cli() -> Command {
    let mut path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    path.push("target/debug/testlang");
    let mut command = Command::new(path);
    command.args(["--emit-llvm", "--emit-llvm-target", "std-out"]);
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
    // given
    let expected_output = read_to_string(output_file).unwrap();
    let expected_output = expected_output.trim_end_matches("\n");

    // when
    let output = cli
        .arg(input_file)
        .output()
        .expect("Failed to execute command");

    // then
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stdout = stdout.trim_end_matches("\n");
    let stderr = String::from_utf8(output.stderr).unwrap();

    assert_eq!(stderr, "");
    assert_eq!(stdout, expected_output);
}
