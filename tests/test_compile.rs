use rstest::*;
use std::fs::{read_dir, read_to_string, DirEntry};
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
    command.args(["-d", "llvmir", "-t", "stdout"]);
    command
}

fn split_into_two<DES>(
    dir: &String,
    dir_entries: DES,
    match_fst: fn(&String) -> bool,
    match_snd: fn(&String) -> bool,
) -> Result<(Vec<String>, Vec<String>), Vec<String>>
where
    DES: Iterator<Item = std::io::Result<DirEntry>>,
{
    let mut fst = Vec::new();
    let mut snd = Vec::new();
    let mut mismatched = Vec::new();
    for e in dir_entries {
        let file_name = e.unwrap().file_name().into_string().unwrap();

        let v = if match_fst(&file_name) {
            &mut fst
        } else if match_snd(&file_name) {
            &mut snd
        } else {
            &mut mismatched
        };

        v.push(format!("{}/{}", dir, file_name))
    }

    fst.sort();
    snd.sort();
    mismatched.sort();

    if mismatched.is_empty() {
        Ok((fst, snd))
    } else {
        Err(mismatched)
    }
}

#[rstest]
#[case("test1")]
#[case("test2")]
#[case("test3")]
#[case("test4")]
fn test_compile(mut cli: Command, #[case] dir: &str) {
    // given
    let dir = "tests/data/".to_string() + dir;
    let (input_files, output_files) = split_into_two(
        &dir,
        read_dir(&dir).unwrap(),
        |name| name.ends_with(".hff"),
        |name| name.ends_with(".ir"),
    )
    .unwrap();

    println!("{:#?} {:#?}", input_files, output_files);

    let expected_output = output_files
        .into_iter()
        .map(|of| {
            read_to_string(of)
                .map(|s| s.trim_end_matches("\n").to_string())
                .unwrap()
        })
        .collect::<Vec<_>>()
        .join("\n\n");

    // when
    let output = cli.args(input_files).output().unwrap();

    // then
    let stdout = String::from_utf8(output.stdout)
        .map(|s| s.trim_end_matches("\n").to_string())
        .unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();

    //println!("{}", stdout);
    //println!("{}", stderr);

    assert!(stderr.is_empty());
    assert_eq!(stdout, expected_output);
}
