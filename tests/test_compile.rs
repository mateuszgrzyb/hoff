use std::{
  fs::{read_dir, read_to_string, DirEntry},
  path::PathBuf,
  process::Command,
};

use colored::{ColoredString, Colorize};
use rstest::*;
use similar::{ChangeTag, TextDiff};

#[fixture]
fn command() -> Command {
  let mut path = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
  path.push("target/debug/hoff");
  Command::new(path)
}

#[fixture]
fn cli(mut command: Command) -> Command {
  command.args(["-d", "llvmir", "-t", "stdout", "--sort-decls"]);
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

fn bold_red(diff_str: String) -> ColoredString {
  diff_str.bold().red()
}

fn bold_green(diff_str: String) -> ColoredString {
  diff_str.bold().green()
}

fn black(diff_str: String) -> ColoredString {
  diff_str.black()
}

#[rstest]
#[case("test1")]
#[case("test2")]
#[case("test3")]
#[case("test4")]
#[case("test5")]
#[case("test6")]
#[case("test7")]
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

  println!("stderr: {}", stderr);
  assert!(stderr.is_empty());

  let diff = TextDiff::from_lines(&stdout, &expected_output);

  for change in diff.iter_all_changes() {
    let (sign, colorizer): (&str, fn(String) -> ColoredString) =
      match change.tag() {
        ChangeTag::Delete => ("-", bold_red),
        ChangeTag::Insert => ("+", bold_green),
        ChangeTag::Equal => (" ", black),
      };
    let diff_str = colorizer(format!("{sign} {change}"));

    print!("{diff_str}");
  }

  assert!(stdout == expected_output);
}
