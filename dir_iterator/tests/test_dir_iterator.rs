use std::env::current_dir;

use dir_iterator::DirIterator;

use rstest::*;

#[rstest]
#[case(
  "test_dir",
  &[
    "",
    "/a",
    "/b",
    "/b/e",
    "/b/d",
    "/b/c",
    "/b/e/f",
    "/b/e/g",
  ]
)]
#[case(
  "test_file",
  &[
    "",
  ]
)]
fn test_dir_iterator(
  #[case] test_path: &str,
  #[case] expected_results: &[&str],
) {
  // given
  let current_dir = current_dir().unwrap().to_str().unwrap().to_string();
  let current_dir = format!("{current_dir}/tests/{test_path}");

  let expected_result = Vec::from(expected_results);

  // when
  let current_dir_iterator = DirIterator::create(&current_dir);

  let result = current_dir_iterator
    .map(|e| {
      e.into_os_string()
        .into_string()
        .unwrap()
        .trim_start_matches(&current_dir)
        .into()
    })
    .collect::<Vec<String>>();

  // then
  assert_eq!(expected_result, result);
}
