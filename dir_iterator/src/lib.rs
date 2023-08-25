use std::{
  collections::VecDeque,
  fs::{metadata, read_dir, DirEntry},
  io::Error,
  path::{Path, PathBuf},
};

#[macro_export]
macro_rules! err {
  ($V:expr) => {
    Box::<dyn std::error::Error>::from($V)
  };
}

#[derive(Clone)]
pub struct DirIterator {
  current: Option<PathBuf>,
  children: VecDeque<PathBuf>,
}

type R<V> = Result<V, Box<dyn std::error::Error>>;

impl DirIterator {
  pub fn create<AP>(current: AP) -> Self
  where
    AP: AsRef<Path>,
  {
    Self {
      current: None,
      children: VecDeque::from([current.as_ref().to_path_buf()]),
    }
  }

  fn next(&mut self) -> R<PathBuf> {
    if let Some(current) = &self.current {
      let md = metadata(current)?;

      if md.is_dir() {
        let current_children = read_dir(current)?
          .map(convert_dir_entry_to_path_name)
          .collect::<R<Vec<PathBuf>>>()?;

        self.children.append(&mut current_children.into());
      }
    };

    let next = self.children.pop_front().ok_or(err!("Empty queue"))?;

    self.current = Some(next.clone());

    Ok(next)
  }
}

fn convert_dir_entry_to_path_name(de: Result<DirEntry, Error>) -> R<PathBuf> {
  Ok(de?.path())
}

impl Iterator for DirIterator {
  type Item = PathBuf;

  fn next(&mut self) -> Option<Self::Item> {
    DirIterator::next(self).ok()
  }
}
