/* Description: ???

Copyright (C) 2024 Danny McClanahan <dmcC2@hypnicjerk.ai>
SPDX-License-Identifier: GPL-3.0-or-later

This file is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3 of the
License, or (at your option) any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>. */

//! ???

use std::{
  fs,
  io::{self, Seek, Write},
};

use displaydoc::Display;
use parking_lot::Mutex;
use thiserror::Error;

use super::{path, ConcatReceiver};

#[derive(Debug, Display, Error)]
pub enum FileHandleError {
  /// path error
  Path(#[from] path::PathError),
  /// i/o error
  Io(#[from] io::Error),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PathCreationBehavior {
  RequireParents,
  CreateParentsIfNotExists,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FileCreationBehavior {
  RequireNotExists(PathCreationBehavior),
  TruncateIfExists(PathCreationBehavior),
  AppendIfExists(PathCreationBehavior),
  RequireExistsForAppend,
}

impl FileCreationBehavior {
  fn require_not_exists(
    abs_path: path::AbsolutePath,
    path_behavior: PathCreationBehavior,
  ) -> Result<fs::File, FileHandleError> {
    let mut opts = fs::OpenOptions::new();
    opts.write(true).create_new(true);
    #[cfg(test)]
    opts.read(true);
    match path_behavior {
      PathCreationBehavior::RequireParents => opts.open(abs_path.as_ref()).map_err(|e| e.into()),
      PathCreationBehavior::CreateParentsIfNotExists => match opts.open(abs_path.as_ref()) {
        Ok(f) => Ok(f),
        Err(e) => match e.kind() {
          /* If the file already exists, that is an error case for this request, so propagate. */
          io::ErrorKind::AlreadyExists => Err(e.into()),
          io::ErrorKind::NotFound => {
            /* The file doesn't exist, but neither do its parents, so create them. */
            abs_path.parent()?.create_dir_all()?;
            opts.open(abs_path.as_ref()).map_err(|e| e.into())
          },
          _ => Err(e.into()),
        },
      },
    }
  }

  fn truncate_if_exists(
    abs_path: path::AbsolutePath,
    path_behavior: PathCreationBehavior,
  ) -> Result<fs::File, FileHandleError> {
    let mut opts = fs::OpenOptions::new();
    opts.write(true).truncate(true).create(true);
    #[cfg(test)]
    opts.read(true);
    match path_behavior {
      PathCreationBehavior::RequireParents => opts.open(abs_path.as_ref()).map_err(|e| e.into()),
      PathCreationBehavior::CreateParentsIfNotExists => match opts.open(abs_path.as_ref()) {
        Ok(f) => Ok(f),
        Err(e) => match e.kind() {
          /* This means the parents don't exist, so create them and try again. */
          io::ErrorKind::NotFound => {
            abs_path.parent()?.create_dir_all()?;
            opts.open(abs_path.as_ref()).map_err(|e| e.into())
          },
          _ => Err(e.into()),
        },
      },
    }
  }

  fn append_if_exists(
    abs_path: path::AbsolutePath,
    path_behavior: PathCreationBehavior,
  ) -> Result<fs::File, FileHandleError> {
    let mut opts = fs::OpenOptions::new();
    opts.write(true).create(true);
    #[cfg(test)]
    opts.read(true);
    let mut f = match path_behavior {
      PathCreationBehavior::RequireParents => opts.open(abs_path.as_ref())?,
      PathCreationBehavior::CreateParentsIfNotExists => match opts.open(abs_path.as_ref()) {
        Ok(f) => f,
        Err(e) => match e.kind() {
          /* This means the parents don't exist, so create them and try again. */
          io::ErrorKind::NotFound => {
            abs_path.parent()?.create_dir_all()?;
            opts.open(abs_path.as_ref())?
          },
          _ => return Err(e.into()),
        },
      },
    };
    f.seek(io::SeekFrom::End(0))?;
    Ok(f)
  }

  fn require_exists_for_append(abs_path: path::AbsolutePath) -> Result<fs::File, io::Error> {
    let mut opts = fs::OpenOptions::new();
    opts.write(true);
    #[cfg(test)]
    opts.read(true);
    let mut f = opts.open(abs_path.as_ref())?;
    f.seek(io::SeekFrom::End(0))?;
    Ok(f)
  }

  pub fn invoke(self, abs_path: path::AbsolutePath) -> Result<fs::File, FileHandleError> {
    match self {
      Self::RequireNotExists(path_behavior) => Self::require_not_exists(abs_path, path_behavior),
      Self::TruncateIfExists(path_behavior) => Self::truncate_if_exists(abs_path, path_behavior),
      Self::AppendIfExists(path_behavior) => Self::append_if_exists(abs_path, path_behavior),
      Self::RequireExistsForAppend => {
        Self::require_exists_for_append(abs_path).map_err(|e| e.into())
      },
    }
  }
}

#[derive(Debug)]
pub struct FileStream {
  inner: Mutex<fs::File>,
}

impl FileStream {
  pub const fn new(inner: fs::File) -> Self {
    Self {
      inner: Mutex::new(inner),
    }
  }

  pub fn into_inner(self) -> fs::File { self.inner.into_inner() }

  pub fn open(
    abs_path: path::AbsolutePath,
    create_behavior: FileCreationBehavior,
  ) -> Result<Self, FileHandleError> {
    let f = create_behavior.invoke(abs_path)?;
    Ok(Self::new(f))
  }
}

impl ConcatReceiver for FileStream {
  type Chunk<'chunk> = &'chunk [u8];
  type Ready = ();
  type E = io::Error;
  fn recv_chunk<'chunk>(&self, chunk: Self::Chunk<'chunk>) -> Result<(), io::Error> {
    self.inner.lock().write_all(chunk)?;
    Ok(())
  }

  fn finalize(&mut self) -> Result<(), io::Error> {
    self.inner.get_mut().sync_data()?;
    Ok(())
  }
}

#[cfg(test)]
mod test {
  use std::io::{self, Read, Seek};

  use tempfile::{tempdir, tempfile};

  use super::*;

  #[test]
  fn basic_recv() {
    let mut f = FileStream::new(tempfile().unwrap());

    f.recv_chunk(b"asdf").unwrap();
    f.recv_chunk(b"asdf").unwrap();
    f.finalize().unwrap();

    let mut f = f.into_inner();
    f.rewind().unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    assert_eq!(&s, "asdfasdf");
  }

  #[test]
  fn dir_creation() {
    let td = tempdir().unwrap();

    let p = path::AbsolutePath::new(td.path().join("a/b")).unwrap();
    match FileStream::open(
      p.clone(),
      FileCreationBehavior::RequireNotExists(PathCreationBehavior::RequireParents),
    ) {
      Err(FileHandleError::Io(e)) => assert_eq!(e.kind(), io::ErrorKind::NotFound),
      _ => unreachable!(),
    }

    let _f = FileStream::open(
      p,
      FileCreationBehavior::RequireNotExists(PathCreationBehavior::CreateParentsIfNotExists),
    )
    .unwrap();
  }

  #[test]
  fn resolve_file() {
    let td = tempdir().unwrap();

    let p = path::AbsolutePath::new(td.path().join("asdf")).unwrap();
    match FileStream::open(p.clone(), FileCreationBehavior::RequireExistsForAppend) {
      Err(FileHandleError::Io(e)) => assert_eq!(e.kind(), io::ErrorKind::NotFound),
      _ => unreachable!(),
    }

    {
      let mut f = FileStream::open(
        p.clone(),
        FileCreationBehavior::TruncateIfExists(PathCreationBehavior::RequireParents),
      )
      .unwrap();
      f.recv_chunk(b"asdf").unwrap();
      f.recv_chunk(b"asdf").unwrap();
      f.finalize().unwrap();
    }
    let mut f = FileStream::open(
      p,
      FileCreationBehavior::AppendIfExists(PathCreationBehavior::RequireParents),
    )
    .unwrap();
    f.recv_chunk(b"bsdf").unwrap();
    f.recv_chunk(b"bsdf").unwrap();
    f.finalize().unwrap();

    let mut f = f.into_inner();
    f.rewind().unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();
    assert_eq!(&s, "asdfasdfbsdfbsdf");
  }
}
