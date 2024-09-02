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

#![warn(rustdoc::missing_crate_level_docs)]
/* #![warn(missing_docs)] */
#![doc(test(attr(deny(warnings))))]

//! ???

use std::io::{self, Write};


/* Want to get something like output teeing from zip cli at https://github.com/cosmicexplorer/zip2/blob/cli/cli/src/extract.rs#L95-L113 */

pub trait ConcatReceiver {
  type Chunk<'chunk>;
  type Ready;
  type E;
  fn recv_chunk<'chunk>(&mut self, chunk: Self::Chunk<'chunk>) -> Result<Self::Ready, Self::E>;

  fn finalize(&mut self) -> Result<Self::Ready, Self::E>;
}

pub struct ByteStream<S> {
  inner: S,
}

impl<S> ByteStream<S> {
  pub const fn new(inner: S) -> Self { Self { inner } }
}

impl<S> ConcatReceiver for ByteStream<S>
where S: io::Write
{
  type Chunk<'chunk> = &'chunk [u8];
  type Ready = ();
  type E = io::Error;
  fn recv_chunk<'chunk>(&mut self, chunk: Self::Chunk<'chunk>) -> Result<(), io::Error> {
    self.inner.write_all(chunk)?;
    Ok(())
  }

  fn finalize(&mut self) -> Result<(), io::Error> { self.inner.flush() }
}

pub trait EntryReceiver {
  type EntrySpec;

  type E;
  type Receiver: ConcatReceiver;

  fn generate_entry_handle(&self, name: Self::EntrySpec) -> Result<Self::Receiver, Self::E>;
  fn finalize_entries(&mut self) -> Result<(), Self::E>;
}

pub mod path {
  use std::{
    fs, io,
    path::{Path, PathBuf},
  };

  use displaydoc::Display;
  use thiserror::Error;

  #[derive(Debug, Display, Error)]
  pub enum PathError {
    /// the given path should have been absolute, but wasn't: {0:?}
    WasNotAbsolute(PathBuf),
    /// the given path should have been relative, but wasn't: {0:?}
    WasNotRelative(PathBuf),
    /// the given path was expected to have a parent, but didn't: {0:?}
    HadNoParent(PathBuf),
  }

  #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
  pub struct AbsolutePath(PathBuf);

  impl AbsolutePath {
    pub fn new(inner: PathBuf) -> Result<Self, PathError> {
      if !inner.is_absolute() {
        Err(PathError::WasNotAbsolute(inner))
      } else {
        Ok(Self(inner))
      }
    }

    pub fn canonicalize(&self) -> Result<CanonicalPath, io::Error> {
      let s: &Path = self.as_ref();
      let p = s.canonicalize()?;
      Ok(CanonicalPath::new(p))
    }

    pub fn parent(&self) -> Result<Self, PathError> {
      let s: &Path = self.as_ref();
      match s.parent() {
        None => Err(PathError::HadNoParent(s.to_path_buf())),
        Some(p) => Ok(Self(p.to_path_buf())),
      }
    }

    /// Like [`fs::create_dir_all`], except handles concurrent calls among
    /// multiple threads or processes. Originally lifted from rustc, then from
    /// pants.
    fn safe_create_dir_all_ioerror(path: &Path) -> Result<(), io::Error> {
      match fs::create_dir(path) {
        Ok(()) => return Ok(()),
        Err(ref e) if e.kind() == io::ErrorKind::AlreadyExists => return Ok(()),
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {},
        Err(e) => return Err(e),
      }
      match path.parent() {
        Some(p) => Self::safe_create_dir_all_ioerror(p)?,
        None => return Ok(()),
      }
      match fs::create_dir(path) {
        Ok(()) => Ok(()),
        Err(ref e) if e.kind() == io::ErrorKind::AlreadyExists => Ok(()),
        Err(e) => Err(e),
      }
    }

    pub fn create_dir_all(&self) -> Result<(), io::Error> {
      Self::safe_create_dir_all_ioerror(self.as_ref())
    }
  }

  impl AsRef<Path> for AbsolutePath {
    fn as_ref(&self) -> &Path { self.0.as_ref() }
  }

  #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
  pub struct CanonicalPath(PathBuf);

  impl CanonicalPath {
    const fn new(inner: PathBuf) -> Self { Self(inner) }
  }

  impl AsRef<Path> for CanonicalPath {
    fn as_ref(&self) -> &Path { self.0.as_ref() }
  }

  impl From<CanonicalPath> for AbsolutePath {
    fn from(x: CanonicalPath) -> Self {
      let CanonicalPath(inner) = x;
      AbsolutePath(inner)
    }
  }

  #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
  pub struct RelativePath(PathBuf);

  impl RelativePath {
    pub fn new(inner: PathBuf) -> Result<Self, PathError> {
      if !inner.is_relative() {
        Err(PathError::WasNotRelative(inner))
      } else {
        Ok(Self(inner))
      }
    }
  }

  impl AsRef<Path> for RelativePath {
    fn as_ref(&self) -> &Path { self.0.as_ref() }
  }
}

pub mod filesystem {
  use std::{
    fs,
    io::{self, Seek, Write},
  };

  use displaydoc::Display;
  use indexmap::IndexMap;
  use thiserror::Error;

  use super::{path, ConcatReceiver};

  #[derive(Debug, Display, Error)]
  pub enum FileHandleError {
    /// path error
    Path(#[from] path::PathError),
    /// i/o error
    Io(#[from] io::Error),
  }

  pub enum PathCreationBehavior {
    RequireParents,
    CreateParentsIfNotExists,
  }

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
      let mut f = fs::OpenOptions::new().write(true).open(abs_path.as_ref())?;
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

  pub struct FileStream {
    inner: fs::File,
  }

  impl FileStream {
    pub const fn new(inner: fs::File) -> Self { Self { inner } }

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
    fn recv_chunk<'chunk>(&mut self, chunk: Self::Chunk<'chunk>) -> Result<(), io::Error> {
      self.inner.write_all(chunk)?;
      Ok(())
    }

    fn finalize(&mut self) -> Result<(), io::Error> {
      self.inner.sync_data()?;
      Ok(())
    }
  }

  pub struct FileSpec {
    relpath: path::RelativePath,
    create_behavior: FileCreationBehavior,
  }

  pub struct OutputDir {
    root: path::AbsolutePath,
    active_entries: IndexMap<path::CanonicalPath, FileStream>,
  }
}

#[cfg(test)]
mod test {


  #[test]
  fn f() {
    assert!(true);
  }
}
