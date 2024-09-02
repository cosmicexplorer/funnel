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

use std::ops;

pub trait ConcatReceiver {
  type Chunk<'chunk>;
  type Ready;
  type E;
  fn recv_chunk<'chunk>(&self, chunk: Self::Chunk<'chunk>) -> Result<Self::Ready, Self::E>;

  fn finalize(&mut self) -> Result<Self::Ready, Self::E>;
}

pub trait EntryReceiver {
  type EntryName;
  type EntrySpec;

  type E;
  type Receiver: ConcatReceiver;

  fn generate_entry_handle(
    &mut self,
    name: Self::EntryName,
    spec: Self::EntrySpec,
  ) -> Result<(), Self::E>;

  fn dereference_entry(
    &self,
    name: Self::EntryName,
  ) -> Result<impl ops::Deref<Target=Self::Receiver>, Self::E>;

  fn finalize_entries(&mut self) -> Result<(), Self::E>;
}

pub mod stream_wrapper {
  use std::io;

  use parking_lot::Mutex;

  use super::ConcatReceiver;

  #[derive(Debug)]
  pub struct ByteStream<S> {
    inner: Mutex<S>,
  }

  impl<S> ByteStream<S> {
    pub const fn new(inner: S) -> Self {
      Self {
        inner: Mutex::new(inner),
      }
    }

    pub fn into_inner(self) -> S { self.inner.into_inner() }
  }

  impl<S> ConcatReceiver for ByteStream<S>
  where S: io::Write
  {
    type Chunk<'chunk> = &'chunk [u8];
    type Ready = ();
    type E = io::Error;
    fn recv_chunk<'chunk>(&self, chunk: Self::Chunk<'chunk>) -> Result<(), io::Error> {
      self.inner.lock().write_all(chunk)?;
      Ok(())
    }

    fn finalize(&mut self) -> Result<(), io::Error> { self.inner.get_mut().flush() }
  }

  #[cfg(test)]
  mod test {
    use std::io::Cursor;

    use super::*;

    #[test]
    fn simple_wrapper() {
      let out = Cursor::new(Vec::new());
      let mut s = ByteStream::new(out);

      s.recv_chunk(b"asdf").unwrap();
      s.recv_chunk(b"asdf").unwrap();
      s.finalize().unwrap();

      let out = s.into_inner().into_inner();
      assert_eq!(&out, b"asdfasdf");
    }
  }
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

pub mod file_handle {
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
}

pub mod output_dir {
  use std::{io, sync::Arc};

  use displaydoc::Display;
  use indexmap::IndexMap;
  use thiserror::Error;

  use super::{file_handle, path, ConcatReceiver, EntryReceiver};

  #[derive(Debug, Display, Error)]
  pub enum DirBookkeepingError {
    /** entry with name {1:?} and spec {2:?} could not be created, due to
     * entry {0:?} already existing at canonical path {3:?} */
    OverlappingCanonicalPath(String, String, FileSpec, path::CanonicalPath),
    /// entry with name {0:?} already exists
    NameAlreadyExists(String),
    /// entry with name {0:?} not found
    NameNotFound(String),
    /** not all handles were dropped for entry {0:?} when output dir was
     * finalized */
    NotAllHandlesDropped(String),
  }

  #[derive(Debug, Display, Error)]
  pub enum OutputDirError {
    /// path error
    Path(#[from] path::PathError),
    /// i/o error
    Io(#[from] io::Error),
    /// file handle error
    File(#[from] file_handle::FileHandleError),
    /// dir bookkeeping error
    Bookkeeping(#[from] DirBookkeepingError),
  }

  #[derive(Debug, Clone)]
  pub struct FileSpec {
    pub relpath: path::RelativePath,
    pub create_behavior: file_handle::FileCreationBehavior,
  }

  pub struct OutputDir {
    root: path::AbsolutePath,
    active_entries: IndexMap<String, Arc<file_handle::FileStream>>,
    path_mapping: IndexMap<path::CanonicalPath, String>,
  }

  impl OutputDir {
    pub fn create(root: path::AbsolutePath) -> Result<Self, io::Error> {
      root.create_dir_all()?;
      Ok(Self {
        root,
        active_entries: IndexMap::new(),
        path_mapping: IndexMap::new(),
      })
    }
  }

  impl EntryReceiver for OutputDir {
    type EntryName = String;
    type EntrySpec = FileSpec;

    type E = OutputDirError;
    type Receiver = file_handle::FileStream;

    fn generate_entry_handle(
      &mut self,
      name: String,
      spec: FileSpec,
    ) -> Result<(), OutputDirError> {
      if self.active_entries.contains_key(&name) {
        return Err(DirBookkeepingError::NameAlreadyExists(name).into());
      }
      let FileSpec {
        relpath,
        create_behavior,
      } = spec.clone();
      let new_path = path::AbsolutePath::new(self.root.as_ref().join(relpath.as_ref()))?;

      let canon_path: Option<path::CanonicalPath> = match new_path.canonicalize() {
        Ok(canon_path) => {
          if let Some(existing_name) = self.path_mapping.get(&canon_path) {
            return Err(
              DirBookkeepingError::OverlappingCanonicalPath(
                existing_name.clone(),
                name,
                spec,
                canon_path,
              )
              .into(),
            );
          }
          Some(canon_path)
        },
        Err(_) => None,
      };

      let new_stream = file_handle::FileStream::open(new_path.clone(), create_behavior)?;
      assert!(self
        .active_entries
        .insert(name.clone(), Arc::new(new_stream))
        .is_none());

      let canon_path = match canon_path {
        Some(canon_path) => canon_path,
        None => new_path.canonicalize()?,
      };
      assert!(self.path_mapping.insert(canon_path, name).is_none());
      Ok(())
    }

    #[allow(refining_impl_trait)]
    fn dereference_entry(
      &self,
      name: String,
    ) -> Result<Arc<file_handle::FileStream>, OutputDirError> {
      match self.active_entries.get(&name) {
        None => Err(DirBookkeepingError::NameNotFound(name).into()),
        Some(entry) => Ok(Arc::clone(&entry)),
      }
    }

    fn finalize_entries(&mut self) -> Result<(), OutputDirError> {
      for (name, mut stream) in self.active_entries.drain(..) {
        let stream = match Arc::get_mut(&mut stream) {
          None => return Err(DirBookkeepingError::NotAllHandlesDropped(name).into()),
          Some(stream) => stream,
        };
        stream.finalize()?;
      }
      self.path_mapping.clear();
      Ok(())
    }
  }

  #[cfg(test)]
  mod test {
    use std::fs;

    use tempfile::tempdir;

    use super::*;
    use crate::file_handle::{FileCreationBehavior, PathCreationBehavior};

    #[test]
    fn resolve_file() {
      let td = tempdir().unwrap();

      let root = path::AbsolutePath::new(td.path().to_path_buf()).unwrap();
      let mut dir = OutputDir::create(root).unwrap();

      let p1 = path::RelativePath::new("asdf".into()).unwrap();
      let spec1 = FileSpec {
        relpath: p1,
        create_behavior: FileCreationBehavior::TruncateIfExists(
          PathCreationBehavior::RequireParents,
        ),
      };
      let p2 = path::RelativePath::new("bsdf".into()).unwrap();
      let spec2 = FileSpec {
        relpath: p2,
        create_behavior: FileCreationBehavior::TruncateIfExists(
          PathCreationBehavior::RequireParents,
        ),
      };

      let s1 = "entry-a".to_string();
      let s2 = "entry-b".to_string();
      dir
        .generate_entry_handle(s1.clone(), spec1.clone())
        .unwrap();

      /* (1) check matching name */
      assert!(matches!(
        dir.generate_entry_handle(s1.clone(), spec1.clone()),
        Err(OutputDirError::Bookkeeping(
          DirBookkeepingError::NameAlreadyExists(_)
        ))
      ));
      /* (2) check matching path */
      assert!(matches!(
        dir.generate_entry_handle(s2.clone(), spec1),
        Err(OutputDirError::Bookkeeping(
          DirBookkeepingError::OverlappingCanonicalPath(_, _, _, _)
        ))
      ));
      /* (3) check separate name */
      dir.generate_entry_handle(s2.clone(), spec2).unwrap();

      {
        /* Get outputs, write to them, then drop them. */
        let f1 = dir.dereference_entry(s1.clone()).unwrap();
        let f2 = dir.dereference_entry(s2.clone()).unwrap();
        assert!(matches!(
          dir.dereference_entry("bbbbb".to_string()),
          Err(OutputDirError::Bookkeeping(
            DirBookkeepingError::NameNotFound(_)
          ))
        ));

        f1.recv_chunk(b"1234").unwrap();
        f2.recv_chunk(b"5678").unwrap();
      }

      dir.finalize_entries().unwrap();

      let o1 = fs::read(td.path().join("asdf")).unwrap();
      assert_eq!(b"1234".as_ref(), &o1);
      let o2 = fs::read(td.path().join("bsdf")).unwrap();
      assert_eq!(b"5678".as_ref(), &o2);
    }

    #[test]
    fn not_all_dropped() {
      let td = tempdir().unwrap();

      let root = path::AbsolutePath::new(td.path().to_path_buf()).unwrap();
      let mut dir = OutputDir::create(root).unwrap();

      let p1 = path::RelativePath::new("asdf".into()).unwrap();
      let spec1 = FileSpec {
        relpath: p1,
        create_behavior: FileCreationBehavior::TruncateIfExists(
          PathCreationBehavior::RequireParents,
        ),
      };

      let s1 = "entry-a".to_string();
      dir
        .generate_entry_handle(s1.clone(), spec1.clone())
        .unwrap();

      let _f1 = dir.dereference_entry(s1.clone()).unwrap();

      assert!(matches!(
        dir.finalize_entries(),
        Err(OutputDirError::Bookkeeping(
          DirBookkeepingError::NotAllHandlesDropped(_)
        ))
      ));
    }
  }
}
