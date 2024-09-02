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

use std::{io, sync::Arc};

use displaydoc::Display;
use indexmap::IndexMap;
use thiserror::Error;

use super::{file, path, ConcatReceiver, EntryReceiver};

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
  File(#[from] file::FileHandleError),
  /// dir bookkeeping error
  Bookkeeping(#[from] DirBookkeepingError),
}

#[derive(Debug, Clone)]
pub struct FileSpec {
  pub relpath: path::RelativePath,
  pub create_behavior: file::FileCreationBehavior,
}

pub struct OutputDir {
  root: path::AbsolutePath,
  active_entries: IndexMap<String, Arc<file::FileStream>>,
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
  type Receiver = file::FileStream;

  fn generate_entry_handle(&mut self, name: String, spec: FileSpec) -> Result<(), OutputDirError> {
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

    let new_stream = file::FileStream::open(new_path.clone(), create_behavior)?;
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
  fn dereference_entry(&self, name: String) -> Result<Arc<file::FileStream>, OutputDirError> {
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

  use super::{
    super::file::{FileCreationBehavior, PathCreationBehavior},
    *,
  };

  #[test]
  fn resolve_file() {
    let td = tempdir().unwrap();

    let root = path::AbsolutePath::new(td.path().to_path_buf()).unwrap();
    let mut dir = OutputDir::create(root).unwrap();

    let p1 = path::RelativePath::new("asdf".into()).unwrap();
    let spec1 = FileSpec {
      relpath: p1,
      create_behavior: FileCreationBehavior::TruncateIfExists(PathCreationBehavior::RequireParents),
    };
    let p2 = path::RelativePath::new("bsdf".into()).unwrap();
    let spec2 = FileSpec {
      relpath: p2,
      create_behavior: FileCreationBehavior::TruncateIfExists(PathCreationBehavior::RequireParents),
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
      create_behavior: FileCreationBehavior::TruncateIfExists(PathCreationBehavior::RequireParents),
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
