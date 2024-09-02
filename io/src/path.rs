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
