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

use std::ops;

pub mod dir;
pub mod file;
pub mod path;


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
