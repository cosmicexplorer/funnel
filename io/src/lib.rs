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

pub mod merge;
pub mod split;


#[macro_export]
macro_rules! interruptible_buffered_io_op {
  ($op:expr) => {
    match $op {
      Ok(n) => n,
      Err(e) if e.kind() == ::std::io::ErrorKind::Interrupted => continue,
      Err(e) => return Err(e),
    }
  };
}

pub mod util {
  use std::io::{self, Read, Write};

  pub struct TakeWrite<W> {
    inner: W,
    limit: u64,
  }

  impl<W> TakeWrite<W> {
    pub const fn take(inner: W, limit: u64) -> Self { Self { inner, limit } }

    #[allow(dead_code)]
    #[inline(always)]
    pub const fn limit(&self) -> u64 { self.limit }

    #[allow(dead_code)]
    pub fn into_inner(self) -> W { self.inner }
  }

  impl<W> Write for TakeWrite<W>
  where W: Write
  {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
      if self.limit == 0 {
        return Ok(0);
      }

      let buf_len: u64 = buf.len().try_into().unwrap();
      let to_write_offset: u64 = buf_len.min(self.limit);
      let to_write: usize = to_write_offset.try_into().unwrap();

      let num_written: usize = self.inner.write(&buf[..to_write])?;
      assert!(num_written <= to_write);
      let num_written_offset: u64 = num_written.try_into().unwrap();
      self.limit -= num_written_offset;
      Ok(num_written)
    }

    fn flush(&mut self) -> io::Result<()> { self.inner.flush() }
  }

  pub fn copy_via_buf(
    r: &mut (impl Read+?Sized),
    w: &mut (impl Write+?Sized),
    buf: &mut [u8],
  ) -> io::Result<u64> {
    assert!(!buf.is_empty());
    let mut total_copied: u64 = 0;

    loop {
      let num_read: usize = interruptible_buffered_io_op![r.read(buf)];
      if num_read == 0 {
        break;
      }
      let num_read_offset: u64 = num_read.try_into().unwrap();

      /* TODO: use a ring buffer instead of .write_all() here? */
      w.write_all(&buf[..num_read])?;
      total_copied += num_read_offset;
    }

    Ok(total_copied)
  }

  #[cfg(test)]
  mod test {
    use std::{
      fs,
      io::{self, Cursor, Seek},
    };

    use tempfile;

    use super::*;

    fn readable_file(input: &[u8]) -> io::Result<fs::File> {
      let mut i = tempfile::tempfile()?;
      i.write_all(input)?;
      i.rewind()?;
      Ok(i)
    }

    #[test]
    fn take_write_copy() {
      let mut i = readable_file(b"asdf".as_ref()).unwrap();
      let out = Cursor::new(Vec::new());
      let mut limited = TakeWrite::take(out, 3);
      assert_eq!(3, limited.limit());

      let mut buf = [0u8; 15];

      assert_eq!(
        io::ErrorKind::WriteZero,
        copy_via_buf(&mut i, &mut limited, &mut buf[..])
          .err()
          .unwrap()
          .kind()
      );
      assert_eq!(0, limited.limit());
      let out = limited.into_inner().into_inner();
      assert_eq!(&out[..], b"asd".as_ref());
    }
  }
}
