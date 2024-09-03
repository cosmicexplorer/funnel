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

use std::{io, mem::MaybeUninit, ops};

use crate::interruptible_buffered_io_op;

pub trait FixedFile {
  fn extent(&self) -> u64;

  #[inline(always)]
  fn convert_range(&self, range: impl ops::RangeBounds<u64>) -> io::Result<ops::Range<u64>> {
    let len = self.extent();
    let start = match range.start_bound() {
      ops::Bound::Included(&start) => start,
      ops::Bound::Excluded(start) => start
        .checked_add(1)
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "start too large"))?,
      ops::Bound::Unbounded => 0,
    };
    let end = {
      let unclamped_end = match range.end_bound() {
        ops::Bound::Included(end) => end
          .checked_add(1)
          .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "end too large"))?,
        ops::Bound::Excluded(&end) => end,
        ops::Bound::Unbounded => len,
      };
      #[allow(clippy::let_and_return)]
      let clamped_end = unclamped_end.min(len);
      clamped_end
    };

    if start > end {
      return Err(io::Error::new(
        io::ErrorKind::InvalidInput,
        "start past end",
      ));
    }
    Ok(ops::Range { start, end })
  }

  #[inline(always)]
  fn range_len(&self, start: u64, len: usize) -> io::Result<usize> {
    let len: u64 = len.try_into().unwrap();
    let ops::Range { start, end } = self.convert_range(start..(start + len))?;
    let len: u64 = end - start;
    Ok(len.try_into().unwrap())
  }
}

pub trait InputFile: FixedFile {
  fn pread(&self, start: u64, buf: &mut [MaybeUninit<u8>]) -> io::Result<usize>;

  fn pread_all(&self, start: u64, buf: &mut [MaybeUninit<u8>]) -> io::Result<()> {
    let len: usize = buf.len();
    let mut input_offset: u64 = start;
    let mut remaining_to_read: usize = len;

    while remaining_to_read > 0 {
      let num_read: usize = interruptible_buffered_io_op![
        self.pread(input_offset, &mut buf[(len - remaining_to_read)..])
      ];
      if num_read == 0 {
        return Err(io::Error::new(
          io::ErrorKind::UnexpectedEof,
          "pread less than expected range",
        ));
      }
      assert!(num_read <= remaining_to_read);
      remaining_to_read -= num_read;
      let num_read_offset: u64 = num_read.try_into().unwrap();
      input_offset += num_read_offset;
    }

    Ok(())
  }
}

pub trait OutputFile: FixedFile {
  fn pwrite(&mut self, start: u64, buf: &[u8]) -> io::Result<usize>;

  fn pwrite_all(&mut self, start: u64, buf: &[u8]) -> io::Result<()> {
    let len: usize = buf.len();
    let mut output_offset: u64 = start;
    let mut remaining_to_write: usize = len;

    while remaining_to_write > 0 {
      let num_written: usize = interruptible_buffered_io_op![
        self.pwrite(output_offset, &buf[(len - remaining_to_write)..])
      ];
      if num_written == 0 {
        return Err(io::Error::new(
          io::ErrorKind::WriteZero,
          "pwrite less than expected range",
        ));
      }
      assert!(num_written <= remaining_to_write);
      remaining_to_write -= num_written;
      let num_written_offset: u64 = num_written.try_into().unwrap();
      output_offset += num_written_offset;
    }

    Ok(())
  }
}

pub trait UnboundedOutputFile: io::Write {}

/* TODO: copy range to a file of unknown size! */
pub trait CopyRange {
  type InF: InputFile;
  type OutF: OutputFile;

  fn copy_file_range(
    &mut self,
    from: (&Self::InF, u64),
    to: (&mut Self::OutF, u64),
    len: usize,
  ) -> io::Result<usize>;

  fn copy_file_range_all(
    &mut self,
    from: (&Self::InF, u64),
    mut to: (&mut Self::OutF, u64),
    len: usize,
  ) -> io::Result<()> {
    #[allow(clippy::needless_borrow)]
    let (ref from, from_offset) = from;
    let (ref mut to, to_offset) = to;

    let mut remaining_to_copy: usize = len;
    let mut input_offset: u64 = from_offset;
    let mut output_offset: u64 = to_offset;

    while remaining_to_copy > 0 {
      let num_copied: usize = interruptible_buffered_io_op![self.copy_file_range(
        (from, input_offset),
        (to, output_offset),
        remaining_to_copy,
      )];
      if num_copied == 0 {
        return Err(io::Error::new(
          io::ErrorKind::UnexpectedEof,
          "copied less than expected file range",
        ));
      }
      assert!(num_copied <= remaining_to_copy);
      remaining_to_copy -= num_copied;
      let num_copied_offset: u64 = num_copied.try_into().unwrap();
      input_offset += num_copied_offset;
      output_offset += num_copied_offset;
    }

    Ok(())
  }
}

pub trait UnboundedCopyRange {
  type InF: InputFile;
  type OutF: UnboundedOutputFile;

  fn copy_file_range(
    &mut self,
    from: (&Self::InF, u64),
    to: &mut Self::OutF,
    len: usize,
  ) -> io::Result<usize>;

  fn unbounded_copy_file_range_to_end(
    &mut self,
    _from: (&Self::InF, u64),
    _to: &mut Self::OutF,
    _chunk_size: usize,
  ) -> io::Result<u64> {
    todo!("implement this by copying in chunks from the provided offset to the end")
  }
}

#[cfg(unix)]
pub mod unix {
  use std::{
    fs, io,
    marker::PhantomData,
    mem::MaybeUninit,
    os::fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd, RawFd},
    slice,
  };

  use libc;

  use super::{CopyRange, FixedFile, InputFile, OutputFile};

  #[derive(Debug, Copy, Clone)]
  pub struct FileInput<'fd> {
    handle: BorrowedFd<'fd>,
    extent: u64,
  }

  pub(crate) fn fstat(fd: RawFd) -> io::Result<libc::stat> {
    let fd: libc::c_int = fd;
    let mut stat: MaybeUninit<libc::stat> = MaybeUninit::uninit();

    syscall_errno![unsafe { libc::fstat(fd, stat.as_mut_ptr()) }];
    Ok(unsafe { stat.assume_init() })
  }

  pub(crate) fn get_len(fd: RawFd) -> io::Result<u64> {
    let libc::stat { st_size, .. } = fstat(fd)?;
    let size: u64 = st_size.try_into().unwrap();
    Ok(size)
  }

  impl<'fd> FileInput<'fd> {
    pub fn new(file: &'fd impl AsFd) -> io::Result<Self> {
      let handle = file.as_fd();
      let extent = get_len(handle.as_raw_fd())?;
      Ok(Self { handle, extent })
    }

    pub(crate) fn fd(&self) -> RawFd { self.handle.as_raw_fd() }

    pub fn on_same_device(&self, to: &FileOutput) -> io::Result<bool> {
      let libc::stat {
        st_dev: from_dev, ..
      } = fstat(self.fd())?;
      let libc::stat { st_dev: to_dev, .. } = fstat(to.fd())?;
      Ok(from_dev == to_dev)
    }
  }

  impl<'fd> FixedFile for FileInput<'fd> {
    fn extent(&self) -> u64 { self.extent }
  }

  impl<'fd> InputFile for FileInput<'fd> {
    fn pread(&self, start: u64, buf: &mut [MaybeUninit<u8>]) -> io::Result<usize> {
      let count = self.range_len(start, buf.len())?;

      let fd: libc::c_int = self.fd();
      let p: *mut libc::c_void = buf.as_mut_ptr().cast();
      let offset: libc::off_t = start.try_into().unwrap();

      let n: usize = syscall_errno![unsafe { libc::pread(fd, p, count, offset) }]
        .try_into()
        .unwrap();
      Ok(n)
    }
  }

  /* TODO: impl UnboundedOutputFile, impl UnboundedCopyRange, impl
   * UnboundedReadSplicer! */
  pub struct UnboundedFileOutput {
    handle: OwnedFd,
  }

  impl UnboundedFileOutput {
    pub fn new(file: fs::File) -> Self {
      Self {
        handle: file.into(),
      }
    }

    #[allow(dead_code)]
    pub(crate) fn fd(&self) -> RawFd { self.handle.as_raw_fd() }

    pub fn into_file(self) -> fs::File { self.handle.into() }
  }

  pub struct FileOutput {
    handle: OwnedFd,
    extent: u64,
  }

  impl FileOutput {
    pub fn new(file: fs::File, extent: u64) -> io::Result<Self> {
      file.set_len(extent)?;
      Ok(Self {
        handle: file.into(),
        extent,
      })
    }

    pub(crate) fn fd(&self) -> RawFd { self.handle.as_raw_fd() }

    pub fn into_file(self) -> fs::File { self.handle.into() }
  }

  impl FixedFile for FileOutput {
    fn extent(&self) -> u64 { self.extent }
  }

  impl OutputFile for FileOutput {
    fn pwrite(&mut self, start: u64, buf: &[u8]) -> io::Result<usize> {
      let count = self.range_len(start, buf.len())?;

      let fd: libc::c_int = self.fd();
      let p: *const libc::c_void = buf.as_ptr().cast();
      let offset: libc::off_t = start.try_into().unwrap();

      let n: usize = syscall_errno![unsafe { libc::pwrite(fd, p, count, offset) }]
        .try_into()
        .unwrap();
      Ok(n)
    }
  }

  pub struct FileBufferCopy<'infd, 'buf> {
    buf: &'buf mut [u8],
    _ph: PhantomData<&'infd u8>,
  }

  impl<'infd, 'buf> FileBufferCopy<'infd, 'buf> {
    pub fn new(buf: &'buf mut [u8]) -> Self {
      assert!(!buf.is_empty());
      Self {
        buf,
        _ph: PhantomData,
      }
    }
  }

  impl<'infd, 'buf> CopyRange for FileBufferCopy<'infd, 'buf> {
    type InF = FileInput<'infd>;
    type OutF = FileOutput;

    fn copy_file_range(
      &mut self,
      from: (&Self::InF, u64),
      mut to: (&mut Self::OutF, u64),
      len: usize,
    ) -> io::Result<usize> {
      #[allow(clippy::needless_borrow)]
      let (ref from, from_start) = from;
      let (ref mut to, to_start) = to;

      let buf_clamped_len = len.min(self.buf.len());
      let from_len = from.range_len(from_start, buf_clamped_len)?;
      let to_len = to.range_len(to_start, buf_clamped_len)?;
      let clamped_len = from_len.min(to_len);
      if clamped_len == 0 {
        return Ok(0);
      }

      let clamped_buf: &'buf mut [MaybeUninit<u8>] = {
        let p: *mut MaybeUninit<u8> = self.buf.as_mut_ptr().cast();
        unsafe { slice::from_raw_parts_mut(p, clamped_len) }
      };

      let num_read: usize = from.pread(from_start, clamped_buf)?;
      assert!(num_read > 0);
      assert!(num_read <= clamped_buf.len());

      let result_buf: &'buf [u8] = {
        let p: *const u8 = clamped_buf.as_mut_ptr().cast_const().cast();
        unsafe { slice::from_raw_parts(p, num_read) }
      };

      /* TODO: use a ring buffer instead of .pwrite_all() here! */
      to.pwrite_all(to_start, result_buf)?;

      Ok(result_buf.len())
    }
  }

  #[cfg(test)]
  mod test {
    use std::{
      fs,
      io::{self, prelude::*},
      mem,
    };

    use tempfile;

    use super::*;

    fn readable_file(input: &[u8]) -> io::Result<fs::File> {
      let mut i = tempfile::tempfile()?;
      i.write_all(input)?;
      Ok(i)
    }

    #[allow(clippy::missing_transmute_annotations)]
    #[test]
    fn pread() {
      let i = readable_file(b"asdf").unwrap();
      let ii = FileInput::new(&i).unwrap();

      let buf: MaybeUninit<[u8; 10]> = MaybeUninit::zeroed();
      let mut buf: [MaybeUninit<u8>; 10] = unsafe { mem::transmute(buf) };
      assert_eq!(2, ii.pread(0, &mut buf[..2]).unwrap());
      assert_eq!(
        unsafe { mem::transmute::<_, &[u8]>(&buf[..2]) },
        b"as".as_ref()
      );
      assert_eq!(3, ii.pread(1, &mut buf[4..]).unwrap());
      assert_eq!(unsafe { mem::transmute::<_, &[u8]>(&buf[..]) }, &[
        b'a', b's', 0, 0, b's', b'd', b'f', 0, 0, 0
      ]);
    }

    #[test]
    fn pwrite() {
      let o = tempfile::tempfile().unwrap();
      let mut oo = FileOutput::new(o, 10).unwrap();

      let i = b"asdf";
      assert_eq!(2, oo.pwrite(0, &i[..2]).unwrap());
      assert_eq!(3, oo.pwrite(4, &i[1..]).unwrap());
      assert_eq!(1, oo.pwrite(9, &i[..]).unwrap());

      let mut o = oo.into_file();
      o.rewind().unwrap();
      let mut buf = Vec::new();
      o.read_to_end(&mut buf).unwrap();
      assert_eq!(&buf[..], &[b'a', b's', 0, 0, b's', b'd', b'f', 0, 0, b'a']);
    }

    #[test]
    fn copy_file_range() {
      let i = readable_file(b"asdf").unwrap();
      let ii = FileInput::new(&i).unwrap();

      let o = tempfile::tempfile().unwrap();
      let mut oo = FileOutput::new(o, 10).unwrap();

      /* Buffer is size 2, which limits the max size of individual
       * copy_file_range() calls. */
      let mut buf = vec![0u8; 2].into_boxed_slice();

      let mut c = FileBufferCopy::new(&mut buf);
      assert_eq!(2, c.copy_file_range((&ii, 0), (&mut oo, 0), 2).unwrap());
      assert_eq!(2, c.copy_file_range((&ii, 1), (&mut oo, 4), 20).unwrap());
      assert_eq!(1, c.copy_file_range((&ii, 0), (&mut oo, 9), 35).unwrap());

      let mut o = oo.into_file();
      o.rewind().unwrap();
      let mut buf = Vec::new();
      o.read_to_end(&mut buf).unwrap();

      assert_eq!(&buf[..], &[b'a', b's', 0, 0, b's', b'd', 0, 0, 0, b'a']);
    }
  }
}

#[cfg(target_os = "linux")]
pub mod linux {
  use std::{io, marker::PhantomData};

  use libc;

  use super::{
    unix::{FileInput, FileOutput},
    CopyRange, FixedFile,
  };

  pub struct FileCopy<'infd>(PhantomData<&'infd u8>);

  impl<'infd> FileCopy<'infd> {
    pub const fn new() -> Self { Self(PhantomData) }
  }

  impl<'infd> CopyRange for FileCopy<'infd> {
    type InF = FileInput<'infd>;
    type OutF = FileOutput;

    fn copy_file_range(
      &mut self,
      from: (&Self::InF, u64),
      to: (&mut Self::OutF, u64),
      len: usize,
    ) -> io::Result<usize> {
      let (from, from_start) = from;
      let (to, to_start) = to;

      let from_len = from.range_len(from_start, len)?;
      let to_len = to.range_len(to_start, len)?;
      let clamped_len = from_len.min(to_len);

      let from_fd: libc::c_int = from.fd();
      let mut from_offset: libc::off64_t = from_start.try_into().unwrap();
      let to_fd: libc::c_int = to.fd();
      let mut to_offset: libc::off64_t = to_start.try_into().unwrap();

      let flags: libc::c_uint = 0;

      let n: usize = syscall_errno![unsafe {
        libc::copy_file_range(
          from_fd,
          &mut from_offset,
          to_fd,
          &mut to_offset,
          clamped_len,
          flags,
        )
      }]
      .try_into()
      .unwrap();
      Ok(n)
    }
  }

  #[cfg(test)]
  mod test {
    use std::{
      fs,
      io::{self, prelude::*},
    };

    use tempfile;

    use super::*;

    fn readable_file(input: &[u8]) -> io::Result<fs::File> {
      let mut i = tempfile::tempfile()?;
      i.write_all(input)?;
      Ok(i)
    }

    #[test]
    fn copy_file_range() {
      let i = readable_file(b"asdf").unwrap();
      let ii = FileInput::new(&i).unwrap();

      let o = tempfile::tempfile().unwrap();
      let mut oo = FileOutput::new(o, 10).unwrap();

      let mut c = FileCopy::new();
      assert_eq!(2, c.copy_file_range((&ii, 0), (&mut oo, 0), 2).unwrap());
      assert_eq!(3, c.copy_file_range((&ii, 1), (&mut oo, 4), 20).unwrap());
      assert_eq!(1, c.copy_file_range((&ii, 0), (&mut oo, 9), 35).unwrap());

      let mut o = oo.into_file();
      o.rewind().unwrap();
      let mut buf = Vec::new();
      o.read_to_end(&mut buf).unwrap();

      assert_eq!(&buf[..], &[b'a', b's', 0, 0, b's', b'd', b'f', 0, 0, b'a']);
    }
  }
}
