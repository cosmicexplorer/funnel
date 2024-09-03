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

use std::io;

use super::file::{InputFile, OutputFile, UnboundedOutputFile};
use crate::interruptible_buffered_io_op;

pub trait WriteEnd: io::Write {}

pub trait WriteSplicer {
  type InF: InputFile;
  type OutP: WriteEnd;

  fn splice_from_file(
    &mut self,
    from: (&Self::InF, u64),
    to: &mut Self::OutP,
    len: usize,
  ) -> io::Result<usize>;

  fn splice_from_file_all(
    &mut self,
    from: (&Self::InF, u64),
    to: &mut Self::OutP,
    len: usize,
  ) -> io::Result<()> {
    #[allow(clippy::needless_borrow)]
    let (ref from, from_offset) = from;

    let mut remaining_to_read: usize = len;
    let mut input_offset: u64 = from_offset;
    while remaining_to_read > 0 {
      let num_read: usize = interruptible_buffered_io_op![self.splice_from_file(
        (from, input_offset),
        to,
        remaining_to_read
      )];
      if num_read == 0 {
        return Err(io::Error::new(
          io::ErrorKind::UnexpectedEof,
          "spliced less than expected range from file",
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

pub trait ReadEnd: io::Read {}

pub trait ReadSplicer {
  type InP: ReadEnd;
  type OutF: OutputFile;

  fn splice_to_file(
    &mut self,
    from: &mut Self::InP,
    to: (&mut Self::OutF, u64),
    len: usize,
  ) -> io::Result<usize>;

  fn splice_to_file_all(
    &mut self,
    from: &mut Self::InP,
    mut to: (&mut Self::OutF, u64),
    len: usize,
  ) -> io::Result<()> {
    let (ref mut to, to_offset) = to;

    let mut remaining_to_write: usize = len;
    let mut output_offset: u64 = to_offset;
    while remaining_to_write > 0 {
      let num_written: usize = interruptible_buffered_io_op![self.splice_to_file(
        from,
        (to, output_offset),
        remaining_to_write
      )];
      if num_written == 0 {
        return Err(io::Error::new(
          io::ErrorKind::WriteZero,
          "spliced less than expected range to file",
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

/* TODO: splice to a file of unknown size! */
pub trait UnboundedReadSplicer {
  type InP: ReadEnd;
  type OutF: UnboundedOutputFile;

  fn splice_to_file(
    &mut self,
    from: &mut Self::InP,
    to: &mut Self::OutF,
    len: usize,
  ) -> io::Result<usize>;

  fn unbounded_splice_to_end(
    &mut self,
    from: &mut Self::InP,
    to: &mut Self::OutF,
    chunk_size: usize,
  ) -> io::Result<u64> {
    let mut total_spliced: u64 = 0;

    loop {
      let num_written: usize =
        interruptible_buffered_io_op![self.splice_to_file(from, to, chunk_size)];
      if num_written == 0 {
        break;
      }
      let num_written_offset: u64 = num_written.try_into().unwrap();
      total_spliced += num_written_offset;
    }

    Ok(total_spliced)
  }
}

#[cfg(unix)]
pub mod unix {
  use std::{
    io::{self, Read, Write},
    marker::PhantomData,
    mem::MaybeUninit,
    os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd},
    slice,
  };

  use libc;

  use super::{
    super::file::{
      unix::{FileInput, FileOutput},
      FixedFile, InputFile, OutputFile,
    },
    ReadEnd, ReadSplicer, WriteEnd, WriteSplicer,
  };

  pub struct WritePipe {
    handle: OwnedFd,
  }

  impl WritePipe {
    pub(crate) unsafe fn from_fd(fd: RawFd) -> Self {
      Self {
        handle: OwnedFd::from_raw_fd(fd),
      }
    }

    pub(crate) fn fd(&self) -> RawFd { self.handle.as_raw_fd() }
  }

  impl io::Write for WritePipe {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
      let fd: libc::c_int = self.fd();

      /* TODO: use vmsplice() instead on linux! However, UB results if the buffer
       * is modified before the data is read by the output: see
       * https://stackoverflow.com/questions/70515745/how-do-i-use-vmsplice-to-correctly-output-to-a-pipe.
       * This may be possible to handle with some sort of ring buffer, but for now
       * let's take the hit and avoid race conditions by using write() on
       * all unix-likes. */
      let n: usize = syscall_errno![unsafe { libc::write(fd, buf.as_ptr().cast(), buf.len()) }]
        .try_into()
        .unwrap();
      Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> { Ok(()) }
  }

  impl WriteEnd for WritePipe {}

  pub struct ReadPipe {
    handle: OwnedFd,
  }

  impl ReadPipe {
    pub(crate) unsafe fn from_fd(fd: RawFd) -> Self {
      Self {
        handle: OwnedFd::from_raw_fd(fd),
      }
    }

    pub(crate) fn fd(&self) -> RawFd { self.handle.as_raw_fd() }
  }

  impl io::Read for ReadPipe {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
      let fd: libc::c_int = self.fd();

      /* TODO: vmsplice() on linux currently offers no additional optimization for
       * reads, so just use read() on all platforms. Also note as in
       * WritePipe::write() that some sort of ring buffer is probably
       * necessary to avoid race conditions if this optimization is
       * performed. */
      let n: usize = syscall_errno![unsafe { libc::read(fd, buf.as_mut_ptr().cast(), buf.len()) }]
        .try_into()
        .unwrap();
      Ok(n)
    }
  }

  impl ReadEnd for ReadPipe {}

  pub fn create_pipe() -> io::Result<(ReadPipe, WritePipe)> {
    let mut fds: [libc::c_int; 2] = [0; 2];
    syscall_errno![unsafe { libc::pipe(fds.as_mut_ptr()) }];
    let [r, w] = fds;
    let (r, w) = unsafe { (ReadPipe::from_fd(r), WritePipe::from_fd(w)) };
    Ok((r, w))
  }

  pub struct PipeWriteBufferSplicer<'infd, 'buf> {
    buf: &'buf mut [u8],
    _ph: PhantomData<&'infd u8>,
  }

  impl<'infd, 'buf> PipeWriteBufferSplicer<'infd, 'buf> {
    pub fn new(buf: &'buf mut [u8]) -> Self {
      assert!(!buf.is_empty());
      Self {
        buf,
        _ph: PhantomData,
      }
    }
  }

  impl<'infd, 'buf> WriteSplicer for PipeWriteBufferSplicer<'infd, 'buf> {
    type InF = FileInput<'infd>;
    type OutP = WritePipe;

    fn splice_from_file(
      &mut self,
      from: (&Self::InF, u64),
      to: &mut Self::OutP,
      len: usize,
    ) -> io::Result<usize> {
      #[allow(clippy::needless_borrow)]
      let (ref from, from_start) = from;

      let buf_clamped_len = len.min(self.buf.len());
      let from_len = from.range_len(from_start, buf_clamped_len)?;
      let clamped_len = from_len;
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

      /* TODO: use a ring buffer instead of .write_all() here! */
      to.write_all(result_buf)?;

      Ok(result_buf.len())
    }
  }

  pub struct PipeReadBufferSplicer<'buf> {
    buf: &'buf mut [u8],
  }

  impl<'buf> PipeReadBufferSplicer<'buf> {
    pub fn new(buf: &'buf mut [u8]) -> Self {
      assert!(!buf.is_empty());
      Self { buf }
    }
  }

  impl<'buf> ReadSplicer for PipeReadBufferSplicer<'buf> {
    type InP = ReadPipe;
    type OutF = FileOutput;

    fn splice_to_file(
      &mut self,
      from: &mut Self::InP,
      mut to: (&mut Self::OutF, u64),
      len: usize,
    ) -> io::Result<usize> {
      let (ref mut to, to_start) = to;

      let buf_clamped_len = len.min(self.buf.len());
      let to_len = to.range_len(to_start, buf_clamped_len)?;
      let clamped_len = to_len;
      if clamped_len == 0 {
        return Ok(0);
      }

      let clamped_buf: &'buf mut [u8] =
        unsafe { slice::from_raw_parts_mut(self.buf.as_mut_ptr(), clamped_len) };

      let num_read: usize = from.read(clamped_buf)?;
      if num_read == 0 {
        return Err(io::Error::new(
          io::ErrorKind::UnexpectedEof,
          "expected to read nonzero from blocking pipe",
        ));
      }
      assert!(num_read <= clamped_buf.len());

      let result_buf: &'buf [u8] =
        unsafe { slice::from_raw_parts(clamped_buf.as_mut_ptr().cast_const(), num_read) };

      /* TODO: use a ring buffer instead of .pwrite_all() here! */
      to.pwrite_all(to_start, result_buf)?;

      Ok(result_buf.len())
    }
  }

  #[cfg(test)]
  mod test {
    use std::{fs, io::prelude::*, thread};

    use tempfile;

    use super::*;

    fn readable_file(input: &[u8]) -> io::Result<fs::File> {
      let mut i = tempfile::tempfile()?;
      i.write_all(input)?;
      Ok(i)
    }

    #[test]
    fn read_write_pipe() {
      let (mut r, mut w) = create_pipe().unwrap();

      let t = thread::spawn(move || w.write_all(b"asdf"));
      /* The write end is dropped after the string is written, which stops
       * .read_to_end() from blocking. */
      let mut buf: Vec<u8> = Vec::new();
      r.read_to_end(&mut buf).unwrap();
      assert_eq!(b"asdf".as_ref(), &buf[..]);
      t.join().unwrap().unwrap();
    }

    #[test]
    fn splice_from_file() {
      let (mut r, mut w) = create_pipe().unwrap();

      let t = thread::spawn(move || {
        let i = readable_file(b"asdf").unwrap();
        let ii = FileInput::new(&i).unwrap();
        /* Buffer is size 2, which limits the max size of individual splice() calls. */
        let mut buf = vec![0u8; 2].into_boxed_slice();
        let mut s = PipeWriteBufferSplicer::new(&mut buf);
        s.splice_from_file((&ii, 1), &mut w, 13)
      });

      let mut buf: Vec<u8> = Vec::new();
      r.read_to_end(&mut buf).unwrap();
      /* Started from offset 1, and buf limited to 2, so only get 2 chars. */
      assert_eq!(b"sd".as_ref(), &buf[..]);
      assert_eq!(2, t.join().unwrap().unwrap());
    }

    #[test]
    fn splice_to_file() {
      let o = tempfile::tempfile().unwrap();
      let mut oo = FileOutput::new(o, 5).unwrap();

      let (mut r, mut w) = create_pipe().unwrap();
      let t = thread::spawn(move || w.write_all(b"asdfasdf"));

      /* Buffer is size 2, which limits the max size of individual splice() calls. */
      let mut buf = vec![0u8; 2].into_boxed_slice();
      let mut s = PipeReadBufferSplicer::new(&mut buf);
      assert_eq!(2, s.splice_to_file(&mut r, (&mut oo, 2), 13).unwrap());

      let mut o = oo.into_file();
      o.rewind().unwrap();
      let mut buf: Vec<u8> = Vec::new();
      o.read_to_end(&mut buf).unwrap();

      /* Started from offset 2, and buf limited to 2, so only get 2 chars. */
      assert_eq!(&buf[..], &[0, 0, b'a', b's', 0]);

      /* Get remaining chars written. */
      buf.clear();
      r.read_to_end(&mut buf).unwrap();
      assert_eq!(&buf[..], b"dfasdf".as_ref());

      t.join().unwrap().unwrap();
    }
  }
}

#[cfg(target_os = "linux")]
pub mod linux {
  use std::{io, marker::PhantomData, ptr};

  use libc;

  use super::{
    super::file::{
      unix::{FileInput, FileOutput},
      FixedFile,
    },
    unix::{ReadPipe, WritePipe},
    ReadSplicer, WriteSplicer,
  };

  pub struct PipeWriteSplicer<'infd>(PhantomData<&'infd u8>);

  impl<'infd> PipeWriteSplicer<'infd> {
    pub const fn new() -> Self { Self(PhantomData) }
  }

  impl<'infd> WriteSplicer for PipeWriteSplicer<'infd> {
    type InF = FileInput<'infd>;
    type OutP = WritePipe;

    fn splice_from_file(
      &mut self,
      from: (&Self::InF, u64),
      to: &mut Self::OutP,
      len: usize,
    ) -> io::Result<usize> {
      let (from, from_start) = from;

      let count = from.range_len(from_start, len)?;

      let from_fd: libc::c_int = from.fd();
      let mut from_offset: libc::loff_t = from_start.try_into().unwrap();
      let to_fd: libc::c_int = to.fd();

      let flags: libc::c_uint = 0;
      let n: usize = syscall_errno![unsafe {
        libc::splice(
          from_fd,
          &mut from_offset,
          to_fd,
          ptr::null_mut(),
          count,
          flags,
        )
      }]
      .try_into()
      .unwrap();
      Ok(n)
    }
  }

  pub struct PipeReadSplicer;

  impl ReadSplicer for PipeReadSplicer {
    type InP = ReadPipe;
    type OutF = FileOutput;

    fn splice_to_file(
      &mut self,
      from: &mut Self::InP,
      to: (&mut Self::OutF, u64),
      len: usize,
    ) -> io::Result<usize> {
      let (to, to_start) = to;

      let count = to.range_len(to_start, len)?;

      let from_fd: libc::c_int = from.fd();
      let to_fd: libc::c_int = to.fd();
      let mut to_offset: libc::loff_t = to_start.try_into().unwrap();

      let flags: libc::c_uint = 0;
      let n: usize = syscall_errno![unsafe {
        libc::splice(
          from_fd,
          ptr::null_mut(),
          to_fd,
          &mut to_offset,
          count,
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
    use std::{fs, io::prelude::*, thread};

    use tempfile;

    use super::{super::unix::create_pipe, *};

    fn readable_file(input: &[u8]) -> io::Result<fs::File> {
      let mut i = tempfile::tempfile()?;
      i.write_all(input)?;
      Ok(i)
    }

    #[test]
    fn splice_from_file() {
      let (mut r, mut w) = create_pipe().unwrap();
      let t = thread::spawn(move || {
        let i = readable_file(b"asdf").unwrap();
        let ii = FileInput::new(&i).unwrap();
        let mut s = PipeWriteSplicer::new();
        s.splice_from_file((&ii, 1), &mut w, 13)
      });

      let mut buf: Vec<u8> = Vec::new();
      r.read_to_end(&mut buf).unwrap();
      /* Started from offset 1, so only get 3 chars. */
      assert_eq!(b"sdf".as_ref(), &buf[..]);
      assert_eq!(3, t.join().unwrap().unwrap());
    }

    #[test]
    fn splice_to_file() {
      let o = tempfile::tempfile().unwrap();
      let mut oo = FileOutput::new(o, 5).unwrap();

      let (mut r, mut w) = create_pipe().unwrap();
      let t = thread::spawn(move || w.write_all(b"asdfasdf"));

      let mut s = PipeReadSplicer;
      assert_eq!(3, s.splice_to_file(&mut r, (&mut oo, 2), 13).unwrap());

      let mut o = oo.into_file();
      o.rewind().unwrap();
      let mut buf: Vec<u8> = Vec::new();
      o.read_to_end(&mut buf).unwrap();

      /* Started from offset 2, so only get 3 chars. */
      assert_eq!(&buf[..], &[0, 0, b'a', b's', b'd']);

      /* Get remaining chars written. */
      buf.clear();
      r.read_to_end(&mut buf).unwrap();
      assert_eq!(&buf[..], b"fasdf".as_ref());

      t.join().unwrap().unwrap();
    }
  }
}
