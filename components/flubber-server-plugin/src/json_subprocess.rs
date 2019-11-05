use bytes::{Bytes, BytesMut};
use either::Either;
use serde_json::{de::SliceRead, StreamDeserializer, Value};
use std::{
    ffi::OsStr,
    fmt::Debug,
    mem::replace,
    pin::Pin,
    process::Stdio,
    task::{Context, Poll},
};
use tokio::prelude::*;
use tokio_process::{Child, ChildStdin, ChildStdout, Command};

/// The lowest-level wrapper for a plugin. Just a subprocess with JSON serialization and
/// deserialization over stdin/stdout.
#[derive(Debug)]
pub struct JsonSubprocess {
    child: Child,
    stdin: ChildStdin,
    stdin_buf: Bytes,
    stdout: ChildStdout,
    stdout_buf: BytesMut,
}

impl JsonSubprocess {
    /// The number of bytes to try reading from the subprocess at once.
    ///
    /// Someday, tuning this might make sense, but I doubt it.
    const STDOUT_BUF_SIZE: usize = 4096;

    /// Creates a new `JsonSubprocess`.
    pub fn new<Arg: AsRef<OsStr>, Args: IntoIterator<Item = Arg>, Cmd: AsRef<OsStr>>(
        cmd: Cmd,
        args: Args,
    ) -> Result<JsonSubprocess, std::io::Error> {
        let mut child = Command::new(cmd)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()?;
        let stdin = child.stdin().take().unwrap();
        let stdout = child.stdout().take().unwrap();
        Ok(JsonSubprocess {
            child,
            stdin,
            stdin_buf: Bytes::new(),
            stdout,
            stdout_buf: BytesMut::new(),
        })
    }
}

impl Sink<Value> for JsonSubprocess {
    type Error = Either<std::io::Error, serde_json::Error>;

    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        self.poll_flush(cx)
    }

    fn start_send(mut self: Pin<&mut Self>, item: Value) -> Result<(), Self::Error> {
        assert!(self.stdin_buf.is_empty());
        self.stdin_buf = item.to_string().into();
        Ok(())
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        // TODO: What's the right thing to do here, panic-wise? Poisoning?
        let mut buf = replace(&mut self.stdin_buf, Bytes::new());
        let mut out = Poll::Ready(Ok(()));

        while !buf.is_empty() {
            match AsyncWrite::poll_write(Pin::new(&mut self.stdin), cx, &buf) {
                Poll::Ready(Ok(n)) => buf.advance(n),
                Poll::Ready(Err(err)) => {
                    out = Poll::Ready(Err(Either::Left(err)));
                    break;
                }
                Poll::Pending => {
                    out = Poll::Pending;
                    break;
                }
            }
        }

        drop(replace(&mut self.stdin_buf, buf));
        out
    }

    fn poll_close(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        Sink::poll_flush(Pin::new(&mut self), cx)
            .map(|r| r.and_then(|()| self.child.kill().map_err(Either::Left)))
    }
}

impl Stream for JsonSubprocess {
    type Item = Result<Value, Either<std::io::Error, serde_json::Error>>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Option<Self::Item>> {
        // TODO: What's the right thing to do here, panic-wise? Poisoning?
        let mut buf = replace(&mut self.stdout_buf, BytesMut::new());

        let out = loop {
            // Try to parse out a value.
            let mut des = StreamDeserializer::new(SliceRead::new(&buf));
            match des.next() {
                Some(Ok(value)) => {
                    let n = des.byte_offset();
                    buf.advance(n);
                    break Poll::Ready(Some(Ok(value)));
                }
                Some(Err(err)) if err.is_eof() => {}
                None => {}
                Some(Err(err)) => break Poll::Ready(Some(Err(Either::Right(err)))),
            }

            // Get more data for the buffer.
            buf.reserve(Self::STDOUT_BUF_SIZE);
            match AsyncRead::poll_read_buf(Pin::new(&mut self.stdout), cx, &mut buf) {
                Poll::Ready(Ok(_)) => { /* poll_read_buf advances the buffer */ }
                Poll::Ready(Err(err)) => break Poll::Ready(Some(Err(Either::Left(err)))),
                Poll::Pending => break Poll::Pending,
            }
        };

        drop(replace(&mut self.stdout_buf, buf));
        out
    }
}
