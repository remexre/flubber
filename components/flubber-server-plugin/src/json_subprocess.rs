use bytes::Bytes;
use either::Either;
use flubber_utils::read_jsons;
use serde_json::Value;
use std::{
    ffi::OsStr,
    mem::replace,
    pin::Pin,
    process::Stdio,
    task::{Context, Poll},
};
use tokio::prelude::*;
use tokio_process::{Child, ChildStdin, Command};

/// The lowest-level wrapper for a plugin. Just a subprocess with JSON serialization and
/// deserialization over stdin/stdout.
#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct JsonSubprocess {
    child: Child,
    stdin: ChildStdin,
    stdin_buf: Bytes,
    // TODO: existential
    #[derivative(Debug = "ignore")]
    stdout: Pin<
        Box<dyn Stream<Item = Result<Value, Either<std::io::Error, serde_json::Error>>> + Send>,
    >,
}

impl JsonSubprocess {
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
            stdout: Box::pin(read_jsons(stdout)),
        })
    }
}

// TODO: Kill/combinatorify this.
impl Sink<Value> for JsonSubprocess {
    type Error = Either<std::io::Error, serde_json::Error>;

    fn poll_ready(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        self.poll_flush(cx)
    }

    fn start_send(mut self: Pin<&mut Self>, item: Value) -> Result<(), Self::Error> {
        assert!(self.stdin_buf.is_empty());
        let mut s = item.to_string();
        s.push('\n');
        self.stdin_buf = s.into();
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

        if let Poll::Ready(Ok(())) = out {
            match AsyncWrite::poll_flush(Pin::new(&mut self.stdin), cx) {
                Poll::Ready(Ok(())) => Poll::Ready(Ok(())),
                Poll::Ready(Err(err)) => Poll::Ready(Err(Either::Left(err))),
                Poll::Pending => Poll::Pending,
            }
        } else {
            out
        }
    }

    fn poll_close(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        Sink::poll_flush(Pin::new(&mut self), cx)
            .map(|r| r.and_then(|()| self.child.kill().map_err(Either::Left)))
    }
}

impl Stream for JsonSubprocess {
    type Item = Result<Value, Either<std::io::Error, serde_json::Error>>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Option<Self::Item>> {
        Stream::poll_next(self.stdout.as_mut(), cx)
    }
}
