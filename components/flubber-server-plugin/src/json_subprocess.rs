use flubber_utils::JsonCodec;
use serde_json::Value;
use std::{
    ffi::OsStr,
    pin::Pin,
    process::Stdio,
    task::{Context, Poll},
};
use tokio::{
    codec::{FramedRead, FramedWrite},
    net::process::{Child, ChildStdin, ChildStdout, Command},
    prelude::*,
};

/// The lowest-level wrapper for a plugin. Just a subprocess with JSON serialization and
/// deserialization over stdin/stdout.
#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct JsonSubprocess {
    child: Child,
    stdin: FramedWrite<ChildStdin, JsonCodec<Value>>,
    stdout: FramedRead<ChildStdout, JsonCodec<Value>>,
}

impl JsonSubprocess {
    /// Creates a new `JsonSubprocess`.
    pub fn new<Arg, Args, Cmd, Env, K, V>(
        cmd: Cmd,
        args: Args,
        env: Env,
    ) -> Result<JsonSubprocess, std::io::Error>
    where
        Arg: AsRef<OsStr>,
        Args: IntoIterator<Item = Arg>,
        Cmd: AsRef<OsStr>,
        Env: IntoIterator<Item = (K, V)>,
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        let mut child = Command::new(cmd)
            .args(args)
            .envs(env)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()?;
        let stdin = child.stdin().take().unwrap();
        let stdout = child.stdout().take().unwrap();
        Ok(JsonSubprocess {
            child,
            stdin: FramedWrite::new(stdin, JsonCodec::default()),
            stdout: FramedRead::new(stdout, JsonCodec::default()),
        })
    }
}

impl Sink<Value> for JsonSubprocess {
    type Error = std::io::Error;

    fn poll_ready(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        Sink::poll_ready(Pin::new(&mut self.stdin), cx)
    }

    fn start_send(mut self: Pin<&mut Self>, item: Value) -> Result<(), Self::Error> {
        Sink::start_send(Pin::new(&mut self.stdin), item)
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        Sink::poll_flush(Pin::new(&mut self.stdin), cx)
    }

    fn poll_close(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        Sink::poll_close(Pin::new(&mut self.stdin), cx)
    }
}

impl Stream for JsonSubprocess {
    type Item = Result<Value, std::io::Error>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Option<Self::Item>> {
        Stream::poll_next(Pin::new(&mut self.stdout), cx)
    }
}
