use crate::proto::{InitInfo, RequestBody, ResponseBody, Update, Version};
use anyhow::{Error, Result};
use serde::Serialize;
use std::{
    pin::Pin,
    task::{Context, Poll},
};
use tokio::{
    io::{stdin, Stdin},
    prelude::*,
};

/// An abstraction for the parent process of the plugin, i.e. the server.
#[derive(Debug)]
pub struct Parent {
    stdin: Stdin,
}

impl Parent {
    /// Creates a new `Parent`.
    pub async fn new(name: String, major: u32, minor: u32, patch: u32) -> Result<Parent> {
        let parent = Parent { stdin: stdin() };
        parent.write(&InitInfo {
            plugin_name: name,
            plugin_version: Version(major, minor, patch),
            protocol_version: Version(0, 1, 0),
        })?;
        Ok(parent)
    }

    // Currently, Stdout's AsyncWrite impl is broken, so we just synchronously write... It can't be
    // *that* bad, right?
    fn write<T: Serialize>(&self, val: &T) -> Result<()> {
        let buf = serde_json::to_string(val)?;
        println!("{}", buf);
        Ok(())
    }
}

impl Sink<Update> for Parent {
    type Error = Error;

    fn poll_ready(self: Pin<&mut Self>, _cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn start_send(self: Pin<&mut Self>, item: Update) -> Result<(), Self::Error> {
        self.write(&item)
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn poll_close(self: Pin<&mut Self>, _cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }
}

impl Stream for Parent {
    type Item = Result<ReqRes>;

    fn poll_next(self: Pin<&mut Self>, _cx: &mut Context) -> Poll<Option<Self::Item>> {
        unimplemented!()
    }
}

/// An abstraction for the parent process of the plugin, i.e. the server.
#[derive(Debug)]
pub struct ReqRes {
    /// The request being responded to.
    pub req: RequestBody,
    // Once we're doing async I/O to stdout, a locked handle to stdout will need to live here.
    // "Conveniently," the name of the type that does said locking has also changd from this
    // release of Tokio to the latest (Lock to Mutex), so I'm not gonna bother implementing it at
    // all for now.
}

impl ReqRes {
    /// Responds to the request.
    pub async fn respond(self, _body: ResponseBody) -> Result<()> {
        unimplemented!()
    }
}
