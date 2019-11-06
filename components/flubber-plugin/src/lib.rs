//! A utility library for more easily implementing plugins.
#![deny(
    bad_style,
    bare_trait_objects,
    const_err,
    dead_code,
    improper_ctypes,
    legacy_directory_ownership,
    missing_debug_implementations,
    missing_docs,
    no_mangle_generic_items,
    non_shorthand_field_patterns,
    overflowing_literals,
    path_statements,
    patterns_in_fns_without_body,
    plugin_as_library,
    private_in_public,
    safe_extern_statics,
    trivial_casts,
    trivial_numeric_casts,
    unconditional_recursion,
    // unions_with_drop_fields,
    unsafe_code,
    unused,
    unused_allocation,
    unused_comparisons,
    unused_extern_crates,
    unused_import_braces,
    unused_parens,
    unused_qualifications,
    unused_results,
    while_true
)]

use crate::proto::{InitInfo, RequestBody, ResponseBody, Update, Version};
use anyhow::{Error, Result};
pub use flubber_plugin_proto as proto;
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
        let mut buf = serde_json::to_string(val)?;
        buf.push('\n');
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
