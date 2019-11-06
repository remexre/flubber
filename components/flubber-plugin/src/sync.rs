//! Synchronous version of the API.
//!
//! This works by spawning a worker thread and running a single-threaded runtime on it.

use crate::proto::{InitInfo, Request, RequestBody, ResponseBody, Update, Version};
use anyhow::Result;
use bytes::BytesMut;
use serde_json::de::Deserializer;
use std::thread::spawn;
use tokio::{
    io::stdin,
    prelude::*,
    runtime::current_thread::Runtime,
    sync::mpsc::{channel, Receiver},
};

/// Starts a connection to a server, which should act as the parent process.
pub fn start_plugin(
    name: String,
    major: u32,
    minor: u32,
    patch: u32,
) -> (Receiver<ReqRes>, UpdateSender) {
    let (mut send, recv) = channel(8);
    println!(
        "{}",
        serde_json::to_string(&InitInfo {
            plugin_name: name,
            plugin_version: Version(major, minor, patch),
            protocol_version: Version(0, 1, 0),
        })
        .unwrap()
    );
    drop(spawn(move || {
        Runtime::new().unwrap().block_on({
            async move {
                let mut buf = BytesMut::new();
                let mut input = stdin();
                loop {
                    let mut tmp_buf = [0u8; 4096];
                    let n = input.read(&mut tmp_buf).await.unwrap();
                    buf.extend_from_slice(&tmp_buf[..n]);

                    // Try to parse out a value.
                    let mut des = Deserializer::from_slice(&buf).into_iter::<Request>();
                    match des.next() {
                        Some(Ok(value)) => {
                            let n = des.byte_offset();
                            buf.advance(n);
                            send.send(ReqRes {
                                sequence_number: value.sequence_number,
                                req: value.body,
                            })
                            .await
                            .unwrap();
                        }
                        Some(Err(err)) if err.is_eof() => {}
                        None => {}
                        Some(Err(err)) => panic!("{}", err),
                    }
                }
            }
        })
    }));
    (recv, UpdateSender { _dummy: () })
}

/// An abstraction for the parent process of the plugin, i.e. the server.
#[derive(Debug)]
pub struct ReqRes {
    /// The sequence number of the request.
    sequence_number: u32,

    /// The request being responded to.
    pub req: RequestBody,
}

impl ReqRes {
    /// Responds to the request.
    pub fn respond(self, _body: ResponseBody) -> Result<()> {
        unimplemented!()
    }
}

/// A type for sending updates back to the server.
#[derive(Clone, Debug)]
pub struct UpdateSender {
    _dummy: (),
}

impl UpdateSender {
    /// Sends an update to the server.
    pub fn send(&self, _update: Update) {
        unimplemented!()
    }
}
