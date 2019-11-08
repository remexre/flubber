//! A utility library for more easily implementing backends.
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

use crate::proto::{InitInfo, Request, RequestBody, Response, ResponseBody, Update, Version};
pub use flubber_backend_proto as proto;
use flubber_utils::JsonCodec;
use futures_util::{
    future::{ready, Either, Ready},
    stream::{once, select},
};
use serde_json::Value;
use std::{
    error::Error,
    pin::Pin,
    task::{Context, Poll},
};
use tokio::{
    codec::{FramedRead, FramedWrite},
    io::{stdin, stdout},
    prelude::*,
};
use tower::{util::CallAll, Service};

/// Runs a backend.
pub fn run_backend<S, U>(
    name: String,
    major: u32,
    minor: u32,
    patch: u32,
    service: S,
    updates: U,
) -> impl Future<Output = Result<(), Box<dyn Error + 'static + Sync + Send>>>
where
    S: Service<RequestBody, Response = ResponseBody>,
    S::Error: Into<Box<dyn Error + 'static + Sync + Send>>,
    U: Stream<Item = Update>,
{
    let service = Wrapper(service);
    once(ready(
        serde_json::to_value(InitInfo {
            backend_name: name,
            backend_version: Version(major, minor, patch),
            protocol_version: Version(0, 1, 0),
        })
        .map_err(Into::into),
    ))
    .chain(select(
        CallAll::new(service, FramedRead::new(stdin(), JsonCodec::default())).unordered(),
        updates.map(|u| serde_json::to_value(u).map_err(Into::into)),
    ))
    .forward(FramedWrite::new(stdout(), JsonCodec::default()).sink_err_into())
}

#[derive(Debug)]
struct Wrapper<T>(T);

impl<S, E> Service<Result<Request, E>> for Wrapper<S>
where
    S: Service<RequestBody, Response = ResponseBody>,
    S::Error: Into<Box<dyn Error + 'static + Sync + Send>>,
    E: Into<Box<dyn Error + 'static + Sync + Send>>,
{
    type Response = Value;
    type Error = Box<dyn Error + 'static + Sync + Send>;
    type Future = Either<WrapperFuture<S>, Ready<Result<Value, Self::Error>>>;

    fn poll_ready(&mut self, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx).map_err(Into::into)
    }

    fn call(&mut self, req: Result<Request, E>) -> Self::Future {
        match req {
            Ok(req) => WrapperFuture {
                future: self.0.call(req.body),
                sequence_number: req.sequence_number,
            }
            .left_future(),
            Err(err) => ready(Err(err.into())).right_future(),
        }
    }
}

struct WrapperFuture<S: Service<RequestBody, Response = ResponseBody>> {
    future: S::Future,
    sequence_number: u32,
}

impl<S: Service<RequestBody, Response = ResponseBody>> WrapperFuture<S> {
    pin_utils::unsafe_pinned!(future: S::Future);
}

impl<S: Service<RequestBody, Response = ResponseBody>> Future for WrapperFuture<S>
where
    S::Error: Into<Box<dyn Error + 'static + Sync + Send>>,
{
    type Output = Result<Value, Box<dyn Error + 'static + Sync + Send>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        let sequence_number = self.sequence_number;
        match self.future().poll(cx) {
            Poll::Ready(Ok(body)) => Poll::Ready(
                serde_json::to_value(Response {
                    sequence_number,
                    body,
                })
                .map_err(Into::into),
            ),
            Poll::Ready(Err(e)) => Poll::Ready(Err(e.into())),
            Poll::Pending => Poll::Pending,
        }
    }
}
