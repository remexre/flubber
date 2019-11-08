//! Abstractions for plugins.
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

mod basic_plugin;
mod json_subprocess;

pub use self::{
    basic_plugin::{BasicPlugin, BasicPluginSender, PluginError},
    json_subprocess::JsonSubprocess,
};
use flubber_plugin_proto::{
    Message, MessageID, NewMessage, NewRoom, ResponseError, Room, RoomID, Update,
};
use std::{
    ffi::OsString,
    pin::Pin,
    task::{Context, Poll},
    time::Duration,
};
use tokio::prelude::*;

/// The highest-level interface to a plugin.
///
/// - On plugin crash or protocol violation, restarts the plugin with exponential backoff. The
/// exponential backoff time is reset once an update is received, or a request is responded to with
/// a non-error response.
/// - Requests have timeouts. A non-retryable error occurs if the request times out.
/// - On retryable error, tries again with exponental backoff. If 10 failures occur, stops
/// retrying, and promotes the error to a `ResponseError`.
#[derive(Debug)]
pub struct Plugin {
    inner: Option<()>,
    cmd: OsString,
    args: Vec<OsString>,
    env: Vec<(OsString, OsString)>,
    next_backoff: Duration,
    shutting_down: bool,
}

impl Plugin {
    /// The initial amount of time to wait between retries, in milliseconds.
    const BASE_BACKOFF_TIME_MS: u64 = 100;

    /// The initial amount of time to wait between retries.
    const BASE_BACKOFF_TIME: Duration = Duration::from_millis(Self::BASE_BACKOFF_TIME_MS);

    /// The maximum amount of time to wait between retries.
    const MAX_BACKOFF_TIME: Duration =
        Duration::from_millis(Self::BASE_BACKOFF_TIME_MS * (1 << 10));

    /// Creates a new `Plugin`.
    pub fn new<Arg, Args, Cmd, Env, K, V>(cmd: Cmd, args: Args, env: Env) -> Plugin
    where
        Arg: Into<OsString>,
        Args: IntoIterator<Item = Arg>,
        Cmd: Into<OsString>,
        Env: IntoIterator<Item = (K, V)>,
        K: Into<OsString>,
        V: Into<OsString>,
    {
        let cmd = cmd.into();
        let args = args.into_iter().map(Into::into).collect();
        let env = env.into_iter().map(|(k, v)| (k.into(), v.into())).collect();
        Plugin {
            inner: None,
            cmd,
            args,
            env,
            next_backoff: Self::BASE_BACKOFF_TIME,
            shutting_down: false,
        }
    }

    /// Polls for a reference to the `BasicPlugin`.
    fn poll_basic_plugin(&mut self) -> Poll<&mut BasicPlugin> {
        unimplemented!()
    }

    /// Returns a `PluginSender` for the plugin.
    pub fn sender(&self) -> PluginSender {
        unimplemented!()
    }

    /// Handles the plugin needing a restart.
    fn restart(&mut self) {
        let next = self.next_backoff;
        if next > Self::MAX_BACKOFF_TIME {
            self.shutting_down = true;
        } else {
            unimplemented!()
        }
    }
}

impl Stream for Plugin {
    type Item = Update;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Option<Self::Item>> {
        loop {
            if self.shutting_down {
                return Poll::Ready(None);
            }

            match self.poll_basic_plugin() {
                Poll::Ready(plugin) => match Stream::poll_next(Pin::new(plugin), cx) {
                    Poll::Ready(Some(u)) => break Poll::Ready(Some(u)),
                    Poll::Ready(None) => self.restart(),
                    Poll::Pending => break Poll::Pending,
                },
                Poll::Pending => break Poll::Pending,
            }
        }
    }
}

/// A value that can issue requests to a `Plugin`. Note that requests are not serviced unless the
/// `Plugin` is being polled on as a `Stream`.
#[derive(Clone, Debug)]
pub struct PluginSender {
    inner: (),
}

macro_rules! sender_methods {
    ($(
        $(#[$attrs:meta])*
        pub async fn $name:ident($arg:ident: $argt:ty) -> $outt:ty;
    )*) => {
        impl PluginSender {
            $(
                #[allow(unused)] // until finished implementing
                $(#[$attrs])*
                pub async fn $name(
                    &self,
                    $arg: $argt,
                ) -> Result<$outt, ResponseError> {
                    unimplemented!()
                }
            )*
        }
    };
}

sender_methods! {
    /// Requests some number of messages earlier in history than the given one.
    pub async fn message_get_before(id: MessageID) -> ();

    /// Requests information about a message by ID.
    pub async fn message_get(id: MessageID) -> Message;

    /// Requests a message be sent.
    pub async fn message_send(msg: NewMessage) -> MessageID;

    /// Requests information about a room by ID.
    pub async fn room_get(id: RoomID) -> Room;

    /// Requests a room be created.
    pub async fn room_create(room: NewRoom) -> RoomID;

    /// Requests the ID of the named room.
    pub async fn room_lookup(name: String) -> RoomID;

    /// Requests that a room be joined.
    pub async fn room_join(id: RoomID) -> ();

    /// Requests that a room be left.
    pub async fn room_leave(id: RoomID) -> ();
}
