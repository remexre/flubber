use super::JsonSubprocess;
use anyhow::{anyhow, Context as _};
use derive_more::Display;
use either::Either;
use flubber_plugin_proto::{
    InitInfo, Message, MessageID, NewMessage, NewRoom, Request, RequestBody, Response,
    ResponseBody, ResponseError, ResponseOrUpdate, Room, RoomID, Update,
};
use log::warn;
use std::{
    collections::HashMap,
    error::Error,
    ffi::OsStr,
    pin::Pin,
    sync::Arc,
    task::{Context, Poll},
};
use tokio::{
    prelude::*,
    sync::{mpsc, oneshot},
};

/// An error due to a plugin misbehaving in some manner.
#[derive(Clone, Debug, Display)]
pub enum PluginError {
    /// An I/O error occurred.
    #[display(fmt = "Got an I/O error when communicating with plugin: {}", _0)]
    Io(Arc<std::io::Error>),

    /// The plugin was terminated for a protocol violation.
    #[display(fmt = "The plugin violated the protocol: {}", _0)]
    ProtocolViolation(Arc<anyhow::Error>),

    /// The plugin is shutting down.
    #[display(fmt = "The plugin is shutting down.")]
    ShuttingDown,
}

impl Error for PluginError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            PluginError::Io(err) => Some(&**err),
            PluginError::ProtocolViolation(err) => Some(&***err),
            PluginError::ShuttingDown => None,
        }
    }
}

/// Slightly lower-level than (and used to implement) `Plugin`.
///
/// - On plugin crash or protocol violation, the update stream ends.
/// - On plugin crash or protocol violation, all outstanding requests, and all future requests fail
/// with a `PluginError`.
/// - Response errors are returned as-is.
#[derive(Debug)]
pub struct BasicPlugin {
    init_info: InitInfo,
    inner: JsonSubprocess,
    // TODO: Would another data structure be better?
    channels: HashMap<u32, oneshot::Sender<Result<ResponseBody, PluginError>>>,
    next_sequence_number: u32,
    queued_req: Option<Request>,
    req_sender: mpsc::Sender<(
        RequestBody,
        oneshot::Sender<Result<ResponseBody, PluginError>>,
    )>,
    req_recver: mpsc::Receiver<(
        RequestBody,
        oneshot::Sender<Result<ResponseBody, PluginError>>,
    )>,
    shutting_down: bool,
}

impl BasicPlugin {
    /// Creates a new `BasicPlugin`.
    pub async fn new<Arg: AsRef<OsStr>, Args: IntoIterator<Item = Arg>, Cmd: AsRef<OsStr>>(
        cmd: Cmd,
        args: Args,
    ) -> Result<BasicPlugin, PluginError> {
        let mut inner = JsonSubprocess::new(cmd, args)
            .map_err(Arc::new)
            .map_err(PluginError::Io)?;

        let init_info = match inner.next().await {
            Some(Ok(val)) => val,
            Some(Err(Either::Left(err))) => return Err(PluginError::Io(Arc::new(err))),
            Some(Err(Either::Right(err))) => {
                return Err(PluginError::ProtocolViolation(Arc::new(err.into())));
            }
            None => {
                return Err(PluginError::ProtocolViolation(Arc::new(anyhow!(
                    "Plugin exited before it sent the InitInfo"
                ))))
            }
        };
        let init_info = serde_json::from_value(init_info)
            .context("Invalid InitInfo")
            .map_err(Arc::new)
            .map_err(PluginError::ProtocolViolation)?;
        log::debug!("Got InitInfo: {:?}", init_info);

        // Small buffer size -> maximum backpressure.
        let (req_sender, req_recver) = mpsc::channel(1);

        Ok(BasicPlugin {
            init_info,
            inner,
            channels: HashMap::new(),
            next_sequence_number: 0,
            queued_req: None,
            req_sender,
            req_recver,
            shutting_down: false,
        })
    }

    /// Return the `InitInfo` of the plugin.
    pub fn init_info(&self) -> &InitInfo {
        &self.init_info
    }

    /// Returns a `BasicPluginSender` for the plugin.
    pub fn sender(&self) -> BasicPluginSender {
        BasicPluginSender {
            sender: self.req_sender.clone(),
        }
    }

    /// Gets the next available sequence number.
    fn next_sequence_number(&mut self) -> u32 {
        loop {
            let n = self.next_sequence_number;
            self.next_sequence_number = n.wrapping_add(1);
            if !self.channels.contains_key(&n) {
                break n;
            } else {
                warn!(
                    "Plugin ran out of sequence numbers -- not only are we likely using gigs of \
                     RAM, CPU performance is about to hit a cliff!"
                );
            }
        }
    }

    /// Fails all requests with a `PluginError`.
    fn on_error(&mut self, err: PluginError) {
        self.shutting_down = true;
        for (_, ch) in self.channels.drain() {
            drop(ch.send(Err(err.clone())));
        }
    }

    /// Fails all requests with a `PluginError`.
    fn on_either_error(&mut self, err: Either<std::io::Error, serde_json::Error>) {
        self.on_error(match err {
            Either::Left(err) => PluginError::Io(Arc::new(err)),
            Either::Right(err) => PluginError::ProtocolViolation(Arc::new(err.into())),
        })
    }

    /// Responds to a request, if possible.
    fn on_response(&mut self, response: Response) {
        match self.channels.remove(&response.sequence_number) {
            Some(ch) => match ch.send(Ok(response.body)) {
                Ok(()) => {}
                Err(_) => {
                    // TODO: Log the response.
                    warn!(
                        "Got a response to dropped request -- either the request timed out, or \
                         we're shutting down."
                    );
                }
            },
            None => {
                // TODO: Log the response.
                warn!(
                    "Got a response to failed request -- the plugin may be confused about \
                     sequence numbers?"
                )
            }
        }
    }
}

impl Stream for BasicPlugin {
    type Item = Update;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Option<Self::Item>> {
        loop {
            'incoming: while !self.shutting_down {
                // Flush the child.
                match Sink::poll_flush(Pin::new(&mut self.inner), cx) {
                    Poll::Ready(Ok(())) => {}
                    Poll::Ready(Err(err)) => {
                        self.on_either_error(err);
                        break 'incoming;
                    }
                    Poll::Pending => break 'incoming,
                }

                // Try to get another request to send.
                let req = match self.queued_req.take() {
                    Some(req) => req,
                    None => match Stream::poll_next(Pin::new(&mut self.req_recver), cx) {
                        Poll::Ready(Some((body, ch))) => {
                            let sequence_number = self.next_sequence_number();
                            drop(self.channels.insert(sequence_number, ch));
                            Request {
                                sequence_number,
                                body,
                            }
                        }
                        Poll::Ready(None) => {
                            unreachable!("BasicPlugin was half-dropped or something?")
                        }
                        Poll::Pending => break 'incoming,
                    },
                };

                // Prepare to send the request.
                match Sink::poll_ready(Pin::new(&mut self.inner), cx) {
                    Poll::Ready(Ok(())) => {}
                    Poll::Ready(Err(err)) => {
                        self.on_either_error(err);
                        break 'incoming;
                    }
                    Poll::Pending => {
                        self.queued_req = Some(req);
                        break 'incoming;
                    }
                }
                let value = match serde_json::to_value(&req) {
                    Ok(value) => value,
                    Err(err) => {
                        self.on_error(PluginError::ProtocolViolation(Arc::new(err.into())));
                        break 'incoming;
                    }
                };

                // Send the request to the child.
                match Sink::start_send(Pin::new(&mut self.inner), value) {
                    Ok(()) => {}
                    Err(err) => {
                        self.on_either_error(err);
                        break 'incoming;
                    }
                }
            }

            // Remove obsoleted channels.
            self.channels.retain(|_, ch| !ch.is_closed());

            // Try to read a value from the plugin.
            match Stream::poll_next(Pin::new(&mut self.inner), cx) {
                Poll::Ready(Some(Ok(value))) => match serde_json::from_value(value) {
                    Ok(ResponseOrUpdate::Response(response)) => self.on_response(response),
                    Ok(ResponseOrUpdate::Update(update)) => break Poll::Ready(Some(update)),
                    Err(err) => {
                        self.on_error(PluginError::ProtocolViolation(Arc::new(err.into())));
                        break Poll::Ready(None);
                    }
                },
                Poll::Ready(Some(Err(err))) => {
                    self.on_either_error(err);
                    break Poll::Ready(None);
                }
                Poll::Ready(None) => break Poll::Ready(None),
                Poll::Pending => break Poll::Pending,
            }
        }
    }
}

/// A value that can issue requests to a `BasicPlugin`. Note that requests are not serviced unless
/// the `BasicPlugin` is being polled on as a `Stream`.
#[derive(Clone, Debug)]
pub struct BasicPluginSender {
    sender: mpsc::Sender<(
        RequestBody,
        oneshot::Sender<Result<ResponseBody, PluginError>>,
    )>,
}

macro_rules! request {
    ($self:expr, $req:expr, $pat:pat) => {{
        let fut = $self.request($req);
        async move {
            match fut.await {
                Ok($pat) => Ok(()),
                Ok(ResponseBody::Error(err)) => Err(Either::Right(err)),
                Ok(body) => Err(Either::Left(PluginError::ProtocolViolation(Arc::new(
                    anyhow!("Invalid response: {:?}", body),
                )))),
                Err(err) => Err(Either::Left(err)),
            }
        }
    }};

    ($self:expr, $req:expr, $pat:pat, $var:ident) => {{
        let fut = $self.request($req);
        async move {
            match fut.await {
                Ok($pat) => Ok($var),
                Ok(ResponseBody::Error(err)) => Err(Either::Right(err)),
                Ok(body) => Err(Either::Left(PluginError::ProtocolViolation(Arc::new(
                    anyhow!("Invalid response: {:?}", body),
                )))),
                Err(err) => Err(Either::Left(err)),
            }
        }
    }};
}

impl BasicPluginSender {
    /// Makes a request, without doing anything with the response.
    fn request(&self, req: RequestBody) -> impl Future<Output = Result<ResponseBody, PluginError>> {
        let (send, recv) = oneshot::channel();
        let mut sender = self.sender.clone();
        async move {
            if sender.send((req, send)).await.is_err() {
                return Err(PluginError::ShuttingDown);
            }
            match recv.await {
                Ok(r) => r,
                Err(_) => Err(PluginError::ShuttingDown),
            }
        }
    }

    /// Requests some number of messages earlier in history than the given one.
    pub fn message_get_before(
        &self,
        id: MessageID,
    ) -> impl Future<Output = Result<(), Either<PluginError, ResponseError>>> {
        request!(
            self,
            RequestBody::MessageGetBefore(id),
            ResponseBody::Success
        )
    }

    /// Requests information about a message by ID.
    pub fn message_get(
        &self,
        id: MessageID,
    ) -> impl Future<Output = Result<Message, Either<PluginError, ResponseError>>> {
        request!(
            self,
            RequestBody::MessageGet(id),
            ResponseBody::Message(msg),
            msg
        )
    }

    /// Requests a message be sent.
    pub fn message_send(
        &self,
        msg: NewMessage,
    ) -> impl Future<Output = Result<MessageID, Either<PluginError, ResponseError>>> {
        request!(
            self,
            RequestBody::MessageSend(msg),
            ResponseBody::MessageID(id),
            id
        )
    }

    /// Requests information about a room by ID.
    pub fn room_get(
        &self,
        id: RoomID,
    ) -> impl Future<Output = Result<Room, Either<PluginError, ResponseError>>> {
        request!(
            self,
            RequestBody::RoomGet(id),
            ResponseBody::Room(room),
            room
        )
    }

    /// Requests a room be created.
    pub fn room_create(
        &self,
        room: NewRoom,
    ) -> impl Future<Output = Result<RoomID, Either<PluginError, ResponseError>>> {
        request!(
            self,
            RequestBody::RoomCreate(room),
            ResponseBody::RoomID(id),
            id
        )
    }

    /// Requests the ID of the named room.
    pub fn room_lookup(
        &self,
        name: String,
    ) -> impl Future<Output = Result<RoomID, Either<PluginError, ResponseError>>> {
        request!(
            self,
            RequestBody::RoomLookup(name),
            ResponseBody::RoomID(id),
            id
        )
    }

    /// Requests that a room be joined.
    pub fn room_join(
        &self,
        id: RoomID,
    ) -> impl Future<Output = Result<(), Either<PluginError, ResponseError>>> {
        request!(self, RequestBody::RoomJoin(id), ResponseBody::Success)
    }

    /// Requests that a room be left.
    pub fn room_leave(
        &self,
        id: RoomID,
    ) -> impl Future<Output = Result<(), Either<PluginError, ResponseError>>> {
        request!(self, RequestBody::RoomLeave(id), ResponseBody::Success)
    }
}
