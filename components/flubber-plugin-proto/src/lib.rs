//! The protocol between the server and plugins.
//!
//! ## Example Session
//!
//! ```json
//! P: {"plugin_name": "Example", "plugin_version": [0, 0, 1], "protocol_version": [0, 1, 0]}
//! S: {"sequence_number": 0, "body": {"type": "RoomLookup", "value": "#general"}}
//! P: {"sequence_number": 0, "body": {"type": "RoomID", "value": "#general"}}
//! S: {"sequence_number": 1, "body": {"type": "RoomJoin", "value": "#general"}}
//! P: {"sequence_number": 1, "body": {"type": "Success", "value": null}}
//! S: {"sequence_number": 2, "body": {"type": "MessageSend", "value": {"recipient": "#general", "attachments": [], "content": {"type": "Text", "value: "Hello, world!"}, "extra": null}}}
//! P: {"sequence_number": 2, "body": {"type": "MessageID", "value": "test"}}
//! ```
//!
//! TODO:
//! - Work out how emotes should work
//!   - EmoteID? Store emotes by hash?
//! - Users
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

pub mod serde;

use chrono::{DateTime, Utc};
use mime::Mime;
use serde_derive::{Deserialize, Serialize};
use serde_json::Value as Json;
use std::{
    error::Error,
    fmt::{Display, Formatter},
};
use sval::Value;

/// Plugin sends this to the server when it starts.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct InitInfo {
    /// The name of the plugin.
    pub plugin_name: String,

    /// The version of the plugin.
    pub plugin_version: Version,

    /// The version of the protocol. This is version `0.1.0`.
    pub protocol_version: Version,
}

/// The version of the plugin or protocol.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct Version(pub u32, pub u32, pub u32);

/// A name for a message on a service. This should uniquely identify the message, even if it gets edited.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct MessageID(pub String);

/// A name for a room on a service. This should uniquely identify a room through renames if possible.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct RoomID(pub String);

/// A name for a user on a service. This should uniquely identify a user through renames if possible.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct UserID(pub String);

/// A RoomID or UserID.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[serde(untagged)]
#[sval(derive_from = "serde")]
pub enum RoomIDOrUserID {
    /// A RoomID.
    Room(RoomID),

    /// A UserID.
    User(UserID),
}

/// A message sent from a user to another user or a room.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct Message {
    /// The ID of the message.
    pub id: MessageID,

    /// The sending User.
    pub sender: UserID,

    /// The Room sent to, or the User who was DM'd.
    pub recipient: RoomIDOrUserID,

    /// Attachments sent with the message.
    pub attachments: Vec<MessageAttachment>,

    /// The body of the message.
    pub content: MessageContent,

    /// The time the message was created.
    #[serde(with = "crate::serde::unix_ms")]
    pub create_time: DateTime<Utc>,

    /// The time the message was last edited.
    #[serde(with = "crate::serde::unix_ms")]
    pub edit_time: DateTime<Utc>,

    /// Extra plugin-specific data.
    #[serde(default)]
    pub extra: Json,
}

/// A message sent from a user to another user or a room.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct NewMessage {
    /// The Room to send to, or the User to DM.
    pub recipient: RoomIDOrUserID,

    /// Attachments sent with the message.
    pub attachments: Vec<MessageAttachment>,

    /// The body of the message.
    pub content: MessageContent,

    /// Extra plugin-specific data.
    #[serde(default)]
    pub extra: Json,
}

/// MIME-typed data attached to a message.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct MessageAttachment {
    /// The mime type of a message.
    #[serde(with = "crate::serde::mime")]
    pub mime: Mime,

    /// The contents of the attachment.
    #[serde(with = "crate::serde::base64")]
    pub data: Vec<u8>,
}

/// The contents of a message.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[serde(tag = "type", content = "value")]
#[sval(derive_from = "serde")]
pub enum MessageContent {
    /// Displays the enclosed content in bold.
    Bold(Box<MessageContent>),

    /// Concatenates the enclosed content.
    Concat(Vec<MessageContent>),

    /// Displays the enclosed content crossed out.
    Crossout(Box<MessageContent>),

    /// An emote.
    ///
    /// **TODO**: How to do these? Just make them cached images?
    Emote(String),

    /// Displays the enclosed content in italics.
    Italic(Box<MessageContent>),

    /// A link to a message.
    MessageLink(MessageID),

    /// A link to a room.
    RoomLink(RoomID),

    /// Plain text.
    Text(String),

    /// A link to a resource by URL.
    UrlLink(String),

    /// Displays the enclosed content with an underline.
    Underline(Box<MessageContent>),

    /// A link to a user.
    UserLink(UserID),
}

/// The information corresponding to a room.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct Room {
    /// The ID of the room.
    pub id: RoomID,

    /// The room which is the parent of this room.
    pub parent: Option<RoomID>,

    /// The name of the room.
    pub name: String,

    /// Whether the room can be sent to.
    pub sendable: bool,
}

/// A request to create a new room with the given properties.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct NewRoom {
    /// The room which is the parent of this room.
    pub parent: Option<RoomID>,

    /// The name of the room.
    pub name: String,

    /// Whether the room can be sent to.
    pub sendable: bool,
}

/// Information sent from the plugin to the server.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[serde(tag = "type", content = "value")]
#[sval(derive_from = "serde")]
pub enum Update {
    /// Notification that a room was created or edited.
    RoomUpsert(Room),

    /// Notification that a room was deleted.
    RoomDelete(RoomID),

    /// Notification that a message was created or edited.
    MessageUpsert(Message),

    /// Notification that a message was deleted.
    MessageDelete(MessageID),
}

/// A request as sent to the plugin.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct Request {
    /// The sequence number of the request. Two requests with the same sequence number may not be
    /// in flight at the same time.
    pub sequence_number: u32,

    /// The contents of the request.
    pub body: RequestBody,
}

/// A response as sent from the plugin.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct Response {
    /// The sequence number, which must match the sequence number in the request.
    pub sequence_number: u32,

    /// The contents of the response.
    pub body: ResponseBody,
}

/// A Response or Update.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[serde(untagged)]
#[sval(derive_from = "serde")]
pub enum ResponseOrUpdate {
    /// A Response.
    Response(Response),

    /// An Update.
    Update(Update),
}

/// A request made to a server.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[serde(tag = "type", content = "value")]
#[sval(derive_from = "serde")]
pub enum RequestBody {
    /// A request to get some number of messages earlier in history than the given one.
    ///
    /// The only valid non-error response is a `ResponseBody::Success`.
    MessageGetBefore(MessageID),

    /// A request to get information about a message by ID.
    ///
    /// The only valid non-error response is a `ResponseBody::Message`.
    MessageGet(MessageID),

    /// A request to send a message.
    ///
    /// The only valid non-error response is a `ResponseBody::MessageID`.
    MessageSend(NewMessage),

    /// A request to get information about a room by ID.
    ///
    /// The only valid non-error response is a `ResponseBody::Room`.
    RoomGet(RoomID),

    /// A request to create a room.
    ///
    /// The only valid non-error response is a `ResponseBody::RoomID`.
    RoomCreate(NewRoom),

    /// A request to get the ID of a named room.
    ///
    /// The only valid non-error response is a `ResponseBody::RoomID`.
    RoomLookup(String),

    /// A request to join a room.
    ///
    /// The only valid non-error response is a `ResponseBody::Success`.
    RoomJoin(RoomID),

    /// A request to leave a room.
    ///
    /// The only valid non-error response is a `ResponseBody::Success`.
    RoomLeave(RoomID),
}

/// The response to a request.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[serde(tag = "type", content = "value")]
#[sval(derive_from = "serde")]
pub enum ResponseBody {
    /// The request succeeded without returning a response.
    Success,

    /// The request succeeded, resulting in a message.
    Message(Message),

    /// The request succeeded, resulting in a room.
    Room(Room),

    /// The request succeeded, resulting in a message ID.
    MessageID(MessageID),

    /// The request succeeded, resulting in a room ID.
    RoomID(RoomID),

    /// The request failed.
    Error(ResponseError),
}

/// An error with a request.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, Value)]
#[sval(derive_from = "serde")]
pub struct ResponseError {
    /// An error message.
    pub message: String,

    /// Additional information to be logged for debugging, but not necessarily shown to the user.
    #[serde(default)]
    pub debug_info: Json,

    /// Whether the error can be resolved by retrying the request. Note that this typically implies
    /// that the action was idempotent; see https://www.tedinski.com/2019/02/20/idempotence.html.
    #[serde(default)]
    pub retry: bool,
}

impl Display for ResponseError {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        fmt.write_str(&self.message)
    }
}

impl Error for ResponseError {
    fn description(&self) -> &str {
        &self.message
    }
}
