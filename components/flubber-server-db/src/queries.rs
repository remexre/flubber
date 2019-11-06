use crate::Database;
use anyhow::Result;
use chrono::{DateTime, Utc};
use flubber_plugin_proto::{Message, MessageID, Room, RoomID};
use rusqlite::Connection;
use tokio::sync::oneshot;

macro_rules! queries {
    ($($(#[$meta:meta])* fn $name:ident($($arg:ident : $argt:ty),*) -> $outt:ty $body:block)*) => {
        #[allow(non_camel_case_types)]
        pub enum Query {
            $($name($($argt,)* oneshot::Sender<Result<$outt>>),)*
        }

        impl Database {
            $(
                $(#[$meta])*
                pub async fn $name(&mut self, $($arg : $argt),*) -> Result<$outt> {
                    let (send, recv) = oneshot::channel();
                    let query = Query::$name($($arg,)* send);
                    self.send.send(query)?;
                    recv.await?
                }
            )*
        }

        pub trait ConnectionExt {
            fn handle_query(&self, query: Query);
        }

        impl ConnectionExt for Connection {
            fn handle_query(&self, query: Query) {
                match query {
                    $(Query::$name($($arg,)* send) => {
                        drop(send.send((|| $body)()));
                    },)*
                }
            }
        }
    };
}

queries! {
    /// Gets a room by ID.
    fn get_room(_plugin: String, _id: RoomID) -> Option<Room> { unimplemented!() }

    /// Inserts or updates a room.
    fn upsert_room(_plugin: String, _room: Room) -> () { unimplemented!() }

    /// Marks a room as deleted.
    fn delete_room(_plugin: String, _id: RoomID) -> () { unimplemented!() }

    /// Lists rooms.
    fn list_rooms(_plugin: String) -> Vec<Room> { unimplemented!() }

    /// Gets a message by ID.
    fn get_message(_plugin: String, _id: MessageID) -> Option<Message> { unimplemented!() }

    /// Inserts or updates a message.
    fn upsert_message(_plugin: String, _message: Message) -> () { unimplemented!() }

    /// Marks a message as deleted.
    fn delete_message(_plugin: String, _id: MessageID) -> () { unimplemented!() }

    /// Lists messages.
    fn list_messages(
        _plugin: String,
        _room: RoomID,
        _before: Option<DateTime<Utc>>,
        _after: Option<DateTime<Utc>>
    ) -> Vec<MessageID> { unimplemented!() }

    /// Lists the plugins that have been seen.
    fn list_plugins() -> Vec<String> { unimplemented!() }

    /// Gets an attachment by hash.
    fn get_attachment(_hash: String) -> Vec<u8> { unimplemented!() }
}
