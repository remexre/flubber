use crate::Database;
use anyhow::Result;
use chrono::{DateTime, Utc};
use flubber_plugin_proto::{Message, MessageID, Room, RoomID};
use tokio::sync::oneshot;

macro_rules! queries {
    ($($(#[$meta:meta])* fn $name:ident($($arg:ident : $argt:ty),*) -> $outt:ty;)*) => {
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
                    self.send.send(query).await?;
                    recv.await?
                }
            )*
        }
    };
}

queries! {
    /// Gets a room by ID.
    fn get_room(plugin: String, id: RoomID) -> Option<Room>;

    /// Inserts or updates a room.
    fn upsert_room(plugin: String, room: Room) -> ();

    /// Marks a room as deleted.
    fn delete_room(plugin: String, id: RoomID) -> ();

    /// Lists rooms.
    fn list_rooms(plugin: String) -> Vec<Room>;

    /// Gets a message by ID.
    fn get_message(plugin: String, id: MessageID) -> Option<Message>;

    /// Inserts or updates a message.
    fn upsert_message(plugin: String, message: Message) -> ();

    /// Marks a message as deleted.
    fn delete_message(plugin: String, id: MessageID) -> ();

    /// Lists messages.
    fn list_messages(
        plugin: String,
        room: RoomID,
        before: Option<DateTime<Utc>>,
        after: Option<DateTime<Utc>>
    ) -> Vec<MessageID>;

    /// Lists the plugins that have been seen.
    fn list_plugins() -> Vec<String>;
}
