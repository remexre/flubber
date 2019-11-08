//! Database bindings for the Flubber server.
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

#[macro_use]
mod macros;

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use flubber_plugin_proto::{Message, MessageID, Room, RoomID};
use rusqlite::{params, Connection, OptionalExtension};
use std::{
    collections::HashSet,
    path::Path,
    sync::{mpsc, Arc},
    thread::{spawn, JoinHandle},
};
use tokio::sync::oneshot;

/// A connection to the database. Cheaply clonable.
///
/// Since database operations are synchronous, holds open a separate thread for them.
#[derive(Clone, Debug)]
pub struct Database {
    send: mpsc::SyncSender<Query>,
    thread: Arc<JoinHandle<()>>,
}

impl Database {
    /// Opens a database "connection" to the given path. Note that this blocks the current
    /// thread.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Database> {
        let conn = Connection::open(path).context("Failed to open connection to database")?;
        conn.execute_batch(
            r#"
            create table if not exists blobs
              ( hash text not null
              , content blob not null
              , constraint blobHashUnique unique (hash) on conflict ignore
              );
            create table if not exists rooms
              ( plugin text not null
              , id integer not null
              , parent integer
              , name text not null
              -- TODO: Keep track of edits?
              , sendable integer not null
              , deleted integer not null
              , constraint roomIdUnique unique (plugin, id)
              -- TODO: Check parent?
              );
            create table if not exists messages
              ( plugin text not null
              , id integer not null
              , sender integer not null
              , recipientType integer not null -- 0 = room, 1 = user
              , recipient integer not null
              , content text not null -- as JSON
              , createTime integer not null -- millis since epoch
              , editTime integer not null -- millis since epoch
              , extra text not null -- as JSON
              , previous integer references messages(rowid) -- for tracking edits
              , deleted integer not null
              , constraint messageIdUnique unique (plugin, id)
              -- TODO: Check sender?
              -- TODO: Check recipient?
              );
            create table if not exists message_attachments
              ( message integer not null references messages(rowid)
              , attachment text not null references blobs(hash)
              );
            "#,
        )
        .context("Failed to initialize database")?;
        let (send, recv) = mpsc::sync_channel(8);
        let thread = Arc::new(spawn(move || {
            recv.into_iter().for_each(|query| conn.handle_query(query));
        }));
        Ok(Database { send, thread })
    }
}

queries! {
    /// Gets a room by ID.
    fn get_room(&self, plugin: String, id: RoomID) -> Option<Room> {
        self.query_row(
            r#"select (parent, name, sendable) from rooms
               where plugin = ? and id = ? and deleted = 0"#,
            params![plugin, id.0.clone()],
            |row| Ok(Room {
                id,
                parent: row.get::<_, Option<_>>(0)?.map(RoomID),
                name: row.get(1)?,
                sendable: row.get(2)?,
            }),
        ).optional().map_err(Into::into)
    }

    /// Inserts or updates a room.
    fn upsert_room(&self, _plugin: String, _room: Room) -> () { unimplemented!() }

    /// Marks a room as deleted.
    fn delete_room(&self, _plugin: String, _id: RoomID) -> () { unimplemented!() }

    /// Lists rooms.
    fn list_rooms(&self, plugin: String) -> HashSet<RoomID> {
        let mut stmt = self.prepare(r"select id from rooms where plugin = ?")?;
        let mut rows = stmt.query(params![plugin])?;

        let mut rooms = HashSet::new();
        while let Some(row) = rows.next()? {
            let room = row.get(0)?;
            drop(rooms.insert(RoomID(room)));
        }

        Ok(rooms)
    }

    /// Gets a message by ID.
    fn get_message(&self, _plugin: String, _id: MessageID) -> Option<Message> { unimplemented!() }

    /// Inserts or updates a message.
    fn upsert_message(&self, _plugin: String, _message: Message) -> () { unimplemented!() }

    /// Marks a message as deleted.
    fn delete_message(&self, _plugin: String, _id: MessageID) -> () { unimplemented!() }

    /// Lists messages.
    fn list_messages(
        &self,
        _plugin: String,
        _room: RoomID,
        _before: Option<DateTime<Utc>>,
        _after: Option<DateTime<Utc>>
    ) -> HashSet<MessageID> { unimplemented!() }

    /// Lists the plugins that have been seen.
    fn list_plugins(&self) -> HashSet<String> {
        let mut stmt = self.prepare(r#"
            select distinct plugin from rooms
            union
            select distinct plugin from messages
        "#)?;
        let mut rows = stmt.query(params![])?;

        let mut plugins = HashSet::new();
        while let Some(row) = rows.next()? {
            let plugin = row.get(0)?;
            drop(plugins.insert(plugin));
        }

        Ok(plugins)
    }

    /// Gets an attachment by hash.
    fn get_attachment(&self, _hash: String) -> Vec<u8> { unimplemented!() }
}
