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

mod queries;

use crate::queries::{ConnectionExt, Query};
use anyhow::Result;
use rusqlite::Connection;
use std::{
    path::Path,
    sync::{mpsc, Arc},
    thread::{spawn, JoinHandle},
};

/// A connection to the database.
///
/// Since database operations are synchronous, holds open a separate thread for them.
#[derive(Clone, Debug)]
pub struct Database {
    send: mpsc::SyncSender<Query>,
    thread: Arc<JoinHandle<()>>,
}

impl Database {
    /// Opens a database "connection" to the given path.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Database> {
        let conn = Connection::open(path)?;
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
        )?;
        let (send, recv) = mpsc::sync_channel(8);
        let thread = Arc::new(spawn(move || {
            recv.into_iter().for_each(|query| conn.handle_query(query));
        }));
        Ok(Database { send, thread })
    }
}
