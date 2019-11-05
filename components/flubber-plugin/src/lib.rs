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

use anyhow::Result;
pub use flubber_plugin_proto as proto;
use flubber_plugin_proto::{InitInfo, Version};
use serde::Serialize;
use tokio::{
    io::{stdin, stdout, Stdin, Stdout},
    prelude::*,
};

/// A wrapper around stdio for communicating with the server from a plugin.
#[derive(Debug)]
pub struct Parent {
    stdin: Stdin,
    stdout: Stdout,
}

impl Parent {
    /// Creates a new `Parent`.
    pub async fn new(name: String, major: u32, minor: u32, patch: u32) -> Result<Parent> {
        let stdin = stdin();
        let mut stdout = stdout();
        write(
            &mut stdout,
            &InitInfo {
                plugin_name: name,
                plugin_version: Version(major, minor, patch),
                protocol_version: Version(0, 1, 0),
            },
        )
        .await?;
        Ok(Parent { stdin, stdout })
    }

    /// Creates a new `Parent` from the info in `Cargo.toml`.
    pub async fn new_from_cargo_toml() -> Result<Parent> {
        Parent::new(
            env!("CARGO_PKG_NAME").to_string(),
            env!("CARGO_PKG_VERSION_MAJOR").parse().unwrap(),
            env!("CARGO_PKG_VERSION_MINOR").parse().unwrap(),
            env!("CARGO_PKG_VERSION_PATCH").parse().unwrap(),
        )
        .await
    }
}

async fn write<T: Serialize, W: AsyncWrite + Unpin>(wr: &mut W, val: &T) -> Result<()> {
    let mut buf = serde_json::to_vec(val)?;
    buf.push(b'\n');
    wr.write_all(&buf).await?;
    wr.flush().await?;
    Ok(())
}
