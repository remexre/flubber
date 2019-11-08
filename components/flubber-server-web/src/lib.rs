//! The HTTP interface to the Flubber server.
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

use flubber_server_db::Database;
use std::net::SocketAddr;
use warp::{filters::BoxedFilter, Filter, Reply};

/// Listens for incoming HTTP connections.
pub async fn start_http(db: Database, addr: impl Into<SocketAddr> + 'static) {
    warp::serve(filter(db)).run(addr).await
}

/// Returns the `Filter` used by `start`.
pub fn filter(_db: Database) -> BoxedFilter<(impl Reply,)> {
    warp::any().map(warp::reply).boxed()
}
