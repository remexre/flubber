//! Utilities used in Flubber.
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

use bytes::BytesMut;
use either::Either;
use serde::{de::DeserializeOwned, Serialize};
use serde_json::Deserializer;
use tokio::{prelude::*, stream::unfold};

/// Reads a value as JSON from an `AsyncRead`.
pub fn read_jsons<T: DeserializeOwned, R: 'static + AsyncRead + Unpin>(
    r: R,
) -> impl Stream<Item = Result<T, Either<std::io::Error, serde_json::Error>>> {
    unfold((BytesMut::new(), r), move |(mut buf, mut r)| {
        async {
            let mut chunk = [0u8; 4096];
            let result = loop {
                // Try to read.
                match r.read(&mut chunk).await {
                    Ok(n) => buf.extend_from_slice(&chunk[..n]),
                    Err(err) => break Err(Either::Left(err)),
                }

                // Try to parse.
                let mut des = Deserializer::from_slice(&buf).into_iter::<T>();
                match des.next() {
                    Some(Ok(value)) => {
                        let n = des.byte_offset();
                        buf.advance(n);
                        break Ok(value);
                    }
                    Some(Err(err)) => break Err(Either::Right(err)),
                    None => {}
                }
            };
            Some((result, (buf, r)))
        }
    })
}

/// Writes the given value as JSON to an `AsyncWrite`, with a trailing newline.
pub async fn write_json<T: Serialize, W: AsyncWrite + Unpin>(
    w: &mut W,
    value: &T,
) -> Result<(), Either<std::io::Error, serde_json::Error>> {
    let bytes = serde_json::to_vec(value).map_err(Either::Right)?;
    w.write_all(&bytes).await.map_err(Either::Left)?;
    w.flush().await.map_err(Either::Left)?;
    Ok(())
}
