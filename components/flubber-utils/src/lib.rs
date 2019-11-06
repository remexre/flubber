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
use serde::{de::DeserializeOwned, Serialize};
use serde_json::Deserializer;
use std::marker::PhantomData;
use tokio::codec::{Decoder, Encoder};

/// An encoder and decoder for JSON.
#[derive(Debug, Default)]
pub struct JsonCodec<T>(pub PhantomData<T>);

impl<T: DeserializeOwned> Decoder for JsonCodec<T> {
    type Item = T;
    type Error = std::io::Error;
    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        let mut des = Deserializer::from_slice(&src).into_iter::<T>();
        match des.next() {
            Some(Ok(value)) => {
                let n = des.byte_offset();
                src.advance(n);
                Ok(Some(value))
            }
            Some(Err(err)) => {
                if err.is_eof() {
                    Ok(None)
                } else {
                    Err(err.into())
                }
            }
            None => Ok(None),
        }
    }
}

impl<T: Serialize> Encoder for JsonCodec<T> {
    type Item = T;
    type Error = std::io::Error;
    fn encode(&mut self, item: Self::Item, dst: &mut BytesMut) -> Result<(), Self::Error> {
        dst.extend_from_slice(&serde_json::to_vec(&item)?);
        Ok(())
    }
}
