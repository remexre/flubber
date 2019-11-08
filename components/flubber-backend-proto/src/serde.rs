//! Serde serialization modules.

/// Serde serialization of bytes as base64 strings.
pub mod base64 {
    use base64::{decode, encode};
    use serde::{de::Unexpected, Deserializer, Serializer};
    use std::fmt::Formatter;

    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = Vec<u8>;

        fn expecting(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
            fmt.write_str("a base64 string")
        }

        fn visit_str<E: serde::de::Error>(self, s: &str) -> Result<Vec<u8>, E> {
            decode(s).map_err(|_| E::invalid_value(Unexpected::Str(s), &self))
        }
    }

    /// Deserializes from a base64 string.
    pub fn deserialize<'de, D: Deserializer<'de>>(de: D) -> Result<Vec<u8>, D::Error> {
        de.deserialize_str(Visitor)
    }

    /// Serializes to a base64 string.
    pub fn serialize<S: Serializer>(bs: &[u8], ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&encode(bs))
    }
}

/// Serde serialization of MIME types.
pub mod mime {
    use mime::Mime;
    use serde::{de::Unexpected, Deserializer, Serializer};
    use std::fmt::Formatter;

    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = Mime;

        fn expecting(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
            fmt.write_str("a MIME type as a string")
        }

        fn visit_str<E: serde::de::Error>(self, s: &str) -> Result<Mime, E> {
            s.parse()
                .map_err(|_| E::invalid_value(Unexpected::Str(s), &self))
        }
    }

    /// Deserializes from a base64 string.
    pub fn deserialize<'de, D: Deserializer<'de>>(de: D) -> Result<Mime, D::Error> {
        de.deserialize_str(Visitor)
    }

    /// Serializes to a base64 string.
    pub fn serialize<S: Serializer>(mime: &Mime, ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&mime.to_string())
    }
}

/// Serde serialization of UTC datetimes as milliseconds since the Unix epoch.
pub mod unix_ms {
    use chrono::{DateTime, TimeZone, Utc};
    use serde::{Deserializer, Serializer};
    use std::fmt::Formatter;

    struct Visitor;

    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = DateTime<Utc>;

        fn expecting(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
            fmt.write_str("a count of milliseconds since the Unix epoch")
        }

        fn visit_i64<E: serde::de::Error>(self, ts: i64) -> Result<DateTime<Utc>, E> {
            Ok(Utc.timestamp_millis(ts))
        }
    }

    /// Deerializes from a timestamp in milliseconds since the Unix epoch.
    pub fn deserialize<'de, D: Deserializer<'de>>(de: D) -> Result<DateTime<Utc>, D::Error> {
        de.deserialize_i64(Visitor)
    }

    /// Serializes to a timestamp in milliseconds since the Unix epoch.
    pub fn serialize<S: Serializer>(t: &DateTime<Utc>, ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_i64(t.timestamp_millis())
    }
}
