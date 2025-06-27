//! A low level flat representation for most Rust types. Suitable for saving and loading from files.
//!
//! The trait and code currently exists here as there may be some context related things.
//! May become a separate crate at some point

use std::collections::{HashMap, HashSet};

use source_map::{SourceId, SpanWithSource};

use crate::TypeId;

use shared_types::serialization::BinarySerializable;

impl BinarySerializable for TypeId {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&self.0.to_le_bytes());
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, _source: SourceId) -> Self {
		Self(u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]))
	}
}
