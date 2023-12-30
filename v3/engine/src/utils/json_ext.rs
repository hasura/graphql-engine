use std::hash::{Hash, Hasher};

use serde_json as json;

/// A type to extend some functionality of `serde_json::Value`
/// Currently, it only implements the `Hash` trait,
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueExt(pub json::Value);

impl From<json::Value> for ValueExt {
    fn from(value: json::Value) -> Self {
        ValueExt(value)
    }
}

impl Hash for ValueExt {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_value(&self.0, state);
    }
}

fn hash_value<H: Hasher>(value: &json::Value, state: &mut H) {
    match value {
        json::Value::Null => 0.hash(state),
        json::Value::Bool(boolean) => boolean.hash(state),
        json::Value::Number(num) => num.hash(state),
        json::Value::String(string) => string.hash(state),
        json::Value::Array(array) => {
            for item in array {
                hash_value(item, state);
            }
        }
        json::Value::Object(map) => {
            for (k, v) in map {
                k.hash(state);
                hash_value(v, state);
            }
        }
    }
}
