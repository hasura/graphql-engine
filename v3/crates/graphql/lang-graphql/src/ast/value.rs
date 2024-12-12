use super::{common::*, spanning::Spanning};
use serde::{Deserialize, Serialize};

// Represents a float literal
// [Reference](https://spec.graphql.org/October2021/#sec-Float-Value)
// #[derive(Clone, Debug, PartialEq, Eq)]
// pub struct FloatLiteral {
//     pub integer: BigInt,
//     pub fractional: Option<BigUint>,
//     pub exponent: Option<BigUint>,
// }

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum SimpleValue {
    /// `null`.
    Null,
    /// Id
    Id(String),
    /// A number.
    Integer(i64),
    /// A float
    Float(f64),
    /// A string.
    String(String),
    /// A boolean.
    Boolean(bool),
    /// An enum. These are typically in `SCREAMING_SNAKE_CASE`.
    Enum(Name),
}

impl SimpleValue {
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            SimpleValue::Null => serde_json::Value::Null,
            SimpleValue::Id(id) => serde_json::json!(id),
            SimpleValue::Integer(i) => serde_json::json!(i),
            SimpleValue::Float(f) => serde_json::json!(f),
            SimpleValue::String(s) => serde_json::json!(s),
            SimpleValue::Boolean(b) => serde_json::json!(b),
            SimpleValue::Enum(e) => serde_json::json!(e.as_str()),
        }
    }

    pub fn as_id(&self) -> Option<&str> {
        match self {
            SimpleValue::Id(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            SimpleValue::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_enum(&self) -> Option<&Name> {
        match self {
            SimpleValue::Enum(name) => Some(name),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            SimpleValue::Integer(i) => Some(*i),
            _ => None,
        }
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            // Both integer and float input values are accepted for Float type.
            // Ref: https://spec.graphql.org/October2021/#sec-Float.Input-Coercion
            SimpleValue::Float(f) => Some(*f),
            SimpleValue::Integer(i) => Some(*i as f64),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            SimpleValue::Boolean(b) => Some(*b),
            _ => None,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct KeyValue<V> {
    pub key: Spanning<Name>,
    pub value: Spanning<V>,
}

/// A resolved GraphQL value, for example `1` or `"Hello World!"`.
///
/// It can be serialized and deserialized. Enums will be converted to strings. Attempting to
/// serialize `Upload` will fail, and `Enum` and `Upload` cannot be deserialized.
///
/// [Reference](https://spec.graphql.org/June2018/#Value).
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum ConstValue {
    /// A leaf value
    SimpleValue(SimpleValue),
    /// A list of values.
    List(Vec<Spanning<ConstValue>>),
    /// An object. This is a map of keys to values.
    Object(Vec<Spanning<KeyValue<ConstValue>>>),
}

impl ConstValue {
    pub fn kind(&self) -> &'static str {
        match self {
            ConstValue::SimpleValue(simple_value) => match simple_value {
                SimpleValue::Null => "NULL",
                SimpleValue::Id(_) => "ID",
                SimpleValue::Integer(_) => "INTEGER",
                SimpleValue::Float(_) => "FLOAT",
                SimpleValue::String(_) => "STRING",
                SimpleValue::Boolean(_) => "BOOLEAN",
                SimpleValue::Enum(_) => "ENUM",
            },
            ConstValue::List(_) => "LIST",
            ConstValue::Object(_) => "OBJECT",
        }
    }
    pub fn to_json(&self) -> serde_json::Value {
        match self {
            ConstValue::SimpleValue(simple_value) => simple_value.to_json(),
            ConstValue::List(l) => {
                let list = l
                    .iter()
                    .map(|i| i.item.to_json())
                    .collect::<Vec<serde_json::Value>>();
                serde_json::Value::Array(list)
            }
            ConstValue::Object(o) => {
                let hash_map = o
                    .iter()
                    .map(|i| {
                        let KeyValue { key, value } = &i.item;
                        (key.item.to_string(), value.item.to_json())
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(hash_map)
            }
        }
    }
    pub fn is_null(&self) -> bool {
        matches!(self, ConstValue::SimpleValue(SimpleValue::Null))
    }
    pub fn as_id(&self) -> Option<&str> {
        match self {
            ConstValue::SimpleValue(simple_value) => simple_value.as_id(),
            _ => None,
        }
    }
    pub fn as_str(&self) -> Option<&str> {
        match self {
            ConstValue::SimpleValue(simple_value) => simple_value.as_str(),
            _ => None,
        }
    }
    pub fn as_enum(&self) -> Option<&Name> {
        match self {
            ConstValue::SimpleValue(simple_value) => simple_value.as_enum(),
            _ => None,
        }
    }
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            ConstValue::SimpleValue(simple_value) => simple_value.as_i64(),
            _ => None,
        }
    }
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            ConstValue::SimpleValue(simple_value) => simple_value.as_f64(),
            _ => None,
        }
    }
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::SimpleValue(simple_value) => simple_value.as_bool(),
            _ => None,
        }
    }
    pub fn as_object(&self) -> Option<&Vec<Spanning<KeyValue<ConstValue>>>> {
        match self {
            ConstValue::Object(key_values) => Some(key_values),
            _ => None,
        }
    }
    pub fn as_list(&self) -> Option<&Vec<Spanning<ConstValue>>> {
        match self {
            ConstValue::List(values) => Some(values),
            _ => None,
        }
    }
}

/// A GraphQL value, for example `1`, `$name` or `"Hello World!"`. This is
/// [`ConstValue`](enum.ConstValue.html) with variables.
///
/// It can be serialized and deserialized. Enums will be converted to strings. Attempting to
/// serialize `Upload` or `Variable` will fail, and `Enum`, `Upload` and `Variable` cannot be
/// deserialized.
///
/// [Reference](https://spec.graphql.org/June2018/#Value).
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// A variable, without the `$`.
    Variable(Name),
    /// A leaf value
    SimpleValue(SimpleValue),
    /// A list of values.
    List(Vec<Spanning<Value>>),
    /// An object. This is a map of keys to values.
    Object(Vec<Spanning<KeyValue<Value>>>),
}
impl Value {
    pub fn kind(&self) -> &'static str {
        match self {
            Value::SimpleValue(simple_value) => match simple_value {
                SimpleValue::Null => "NULL",
                SimpleValue::Id(_) => "ID",
                SimpleValue::Integer(_) => "INTEGER",
                SimpleValue::Float(_) => "FLOAT",
                SimpleValue::String(_) => "STRING",
                SimpleValue::Boolean(_) => "BOOLEAN",
                SimpleValue::Enum(_) => "ENUM",
            },
            Value::List(_) => "LIST",
            Value::Object(_) => "OBJECT",
            Value::Variable(_) => "VARIABLE",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer_as_float() {
        let int_value = SimpleValue::Integer(1);
        let expected: f64 = 1.0;
        assert_eq!(int_value.as_f64(), Some(expected));
    }
}
