mod json_ext;

pub use json_ext::ValueExt;
use serde::{de::DeserializeOwned, ser::SerializeMap, Deserialize, Serialize};
use std::{collections::HashMap, hash::Hash};

/// HashMapWithJsonKey serializes the `key<K>` as a String
/// during serialization and similarly while deserialization, the keys
/// are expected to be Stringified and it is then deserialized into K.
/// This type can be helpful when a HashMap<K,V> needs to be serialized,
/// where K is a custom struct and which serializes into a JSON object
/// by default, serializing such a HashMap into JSON will throw an error
/// because JSON spec mandates that the keys in a JSON object be keys.
/// So, wrapping the HashMap with this type would serialize the Hashmap's
/// keys as strings and deserialize correspondingly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashMapWithJsonKey<K: Serialize + Eq + Hash + for<'a> Deserialize<'a>, V>(
    pub HashMap<K, V>,
);

impl<K: Serialize + for<'a> Deserialize<'a> + Eq + Hash, V: Serialize> Serialize
    for HashMapWithJsonKey<K, V>
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.0.len()))?;
        for (k, v) in self.0.iter() {
            let stringified_key = serde_json::to_string(k).map_err(serde::ser::Error::custom)?;
            map.serialize_entry(&stringified_key, v)?;
        }
        map.end()
    }
}

impl<'de, K: DeserializeOwned + Hash + Eq + Serialize, V: Deserialize<'de>> Deserialize<'de>
    for HashMapWithJsonKey<K, V>
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let map: HashMap<String, V> = Deserialize::deserialize(deserializer)?;
        let mut result = HashMap::new();
        for (k, v) in map.into_iter() {
            let k_str = serde_json::from_str(&k).map_err(serde::de::Error::custom)?;
            result.insert(k_str, v);
        }
        Ok(HashMapWithJsonKey(result))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use serde::{Deserialize, Serialize};

    use crate::HashMapWithJsonKey;

    #[test]
    fn test_hashmap_with_serializable_key() {
        #[derive(Serialize, Deserialize, Hash, PartialEq, Eq, Debug, Clone)]
        struct Foo {
            x: u32,
        }

        let mut test_map: HashMap<Foo, String> = HashMap::new();
        test_map.insert(Foo { x: 1 }, "hello".to_string());

        // The `test_map` cannot be serialized as is because
        // the key by default is serialized to a JSON object
        // and the JSON spec mandates that in a JSON object, the
        // key must be a String.

        assert!(serde_json::to_string(&test_map).is_err());

        let test_map_with_serializable_key = HashMapWithJsonKey(test_map.clone());

        let serialized_test_map_with_serializable_key =
            serde_json::to_string(&test_map_with_serializable_key).unwrap();

        assert_eq!(
            serialized_test_map_with_serializable_key,
            "{\"{\\\"x\\\":1}\":\"hello\"}"
        );

        let deserialized_test_map_with_serializable_key: HashMapWithJsonKey<Foo, String> =
            serde_json::from_str(&serialized_test_map_with_serializable_key).unwrap();

        assert_eq!(test_map, deserialized_test_map_with_serializable_key.0);
    }
}
