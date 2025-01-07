use crate::traits::{OpenDd, OpenDdDeserializeError};
use core::ops::Deref;
use serde::{Serialize, Serializer};
use std::fmt::Display;

/// Wrapper that combines an item with its parsed JSONPath
/// for use in error reporting
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub path: jsonpath::JSONPath,
    pub value: T,
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.value.fmt(f)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: Serialize> Serialize for Spanned<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.value.serialize(serializer)
    }
}

impl<T: OpenDd> OpenDd for Spanned<T> {
    fn deserialize(
        json: serde_json::Value,
        path: jsonpath::JSONPath,
    ) -> Result<Self, OpenDdDeserializeError> {
        Ok(Spanned {
            value: T::deserialize(json, path.clone())?,
            path,
        })
    }

    fn json_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        T::json_schema(gen)
    }

    fn _schema_name() -> String {
        "Spanned".to_string()
    }

    fn _schema_is_referenceable() -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    #[test]
    fn test_spanned_path() {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples/reference.json");
        let metadata =
            open_dds::Metadata::from_json_str(&std::fs::read_to_string(path).unwrap()).unwrap();

        // Very deliberate tree-walking in the absence of optics.
        let open_dds::Metadata::Versioned(versioned) = metadata else {
            todo!("Test not implemented for unversioned metadata")
        };

        let open_dds::MetadataWithVersion::V2(metadata_v2) = versioned else {
            todo!("Test not implemented for non-V2 metadata")
        };

        for (subgraph_index, subgraph) in metadata_v2.subgraphs.iter().enumerate() {
            for (object_index, object) in subgraph.objects.iter().enumerate() {
                if let open_dds::OpenDdSubgraphObject::Model(model) = object {
                    let expected = format!("$.subgraphs[{subgraph_index}].objects[{object_index}]");
                    assert_eq!(model.path.to_string(), expected);
                }
            }
        }
    }
}
