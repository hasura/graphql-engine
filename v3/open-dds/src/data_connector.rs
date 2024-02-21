use ndc_client::models::{CapabilitiesResponse, SchemaResponse};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

mod v1;

pub use v1::{DataConnectorLinkV1, DataConnectorUrlV1 as DataConnectorUrl, ReadWriteUrls};

/// The name of a data connector.
#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, JsonSchema, derive_more::Display,
)]
pub struct DataConnectorName(pub String);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "DataConnectorLink")]
/// Definition of a data connector, used to bring in sources of data and connect them to OpenDD models and commands.
pub enum DataConnectorLink {
    V1(DataConnectorLinkV1),
}

impl DataConnectorLink {
    pub fn upgrade(self) -> DataConnectorLinkV1 {
        match self {
            DataConnectorLink::V1(v1) => v1,
        }
    }
}

fn ndc_capabilities_response_v01_schema_reference(
    _gen: &mut schemars::gen::SchemaGenerator,
) -> schemars::schema::Schema {
    schemars::schema::Schema::new_ref("https://raw.githubusercontent.com/hasura/ndc-spec/v0.1.0-rc.18/ndc-client/tests/json_schema/capabilities_response.jsonschema".into())
}

fn ndc_schema_response_v01_schema_reference(
    _gen: &mut schemars::gen::SchemaGenerator,
) -> schemars::schema::Schema {
    schemars::schema::Schema::new_ref("https://raw.githubusercontent.com/hasura/ndc-spec/v0.1.0-rc.18/ndc-client/tests/json_schema/schema_response.jsonschema".into())
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(tag = "version")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "VersionedSchemaAndCapabilities")]
pub enum VersionedSchemaAndCapabilities {
    #[serde(rename = "v0.1")]
    V01(SchemaAndCapabilitiesV01),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "SchemaAndCapabilitiesV01")]
pub struct SchemaAndCapabilitiesV01 {
    #[schemars(schema_with = "ndc_schema_response_v01_schema_reference")]
    pub schema: SchemaResponse,
    #[schemars(schema_with = "ndc_capabilities_response_v01_schema_reference")]
    pub capabilities: CapabilitiesResponse,
}

#[cfg(test)]
mod tests {
    use super::DataConnectorLinkV1;
    use crate::data_connector::DataConnectorLink;

    #[test]
    fn test_upgrade() {
        let v1: DataConnectorLink = serde_json::from_str(
            r#"
            {
                "version": "v1",
                "definition": {
                    "name": "foo",
                    "url": {
                        "singleUrl": {
                            "value": "http://foo"
                        }
                    },
                    "headers": {
                        "Authorization": {
                            "value": "Bearer: abc"
                        }
                    },
                    "schema": {
                        "version": "v0.1",
                        "capabilities": {
                            "version": "1",
                            "capabilities": {
                                "query": {},
                                "mutation": {}
                            }
                        },
                        "schema": {
                            "scalar_types": {},
                            "object_types": {},
                            "collections": [],
                            "functions": [],
                            "procedures": []
                        }
                    }
                }
            }
        "#,
        )
        .unwrap();

        let upgraded: DataConnectorLinkV1 = serde_json::from_str(
            r#"
            {
                "name": "foo",
                "url": {
                    "singleUrl": {
                        "value": "http://foo"
                    }
                },
                "headers": {
                    "Authorization": {
                        "value": "Bearer: abc"
                    }
                },
                "schema": {
                    "version": "v0.1",
                    "capabilities": {
                        "version": "1",
                        "capabilities": {
                            "query": {},
                            "mutation": {}
                        }
                    },
                    "schema": {
                        "scalar_types": {},
                        "object_types": {},
                        "collections": [],
                        "functions": [],
                        "procedures": []
                    }
                }
            }
        "#,
        )
        .unwrap();

        assert_eq!(v1.upgrade(), upgraded);
    }
}
