use ndc_models;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

mod v1;

pub use v1::{DataConnectorLinkV1, DataConnectorUrlV1 as DataConnectorUrl, ReadWriteUrls};

use crate::{identifier::Identifier, impl_OpenDd_default_for};

/// The name of a data connector.
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    JsonSchema,
    derive_more::Display,
    opendds_derive::OpenDd,
)]
pub struct DataConnectorName(pub Identifier);

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "DataConnectorLink", example = "DataConnectorLink::example")
)]
/// Definition of a data connector, used to bring in sources of data and connect them to OpenDD models and commands.
pub enum DataConnectorLink {
    V1(DataConnectorLinkV1),
}

impl DataConnectorLink {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "kind": "DataConnectorLink",
            "version": "v1",
            "definition": {
                "name": "data_connector",
                "url": {
                    "singleUrl": {
                        "value": "http://data_connector:8100"
                    }
                },
                "headers": {},
                "schema": {
                    "version": "v0.1",
                    "schema": {
                        "scalar_types": {},
                        "object_types": {},
                        "collections": [],
                        "functions": [],
                        "procedures": []
                    },
                    "capabilities": {
                        "version": "0.1.0",
                        "capabilities": {
                            "query": {
                                "variables": {}
                            },
                            "mutation": {}
                        }
                    },
                }
            }
        })
    }

    pub fn upgrade(self) -> DataConnectorLinkV1 {
        match self {
            DataConnectorLink::V1(v1) => v1,
        }
    }
}

fn ndc_capabilities_response_v01_schema_reference(
    _gen: &mut schemars::gen::SchemaGenerator,
) -> schemars::schema::Schema {
    schemars::schema::Schema::new_ref("https://raw.githubusercontent.com/hasura/ndc-spec/v0.1.2/ndc-models/tests/json_schema/capabilities_response.jsonschema".into())
}

fn ndc_schema_response_v01_schema_reference(
    _gen: &mut schemars::gen::SchemaGenerator,
) -> schemars::schema::Schema {
    schemars::schema::Schema::new_ref("https://raw.githubusercontent.com/hasura/ndc-spec/v0.1.2/ndc-models/tests/json_schema/schema_response.jsonschema".into())
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_internally_tagged,
    json_schema(title = "VersionedSchemaAndCapabilities")
)]
pub enum VersionedSchemaAndCapabilities {
    #[serde(rename = "v0.1")]
    #[opendd(rename = "v0.1")]
    V01(SchemaAndCapabilitiesV01),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "SchemaAndCapabilitiesV01")]
pub struct SchemaAndCapabilitiesV01 {
    #[schemars(schema_with = "ndc_schema_response_v01_schema_reference")]
    pub schema: ndc_models::SchemaResponse,
    #[schemars(schema_with = "ndc_capabilities_response_v01_schema_reference")]
    pub capabilities: ndc_models::CapabilitiesResponse,
}

// Derive OpenDd for `SchemaAdnCapabilitiesV01` by serde Deserialize and schemars JsonSchema implementations.
impl_OpenDd_default_for!(SchemaAndCapabilitiesV01);

#[cfg(test)]
mod tests {
    use super::DataConnectorLinkV1;
    use crate::data_connector::DataConnectorLink;

    #[test]
    fn test_upgrade() {
        let v1: DataConnectorLink = crate::traits::OpenDd::deserialize(serde_json::json!(
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
        ))
        .unwrap();

        let upgraded: DataConnectorLinkV1 = crate::traits::OpenDd::deserialize(serde_json::json!(
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
        ))
        .unwrap();

        assert_eq!(v1.upgrade(), upgraded);
    }
}
