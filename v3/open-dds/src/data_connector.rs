use ndc_client::models::CapabilitiesResponse;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

mod v1;
mod v2;

use v1::DataConnectorV1;
pub use v2::{
    DataConnectorUrlV2 as DataConnectorUrl, DataConnectorV2, ReadWriteUrlsV2 as ReadWriteUrls,
};

/// The name of a data connector.
#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, JsonSchema, derive_more::Display,
)]
pub struct DataConnectorName(pub String);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "DataConnector")]
/// Definition of a data connector, used to bring in sources of data and connect them to OpenDD models and commands.
pub enum DataConnector {
    V1(DataConnectorV1),
    V2(DataConnectorV2),
}

impl DataConnector {
    pub fn upgrade(self) -> DataConnectorV2 {
        match self {
            DataConnector::V1(v1) => v1.upgrade(),
            DataConnector::V2(v2) => v2,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CapabilitiesResponseWithSchema(pub CapabilitiesResponse);

impl JsonSchema for CapabilitiesResponseWithSchema {
    fn schema_name() -> String {
        "CapabilitiesResponse".into()
    }

    fn json_schema(_gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        schemars::schema::Schema::new_ref("https://raw.githubusercontent.com/hasura/ndc-spec/v0.1.0-rc.11/ndc-client/tests/json_schema/capabilities_response.jsonschema".into())
    }
}

fn ndc_schema_response_schema_reference(
    _gen: &mut schemars::gen::SchemaGenerator,
) -> schemars::schema::Schema {
    schemars::schema::Schema::new_ref("https://raw.githubusercontent.com/hasura/ndc-spec/v0.1.0-rc.11/ndc-client/tests/json_schema/schema_response.jsonschema".into())
}

#[cfg(test)]
mod tests {
    use super::{DataConnector, DataConnectorV2};

    #[test]
    fn test_v1_to_v2_upgrade() {
        let v1: DataConnector = serde_json::from_str(
            r#"
            {
                "version": "v1",
                "definition": {
                    "name": "foo",
                    "url": {
                        "singleUrl": "http://foo"
                    },
                    "headers": {
                        "Authorization": {
                            "value": "Bearer: abc"
                        }
                    }
                }
            }
        "#,
        )
        .unwrap();

        let v2: DataConnector = serde_json::from_str(
            r#"
            {
                "version": "v2",
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
                    }
                }
            }
        "#,
        )
        .unwrap();

        let upgraded: DataConnectorV2 = serde_json::from_str(
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
                }
            }
        "#,
        )
        .unwrap();

        assert_eq!(v1.upgrade(), upgraded);
        assert_eq!(v2.upgrade(), upgraded);
    }
}
