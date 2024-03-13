use serde::Serialize;

#[derive(Serialize, Clone, Debug, Default, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(rename = "OpenDdFlags"))]
pub struct Flags {
    #[opendd(default, rename = "require_graphql_config")]
    // By default, OpenDd assumes camel-cased fields, rename to use snake-case
    pub require_graphql_config: bool,
}

impl Flags {
    pub fn default_json() -> serde_json::Value {
        serde_json::json!({
            "require_graphql_config": false
        })
    }
}
