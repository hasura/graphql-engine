use crate::{identifier::Identifier, str_newtype};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

str_newtype!(ViewName over Identifier | doc "The name of a SQL view.");

/// Definition of a SQL view that can be created in the data layer
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "View", example = "View::example")
)]
pub enum View {
    V1(ViewV1),
}

/// Definition of a SQL view that can be created in the data layer
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "ViewV1")]
pub struct ViewV1 {
    /// The name of the view
    pub name: ViewName,
    /// SQL query that defines the view
    pub sql_expression: String,
    /// Optional description
    pub description: Option<String>,
}

impl View {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "kind": "View",
            "version": "v1",
            "definition": {
                "name": "customer_summary",
                "sqlExpression": "SELECT customer_id, COUNT(*) as order_count FROM orders GROUP BY customer_id",
                "description": "Summary of customer orders"
            }
        })
    }

    pub fn upgrade(self) -> ViewV1 {
        match self {
            View::V1(v1) => v1,
        }
    }
}
