use serde::Serialize;

use crate::identifier::SubgraphNameInput;
use crate::str_newtype;

str_newtype!(SqlCatalogName | doc "The name of a SQL catalog.");
str_newtype!(SqlSchemaName | doc "The name of a SQL schema.");

/// Configuration for mapping a subgraph to a specific SQL catalog and schema.
/// By default, models in a subgraph are exposed in the SQL interface as
/// `default.<subgraph_name>.<model_name>`. A `SqlSchemaAlias` overrides this
/// so that the models appear under a different catalog and/or schema name.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "SqlSchemaAlias", example = "SqlSchemaAlias::example")
)]
pub enum SqlSchemaAlias {
    V1(SqlSchemaAliasV1),
}

impl SqlSchemaAlias {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "kind": "SqlSchemaAlias",
            "version": "v1",
            "definition": {
                "subgraph": "my_subgraph",
                "catalog": "shelf",
                "schema": "default"
            }
        })
    }

    pub fn upgrade(self) -> SqlSchemaAliasV1 {
        match self {
            SqlSchemaAlias::V1(v1) => v1,
        }
    }
}

/// Maps all models and views in a subgraph to a given SQL catalog and schema.
///
/// For example, with `catalog: "shelf"` and `schema: "default"`, a model named
/// `Foo` in the mapped subgraph would be queryable as `shelf.default.Foo`
/// as well as `default.<subgraph>.Foo`.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "SqlSchemaAliasV1"))]
pub struct SqlSchemaAliasV1 {
    /// The subgraph whose models should be remapped.
    pub subgraph: SubgraphNameInput,
    /// The SQL catalog name to expose models under.
    pub catalog: SqlCatalogName,
    /// The SQL schema name within the catalog.
    pub schema: SqlSchemaName,
}
