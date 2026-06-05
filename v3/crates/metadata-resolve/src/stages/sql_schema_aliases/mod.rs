use std::collections::BTreeMap;

use open_dds::identifier::SubgraphName;
use open_dds::sql_schema_aliases::{SqlCatalogName, SqlSchemaName};
use serde::{Deserialize, Serialize};

/// A resolved SQL schema alias: maps a subgraph to a (catalog, schema) pair
/// for the SQL interface.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SqlSchemaAlias {
    pub catalog: SqlCatalogName,
    pub schema: SqlSchemaName,
}

/// The output of resolving all `SqlSchemaAlias` metadata objects.
/// Keyed by subgraph name.
pub type SqlSchemaAliases = BTreeMap<SubgraphName, SqlSchemaAlias>;

#[derive(Debug, thiserror::Error)]
pub enum SqlSchemaAliasError {
    #[error("duplicate SQL schema alias for subgraph '{subgraph}'")]
    DuplicateMapping { subgraph: SubgraphName },
}

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Result<SqlSchemaAliases, Vec<SqlSchemaAliasError>> {
    let mut mappings = BTreeMap::new();
    let mut errors = Vec::new();

    for mapping_obj in &metadata_accessor.sql_schema_aliases {
        let subgraph: SubgraphName = (&mapping_obj.subgraph).into();
        let mapping = SqlSchemaAlias {
            catalog: mapping_obj.catalog.clone(),
            schema: mapping_obj.schema.clone(),
        };

        if mappings.insert(subgraph.clone(), mapping).is_some() {
            errors.push(SqlSchemaAliasError::DuplicateMapping { subgraph });
        }
    }

    if errors.is_empty() {
        Ok(mappings)
    } else {
        Err(errors)
    }
}
