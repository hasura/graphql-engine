//! Describe and populate the introspection tables used by data fusion.

use std::{any::Any, sync::Arc};

use async_trait::async_trait;
use column_metadata::{ColumnMetadata, ColumnMetadataRow, COLUMN_METADATA};
use foreign_keys::{InferredForeignKeys, InferredForeignKeysRow, INFERRED_FOREIGN_KEY_CONSTRAINTS};
use indexmap::IndexMap;
use metadata_resolve::{self as resolved, ModelRelationshipTarget};
use table_metadata::{TableMetadata, TableMetadataRow, TABLE_METADATA};
mod datafusion {
    pub(super) use datafusion::{
        catalog::SchemaProvider, datasource::TableProvider, error::Result,
    };
}
use open_dds::relationships::RelationshipType;
use serde::{Deserialize, Serialize};

use super::mem_table::MemTable;

mod column_metadata;
mod foreign_keys;
mod table_metadata;

pub const HASURA_METADATA_SCHEMA: &str = "hasura";

/// Describes the database schema structure and metadata.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Introspection {
    table_metadata: TableMetadata,
    column_metadata: ColumnMetadata,
    inferred_foreign_key_constraints: InferredForeignKeys,
}

impl Introspection {
    /// Derive SQL schema from the Open DDS metadata.
    pub fn from_metadata(
        metadata: &resolved::Metadata,
        schemas: &IndexMap<String, crate::catalog::subgraph::Subgraph>,
    ) -> Self {
        let mut table_metadata_rows = Vec::new();
        let mut column_metadata_rows = Vec::new();
        let mut foreign_key_constraint_rows = Vec::new();
        for (schema_name, schema) in schemas {
            for (table_name, table) in &schema.tables {
                table_metadata_rows.push(TableMetadataRow::new(
                    schema_name.to_string(),
                    table_name.to_string(),
                    table.description.clone(),
                ));
                for (column_name, column_description) in &table.columns {
                    column_metadata_rows.push(ColumnMetadataRow::new(
                        schema_name.to_string(),
                        table_name.clone(),
                        column_name.clone(),
                        column_description.clone(),
                    ));
                }

                // TODO:
                // 1. Need to check if the target_model is part of subgraphs
                // 2. Need to also check for array relationships in case the corresponding
                //    object relationship isn't present
                if let Some(object_type) = metadata.object_types.get(&table.data_type) {
                    for relationship in object_type.relationship_fields.values() {
                        if let metadata_resolve::RelationshipTarget::Model(
                            ModelRelationshipTarget {
                                model_name,
                                relationship_type: RelationshipType::Object,
                                target_typename: _,
                                mappings,
                            },
                        ) = &relationship.target
                        {
                            for mapping in mappings {
                                foreign_key_constraint_rows.push(InferredForeignKeysRow::new(
                                    schema_name.to_string(),
                                    table_name.clone(),
                                    mapping.source_field.field_name.to_string(),
                                    model_name.subgraph.to_string(),
                                    model_name.name.to_string(),
                                    mapping.target_field.field_name.to_string(),
                                ));
                            }
                        }
                    }
                }
            }
        }
        Introspection {
            table_metadata: TableMetadata::new(table_metadata_rows),
            column_metadata: ColumnMetadata::new(column_metadata_rows),
            inferred_foreign_key_constraints: InferredForeignKeys::new(foreign_key_constraint_rows),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct IntrospectionSchemaProvider {
    tables: IndexMap<String, Arc<MemTable>>,
}

impl IntrospectionSchemaProvider {
    pub(crate) fn new(introspection: &Introspection) -> Self {
        let tables = [
            (
                TABLE_METADATA,
                introspection.table_metadata.to_table_provider(),
            ),
            (
                COLUMN_METADATA,
                introspection.column_metadata.to_table_provider(),
            ),
            (
                INFERRED_FOREIGN_KEY_CONSTRAINTS,
                introspection
                    .inferred_foreign_key_constraints
                    .to_table_provider(),
            ),
        ]
        .into_iter()
        .map(|(k, table)| (k.to_string(), Arc::new(table)))
        .collect();
        IntrospectionSchemaProvider { tables }
    }
}

#[async_trait]
impl datafusion::SchemaProvider for IntrospectionSchemaProvider {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn table_names(&self) -> Vec<String> {
        self.tables.keys().cloned().collect::<Vec<_>>()
    }

    async fn table(
        &self,
        name: &str,
    ) -> datafusion::Result<Option<Arc<dyn datafusion::TableProvider>>> {
        Ok(self
            .tables
            .get(name)
            .cloned()
            .map(|table| table as Arc<dyn datafusion::TableProvider>))
    }

    fn table_exist(&self, name: &str) -> bool {
        self.tables.contains_key(name)
    }
}

#[cfg(test)]
mod tests {
    use ::datafusion::{
        catalog::{CatalogProvider, SchemaProvider},
        catalog_common::MemoryCatalogProvider,
    };

    use super::*;
    use ::datafusion::prelude::*;
    use std::sync::Arc;

    fn create_test_introspection() -> Introspection {
        let table_metadata = TableMetadata::new(vec![
            TableMetadataRow::new(
                "public".to_string(),
                "users".to_string(),
                Some("Users table".to_string()),
            ),
            TableMetadataRow::new(
                "public".to_string(),
                "posts".to_string(),
                Some("Posts table".to_string()),
            ),
        ]);

        let column_metadata = ColumnMetadata::new(vec![
            ColumnMetadataRow::new(
                "public".to_string(),
                "users".to_string(),
                "id".to_string(),
                Some("User ID".to_string()),
            ),
            ColumnMetadataRow::new(
                "public".to_string(),
                "users".to_string(),
                "name".to_string(),
                Some("User name".to_string()),
            ),
            ColumnMetadataRow::new(
                "public".to_string(),
                "posts".to_string(),
                "id".to_string(),
                Some("Post ID".to_string()),
            ),
            ColumnMetadataRow::new(
                "public".to_string(),
                "posts".to_string(),
                "user_id".to_string(),
                Some("Author's user ID".to_string()),
            ),
        ]);

        let inferred_foreign_keys = InferredForeignKeys::new(vec![InferredForeignKeysRow::new(
            "public".to_string(),
            "posts".to_string(),
            "user_id".to_string(),
            "public".to_string(),
            "users".to_string(),
            "id".to_string(),
        )]);

        Introspection {
            table_metadata,
            column_metadata,
            inferred_foreign_key_constraints: inferred_foreign_keys,
        }
    }

    #[tokio::test]
    async fn test_introspection_schema_provider_table() {
        let introspection = create_test_introspection();
        let schema_provider = IntrospectionSchemaProvider::new(&introspection);

        let table_metadata = schema_provider.table(TABLE_METADATA).await.unwrap();
        assert!(table_metadata.is_some());

        let column_metadata = schema_provider.table(COLUMN_METADATA).await.unwrap();
        assert!(column_metadata.is_some());

        let foreign_keys = schema_provider
            .table(INFERRED_FOREIGN_KEY_CONSTRAINTS)
            .await
            .unwrap();
        assert!(foreign_keys.is_some());

        let non_existent_table = schema_provider.table("non_existent").await.unwrap();
        assert!(non_existent_table.is_none());
    }

    // ... (keep the create_test_introspection function and other existing tests)

    fn create_test_context(introspection: &Introspection) -> SessionContext {
        let config = SessionConfig::new().with_default_catalog_and_schema("default", "default");
        let schema_provider = Arc::new(IntrospectionSchemaProvider::new(introspection));
        let ctx = SessionContext::new_with_config(config);
        let catalog = MemoryCatalogProvider::new();
        catalog
            .register_schema(HASURA_METADATA_SCHEMA, schema_provider)
            .unwrap();
        ctx.register_catalog("default", Arc::new(catalog));
        ctx
    }

    #[tokio::test]
    async fn test_query_table_metadata() {
        let introspection = create_test_introspection();
        let ctx = create_test_context(&introspection);

        let sql = format!("SELECT * FROM {HASURA_METADATA_SCHEMA}.{TABLE_METADATA}");
        let df = ctx.sql(&sql).await.unwrap();
        let results = df.collect().await.unwrap();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].num_rows(), 2);
    }

    #[tokio::test]
    async fn test_query_column_metadata() {
        let introspection = create_test_introspection();
        let ctx = create_test_context(&introspection);

        let sql = format!(
            "SELECT * FROM {HASURA_METADATA_SCHEMA}.{COLUMN_METADATA} WHERE table_name = 'users'",
        );
        let df = ctx.sql(&sql).await.unwrap();
        let results = df.collect().await.unwrap();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].num_rows(), 2);
    }

    #[tokio::test]
    async fn test_query_inferred_foreign_keys() {
        let introspection = create_test_introspection();
        let ctx = create_test_context(&introspection);

        let sql =
            format!("SELECT * FROM {HASURA_METADATA_SCHEMA}.{INFERRED_FOREIGN_KEY_CONSTRAINTS}",);
        let df = ctx.sql(&sql).await.unwrap();
        let results = df.collect().await.unwrap();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].num_rows(), 1);
    }
}
