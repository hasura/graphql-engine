//! Describe and populate the introspection tables used by data fusion.

use std::{any::Any, sync::Arc};

use async_trait::async_trait;
use foreign_keys::{InferredForeignKeys, InferredForeignKeysRow, INFERRED_FOREIGN_KEY_CONSTRAINTS};
use indexmap::IndexMap;
use metadata_resolve::{self as resolved, ModelRelationshipTarget, Qualified};
use struct_type::{
    StructTypeFieldRow, StructTypeFields, StructTypeRow, StructTypes, STRUCT_TYPE,
    STRUCT_TYPE_FIELD,
};
use table_metadata::{TableMetadata, TableMetadataRow, TABLE_METADATA};
use table_valued_function::*;
mod datafusion {
    pub(super) use datafusion::{
        catalog::SchemaProvider, datasource::TableProvider, error::Result,
    };
}
use open_dds::{commands::CommandName, models::ModelName, relationships::RelationshipType};
use serde::{Deserialize, Serialize};
use unsupported_objects::{
    UnsupportedCommands, UnsupportedCommandsRow, UnsupportedModels, UnsupportedModelsRow,
    UnsupportedObjectTypeFields, UnsupportedObjectTypeFieldsRow, UnsupportedObjectTypes,
    UnsupportedObjectTypesRow, UnsupportedScalars, UnsupportedScalarsRow, UNSUPPORTED_COMMANDS,
    UNSUPPORTED_MODELS, UNSUPPORTED_OBJECT_TYPES, UNSUPPORTED_OBJECT_TYPE_FIELDS,
    UNSUPPORTED_SCALARS,
};

use super::{
    command::UnsupportedCommand, mem_table::MemTable, model::UnsupportedModel, types::TypeRegistry,
};

mod foreign_keys;
mod struct_type;
mod table_metadata;
mod table_valued_function;
mod unsupported_objects;

pub const HASURA_METADATA_SCHEMA: &str = "hasura";

/// Describes the database schema structure and metadata.
#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct Introspection {
    struct_types: StructTypes,
    struct_type_fields: StructTypeFields,
    table_metadata: TableMetadata,
    inferred_foreign_key_constraints: InferredForeignKeys,
    functions: TableValuedFunction,
    function_arguments: TableValuedFunctionArgument,
    unsupported_models: UnsupportedModels,
    unsupported_commands: UnsupportedCommands,
    unsupported_scalars: UnsupportedScalars,
    unsupported_object_types: UnsupportedObjectTypes,
    unsupported_object_type_fields: UnsupportedObjectTypeFields,
}

impl Introspection {
    /// Derive SQL schema from the Open DDS metadata.
    pub fn from_metadata(
        metadata: &resolved::Metadata,
        type_registry: &TypeRegistry,
        schemas: &IndexMap<String, crate::catalog::subgraph::Subgraph>,
        functions: &IndexMap<String, Arc<super::command::Command>>,
        unsupported_models: &IndexMap<Qualified<ModelName>, UnsupportedModel>,
        unsupported_commands: &IndexMap<Qualified<CommandName>, UnsupportedCommand>,
    ) -> Self {
        // unsupported scalar types
        let mut unsupported_scalar_types = Vec::new();
        for (name, scalar_type) in type_registry.custom_scalars() {
            if let Err(unsupported) = scalar_type {
                unsupported_scalar_types.push(UnsupportedScalarsRow::new(
                    name.subgraph.to_string(),
                    name.name.to_string(),
                    unsupported.to_string(),
                ));
            }
        }

        let mut struct_types = Vec::new();
        let mut struct_type_fields = Vec::new();
        let mut unsupported_object_types = Vec::new();
        let mut unsupported_object_type_fields = Vec::new();
        for (name, struct_type) in type_registry.struct_types() {
            match struct_type {
                Ok(struct_type) => {
                    struct_types.push(StructTypeRow::new(
                        struct_type.name().clone(),
                        struct_type.description().cloned(),
                    ));
                    for (field_name, unsupported_field) in &struct_type.unsupported_fields {
                        unsupported_object_type_fields.push(UnsupportedObjectTypeFieldsRow::new(
                            name.subgraph.to_string(),
                            name.name.to_string(),
                            field_name.to_string(),
                            unsupported_field.reason.to_string(),
                        ));
                    }
                    for (field_name, field) in struct_type.fields() {
                        struct_type_fields.push(StructTypeFieldRow::new(
                            struct_type.name().clone(),
                            field_name.to_string(),
                            field.data_type.clone(),
                            field.normalized_type.clone(),
                            field.is_nullable,
                            field.description.clone(),
                        ));
                    }
                }
                Err(unsupported_object) => {
                    unsupported_object_types.push(UnsupportedObjectTypesRow::new(
                        name.subgraph.to_string(),
                        name.name.to_string(),
                        unsupported_object.to_string(),
                    ));
                }
            }
        }

        let mut table_metadata_rows = Vec::new();
        let mut foreign_key_constraint_rows = Vec::new();
        for (schema_name, schema) in schemas {
            for (table_name, table) in &schema.tables {
                table_metadata_rows.push(TableMetadataRow::new(
                    schema_name.to_string(),
                    table_name.to_string(),
                    table.struct_type.clone(),
                    table.description.clone(),
                ));

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
        let mut function_rows = Vec::new();
        let mut function_argument_rows = Vec::new();
        for (function_name, function) in functions {
            function_rows.push(TableValuedFunctionRow::new(
                function_name.clone(),
                function.struct_type.clone(),
                function.description.clone(),
            ));

            #[allow(clippy::cast_possible_wrap)]
            for (position, (argument_name, argument)) in function.arguments.iter().enumerate() {
                function_argument_rows.push(TableValuedFunctionArgumentRow::new(
                    function_name.clone(),
                    argument_name.to_string(),
                    position as i64,
                    argument.argument_type.clone(),
                    argument.argument_type_normalized.clone(),
                    argument.is_nullable,
                    argument.description.clone(),
                ));
            }
        }

        // unsupported models
        let mut unsupported_model_rows = Vec::new();
        for (name, unsupported) in unsupported_models {
            unsupported_model_rows.push(UnsupportedModelsRow::new(
                name.subgraph.to_string(),
                name.name.to_string(),
                unsupported.to_string(),
            ));
        }

        // unsupported commands
        let mut unsupported_command_rows = Vec::new();
        for (name, unsupported) in unsupported_commands {
            unsupported_command_rows.push(UnsupportedCommandsRow::new(
                name.subgraph.to_string(),
                name.name.to_string(),
                unsupported.to_string(),
            ));
        }
        Introspection {
            table_metadata: TableMetadata::new(table_metadata_rows),
            inferred_foreign_key_constraints: InferredForeignKeys::new(foreign_key_constraint_rows),
            functions: TableValuedFunction::new(function_rows),
            function_arguments: TableValuedFunctionArgument::new(function_argument_rows),
            struct_types: StructTypes::new(struct_types),
            struct_type_fields: StructTypeFields::new(struct_type_fields),
            unsupported_models: UnsupportedModels::new(unsupported_model_rows),
            unsupported_commands: UnsupportedCommands::new(unsupported_command_rows),
            unsupported_scalars: UnsupportedScalars::new(unsupported_scalar_types),
            unsupported_object_types: UnsupportedObjectTypes::new(unsupported_object_types),
            unsupported_object_type_fields: UnsupportedObjectTypeFields::new(
                unsupported_object_type_fields,
            ),
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
                INFERRED_FOREIGN_KEY_CONSTRAINTS,
                introspection
                    .inferred_foreign_key_constraints
                    .to_table_provider(),
            ),
            (
                TABLE_VALUED_FUNCTION,
                introspection.functions.to_table_provider(),
            ),
            (
                TABLE_VALUED_FUNCTION_ARGUMENT,
                introspection.function_arguments.to_table_provider(),
            ),
            (STRUCT_TYPE, introspection.struct_types.to_table_provider()),
            (
                STRUCT_TYPE_FIELD,
                introspection.struct_type_fields.to_table_provider(),
            ),
            (
                UNSUPPORTED_MODELS,
                introspection.unsupported_models.to_table_provider(),
            ),
            (
                UNSUPPORTED_COMMANDS,
                introspection.unsupported_commands.to_table_provider(),
            ),
            (
                UNSUPPORTED_SCALARS,
                introspection.unsupported_scalars.to_table_provider(),
            ),
            (
                UNSUPPORTED_OBJECT_TYPES,
                introspection.unsupported_object_types.to_table_provider(),
            ),
            (
                UNSUPPORTED_OBJECT_TYPE_FIELDS,
                introspection
                    .unsupported_object_type_fields
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

// #[cfg(test)]
// mod tests {
//     use ::datafusion::{
//         catalog::{CatalogProvider, SchemaProvider},
//         catalog_common::MemoryCatalogProvider,
//     };

//     use super::*;
//     use ::datafusion::prelude::*;
//     use std::sync::Arc;

//     fn create_test_introspection() -> Introspection {
//         let table_metadata = TableMetadata::new(vec![
//             TableMetadataRow::new(
//                 "public".to_string(),
//                 "users".to_string(),
//                 Some("Users table".to_string()),
//             ),
//             TableMetadataRow::new(
//                 "public".to_string(),
//                 "posts".to_string(),
//                 Some("Posts table".to_string()),
//             ),
//         ]);

//         let column_metadata = ColumnMetadata::new(vec![
//             ColumnMetadataRow::new(
//                 "public".to_string(),
//                 "users".to_string(),
//                 "id".to_string(),
//                 Some("User ID".to_string()),
//             ),
//             ColumnMetadataRow::new(
//                 "public".to_string(),
//                 "users".to_string(),
//                 "name".to_string(),
//                 Some("User name".to_string()),
//             ),
//             ColumnMetadataRow::new(
//                 "public".to_string(),
//                 "posts".to_string(),
//                 "id".to_string(),
//                 Some("Post ID".to_string()),
//             ),
//             ColumnMetadataRow::new(
//                 "public".to_string(),
//                 "posts".to_string(),
//                 "user_id".to_string(),
//                 Some("Author's user ID".to_string()),
//             ),
//         ]);

//         let inferred_foreign_keys = InferredForeignKeys::new(vec![InferredForeignKeysRow::new(
//             "public".to_string(),
//             "posts".to_string(),
//             "user_id".to_string(),
//             "public".to_string(),
//             "users".to_string(),
//             "id".to_string(),
//         )]);
//         let functions = TableValuedFunction::new(vec![TableValuedFunctionRow::new(
//             "get_user_posts".to_string(),
//             Some("Get posts for a specific user".to_string()),
//         )]);

//         let function_fields = TableValuedFunctionField::new(vec![
//             TableValuedFunctionFieldRow::new(
//                 "get_user_posts".to_string(),
//                 "post_id".to_string(),
//                 &::datafusion::arrow::datatypes::DataType::Int32,
//                 false,
//                 Some("Post ID".to_string()),
//             ),
//             TableValuedFunctionFieldRow::new(
//                 "get_user_posts".to_string(),
//                 "title".to_string(),
//                 &::datafusion::arrow::datatypes::DataType::Utf8,
//                 false,
//                 Some("Post title".to_string()),
//             ),
//         ]);

//         let function_arguments =
//             TableValuedFunctionArgument::new(vec![TableValuedFunctionArgumentRow::new(
//                 "get_user_posts".to_string(),
//                 "user_id".to_string(),
//                 0,
//                 &::datafusion::arrow::datatypes::DataType::Int32,
//                 false,
//                 Some("User ID".to_string()),
//             )]);
//         Introspection {
//             table_metadata,
//             column_metadata,
//             inferred_foreign_key_constraints: inferred_foreign_keys,
//             functions,
//             function_fields,
//             function_arguments,
//         }
//     }

//     #[tokio::test]
//     async fn test_introspection_schema_provider_table() {
//         let introspection = create_test_introspection();
//         let schema_provider = IntrospectionSchemaProvider::new(&introspection);

//         let table_metadata = schema_provider.table(TABLE_METADATA).await.unwrap();
//         assert!(table_metadata.is_some());

//         let column_metadata = schema_provider.table(COLUMN_METADATA).await.unwrap();
//         assert!(column_metadata.is_some());

//         let foreign_keys = schema_provider
//             .table(INFERRED_FOREIGN_KEY_CONSTRAINTS)
//             .await
//             .unwrap();
//         assert!(foreign_keys.is_some());

//         let non_existent_table = schema_provider.table("non_existent").await.unwrap();
//         assert!(non_existent_table.is_none());
//     }

//     // ... (keep the create_test_introspection function and other existing tests)

//     fn create_test_context(introspection: &Introspection) -> SessionContext {
//         let config = SessionConfig::new().with_default_catalog_and_schema("default", "default");
//         let schema_provider = Arc::new(IntrospectionSchemaProvider::new(introspection));
//         let ctx = SessionContext::new_with_config(config);
//         let catalog = MemoryCatalogProvider::new();
//         catalog
//             .register_schema(HASURA_METADATA_SCHEMA, schema_provider)
//             .unwrap();
//         ctx.register_catalog("default", Arc::new(catalog));
//         ctx
//     }

//     #[tokio::test]
//     async fn test_query_table_metadata() {
//         let introspection = create_test_introspection();
//         let ctx = create_test_context(&introspection);

//         let sql = format!("SELECT * FROM {HASURA_METADATA_SCHEMA}.{TABLE_METADATA}");
//         let df = ctx.sql(&sql).await.unwrap();
//         let results = df.collect().await.unwrap();

//         assert_eq!(results.len(), 1);
//         assert_eq!(results[0].num_rows(), 2);
//     }

//     #[tokio::test]
//     async fn test_query_column_metadata() {
//         let introspection = create_test_introspection();
//         let ctx = create_test_context(&introspection);

//         let sql = format!(
//             "SELECT * FROM {HASURA_METADATA_SCHEMA}.{COLUMN_METADATA} WHERE table_name = 'users'",
//         );
//         let df = ctx.sql(&sql).await.unwrap();
//         let results = df.collect().await.unwrap();

//         assert_eq!(results.len(), 1);
//         assert_eq!(results[0].num_rows(), 2);
//     }

//     #[tokio::test]
//     async fn test_query_inferred_foreign_keys() {
//         let introspection = create_test_introspection();
//         let ctx = create_test_context(&introspection);

//         let sql =
//             format!("SELECT * FROM {HASURA_METADATA_SCHEMA}.{INFERRED_FOREIGN_KEY_CONSTRAINTS}",);
//         let df = ctx.sql(&sql).await.unwrap();
//         let results = df.collect().await.unwrap();

//         assert_eq!(results.len(), 1);
//         assert_eq!(results[0].num_rows(), 1);
//     }

//     #[tokio::test]
//     async fn test_query_table_valued_function() {
//         let introspection = create_test_introspection();
//         let ctx = create_test_context(&introspection);

//         let sql = format!("SELECT * FROM {HASURA_METADATA_SCHEMA}.{TABLE_VALUED_FUNCTION}");
//         let df = ctx.sql(&sql).await.unwrap();
//         let results = df.collect().await.unwrap();

//         assert_eq!(results.len(), 1);
//         assert_eq!(results[0].num_rows(), 1);
//     }

//     #[tokio::test]
//     async fn test_query_table_valued_function_field() {
//         let introspection = create_test_introspection();
//         let ctx = create_test_context(&introspection);

//         let sql = format!("SELECT * FROM {HASURA_METADATA_SCHEMA}.{TABLE_VALUED_FUNCTION_FIELD}");
//         let df = ctx.sql(&sql).await.unwrap();
//         let results = df.collect().await.unwrap();

//         assert_eq!(results.len(), 1);
//         assert_eq!(results[0].num_rows(), 2);
//     }

//     #[tokio::test]
//     async fn test_query_table_valued_function_argument() {
//         let introspection = create_test_introspection();
//         let ctx = create_test_context(&introspection);

//         let sql =
//             format!("SELECT * FROM {HASURA_METADATA_SCHEMA}.{TABLE_VALUED_FUNCTION_ARGUMENT}");
//         let df = ctx.sql(&sql).await.unwrap();
//         let results = df.collect().await.unwrap();

//         assert_eq!(results.len(), 1);
//         assert_eq!(results[0].num_rows(), 1);
//     }
// }
