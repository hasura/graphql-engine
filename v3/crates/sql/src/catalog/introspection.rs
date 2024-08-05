//! Describe and populate the introspection tables used by data fusion.

use std::{any::Any, sync::Arc};

use async_trait::async_trait;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved, ModelRelationshipTarget};
mod datafusion {
    pub(super) use datafusion::{
        arrow::{
            array::RecordBatch,
            datatypes::{DataType, Field, Schema, SchemaRef},
        },
        catalog::schema::SchemaProvider,
        common::ScalarValue,
        datasource::{TableProvider, TableType},
        error::Result,
        execution::context::SessionState,
        logical_expr::Expr,
        physical_plan::{values::ValuesExec, ExecutionPlan},
    };
}
use open_dds::relationships::RelationshipType;
use serde::{Deserialize, Serialize};

pub const HASURA_METADATA_SCHEMA: &str = "hasura";
pub const TABLE_METADATA: &str = "table_metadata";
pub const COLUMN_METADATA: &str = "column_metadata";
pub const INFERRED_FOREIGN_KEY_CONSTRAINTS: &str = "inferred_foreign_key_constraints";

/// Describes the database schema structure and metadata.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Introspection {
    pub(crate) table_metadata: TableMetadata,
    pub(crate) column_metadata: ColumnMetadata,
    pub(crate) inferred_foreign_key_constraints: InferredatafusionoreignKeys,
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
                table_metadata_rows.push(TableRow::new(
                    schema_name.to_string(),
                    table_name.to_string(),
                    table.description.clone(),
                ));
                for (column_name, column_description) in &table.columns {
                    column_metadata_rows.push(ColumnRow {
                        schema_name: schema_name.to_string(),
                        table_name: table_name.clone(),
                        column_name: column_name.clone(),
                        description: column_description.clone(),
                    });
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
                                foreign_key_constraint_rows.push(ForeignKeyRow {
                                    from_schema_name: schema_name.to_string(),
                                    from_table_name: table_name.clone(),
                                    from_column_name: mapping.source_field.field_name.to_string(),
                                    to_schema_name: model_name.subgraph.to_string(),
                                    to_table_name: model_name.name.to_string(),
                                    to_column_name: mapping.target_field.field_name.to_string(),
                                });
                            }
                        }
                    }
                }
            }
        }
        Introspection {
            table_metadata: TableMetadata::new(table_metadata_rows),
            column_metadata: ColumnMetadata::new(column_metadata_rows),
            inferred_foreign_key_constraints: InferredatafusionoreignKeys::new(
                foreign_key_constraint_rows,
            ),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TableMetadata {
    schema: datafusion::SchemaRef,
    rows: Vec<TableRow>,
}

impl TableMetadata {
    pub(crate) fn new(rows: Vec<TableRow>) -> Self {
        let schema_name = datafusion::Field::new("schema_name", datafusion::DataType::Utf8, false);
        let table_name = datafusion::Field::new("table_name", datafusion::DataType::Utf8, false);
        let description = datafusion::Field::new("description", datafusion::DataType::Utf8, true);
        let schema = datafusion::SchemaRef::new(datafusion::Schema::new(vec![
            schema_name,
            table_name,
            description,
        ]));
        TableMetadata { schema, rows }
    }
}

impl TableMetadata {
    fn to_values_table(&self) -> ValuesTable {
        ValuesTable {
            schema: self.schema.clone(),
            rows: self
                .rows
                .iter()
                .map(|row| {
                    vec![
                        ScalarValue::Utf8(Some(row.schema_name.clone())),
                        ScalarValue::Utf8(Some(row.table_name.clone())),
                        ScalarValue::Utf8(row.description.clone()),
                    ]
                })
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TableRow {
    schema_name: String,
    table_name: String,
    description: Option<String>,
}

impl TableRow {
    pub(crate) fn new(
        schema_name: String,
        table_name: String,
        description: Option<String>,
    ) -> Self {
        Self {
            schema_name,
            table_name,
            description,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct ColumnMetadata {
    pub(crate) schema: datafusion::SchemaRef,
    pub(crate) rows: Vec<ColumnRow>,
}

impl ColumnMetadata {
    fn new(rows: Vec<ColumnRow>) -> Self {
        let schema_name = datafusion::Field::new("schema_name", datafusion::DataType::Utf8, false);
        let table_name = datafusion::Field::new("table_name", datafusion::DataType::Utf8, false);
        let column_name = datafusion::Field::new("column_name", datafusion::DataType::Utf8, false);
        let description = datafusion::Field::new("description", datafusion::DataType::Utf8, true);
        let schema = datafusion::SchemaRef::new(datafusion::Schema::new(vec![
            schema_name,
            table_name,
            column_name,
            description,
        ]));
        ColumnMetadata { schema, rows }
    }
    fn to_values_table(&self) -> ValuesTable {
        ValuesTable {
            schema: self.schema.clone(),
            rows: self
                .rows
                .iter()
                .map(|row| {
                    vec![
                        ScalarValue::Utf8(Some(row.schema_name.clone())),
                        ScalarValue::Utf8(Some(row.table_name.clone())),
                        ScalarValue::Utf8(Some(row.column_name.clone())),
                        ScalarValue::Utf8(row.description.clone()),
                    ]
                })
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct ColumnRow {
    schema_name: String,
    table_name: String,
    column_name: String,
    description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct InferredatafusionoreignKeys {
    schema: datafusion::SchemaRef,
    rows: Vec<ForeignKeyRow>,
}

impl InferredatafusionoreignKeys {
    fn new(rows: Vec<ForeignKeyRow>) -> Self {
        let from_schema_name =
            datafusion::Field::new("from_schema_name", datafusion::DataType::Utf8, false);
        let from_table_name =
            datafusion::Field::new("from_table_name", datafusion::DataType::Utf8, false);
        let from_column_name =
            datafusion::Field::new("from_column_name", datafusion::DataType::Utf8, false);
        let to_schema_name =
            datafusion::Field::new("to_schema_name", datafusion::DataType::Utf8, false);
        let to_table_name =
            datafusion::Field::new("to_table_name", datafusion::DataType::Utf8, false);
        let to_column_name =
            datafusion::Field::new("to_column_name", datafusion::DataType::Utf8, false);
        let schema = datafusion::SchemaRef::new(datafusion::Schema::new(vec![
            from_schema_name,
            from_table_name,
            from_column_name,
            to_schema_name,
            to_table_name,
            to_column_name,
        ]));
        InferredatafusionoreignKeys { schema, rows }
    }
    fn to_values_table(&self) -> ValuesTable {
        ValuesTable {
            schema: self.schema.clone(),
            rows: self
                .rows
                .iter()
                .map(|row| {
                    vec![
                        ScalarValue::Utf8(Some(row.from_schema_name.clone())),
                        ScalarValue::Utf8(Some(row.from_table_name.clone())),
                        ScalarValue::Utf8(Some(row.from_column_name.clone())),
                        ScalarValue::Utf8(Some(row.to_schema_name.clone())),
                        ScalarValue::Utf8(Some(row.to_table_name.clone())),
                        ScalarValue::Utf8(Some(row.to_column_name.clone())),
                    ]
                })
                .collect(),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
enum ScalarValue {
    Utf8(Option<String>),
}

impl ScalarValue {
    fn into_datafusion_scalar_value(self) -> datafusion::ScalarValue {
        match self {
            ScalarValue::Utf8(value) => datafusion::ScalarValue::Utf8(value),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
struct ForeignKeyRow {
    from_schema_name: String,
    from_table_name: String,
    from_column_name: String,
    to_schema_name: String,
    to_table_name: String,
    to_column_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct IntrospectionSchemaProvider {
    tables: IndexMap<String, Arc<ValuesTable>>,
}

impl IntrospectionSchemaProvider {
    pub(crate) fn new(introspection: &Introspection) -> Self {
        let tables = [
            (
                TABLE_METADATA,
                introspection.table_metadata.to_values_table(),
            ),
            (
                COLUMN_METADATA,
                introspection.column_metadata.to_values_table(),
            ),
            (
                INFERRED_FOREIGN_KEY_CONSTRAINTS,
                introspection
                    .inferred_foreign_key_constraints
                    .to_values_table(),
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

// A table with static rows
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
struct ValuesTable {
    schema: datafusion::SchemaRef,
    rows: Vec<Vec<ScalarValue>>,
}

#[async_trait]
impl datafusion::TableProvider for ValuesTable {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn schema(&self) -> datafusion::SchemaRef {
        self.schema.clone()
    }

    fn table_type(&self) -> datafusion::TableType {
        datafusion::TableType::View
    }
    async fn scan(
        &self,
        _state: &datafusion::SessionState,
        projection: Option<&Vec<usize>>,
        // filters and limit can be used here to inject some push-down operations if needed
        _filters: &[datafusion::Expr],
        _limit: Option<usize>,
    ) -> datafusion::Result<Arc<dyn datafusion::ExecutionPlan>> {
        let projected_schema = Arc::new(self.schema.project(projection.unwrap_or(&vec![]))?);
        let columnar_projection = projection
            .unwrap_or(&vec![])
            .iter()
            .map(|j| {
                self.rows
                    .iter()
                    .map(|row| row[*j].clone().into_datafusion_scalar_value())
            })
            .map(datafusion::ScalarValue::iter_to_array)
            .collect::<datafusion::Result<Vec<_>>>()?;
        Ok(Arc::new(datafusion::ValuesExec::try_new_from_batches(
            projected_schema.clone(),
            vec![datafusion::RecordBatch::try_new(
                projected_schema,
                columnar_projection,
            )?],
        )?))
    }
}
