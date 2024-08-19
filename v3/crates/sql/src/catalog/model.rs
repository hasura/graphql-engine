//! Describe a model for a SQL table and how to translate datafusion operations on the table
//! to ndc-spec queries.
pub(crate) mod common;
pub(crate) mod filter;

use std::collections::BTreeMap;
use std::{any::Any, sync::Arc};

use ::datafusion::logical_expr::{Expr, TableProviderFilterPushDown};
use async_trait::async_trait;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved, Metadata, Qualified};
use open_dds::arguments::ArgumentName;
use open_dds::identifier::SubgraphName;
use open_dds::{models::ModelName, types::CustomTypeName};

use resolved::TypeMapping;
use serde::{Deserialize, Serialize};

use crate::catalog::model::filter::can_pushdown_filters;
use crate::execute::planner::model::ModelQuery;

mod datafusion {
    pub(super) use datafusion::{
        arrow::datatypes::{DataType, Field, Schema, SchemaBuilder, SchemaRef, TimeUnit},
        catalog::Session,
        common::{DFSchema, DFSchemaRef},
        datasource::{function::TableFunctionImpl, TableProvider, TableType},
        error::Result,
        logical_expr::{Expr, Extension, LogicalPlan},
        physical_plan::ExecutionPlan,
    };
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentInfo {
    pub argument_type: datafusion::DataType,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Model {
    pub subgraph: SubgraphName,
    pub name: ModelName,

    pub description: Option<String>,

    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,

    // Datafusion table schema
    pub schema: datafusion::SchemaRef,

    // For now, descriptions of fields
    pub columns: IndexMap<String, Option<String>>,

    // This is the entry point for the type mappings stored
    // in ModelSource
    pub data_type: Qualified<CustomTypeName>,
}

pub(super) fn object_type_table_schema(
    metadata: &resolved::Metadata,
    object_type: &Qualified<CustomTypeName>,
    // TODO: get rid of this option
    type_mappings: Option<&BTreeMap<Qualified<CustomTypeName>, TypeMapping>>,
) -> (
    // datafusion's table schema
    datafusion::SchemaRef,
    // description of the fields
    IndexMap<String, Option<String>>,
) {
    let object_type_fields = metadata
        .object_types
        .get(object_type)
        .map(|ty| &ty.object_type.fields);
    let mut columns = IndexMap::new();
    let mut builder = datafusion::SchemaBuilder::new();
    let type_mapping = type_mappings.and_then(|type_mappings| type_mappings.get(object_type));
    for (field_name, field_definition) in object_type_fields.into_iter().flatten() {
        let type_representation = type_mapping.and_then(|type_mapping| {
            let resolved::TypeMapping::Object { field_mappings, .. } = type_mapping;
            field_mappings
                .get(field_name)?
                .column_type_representation
                .as_ref()
        });
        let field_type = to_arrow_type(
            metadata,
            &field_definition.field_type.underlying_type,
            type_mappings,
            type_representation,
        );
        if let Some(field_type) = field_type {
            builder.push(datafusion::Field::new(
                field_name.to_string(),
                field_type,
                field_definition.field_type.nullable,
            ));

            let description = if let Some(ndc_models::TypeRepresentation::Enum { one_of }) =
                type_representation
            {
                // TODO: Instead of stuffing the possible enum values in description,
                // surface them in the metadata tables.
                Some(
                    field_definition
                        .description
                        .clone()
                        .unwrap_or_else(String::new)
                        + &format!(" Possible values: {}", one_of.join(", ")),
                )
            } else {
                field_definition.description.clone()
            };
            columns.insert(field_name.to_string(), description);
        }
    }
    let fields = builder.finish().fields;
    (
        datafusion::SchemaRef::new(datafusion::Schema::new(fields)),
        columns,
    )
}

impl Model {
    pub fn from_resolved_model(
        metadata: &resolved::Metadata,
        model: &resolved::ModelWithPermissions,
    ) -> Self {
        let (schema, columns) = {
            let type_mappings = model.model.source.as_ref().map(|o| &o.type_mappings);
            object_type_table_schema(metadata, &model.model.data_type, type_mappings)
        };

        Model {
            subgraph: model.model.name.subgraph.clone(),
            name: model.model.name.name.clone(),
            description: model.model.raw.description.clone(),
            arguments: IndexMap::new(),
            schema,
            data_type: model.model.data_type.clone(),
            columns,
        }
    }
}

pub(crate) struct WithSession<T> {
    pub(crate) session: Arc<Session>,
    pub(crate) value: Arc<T>,
}

// TODO: this will be removed when table valued functions are fully supported
#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct TableValuedFunction {
    pub(crate) metadata: Arc<Metadata>,
    /// Metadata about the model
    pub(crate) model: Arc<Model>,
    pub(crate) session: Arc<Session>,
}

impl TableValuedFunction {
    // TODO: this will be removed when table valued functions are fully supported
    #[allow(dead_code)]
    pub(crate) fn new(metadata: Arc<Metadata>, model: Arc<Model>, session: Arc<Session>) -> Self {
        TableValuedFunction {
            metadata,
            model,
            session,
        }
    }
}

impl datafusion::TableFunctionImpl for TableValuedFunction {
    fn call(
        &self,
        // TODO: table valued function implementation is not yet complete
        _exprs: &[datafusion::Expr],
    ) -> datafusion::Result<Arc<dyn datafusion::TableProvider>> {
        let arguments = BTreeMap::new();
        let table = Table::new(self.metadata.clone(), self.model.clone(), arguments);
        Ok(Arc::new(table) as Arc<dyn datafusion::TableProvider>)
    }
}

/// A Table represents an OpenDD entity which can provide a set of rows.
/// Currently, this is an instatation of a model with concrete arguments
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Table {
    pub(crate) metadata: Arc<Metadata>,
    /// Metadata about the model
    pub(crate) model: Arc<Model>,
    /// This will be empty if the model doesn't take any arguments
    pub(crate) arguments: BTreeMap<ArgumentName, serde_json::Value>,
}

impl Table {
    pub(crate) fn new(
        metadata: Arc<Metadata>,
        model: Arc<Model>,
        arguments: BTreeMap<ArgumentName, serde_json::Value>,
    ) -> Self {
        Table {
            metadata,
            model,
            arguments,
        }
    }
    pub(crate) fn new_no_args(metadata: Arc<Metadata>, model: Arc<Model>) -> Self {
        Table {
            metadata,
            model,
            arguments: BTreeMap::new(),
        }
    }
    pub(crate) fn to_logical_plan(
        &self,
        projected_schema: datafusion::DFSchemaRef,
        filters: &[datafusion::Expr],
        fetch: Option<usize>,
    ) -> datafusion::Result<datafusion::LogicalPlan> {
        let model_query_node = ModelQuery::new(
            &self.model,
            &self.arguments,
            projected_schema,
            filters,
            fetch,
        )?;
        let logical_plan = datafusion::LogicalPlan::Extension(datafusion::Extension {
            node: Arc::new(model_query_node),
        });
        Ok(logical_plan)
    }
}

#[async_trait]
impl datafusion::TableProvider for Table {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn schema(&self) -> datafusion::SchemaRef {
        self.model.schema.clone()
    }

    fn table_type(&self) -> datafusion::TableType {
        datafusion::TableType::Base
    }

    async fn scan(
        &self,
        state: &dyn datafusion::Session,
        projection: Option<&Vec<usize>>,
        // filters and limit can be used here to inject some push-down operations if needed
        filters: &[datafusion::Expr],
        limit: Option<usize>,
    ) -> datafusion::Result<Arc<dyn datafusion::ExecutionPlan>> {
        let projected_schema = self.model.schema.project(projection.unwrap_or(&vec![]))?;
        let qualified_projected_schema = datafusion::DFSchema::from_unqualified_fields(
            projected_schema.fields,
            projected_schema.metadata,
        )?;
        let logical_plan =
            self.to_logical_plan(Arc::new(qualified_projected_schema), filters, limit)?;
        state.create_physical_plan(&logical_plan).await
    }

    fn supports_filters_pushdown(
        &self,
        filters: &[&Expr],
    ) -> datafusion::Result<Vec<TableProviderFilterPushDown>> {
        Ok(can_pushdown_filters(&self.metadata, &self.model, filters))
    }
}

/// Converts an opendd type to an arrow type.
#[allow(clippy::match_same_arms)]
pub(super) fn to_arrow_type(
    metadata: &resolved::Metadata,
    ty: &resolved::QualifiedBaseType,
    type_mappings: Option<&BTreeMap<Qualified<CustomTypeName>, TypeMapping>>,
    type_representation: Option<&ndc_models::TypeRepresentation>,
) -> Option<datafusion::DataType> {
    match &ty {
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Inbuilt(inbuilt_type)) => {
            let data_type = match inbuilt_type {
                open_dds::types::InbuiltType::ID => datafusion::DataType::Utf8,
                open_dds::types::InbuiltType::Int => datafusion::DataType::Int32,
                open_dds::types::InbuiltType::Float => datafusion::DataType::Float32,
                open_dds::types::InbuiltType::Boolean => datafusion::DataType::Boolean,
                open_dds::types::InbuiltType::String => datafusion::DataType::Utf8,
            };
            Some(data_type)
        }
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Custom(custom_type)) => {
            from_named_type(metadata, custom_type, type_mappings, type_representation)
        }
        resolved::QualifiedBaseType::List(element_type) => {
            // TODO: Figure out the right type representation from the equivalent underlying NDC type,
            // instead of passing None
            let data_type =
                to_arrow_type(metadata, &element_type.underlying_type, type_mappings, None)?;
            Some(datafusion::DataType::List(Arc::new(
                datafusion::Field::new("element", data_type, element_type.nullable),
            )))
        }
    }
}

fn from_named_type(
    metadata: &resolved::Metadata,
    custom_type: &Qualified<CustomTypeName>,
    // the type mapping of the enclosing object type, if any
    // this is a hack to get the type representations of scalars, but those should exist
    // separately from the enclosing object type mappings
    type_mappings: Option<&BTreeMap<Qualified<CustomTypeName>, TypeMapping>>,
    type_representation: Option<&ndc_models::TypeRepresentation>,
) -> Option<datafusion::DataType> {
    if let Some(_scalar_type) = metadata.scalar_types.get(custom_type) {
        if let Some(type_representation) = type_representation {
            match type_representation {
                ndc_models::TypeRepresentation::Boolean => Some(datafusion::DataType::Boolean),
                ndc_models::TypeRepresentation::Int8 => Some(datafusion::DataType::Int8),
                ndc_models::TypeRepresentation::Int16 => Some(datafusion::DataType::Int16),
                ndc_models::TypeRepresentation::Int32 => Some(datafusion::DataType::Int32),
                ndc_models::TypeRepresentation::Int64 => Some(datafusion::DataType::Int64),
                ndc_models::TypeRepresentation::Float32 => Some(datafusion::DataType::Float32),
                ndc_models::TypeRepresentation::Float64 |
                // BigDecimal128 is not supported by arrow.
                ndc_models::TypeRepresentation::BigDecimal => Some(datafusion::DataType::Float64),
                ndc_models::TypeRepresentation::String |
                // Can't do anything better for BigInteger, so we just use String.
                ndc_models::TypeRepresentation::BigInteger |
                ndc_models::TypeRepresentation::UUID |
                ndc_models::TypeRepresentation::Enum { .. } => Some(datafusion::DataType::Utf8),
                ndc_models::TypeRepresentation::Date => Some(datafusion::DataType::Date32),
                ndc_models::TypeRepresentation::Timestamp |
                ndc_models::TypeRepresentation::TimestampTZ => Some(
                    datafusion::DataType::Timestamp(datafusion::TimeUnit::Microsecond, None),
                ),
                _ => None,
            }
        } else {
            match custom_type.name.to_string().to_lowercase().as_str() {
                "bool" => Some(datafusion::DataType::Boolean),
                "int8" => Some(datafusion::DataType::Int8),
                "int16" => Some(datafusion::DataType::Int16),
                "int32" => Some(datafusion::DataType::Int32),
                "int64" => Some(datafusion::DataType::Int64),
                "float32" => Some(datafusion::DataType::Float32),
                "float64" |
                // BigDecimal128 is not supported by arrow.
                "bigdecimal" => Some(datafusion::DataType::Float64),
                "varchar" |
                "text" => Some(datafusion::DataType::Utf8),
                "timestamp"
                |
                "timestamptz" => Some(datafusion::DataType::Timestamp(
                    datafusion::TimeUnit::Microsecond,
                    None,
                )),
                _ => None,
            }
        }
    } else if let Some(object_type) = metadata.object_types.get(custom_type) {
        let mut builder = datafusion::SchemaBuilder::new();
        let type_mapping: Option<_> =
            type_mappings.and_then(|type_mappings| type_mappings.get(custom_type));
        for (field_name, field) in &object_type.object_type.fields {
            let type_representation = type_mapping.and_then(|type_mapping| {
                let resolved::TypeMapping::Object { field_mappings, .. } = type_mapping;
                field_mappings
                    .get(field_name)?
                    .column_type_representation
                    .as_ref()
            });

            let data_type = to_arrow_type(
                metadata,
                &field.field_type.underlying_type,
                type_mappings,
                type_representation,
            )?;

            builder.push(datafusion::Field::new(
                field_name.to_string(),
                data_type,
                field.field_type.nullable,
            ));
        }
        Some(datafusion::DataType::Struct(builder.finish().fields))
    } else {
        None
    }
}
