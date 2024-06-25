//! Describe a model for a SQL table and how to translate datafusion operations on the table
//! to ndc-spec queries.

use std::collections::HashMap;
use std::{any::Any, sync::Arc};

use async_trait::async_trait;
use datafusion::common::internal_err;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved, Qualified, SelectPermission};
use open_dds::permissions::Role;
use open_dds::{
    models::ModelName,
    types::{CustomTypeName, FieldName},
};

use serde::{Deserialize, Serialize};
mod df {
    pub(super) use datafusion::arrow::datatypes::Field;
    pub(super) use datafusion::{
        arrow::datatypes::{DataType, Schema, SchemaBuilder, SchemaRef},
        datasource::{TableProvider, TableType},
        execution::context::SessionState,
        logical_expr::Expr,
        physical_plan::ExecutionPlan,
    };
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub(crate) struct TypePermission {
    pub output: open_dds::permissions::TypeOutputPermission,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TypePermissionsOfRole {
    pub(crate) permissions: HashMap<Qualified<CustomTypeName>, TypePermission>,
}

fn get_type_representation<'a>(
    model: &'a resolved::Model,
    field: &FieldName,
) -> Option<&'a ndc_models::TypeRepresentation> {
    model
        .source
        .as_ref()
        .and_then(|source| {
            source
                .type_mappings
                .get(&model.data_type)
                .map(|type_mapping| {
                    let resolved::TypeMapping::Object { field_mappings, .. } = type_mapping;
                    field_mappings
                        .get(field)
                        .map(|mapping| mapping.column_type_representation.as_ref())
                })
        })
        .flatten()
        .flatten()
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Model {
    pub name: ModelName,

    pub description: Option<String>,

    // Datafusion table schema
    pub schema: df::SchemaRef,

    // For now, descriptions of fields
    pub columns: IndexMap<String, Option<String>>,

    // This is the entry point for the type mappings stored
    // in ModelSource
    pub data_type: Qualified<CustomTypeName>,

    // The underlying source to execute ndc queries
    pub source: Option<Arc<metadata_resolve::ModelSource>>,

    // Permisisons for the model. Note that the type permissions will need to be retrieved from the
    // global context
    pub permissions: HashMap<Role, Arc<resolved::SelectPermission>>,
}

impl Model {
    pub fn from_resolved_model(model: &resolved::ModelWithPermissions) -> Self {
        let (schema, columns) = {
            let mut columns = IndexMap::new();
            let mut builder = df::SchemaBuilder::new();
            for (field_name, field_definition) in &model.model.type_fields {
                let ndc_type_representation = get_type_representation(&model.model, field_name);
                let field_type =
                    to_arrow_type(&field_definition.field_type, ndc_type_representation);
                if let Some(field_type) = field_type {
                    builder.push(df::Field::new(
                        field_name.to_string(),
                        field_type,
                        field_definition.field_type.nullable,
                    ));
                    let description = if let Some(ndc_models::TypeRepresentation::Enum { one_of }) =
                        ndc_type_representation
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
            (df::SchemaRef::new(df::Schema::new(fields)), columns)
        };

        let permissions = model
            .select_permissions
            .iter()
            .map(|(role, select_permission)| (role.clone(), Arc::new(select_permission.clone())))
            .collect();

        Model {
            name: model.model.name.name.clone(),
            description: model.model.raw.description.clone(),
            schema,
            data_type: model.model.data_type.clone(),
            source: model
                .model
                .source
                .as_ref()
                .map(|source| Arc::new(source.clone())),
            columns,
            permissions,
        }
    }
}

/// Converts an opendd type to an arrow type.
/// TODO: need to handle complex types
#[allow(clippy::match_same_arms)]
fn to_arrow_type(
    ty: &resolved::QualifiedTypeReference,
    ndc_type_representation: Option<&ndc_models::TypeRepresentation>,
) -> Option<df::DataType> {
    match &ty.underlying_type {
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Inbuilt(inbuilt_type)) => {
            let data_type = match inbuilt_type {
                open_dds::types::InbuiltType::ID => df::DataType::Utf8,
                open_dds::types::InbuiltType::Int => df::DataType::Int32,
                open_dds::types::InbuiltType::Float => df::DataType::Float32,
                open_dds::types::InbuiltType::Boolean => df::DataType::Boolean,
                open_dds::types::InbuiltType::String => df::DataType::Utf8,
            };
            Some(data_type)
        }
        resolved::QualifiedBaseType::Named(resolved::QualifiedTypeName::Custom(custom_type)) => {
            if let Some(type_representation) = ndc_type_representation {
                match type_representation {
                    ndc_models::TypeRepresentation::Boolean => Some(df::DataType::Boolean),
                    ndc_models::TypeRepresentation::String => Some(df::DataType::Utf8),
                    ndc_models::TypeRepresentation::Int8 => Some(df::DataType::Int8),
                    ndc_models::TypeRepresentation::Int16 => Some(df::DataType::Int16),
                    ndc_models::TypeRepresentation::Int32 => Some(df::DataType::Int32),
                    ndc_models::TypeRepresentation::Int64 => Some(df::DataType::Int64),
                    ndc_models::TypeRepresentation::Float32 => Some(df::DataType::Float32),
                    ndc_models::TypeRepresentation::Float64 => Some(df::DataType::Float64),
                    // Can't do anything better for BigInteger, so we just use String.
                    ndc_models::TypeRepresentation::BigInteger => Some(df::DataType::Utf8),
                    // BigDecimal128 is not supported by arrow.
                    ndc_models::TypeRepresentation::BigDecimal => Some(df::DataType::Float64),
                    ndc_models::TypeRepresentation::UUID => Some(df::DataType::Utf8),
                    ndc_models::TypeRepresentation::Date => Some(df::DataType::Date32),
                    ndc_models::TypeRepresentation::Timestamp => Some(df::DataType::Timestamp(
                        datafusion::arrow::datatypes::TimeUnit::Microsecond,
                        None,
                    )),
                    ndc_models::TypeRepresentation::TimestampTZ => Some(df::DataType::Timestamp(
                        datafusion::arrow::datatypes::TimeUnit::Microsecond,
                        None,
                    )),
                    ndc_models::TypeRepresentation::Enum { .. } => Some(df::DataType::Utf8),
                    _ => None,
                }
            } else {
                match custom_type.name.to_string().to_lowercase().as_str() {
                    "bool" => Some(df::DataType::Boolean),
                    "int8" => Some(df::DataType::Int8),
                    "int16" => Some(df::DataType::Int16),
                    "int32" => Some(df::DataType::Int32),
                    "int64" => Some(df::DataType::Int64),
                    "float32" => Some(df::DataType::Float32),
                    "float64" => Some(df::DataType::Float64),
                    "varchar" => Some(df::DataType::Utf8),
                    "text" => Some(df::DataType::Utf8),
                    "timestamp" => Some(df::DataType::Timestamp(
                        datafusion::arrow::datatypes::TimeUnit::Microsecond,
                        None,
                    )),
                    "timestamptz" => Some(df::DataType::Timestamp(
                        datafusion::arrow::datatypes::TimeUnit::Microsecond,
                        None,
                    )),
                    // BigDecimal128 is not supported by arrow.
                    "bigdecimal" => Some(df::DataType::Float64),
                    _ => None,
                }
            }
        }
        resolved::QualifiedBaseType::List(_) => None,
    }
}

#[derive(Debug, Clone)]
pub(crate) struct OpenDDTableProvider {
    pub(crate) session: Arc<Session>,
    pub(crate) http_context: Arc<execute::HttpContext>,
    pub(crate) name: ModelName,
    pub(crate) data_type: Qualified<CustomTypeName>,
    pub(crate) source: Option<Arc<metadata_resolve::ModelSource>>,
    pub(crate) schema: df::SchemaRef,
    pub(crate) select_permission: Option<Arc<SelectPermission>>,
    pub(crate) type_permissions: Option<Arc<TypePermissionsOfRole>>,
}

#[async_trait]
impl df::TableProvider for OpenDDTableProvider {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn schema(&self) -> df::SchemaRef {
        self.schema.clone()
    }

    fn table_type(&self) -> df::TableType {
        df::TableType::Base
    }

    async fn scan(
        &self,
        _state: &df::SessionState,
        _projection: Option<&Vec<usize>>,
        // filters and limit can be used here to inject some push-down operations if needed
        _filters: &[df::Expr],
        _limit: Option<usize>,
    ) -> datafusion::error::Result<Arc<dyn df::ExecutionPlan>> {
        internal_err!("scan shouldn't be called")
    }
}
