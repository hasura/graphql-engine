//! Describe a model for a SQL table and how to translate datafusion operations on the table
//! to ndc-spec queries.

use std::collections::{BTreeMap, HashMap};
use std::{any::Any, sync::Arc};

use async_trait::async_trait;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved, Qualified};
use open_dds::arguments::ArgumentName;
use open_dds::permissions::Role;
use open_dds::{
    models::ModelName,
    types::{CustomTypeName, FieldName},
};

use serde::{Deserialize, Serialize};

use crate::plan::NDCQuery;
mod datafusion {
    pub(super) use datafusion::{
        arrow::datatypes::{DataType, Field, Schema, SchemaBuilder, SchemaRef, TimeUnit},
        common::{DFSchema, DFSchemaRef},
        datasource::function::TableFunctionImpl,
        datasource::{TableProvider, TableType},
        error::Result,
        execution::context::SessionState,
        logical_expr::Expr,
        logical_expr::Extension,
        logical_expr::LogicalPlan,
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct ModelWithPermissions {
    pub(crate) model: Arc<Model>,
    // Permisisons for the model. Note that the type permissions will need to be retrieved from the
    // global context
    pub(crate) permissions: HashMap<Role, Arc<resolved::SelectPermission>>,

    // The underlying source to execute ndc queries
    pub source: Option<Arc<metadata_resolve::ModelSource>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentInfo {
    pub argument_type: datafusion::DataType,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Model {
    pub subgraph: String,
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

impl ModelWithPermissions {
    pub fn from_resolved_model(model: &resolved::ModelWithPermissions) -> Self {
        let (schema, columns) = {
            let mut columns = IndexMap::new();
            let mut builder = datafusion::SchemaBuilder::new();
            for (field_name, field_definition) in &model.model.type_fields {
                let ndc_type_representation = get_type_representation(&model.model, field_name);
                let field_type =
                    to_arrow_type(&field_definition.field_type, ndc_type_representation);
                if let Some(field_type) = field_type {
                    builder.push(datafusion::Field::new(
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
            (
                datafusion::SchemaRef::new(datafusion::Schema::new(fields)),
                columns,
            )
        };

        let permissions = model
            .select_permissions
            .iter()
            .map(|(role, select_permission)| (role.clone(), Arc::new(select_permission.clone())))
            .collect();
        let model_source = model
            .model
            .source
            .as_ref()
            .map(|source| Arc::new(source.clone()));

        let model = Model {
            subgraph: model.model.name.subgraph.clone(),
            name: model.model.name.name.clone(),
            description: model.model.raw.description.clone(),
            arguments: IndexMap::new(),
            schema,
            data_type: model.model.data_type.clone(),
            columns,
        };
        ModelWithPermissions {
            model: model.into(),
            source: model_source,
            permissions,
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
    /// Metadata about the model
    pub(crate) model: Arc<Model>,
    pub(crate) source: Arc<resolved::ModelSource>,
    pub(crate) session: Arc<Session>,
    pub(crate) permission: Arc<resolved::SelectPermission>,
}

impl TableValuedFunction {
    // TODO: this will be removed when table valued functions are fully supported
    #[allow(dead_code)]
    pub(crate) fn new(
        model: Arc<Model>,
        source: Arc<resolved::ModelSource>,
        session: Arc<Session>,
        permission: Arc<resolved::SelectPermission>,
    ) -> Self {
        TableValuedFunction {
            model,
            source,
            session,
            permission,
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
        let table = Table::new(
            self.model.clone(),
            arguments,
            self.source.clone(),
            self.permission.clone(),
        );
        Ok(Arc::new(table) as Arc<dyn datafusion::TableProvider>)
    }
}

/// A Table represents an OpenDD entity which can provide a set of rows.
/// Currently, this is an instatation of a model with concrete arguments
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Table {
    /// Metadata about the model
    pub(crate) model: Arc<Model>,
    /// This will be empty if the model doesn't take any arguments
    pub(crate) arguments: BTreeMap<ArgumentName, serde_json::Value>,
    pub(crate) source: Arc<resolved::ModelSource>,
    pub(crate) permission: Arc<resolved::SelectPermission>,
}

impl Table {
    pub(crate) fn new(
        model: Arc<Model>,
        arguments: BTreeMap<ArgumentName, serde_json::Value>,
        source: Arc<resolved::ModelSource>,
        permission: Arc<resolved::SelectPermission>,
    ) -> Self {
        Table {
            model,
            arguments,
            source,
            permission,
        }
    }
    pub(crate) fn new_no_args(
        model: Arc<Model>,
        source: Arc<resolved::ModelSource>,
        permission: Arc<resolved::SelectPermission>,
    ) -> Self {
        Table {
            model,
            arguments: BTreeMap::new(),
            source,
            permission,
        }
    }
    pub(crate) fn to_logical_plan(
        &self,
        projected_schema: datafusion::DFSchemaRef,
    ) -> datafusion::Result<datafusion::LogicalPlan> {
        let ndc_query_node = NDCQuery::new(
            self.model.clone(),
            self.source.clone(),
            &self.arguments,
            self.permission.clone(),
            projected_schema,
        )?;
        let logical_plan = datafusion::LogicalPlan::Extension(datafusion::Extension {
            node: Arc::new(ndc_query_node),
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
        state: &datafusion::SessionState,
        projection: Option<&Vec<usize>>,
        // filters and limit can be used here to inject some push-down operations if needed
        _filters: &[datafusion::Expr],
        _limit: Option<usize>,
    ) -> datafusion::Result<Arc<dyn datafusion::ExecutionPlan>> {
        let projected_schema = self.model.schema.project(projection.unwrap_or(&vec![]))?;
        let qualified_projected_schema = datafusion::DFSchema::from_unqualified_fields(
            projected_schema.fields,
            projected_schema.metadata,
        )?;
        let logical_plan = self.to_logical_plan(Arc::new(qualified_projected_schema))?;
        state.create_physical_plan(&logical_plan).await
    }
}

/// Converts an opendd type to an arrow type.
/// TODO: need to handle complex types
#[allow(clippy::match_same_arms)]
fn to_arrow_type(
    ty: &resolved::QualifiedTypeReference,
    ndc_type_representation: Option<&ndc_models::TypeRepresentation>,
) -> Option<datafusion::DataType> {
    match &ty.underlying_type {
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
            if let Some(type_representation) = ndc_type_representation {
                match type_representation {
                    ndc_models::TypeRepresentation::Boolean => Some(datafusion::DataType::Boolean),
                    ndc_models::TypeRepresentation::String => Some(datafusion::DataType::Utf8),
                    ndc_models::TypeRepresentation::Int8 => Some(datafusion::DataType::Int8),
                    ndc_models::TypeRepresentation::Int16 => Some(datafusion::DataType::Int16),
                    ndc_models::TypeRepresentation::Int32 => Some(datafusion::DataType::Int32),
                    ndc_models::TypeRepresentation::Int64 => Some(datafusion::DataType::Int64),
                    ndc_models::TypeRepresentation::Float32 => Some(datafusion::DataType::Float32),
                    ndc_models::TypeRepresentation::Float64 => Some(datafusion::DataType::Float64),
                    // Can't do anything better for BigInteger, so we just use String.
                    ndc_models::TypeRepresentation::BigInteger => Some(datafusion::DataType::Utf8),
                    // BigDecimal128 is not supported by arrow.
                    ndc_models::TypeRepresentation::BigDecimal => {
                        Some(datafusion::DataType::Float64)
                    }
                    ndc_models::TypeRepresentation::UUID => Some(datafusion::DataType::Utf8),
                    ndc_models::TypeRepresentation::Date => Some(datafusion::DataType::Date32),
                    ndc_models::TypeRepresentation::Timestamp => Some(
                        datafusion::DataType::Timestamp(datafusion::TimeUnit::Microsecond, None),
                    ),
                    ndc_models::TypeRepresentation::TimestampTZ => Some(
                        datafusion::DataType::Timestamp(datafusion::TimeUnit::Microsecond, None),
                    ),
                    ndc_models::TypeRepresentation::Enum { .. } => Some(datafusion::DataType::Utf8),
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
                    "float64" => Some(datafusion::DataType::Float64),
                    "varchar" => Some(datafusion::DataType::Utf8),
                    "text" => Some(datafusion::DataType::Utf8),
                    "timestamp" => Some(datafusion::DataType::Timestamp(
                        datafusion::TimeUnit::Microsecond,
                        None,
                    )),
                    "timestamptz" => Some(datafusion::DataType::Timestamp(
                        datafusion::TimeUnit::Microsecond,
                        None,
                    )),
                    // BigDecimal128 is not supported by arrow.
                    "bigdecimal" => Some(datafusion::DataType::Float64),
                    _ => None,
                }
            }
        }
        resolved::QualifiedBaseType::List(_) => None,
    }
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
