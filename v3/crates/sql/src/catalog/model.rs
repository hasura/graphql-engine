//! Describe a model for a SQL table and how to translate datafusion operations on the table
//! to ndc-spec queries.
pub(crate) mod common;
pub(crate) mod filter;

use std::collections::BTreeMap;
use std::{any::Any, sync::Arc};

use ::datafusion::common::Constraints;
use ::datafusion::logical_expr::{Expr, TableProviderFilterPushDown};
use ::datafusion::sql::sqlparser::ast::TableConstraint;
use async_trait::async_trait;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved, Metadata, Qualified};
use open_dds::arguments::ArgumentName;
use open_dds::identifier::SubgraphName;
use open_dds::{models::ModelName, types::CustomTypeName};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::catalog::model::filter::can_pushdown_filters;
use crate::execute::planner::model::ModelQuery;

use super::types::{StructTypeName, TypeRegistry, UnsupportedObject};

mod datafusion {
    pub(super) use datafusion::{
        arrow::datatypes::{DataType, SchemaRef},
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

#[derive(Error, Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum UnsupportedModel {
    #[error("return type {type_} not supported: {reason}")]
    ReturnTypeNotSupported {
        type_: Qualified<CustomTypeName>,
        reason: UnsupportedObject,
    },
    #[error("internal error: object type {0} not found in registry")]
    InternalNotFound(Qualified<CustomTypeName>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Model {
    pub(crate) subgraph: SubgraphName,
    pub(crate) name: ModelName,

    pub(crate) description: Option<String>,

    pub(crate) arguments: IndexMap<ArgumentName, ArgumentInfo>,

    // The struct type of the model's object type
    pub(crate) struct_type: StructTypeName,

    // Datafusion table schema
    pub(crate) schema: datafusion::SchemaRef,

    // This is the entry point for the type mappings stored
    // in ModelSource
    pub(crate) data_type: Qualified<CustomTypeName>,
}

impl Model {
    pub fn from_resolved_model(
        type_registry: &TypeRegistry,
        model: &resolved::ModelWithArgumentPresets,
    ) -> Result<Self, UnsupportedModel> {
        let object = type_registry
            .get_object(&model.model.data_type)
            .ok_or_else(|| UnsupportedModel::InternalNotFound(model.model.data_type.clone()))?
            .as_ref()
            .map_err(
                |unsupported_object| UnsupportedModel::ReturnTypeNotSupported {
                    type_: model.model.data_type.clone(),
                    reason: unsupported_object.clone(),
                },
            )?;
        let model = Model {
            subgraph: model.model.name.subgraph.clone(),
            name: model.model.name.name.clone(),
            description: model.model.raw.description.clone(),
            arguments: IndexMap::new(),
            struct_type: object.name.clone(),
            schema: object.schema.clone(),
            data_type: model.model.data_type.clone(),
        };
        Ok(model)
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
        let table = Table::new(self.metadata.clone(), self.model.clone(), arguments)?;
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
    /// DF constraints which can help guide the planner,
    /// e.g. uniqueness constraints establish functional dependencies
    /// which ensure all correct columns are in scope under a GROUP BY.
    pub(crate) constraints: ::datafusion::common::Constraints,
}

impl Table {
    pub(crate) fn new(
        metadata: Arc<Metadata>,
        model: Arc<Model>,
        arguments: BTreeMap<ArgumentName, serde_json::Value>,
    ) -> datafusion::Result<Self> {
        let constraints = compute_table_constraints(&model, &metadata)?;
        Ok(Table {
            metadata,
            model,
            arguments,
            constraints,
        })
    }
    pub(crate) fn new_no_args(
        metadata: Arc<Metadata>,
        model: Arc<Model>,
    ) -> datafusion::Result<Self> {
        Self::new(metadata, model, BTreeMap::new())
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

/// Computes DF constraints from the model metadata.
/// TODO: this function currently uses the select_uniques field from
/// the GraphQL configuration section, but we should have that data on the
/// model itself, since it's not just a GraphQL concern now.
fn compute_table_constraints(
    model: &Arc<Model>,
    metadata: &Arc<Metadata>,
) -> datafusion::Result<::datafusion::common::Constraints> {
    let schema = model.schema.as_ref().clone();
    let df_schema = datafusion::DFSchema::from_unqualified_fields(schema.fields, schema.metadata)?;
    let model = metadata
        .models
        .get(&Qualified::new(model.subgraph.clone(), model.name.clone()))
        .ok_or(::datafusion::error::DataFusionError::Internal(format!(
            "Model {} could not be found",
            model.name
        )))?;

    let table_constraints = model
        .graphql_api
        .select_uniques
        .iter()
        .filter_map(|unique| {
            let mut columns: Vec<::datafusion::sql::sqlparser::ast::Ident> = vec![];

            for (field_name, _) in &unique.unique_identifier {
                let _ = df_schema.field_with_name(None, field_name.as_str()).ok()?;
                columns.push(field_name.as_str().into());
            }

            Some(TableConstraint::Unique {
                name: None,
                index_name: None,
                index_type_display: ::datafusion::sql::sqlparser::ast::KeyOrIndexDisplay::None,
                index_type: None,
                columns,
                index_options: vec![],
                characteristics: None,
            })
        })
        .collect::<Vec<_>>();

    ::datafusion::common::Constraints::new_from_table_constraints(
        table_constraints.as_slice(),
        &Arc::new(df_schema),
    )
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

    fn constraints(&self) -> Option<&Constraints> {
        Some(&self.constraints)
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
