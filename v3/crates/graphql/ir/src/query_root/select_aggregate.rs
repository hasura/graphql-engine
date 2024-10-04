//! model_source IR for 'select_aggregate' operation
//!
//! A 'select_aggregate' operation fetches a set of aggregates over rows of a model

/// Generates the IR for a 'select_aggregate' operation
use hasura_authn_core::SessionVariables;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;

use open_dds;
use serde::Serialize;

use crate::error;
use crate::model_selection;
use graphql_schema::GDS;
use metadata_resolve;
use metadata_resolve::Qualified;
use plan_types::UsagesCounts;

/// IR for the 'select_many' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelectAggregate<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: model_selection::ModelSelection<'s>,

    // The Graphql output type of the operation
    pub(crate) type_container: &'n ast::TypeContainer<ast::TypeName>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub(crate) usage_counts: UsagesCounts,
}

/// Generates the IR for a 'select_aggregate' operation
pub(crate) fn select_aggregate_generate_ir<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectAggregate<'n, 's>, error::Error> {
    let mut usage_counts = UsagesCounts::new();

    let model_selection = model_selection::generate_aggregate_model_selection_ir(
        field,
        field_call,
        data_type,
        model_source,
        model_name,
        session_variables,
        request_headers,
        &mut usage_counts,
    )?;

    Ok(ModelSelectAggregate {
        field_name: field_call.name.clone(),
        model_selection,
        type_container: &field.type_container,
        usage_counts,
    })
}
