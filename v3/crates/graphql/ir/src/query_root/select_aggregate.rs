//! model_source IR for 'select_aggregate' operation
//!
//! A 'select_aggregate' operation fetches a set of aggregates over rows of a model

use graphql_schema::GDS;
use graphql_schema::{self, Annotation, BooleanExpressionAnnotation, ModelInputAnnotation};
/// Generates the IR for a 'select_aggregate' operation
use hasura_authn_core::SessionVariables;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use metadata_resolve;
use metadata_resolve::Qualified;
use open_dds;
use plan::count_model;
use plan_types::UsagesCounts;
use serde::Serialize;

use crate::error;
use crate::filter;
use crate::model_selection;
use crate::GraphqlRequestPipeline;

#[derive(Debug, Serialize)]
pub enum ModelSelectAggregateSelection<'s> {
    Ir(model_selection::ModelSelection<'s>),
    OpenDd(open_dds::query::ModelAggregateSelection),
}

/// IR for the 'select_many' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelectAggregate<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: ModelSelectAggregateSelection<'s>,

    // The Graphql output type of the operation
    pub(crate) type_container: &'n ast::TypeContainer<ast::TypeName>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub(crate) usage_counts: UsagesCounts,
}

/// Generates the IR for a 'select_aggregate' operation
pub(crate) fn select_aggregate_generate_ir<'n, 's>(
    request_pipeline: GraphqlRequestPipeline,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectAggregate<'n, 's>, error::Error> {
    let mut usage_counts = UsagesCounts::new();
    let model_selection =
        match request_pipeline {
            GraphqlRequestPipeline::Old => {
                ModelSelectAggregateSelection::Ir(
                    model_selection::generate_aggregate_model_selection_ir(
                        field,
                        field_call,
                        data_type,
                        model_source,
                        model_name,
                        session_variables,
                        request_headers,
                        // Get all the models/commands that were used as relationships
                        &mut usage_counts,
                    )?,
                )
            }
            GraphqlRequestPipeline::OpenDd => {
                // TODO: De-duplicate the following code with the implementation in super::select_many
                let mut limit = None;
                let mut offset = None;
                let mut where_input = None;

                // Add the name of the root model
                count_model(model_name, &mut usage_counts);

                for argument in field_call.arguments.values() {
                    match argument.info.generic {
                        annotation @ Annotation::Input(graphql_schema::InputAnnotation::Model(
                            model_argument_annotation,
                        )) => match model_argument_annotation {
                            ModelInputAnnotation::ModelLimitArgument => {
                                limit = Some(argument.value.as_int_u32().map_err(
                                    error::Error::map_unexpected_value_to_external_error,
                                )?);
                            }
                            ModelInputAnnotation::ModelOffsetArgument => {
                                offset = Some(argument.value.as_int_u32().map_err(
                                    error::Error::map_unexpected_value_to_external_error,
                                )?);
                            }
                            ModelInputAnnotation::ModelArgumentsExpression
                            | ModelInputAnnotation::ModelOrderByExpression => {
                                // TODO: Handle model arguments and order_by inputs
                            }
                            _ => {
                                return Err(error::InternalEngineError::UnexpectedAnnotation {
                                    annotation: annotation.clone(),
                                })?
                            }
                        },

                        Annotation::Input(graphql_schema::InputAnnotation::BooleanExpression(
                            BooleanExpressionAnnotation::BooleanExpressionRootField,
                        )) => {
                            where_input = Some(argument.value.as_object()?);
                        }

                        annotation => {
                            return Err(error::InternalEngineError::UnexpectedAnnotation {
                                annotation: annotation.clone(),
                            })?
                        }
                    }
                }

                let where_clause = match where_input {
                    Some(where_input) => Some(filter::resolve_filter_expression_open_dd(
                        where_input,
                        session_variables,
                        &mut usage_counts,
                    )?),
                    None => None,
                };
                ModelSelectAggregateSelection::OpenDd(
                    model_selection::model_aggregate_selection_open_dd_ir(
                        &field.selection_set,
                        data_type,
                        model_source,
                        model_name,
                        where_clause,
                        limit,
                        offset,
                        &mut usage_counts,
                    )?,
                )
            }
        };

    Ok(ModelSelectAggregate {
        field_name: field_call.name.clone(),
        model_selection,
        type_container: &field.type_container,
        usage_counts,
    })
}
