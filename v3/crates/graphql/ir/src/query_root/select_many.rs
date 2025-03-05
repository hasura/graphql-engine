//! model_source IR for 'select_many' operation
//!
//! A 'select_many' operation fetches zero or one row from a model

/// Generates the IR for a 'select_many' operation
use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;

use open_dds;
use serde::Serialize;
use std::collections::BTreeMap;

use crate::arguments;
use crate::error;
use crate::filter;
use crate::model_selection;
use crate::order_by::{build_ndc_order_by, build_order_by_open_dd_ir};
use crate::permissions;
use crate::GraphqlRequestPipeline;
use graphql_schema::GDS;
use graphql_schema::{self, Annotation, BooleanExpressionAnnotation, ModelInputAnnotation};
use metadata_resolve;
use metadata_resolve::Qualified;
use plan::{count_model, process_argument_presets_for_model};
use plan_types::UsagesCounts;

#[derive(Debug, Serialize)]
pub enum ModelSelectManySelection<'s> {
    Ir(model_selection::ModelSelection<'s>),
    OpenDd(open_dds::query::ModelSelection),
}

/// IR for the 'select_many' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelectMany<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: ModelSelectManySelection<'s>,

    // The Graphql output type of the operation
    pub type_container: &'n ast::TypeContainer<ast::TypeName>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub usage_counts: UsagesCounts,
}

/// Generates the IR for a 'select_many' operation
#[allow(irrefutable_let_patterns)]
pub fn select_many_generate_ir<'n, 's>(
    request_pipeline: GraphqlRequestPipeline,
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
    model: &'s metadata_resolve::ModelWithPermissions,
    model_source: &'s metadata_resolve::ModelSource,
    models: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::models::ModelName>,
        metadata_resolve::ModelWithPermissions,
    >,
    commands: &'s IndexMap<
        metadata_resolve::Qualified<open_dds::commands::CommandName>,
        metadata_resolve::CommandWithPermissions,
    >,
    object_types: &'s BTreeMap<
        Qualified<open_dds::types::CustomTypeName>,
        metadata_resolve::ObjectTypeWithRelationships,
    >,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectMany<'n, 's>, error::Error> {
    let mut limit = None;
    let mut offset = None;
    let mut where_input = None;
    let mut order_by = None;
    let mut model_arguments = BTreeMap::new();

    // For opendd execution pipeline
    let mut model_arguments_input = None;
    let mut order_by_input = None;

    // Add the name of the root model
    let mut usage_counts = UsagesCounts::new();
    count_model(model_name, &mut usage_counts);

    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(graphql_schema::InputAnnotation::Model(
                model_argument_annotation,
            )) => match model_argument_annotation {
                ModelInputAnnotation::ModelLimitArgument => {
                    // Limit is optional
                    limit = argument
                        .value
                        .as_nullable(normalized_ast::Value::as_int_u32)
                        .map_err(error::Error::map_unexpected_value_to_external_error)?;
                }
                ModelInputAnnotation::ModelOffsetArgument => {
                    // Offset is optional
                    offset = argument
                        .value
                        .as_nullable(normalized_ast::Value::as_int_u32)
                        .map_err(error::Error::map_unexpected_value_to_external_error)?;
                }
                ModelInputAnnotation::ModelArgumentsExpression => match &argument.value {
                    normalized_ast::Value::Object(arguments) => {
                        for argument in arguments.values() {
                            let (ndc_arg_name, ndc_val) = arguments::build_ndc_argument_as_value(
                                &field_call.name,
                                argument,
                                &model_source.type_mappings,
                                object_types,
                                &model_source.data_connector,
                                &session.variables,
                                &mut usage_counts,
                            )?;

                            model_arguments.insert(ndc_arg_name, ndc_val);
                        }
                        model_arguments_input = Some(arguments);
                    }
                    _ => Err(error::InternalEngineError::InternalGeneric {
                        description: "Expected object value for model arguments".into(),
                    })?,
                },
                ModelInputAnnotation::ModelOrderByExpression => {
                    // order by is optional
                    order_by = argument.value.as_nullable(|v| {
                        build_ndc_order_by(
                            v,
                            &session.variables,
                            &mut usage_counts,
                            &model_source.type_mappings,
                            object_types,
                            &model_source.data_connector,
                        )
                    })?;
                    // For opendd execution pipeline
                    // Assign the order_by_input only if it is not null
                    if !argument.value.is_null() {
                        order_by_input = Some(&argument.value);
                    }
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
                // where argument is optional
                where_input = argument
                    .value
                    .as_nullable(normalized_ast::Value::as_object)?;
            }

            annotation => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?
            }
        }
    }

    // add any preset arguments from model permissions
    model_arguments = process_argument_presets_for_model(
        model_arguments,
        model,
        object_types,
        session,
        request_headers,
        &mut usage_counts,
    )?;

    let model_selection = match request_pipeline {
        GraphqlRequestPipeline::OpenDd => {
            let where_clause = match where_input {
                Some(where_input) => Some(filter::resolve_filter_expression_open_dd(
                    where_input,
                    &mut usage_counts,
                )?),
                None => None,
            };

            let model_arguments = model_arguments_input
                .map(|arguments_input| {
                    arguments::resolve_model_arguments_input_opendd(
                        arguments_input,
                        &model_source.type_mappings,
                        &mut usage_counts,
                    )
                })
                .transpose()?;

            let order_by = match order_by_input {
                None => vec![],
                Some(order_by_input) => build_order_by_open_dd_ir(
                    order_by_input,
                    &mut usage_counts,
                    &model_source.data_connector,
                )?,
            };

            let limit: Option<usize> = limit
                .map(|limit| {
                    usize::try_from(limit)
                        .map_err(|_| error::Error::InvalidLimitValue { value: limit })
                })
                .transpose()?;

            let offset: Option<usize> = offset
                .map(|offset| {
                    usize::try_from(offset)
                        .map_err(|_| error::Error::InvalidOffsetValue { value: offset })
                })
                .transpose()?;

            ModelSelectManySelection::OpenDd(model_selection::model_selection_open_dd_ir(
                &field.selection_set,
                model_name,
                models,
                &model_source.type_mappings,
                object_types,
                model_arguments,
                where_clause,
                order_by,
                limit,
                offset,
                &session.variables,
                request_headers,
                // Get all the models/commands that were used as relationships
                &mut usage_counts,
            )?)
        }
        GraphqlRequestPipeline::Old => {
            let where_clause = match where_input {
                Some(where_input) => Some(filter::resolve_filter_expression(
                    where_input,
                    &model_source.data_connector,
                    &model_source.type_mappings,
                    object_types,
                    &session.variables,
                    &mut usage_counts,
                )?),
                None => None,
            };

            let query_filter = filter::QueryFilter {
                where_clause,
                additional_filter: None,
            };

            ModelSelectManySelection::Ir(model_selection::model_selection_ir(
                &field.selection_set,
                data_type,
                model_source,
                model_arguments,
                query_filter,
                permissions::get_select_filter_predicate(&field_call.info)?,
                limit,
                offset,
                order_by,
                models,
                commands,
                object_types,
                session,
                request_headers,
                // Get all the models/commands that were used as relationships
                &mut usage_counts,
            )?)
        }
    };

    Ok(ModelSelectMany {
        field_name: field_call.name.clone(),
        model_selection,
        type_container: &field.type_container,
        usage_counts,
    })
}
