//! model_source IR for 'select_many' operation
//!
//! A 'select_many' operation fetches zero or one row from a model

/// Generates the IR for a 'select_many' operation
use hasura_authn_core::SessionVariables;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;

use open_dds;
use serde::Serialize;
use std::collections::BTreeMap;

use crate::arguments;
use crate::error;
use crate::filter;
use crate::model_selection;
use crate::model_tracking::count_model;
use crate::order_by::build_ndc_order_by;
use crate::permissions;
use graphql_schema::GDS;
use graphql_schema::{self, Annotation, BooleanExpressionAnnotation, ModelInputAnnotation};
use metadata_resolve;
use metadata_resolve::Qualified;
use plan_types::UsagesCounts;

/// IR for the 'select_many' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelectMany<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: model_selection::ModelSelection<'s>,

    // The Graphql output type of the operation
    pub type_container: &'n ast::TypeContainer<ast::TypeName>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub usage_counts: UsagesCounts,
}
/// Generates the IR for a 'select_many' operation
#[allow(irrefutable_let_patterns)]
pub fn select_many_generate_ir<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectMany<'n, 's>, error::Error> {
    let mut limit = None;
    let mut offset = None;
    let mut where_clause = None;
    let mut order_by = None;
    let mut model_arguments = BTreeMap::new();

    // Add the name of the root model
    let mut usage_counts = UsagesCounts::new();
    count_model(model_name, &mut usage_counts);

    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(graphql_schema::InputAnnotation::Model(
                model_argument_annotation,
            )) => match model_argument_annotation {
                ModelInputAnnotation::ModelLimitArgument => {
                    limit = Some(
                        argument
                            .value
                            .as_int_u32()
                            .map_err(error::Error::map_unexpected_value_to_external_error)?,
                    );
                }
                ModelInputAnnotation::ModelOffsetArgument => {
                    offset = Some(
                        argument
                            .value
                            .as_int_u32()
                            .map_err(error::Error::map_unexpected_value_to_external_error)?,
                    );
                }
                ModelInputAnnotation::ModelArgumentsExpression => match &argument.value {
                    normalized_ast::Value::Object(arguments) => {
                        for argument in arguments.values() {
                            let (ndc_arg_name, ndc_val) = arguments::build_ndc_argument_as_value(
                                &field_call.name,
                                argument,
                                &model_source.type_mappings,
                                &model_source.data_connector,
                                session_variables,
                                &mut usage_counts,
                            )?;

                            model_arguments.insert(ndc_arg_name, ndc_val);
                        }
                    }
                    _ => Err(error::InternalEngineError::InternalGeneric {
                        description: "Expected object value for model arguments".into(),
                    })?,
                },
                ModelInputAnnotation::ModelOrderByExpression => {
                    order_by = Some(build_ndc_order_by(
                        argument,
                        session_variables,
                        &mut usage_counts,
                    )?);
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
                where_clause = Some(filter::resolve_filter_expression(
                    argument.value.as_object()?,
                    &model_source.data_connector,
                    &model_source.type_mappings,
                    session_variables,
                    &mut usage_counts,
                )?);
            }

            annotation => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?
            }
        }
    }

    // the first and only argument seemingly being "args"
    let argument_presets = if let Some((_, field_call_argument)) = &field_call.arguments.first() {
        permissions::get_argument_presets(field_call_argument.info.namespaced.as_ref())?
    } else {
        None
    };

    // add any preset arguments from model permissions
    model_arguments = arguments::process_argument_presets(
        &model_source.data_connector,
        &model_source.type_mappings,
        argument_presets,
        &model_source.data_connector_link_argument_presets,
        session_variables,
        request_headers,
        model_arguments,
        &mut usage_counts,
    )?;

    let query_filter = filter::QueryFilter {
        where_clause,
        additional_filter: None,
    };

    let model_selection = model_selection::model_selection_ir(
        &field.selection_set,
        data_type,
        model_source,
        model_arguments,
        query_filter,
        permissions::get_select_filter_predicate(&field_call.info)?,
        limit,
        offset,
        order_by,
        session_variables,
        request_headers,
        // Get all the models/commands that were used as relationships
        &mut usage_counts,
    )?;

    Ok(ModelSelectMany {
        field_name: field_call.name.clone(),
        model_selection,
        type_container: &field.type_container,
        usage_counts,
    })
}
