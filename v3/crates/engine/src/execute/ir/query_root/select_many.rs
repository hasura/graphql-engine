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

use super::error;
use crate::execute::ir::arguments;

use crate::execute::ir::filter;
use crate::execute::ir::filter::ResolvedFilterExpression;
use crate::execute::ir::model_selection;
use crate::execute::ir::order_by::build_ndc_order_by;
use crate::execute::ir::permissions;
use crate::execute::model_tracking::{count_model, UsagesCounts};
use crate::metadata::resolved;
use crate::metadata::resolved::subgraph::Qualified;
use crate::schema::types::{self, Annotation, BooleanExpressionAnnotation, ModelInputAnnotation};
use crate::schema::GDS;

/// IR for the 'select_many' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelectMany<'n, 's> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: model_selection::ModelSelection<'s>,

    // The Graphql output type of the operation
    pub(crate) type_container: &'n ast::TypeContainer<ast::TypeName>,

    // All the models/commands used in this operation. This includes the models/commands
    // used via relationships. And in future, the models/commands used in the filter clause
    pub(crate) usage_counts: UsagesCounts,
}
/// Generates the IR for a 'select_many' operation
#[allow(irrefutable_let_patterns)]
pub(crate) fn select_many_generate_ir<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
    model_source: &'s resolved::model::ModelSource,
    session_variables: &SessionVariables,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectMany<'n, 's>, error::Error> {
    let mut limit = None;
    let mut offset = None;
    let mut filter_clause = ResolvedFilterExpression {
        expression: None,
        relationships: BTreeMap::new(),
    };
    let mut order_by = None;
    let mut model_arguments = BTreeMap::new();

    // Add the name of the root model
    let mut usage_counts = UsagesCounts::new();
    count_model(model_name, &mut usage_counts);

    for argument in field_call.arguments.values() {
        match argument.info.generic {
            annotation @ Annotation::Input(types::InputAnnotation::Model(
                model_argument_annotation,
            )) => match model_argument_annotation {
                ModelInputAnnotation::ModelLimitArgument => {
                    limit = Some(
                        argument
                            .value
                            .as_int_u32()
                            .map_err(error::Error::map_unexpected_value_to_external_error)?,
                    )
                }
                ModelInputAnnotation::ModelOffsetArgument => {
                    offset = Some(
                        argument
                            .value
                            .as_int_u32()
                            .map_err(error::Error::map_unexpected_value_to_external_error)?,
                    )
                }
                ModelInputAnnotation::ModelArgumentsExpression => match &argument.value {
                    normalized_ast::Value::Object(arguments) => {
                        model_arguments.extend(
                            arguments::build_ndc_model_arguments(
                                &field_call.name,
                                arguments.values(),
                                &model_source.type_mappings,
                            )?
                            .into_iter(),
                        );
                    }
                    _ => Err(error::InternalEngineError::InternalGeneric {
                        description: "Expected object value for model arguments".into(),
                    })?,
                },
                ModelInputAnnotation::ModelOrderByExpression => {
                    order_by = Some(build_ndc_order_by(argument, &mut usage_counts)?)
                }
                _ => {
                    return Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: annotation.clone(),
                    })?
                }
            },

            Annotation::Input(types::InputAnnotation::BooleanExpression(
                BooleanExpressionAnnotation::BooleanExpression,
            )) => {
                filter_clause = filter::resolve_filter_expression(
                    argument.value.as_object()?,
                    &mut usage_counts,
                )?;
            }

            annotation => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: annotation.clone(),
                })?
            }
        }
    }

    // the first and only argument seemingly being "args"
    if let Some((_, field_call_argument)) = &field_call.arguments.first() {
        if let Some(argument_presets) =
            permissions::get_argument_presets(field_call_argument.info.namespaced)?
        {
            // add any preset arguments from model permissions
            arguments::process_model_arguments_presets(
                argument_presets,
                session_variables,
                &mut model_arguments,
            )?;
        }
    }

    let model_selection = model_selection::model_selection_ir(
        &field.selection_set,
        data_type,
        model_source,
        model_arguments,
        filter_clause,
        permissions::get_select_filter_predicate(field_call)?,
        limit,
        offset,
        order_by,
        session_variables,
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
