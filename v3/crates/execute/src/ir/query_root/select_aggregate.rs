//! model_source IR for 'select_aggregate' operation
//!
//! A 'select_aggregate' operation fetches a set of aggregates over rows of a model

/// Generates the IR for a 'select_aggregate' operation
use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;

use open_dds;
use schema::InputAnnotation;
use serde::Serialize;
use std::collections::BTreeMap;

use crate::ir::arguments;
use crate::ir::error;
use crate::ir::filter;
use crate::ir::filter::ResolvedFilterExpression;
use crate::ir::model_selection;
use crate::ir::order_by::build_ndc_order_by;
use crate::ir::order_by::ResolvedOrderBy;
use crate::ir::permissions;
use crate::model_tracking::{count_model, UsagesCounts};
use metadata_resolve;
use metadata_resolve::Qualified;
use schema::GDS;
use schema::{self, Annotation, BooleanExpressionAnnotation, ModelInputAnnotation};

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

struct ModelSelectAggregateArguments<'s> {
    model_arguments: BTreeMap<metadata_resolve::ConnectorArgumentName, ndc_models::Argument>,
    filter_input_arguments: FilterInputArguments<'s>,
}

struct FilterInputArguments<'s> {
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<ResolvedOrderBy<'s>>,
    filter_clause: ResolvedFilterExpression<'s>,
}

/// Generates the IR for a 'select_aggregate' operation
#[allow(irrefutable_let_patterns)]
pub(crate) fn select_aggregate_generate_ir<'n, 's>(
    field: &'n normalized_ast::Field<'s, GDS>,
    field_call: &'n normalized_ast::FieldCall<'s, GDS>,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    session_variables: &SessionVariables,
    model_name: &'s Qualified<open_dds::models::ModelName>,
) -> Result<ModelSelectAggregate<'n, 's>, error::Error> {
    let mut usage_counts = UsagesCounts::new();
    count_model(model_name, &mut usage_counts);

    let mut arguments =
        read_model_select_aggregate_arguments(field_call, model_source, &mut usage_counts)?;

    // If there are model arguments presets from permissions, apply them
    if let Some(model_argument_presets) =
        permissions::get_argument_presets(field_call.info.namespaced)?
    {
        arguments::process_model_arguments_presets(
            model_argument_presets,
            session_variables,
            &mut arguments.model_arguments,
            &mut usage_counts,
        )?;
    }

    let model_selection = model_selection::model_aggregate_selection_ir(
        &field.selection_set,
        data_type,
        model_source,
        arguments.model_arguments,
        arguments.filter_input_arguments.filter_clause,
        permissions::get_select_filter_predicate(field_call)?,
        arguments.filter_input_arguments.limit,
        arguments.filter_input_arguments.offset,
        arguments.filter_input_arguments.order_by,
        session_variables,
        // Get all the models/commands that were used as relationships
        &mut usage_counts,
    )?;

    Ok(ModelSelectAggregate {
        field_name: field_call.name.clone(),
        model_selection,
        type_container: &field.type_container,
        usage_counts,
    })
}

fn read_model_select_aggregate_arguments<'s>(
    field_call: &normalized_ast::FieldCall<'s, GDS>,
    model_source: &'s metadata_resolve::ModelSource,
    usage_counts: &mut UsagesCounts,
) -> Result<ModelSelectAggregateArguments<'s>, error::Error> {
    let mut model_arguments = None;
    let mut filter_input_props = None;

    for field_call_argument in field_call.arguments.values() {
        match field_call_argument.info.generic {
            // Model arguments
            Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ModelArgumentsExpression,
            )) => {
                if model_arguments.is_some() {
                    return Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: field_call_argument.info.generic.clone(),
                    }
                    .into());
                }
                model_arguments = Some(match &field_call_argument.value {
                    normalized_ast::Value::Object(model_args_input_props) => {
                        arguments::build_ndc_model_arguments(
                            &field_call.name,
                            model_args_input_props.values(),
                            &model_source.type_mappings,
                        )
                    }
                    _ => Err(error::InternalEngineError::InternalGeneric {
                        description: "Expected object value for model arguments".into(),
                    }
                    .into()),
                }?);
            }

            // Filter input arguments
            Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ModelFilterInputArgument,
            )) => {
                if filter_input_props.is_some() {
                    return Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: field_call_argument.info.generic.clone(),
                    }
                    .into());
                }
                filter_input_props = Some(match &field_call_argument.value {
                    normalized_ast::Value::Object(model_args_input_props) => {
                        Ok(model_args_input_props)
                    }
                    _ => Err(error::Error::Internal(error::InternalError::Engine(
                        error::InternalEngineError::InternalGeneric {
                            description: "Expected object value for model arguments".into(),
                        },
                    ))),
                }?);
            }

            _ => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: field_call_argument.info.generic.clone(),
                }
                .into())
            }
        }
    }

    let filter_input_arguments =
        read_filter_input_arguments(filter_input_props, model_source, usage_counts)?;

    Ok(ModelSelectAggregateArguments {
        model_arguments: model_arguments.unwrap_or_else(BTreeMap::new),
        filter_input_arguments,
    })
}

fn read_filter_input_arguments<'s>(
    filter_input_field_props: Option<&IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>>,
    model_source: &'s metadata_resolve::ModelSource,
    usage_counts: &mut UsagesCounts,
) -> Result<FilterInputArguments<'s>, error::Error> {
    let mut limit = None;
    let mut offset = None;
    let mut order_by = None;
    let mut filter_clause = None;

    if let Some(filter_input_field_props) = filter_input_field_props {
        for filter_input_field_arg in filter_input_field_props.values() {
            match filter_input_field_arg.info.generic {
                // Limit argument
                Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelLimitArgument,
                )) => {
                    if limit.is_some() {
                        return Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: filter_input_field_arg.info.generic.clone(),
                        }
                        .into());
                    }
                    limit = Some(
                        filter_input_field_arg
                            .value
                            .as_int_u32()
                            .map_err(error::Error::map_unexpected_value_to_external_error)?,
                    );
                }

                // Offset argument
                Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelOffsetArgument,
                )) => {
                    if offset.is_some() {
                        return Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: filter_input_field_arg.info.generic.clone(),
                        }
                        .into());
                    }
                    offset = Some(
                        filter_input_field_arg
                            .value
                            .as_int_u32()
                            .map_err(error::Error::map_unexpected_value_to_external_error)?,
                    );
                }

                // Order By argument
                Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelOrderByExpression,
                )) => {
                    if order_by.is_some() {
                        return Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: filter_input_field_arg.info.generic.clone(),
                        }
                        .into());
                    }
                    order_by = Some(build_ndc_order_by(filter_input_field_arg, usage_counts)?);
                }

                // Where argument
                Annotation::Input(InputAnnotation::BooleanExpression(
                    BooleanExpressionAnnotation::BooleanExpression,
                )) => {
                    if filter_clause.is_some() {
                        return Err(error::InternalEngineError::UnexpectedAnnotation {
                            annotation: filter_input_field_arg.info.generic.clone(),
                        }
                        .into());
                    }
                    filter_clause = Some(filter::resolve_filter_expression(
                        filter_input_field_arg.value.as_object()?,
                        &model_source.data_connector,
                        &model_source.type_mappings,
                        usage_counts,
                    )?);
                }

                _ => {
                    return Err(error::InternalEngineError::UnexpectedAnnotation {
                        annotation: filter_input_field_arg.info.generic.clone(),
                    }
                    .into())
                }
            }
        }
    }

    Ok(FilterInputArguments {
        limit,
        offset,
        order_by,
        filter_clause: filter_clause.unwrap_or_else(|| ResolvedFilterExpression {
            expression: None,
            relationships: BTreeMap::new(),
        }),
    })
}
