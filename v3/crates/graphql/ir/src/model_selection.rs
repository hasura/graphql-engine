//! IR for the 'model_selection' type - selecting fields from a model
use graphql_schema::{
    Annotation, BooleanExpressionAnnotation, InputAnnotation, ModelInputAnnotation,
};
use hasura_authn_core::SessionVariables;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use metadata_resolve::QualifiedTypeName;
use open_dds::{
    data_connector::CollectionName,
    types::{CustomTypeName, DataConnectorArgumentName},
};
use serde::Serialize;
use std::collections::BTreeMap;
use std::sync::Arc;

use super::{aggregates, arguments, filter, order_by, permissions, selection_set};
use crate::error;
use crate::model_tracking::count_model;
use graphql_schema::GDS;
use metadata_resolve::Qualified;
use plan_types::{Expression, UsagesCounts};

/// IR fragment for any 'select' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelection<'s> {
    // The data connector backing this model.
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,

    // Source collection in the data connector for this model
    pub collection: &'s CollectionName,

    // Arguments for the NDC collection
    pub arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<'s>>,

    // The boolean expression that would fetch a single row from this model
    pub filter_clause: filter::FilterExpression<'s>,

    // Limit
    pub limit: Option<u32>,

    // Offset
    pub offset: Option<u32>,

    // Order by
    pub order_by: Option<order_by::OrderBy<'s>>,

    // Fields requested from the model
    pub selection: Option<selection_set::ResultSelectionSet<'s>>,

    // Aggregates requested of the model
    pub aggregate_selection: Option<plan_types::AggregateSelectionSet>,
}

struct ModelSelectAggregateArguments<'s> {
    model_arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<'s>>,
    filter_input_arguments: FilterInputArguments<'s>,
}

struct FilterInputArguments<'s> {
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<order_by::OrderBy<'s>>,
    filter_clause: Option<Expression<'s>>,
}

/// Generates the IR fragment for selecting from a model.
#[allow(clippy::too_many_arguments)]
pub fn model_selection_ir<'s>(
    selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_type: &Qualified<CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<'s>>,
    query_filter: filter::QueryFilter<'s>,
    permissions_predicate: &'s metadata_resolve::FilterPermission,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<order_by::OrderBy<'s>>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<ModelSelection<'s>, error::Error> {
    let permission_filter = permissions::build_model_permissions_filter_predicate(
        &model_source.data_connector,
        &model_source.type_mappings,
        permissions_predicate,
        session_variables,
        usage_counts,
    )?;

    let filter_clause = filter::FilterExpression {
        query_filter,
        permission_filter,
        relationship_join_filter: None,
    };

    let field_mappings = get_field_mappings_for_object_type(model_source, data_type)?;
    let selection = selection_set::generate_selection_set_ir(
        selection_set,
        metadata_resolve::FieldNestedness::NotNested,
        &model_source.data_connector,
        &model_source.type_mappings,
        field_mappings,
        session_variables,
        request_headers,
        usage_counts,
    )?;

    Ok(ModelSelection {
        data_connector: model_source.data_connector.clone(),
        collection: &model_source.collection,
        arguments,
        filter_clause,
        limit,
        offset,
        order_by,
        selection: Some(selection),
        aggregate_selection: None,
    })
}

pub fn generate_aggregate_model_selection_ir<'s>(
    field: &normalized_ast::Field<'s, GDS>,
    field_call: &normalized_ast::FieldCall<'s, GDS>,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    model_name: &Qualified<open_dds::models::ModelName>,
    session_variables: &SessionVariables,
    request_headers: &reqwest::header::HeaderMap,
    usage_counts: &mut UsagesCounts,
) -> Result<ModelSelection<'s>, error::Error> {
    count_model(model_name, usage_counts);

    let mut arguments = read_model_select_aggregate_arguments(
        field_call,
        model_source,
        session_variables,
        usage_counts,
    )?;

    let model_argument_presets =
        permissions::get_argument_presets(field_call.info.namespaced.as_ref())?;

    arguments.model_arguments = arguments::process_argument_presets(
        &model_source.data_connector,
        &model_source.type_mappings,
        model_argument_presets,
        &model_source.data_connector_link_argument_presets,
        session_variables,
        request_headers,
        arguments.model_arguments,
        usage_counts,
    )?;

    let query_filter = filter::QueryFilter {
        where_clause: arguments.filter_input_arguments.filter_clause,
        additional_filter: None,
    };

    model_aggregate_selection_ir(
        &field.selection_set,
        data_type,
        model_source,
        arguments.model_arguments,
        query_filter,
        permissions::get_select_filter_predicate(&field_call.info)?,
        arguments.filter_input_arguments.limit,
        arguments.filter_input_arguments.offset,
        arguments.filter_input_arguments.order_by,
        session_variables,
        // Get all the models/commands that were used as relationships
        usage_counts,
    )
}

fn read_model_select_aggregate_arguments<'s>(
    field_call: &normalized_ast::FieldCall<'s, GDS>,
    model_source: &'s metadata_resolve::ModelSource,
    session_variables: &SessionVariables,
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
                model_arguments = Some(
                    field_call_argument
                        .value
                        .as_object()
                        .map_err(std::convert::Into::<error::Error>::into)
                        .map(|model_args_input_props| {
                            let mut inner_model_arguments = BTreeMap::new();

                            for argument in model_args_input_props.values() {
                                if let Ok((ndc_arg_name, ndc_val)) =
                                    arguments::build_ndc_argument_as_value(
                                        &field_call.name,
                                        argument,
                                        &model_source.type_mappings,
                                        &model_source.data_connector,
                                        session_variables,
                                        usage_counts,
                                    )
                                {
                                    inner_model_arguments.insert(ndc_arg_name, ndc_val);
                                }
                            }
                            inner_model_arguments
                        })?,
                );
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
                filter_input_props = Some(field_call_argument.value.as_object()?);
            }

            _ => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: field_call_argument.info.generic.clone(),
                }
                .into())
            }
        }
    }

    let filter_input_arguments = read_filter_input_arguments(
        filter_input_props,
        model_source,
        session_variables,
        usage_counts,
    )?;

    Ok(ModelSelectAggregateArguments {
        model_arguments: model_arguments.unwrap_or_else(BTreeMap::new),
        filter_input_arguments,
    })
}

fn read_filter_input_arguments<'s>(
    filter_input_field_props: Option<&IndexMap<ast::Name, normalized_ast::InputField<'s, GDS>>>,
    model_source: &'s metadata_resolve::ModelSource,
    session_variables: &SessionVariables,
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
                    order_by = Some(order_by::build_ndc_order_by(
                        filter_input_field_arg,
                        session_variables,
                        usage_counts,
                    )?);
                }

                // Where argument
                Annotation::Input(InputAnnotation::BooleanExpression(
                    BooleanExpressionAnnotation::BooleanExpressionRootField,
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
                        session_variables,
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
        filter_clause,
    })
}

/// Generates the IR fragment for selecting an aggregate of a model.
#[allow(clippy::too_many_arguments)]
fn model_aggregate_selection_ir<'s>(
    aggregate_selection_set: &normalized_ast::SelectionSet<'s, GDS>,
    data_type: &Qualified<CustomTypeName>,
    model_source: &'s metadata_resolve::ModelSource,
    arguments: BTreeMap<DataConnectorArgumentName, arguments::Argument<'s>>,
    query_filter: filter::QueryFilter<'s>,
    permissions_predicate: &'s metadata_resolve::FilterPermission,
    limit: Option<u32>,
    offset: Option<u32>,
    order_by: Option<order_by::OrderBy<'s>>,
    session_variables: &SessionVariables,
    usage_counts: &mut UsagesCounts,
) -> Result<ModelSelection<'s>, error::Error> {
    let permission_filter = permissions::build_model_permissions_filter_predicate(
        &model_source.data_connector,
        &model_source.type_mappings,
        permissions_predicate,
        session_variables,
        usage_counts,
    )?;

    let filter_clause = filter::FilterExpression {
        query_filter,
        permission_filter,
        relationship_join_filter: None,
    };

    let field_mappings = get_field_mappings_for_object_type(model_source, data_type)?;
    let aggregate_selection = aggregates::generate_aggregate_selection_set_ir(
        aggregate_selection_set,
        &model_source.data_connector,
        &model_source.type_mappings,
        field_mappings,
        &QualifiedTypeName::Custom(data_type.clone()),
    )?;

    Ok(ModelSelection {
        data_connector: model_source.data_connector.clone(),
        collection: &model_source.collection,
        arguments,
        filter_clause,
        limit,
        offset,
        order_by,
        selection: None,
        aggregate_selection: Some(aggregate_selection),
    })
}

fn get_field_mappings_for_object_type<'s>(
    model_source: &'s metadata_resolve::ModelSource,
    data_type: &Qualified<CustomTypeName>,
) -> Result<&'s BTreeMap<open_dds::types::FieldName, metadata_resolve::FieldMapping>, error::Error>
{
    model_source
        .type_mappings
        .get(data_type)
        .map(|type_mapping| {
            let metadata_resolve::TypeMapping::Object { field_mappings, .. } = type_mapping;
            field_mappings
        })
        .ok_or_else(|| {
            error::InternalEngineError::InternalGeneric {
                description: format!("type '{data_type}' not found in model source type_mappings"),
            }
            .into()
        })
}
