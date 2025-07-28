//! model_source IR for 'select_aggregate' operation
//!
//! A 'select_aggregate' operation fetches a set of aggregates over rows of a model

use graphql_schema::{self, Annotation, BooleanExpressionAnnotation, ModelInputAnnotation};
use graphql_schema::{GDS, InputAnnotation};
/// Generates the IR for a 'select_aggregate' operation
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::normalized_ast;
use metadata_resolve;
use metadata_resolve::Qualified;
use open_dds;
use plan::count_model;
use plan_types::UsagesCounts;
use serde::Serialize;

use crate::arguments;
use crate::error;
use crate::filter;
use crate::flags::GraphqlIrFlags;
use crate::model_selection;
use crate::order_by;

/// IR for the 'select_many' operation on a model
#[derive(Debug, Serialize)]
pub struct ModelSelectAggregate<'n> {
    // The name of the field as published in the schema
    pub field_name: ast::Name,

    pub model_selection: open_dds::query::ModelAggregateSelection,

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
    model_source: &'s metadata_resolve::ModelSource,
    model_name: &'s Qualified<open_dds::models::ModelName>,
    flags: &GraphqlIrFlags,
) -> Result<ModelSelectAggregate<'n>, error::Error> {
    let mut usage_counts = UsagesCounts::new();
    let AggregateQuery {
        limit,
        offset,
        where_clause,
        model_arguments,
        order_by,
    } = aggregate_query(
        field_call,
        model_name,
        model_source,
        flags,
        &mut usage_counts,
    )?;

    let model_selection = model_selection::model_aggregate_selection_open_dd_ir(
        &field.selection_set,
        model_name,
        model_arguments,
        where_clause,
        order_by,
        limit,
        offset,
        &mut usage_counts,
    )?;

    Ok(ModelSelectAggregate {
        field_name: field_call.name.clone(),
        model_selection,
        type_container: &field.type_container,
        usage_counts,
    })
}

pub struct AggregateQuery {
    pub limit: Option<usize>,
    pub offset: Option<usize>,
    pub where_clause: Option<open_dds::query::BooleanExpression>,
    pub model_arguments: Option<IndexMap<open_dds::query::ArgumentName, open_dds::query::Value>>,
    pub order_by: Vec<open_dds::query::OrderByElement>,
}

pub fn aggregate_query(
    field_call: &normalized_ast::FieldCall<'_, GDS>,
    model_name: &Qualified<open_dds::models::ModelName>,
    model_source: &metadata_resolve::ModelSource,
    flags: &GraphqlIrFlags,
    usage_counts: &mut UsagesCounts,
) -> Result<AggregateQuery, error::Error> {
    let mut limit = None;
    let mut offset = None;
    let mut where_input = None;
    let mut model_arguments_input = None;
    let mut order_by_input = None;

    // Add the name of the root model
    count_model(model_name, usage_counts);

    for field_call_argument in field_call.arguments.values() {
        match field_call_argument.info.generic {
            // Model arguments
            Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ModelArgumentsExpression,
            )) => {
                model_arguments_input = Some(field_call_argument.value.as_object()?);
            }
            // Filter input arguments
            Annotation::Input(InputAnnotation::Model(
                ModelInputAnnotation::ModelFilterInputArgument,
            )) => {
                let filter_input_object = field_call_argument.value.as_object()?;
                for filter_input_field_arg in filter_input_object.values() {
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
                            // Limit is optional
                            limit = filter_input_field_arg
                                .value
                                .as_nullable(
                                    &flags.validate_non_null_graphql_variables,
                                    normalized_ast::Value::as_int_u32,
                                )
                                .map_err(error::Error::map_unexpected_value_to_external_error)?;
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
                            // Offset is optional
                            offset = filter_input_field_arg
                                .value
                                .as_nullable(
                                    &flags.validate_non_null_graphql_variables,
                                    normalized_ast::Value::as_int_u32,
                                )
                                .map_err(error::Error::map_unexpected_value_to_external_error)?;
                        }

                        // Order By argument
                        Annotation::Input(InputAnnotation::Model(
                            ModelInputAnnotation::ModelOrderByExpression,
                        )) => {
                            // order by argument is optional
                            if !filter_input_field_arg
                                .value
                                .is_null(&flags.validate_non_null_graphql_variables)
                            {
                                order_by_input = Some(&filter_input_field_arg.value);
                            }
                        }

                        // Where argument
                        Annotation::Input(InputAnnotation::BooleanExpression(
                            BooleanExpressionAnnotation::BooleanExpressionRootField,
                        )) => {
                            if where_input.is_some() {
                                return Err(error::InternalEngineError::UnexpectedAnnotation {
                                    annotation: filter_input_field_arg.info.generic.clone(),
                                }
                                .into());
                            }
                            // where argument is optional
                            if !filter_input_field_arg
                                .value
                                .is_null(&flags.validate_non_null_graphql_variables)
                            {
                                where_input = Some(filter_input_field_arg.value.as_object()?);
                            }
                        }

                        _ => {
                            return Err(error::InternalEngineError::UnexpectedAnnotation {
                                annotation: filter_input_field_arg.info.generic.clone(),
                            }
                            .into());
                        }
                    }
                }
            }

            _ => {
                return Err(error::InternalEngineError::UnexpectedAnnotation {
                    annotation: field_call_argument.info.generic.clone(),
                }
                .into());
            }
        }
    }

    let where_clause = match where_input {
        Some(where_input) => Some(filter::resolve_filter_expression_open_dd(
            where_input,
            usage_counts,
        )?),
        None => None,
    };
    let model_arguments = model_arguments_input
        .map(|arguments_input| {
            arguments::resolve_model_arguments_input_opendd(
                arguments_input,
                &model_source.type_mappings,
                flags,
                usage_counts,
            )
        })
        .transpose()?;

    let order_by = match order_by_input {
        None => vec![],
        Some(order_by_input) => order_by::build_order_by_open_dd_ir(
            order_by_input,
            usage_counts,
            &model_source.data_connector,
        )?,
    };

    let limit: Option<usize> = limit
        .map(|limit| {
            usize::try_from(limit).map_err(|_| error::Error::InvalidLimitValue { value: limit })
        })
        .transpose()?;

    let offset: Option<usize> = offset
        .map(|offset| {
            usize::try_from(offset).map_err(|_| error::Error::InvalidOffsetValue { value: offset })
        })
        .transpose()?;

    Ok(AggregateQuery {
        limit,
        offset,
        where_clause,
        model_arguments,
        order_by,
    })
}
