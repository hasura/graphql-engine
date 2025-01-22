use super::{field_selection, model_target};

use crate::column::to_resolved_column;
use crate::types::PlanError;
use indexmap::IndexMap;
use nonempty::NonEmpty;
use open_dds::aggregates::{
    AggregateExpressionName, AggregationFunctionName, DataConnectorAggregationFunctionName,
};
use open_dds::identifier::SubgraphName;
use std::collections::BTreeMap;
use std::sync::Arc;

use hasura_authn_core::Session;
use metadata_resolve::{Metadata, Qualified};
use open_dds::query::{Aggregate, AggregationFunction, ModelSelection, ModelTarget, Operand};
use plan_types::{
    AggregateFieldSelection, AggregateSelectionSet, ExecutionTree, FieldsSelection, JoinLocations,
    NdcFieldAlias, PredicateQueryTrees, QueryExecutionPlan, QueryNodeNew, UniqueNumber,
};

pub fn from_model_aggregate_selection(
    model_target: &ModelTarget,
    selection: &IndexMap<String, Aggregate>,
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<ExecutionTree, PlanError> {
    let qualified_model_name = metadata_resolve::Qualified::new(
        model_target.subgraph.clone(),
        model_target.model_name.clone(),
    );

    let model = metadata.models.get(&qualified_model_name).ok_or_else(|| {
        PlanError::Internal(format!(
            "model {qualified_model_name} not found in metadata"
        ))
    })?;

    let model_source = model.model.source.as_ref().ok_or_else(|| {
        PlanError::Internal(format!("model {qualified_model_name} has no source"))
    })?;

    let model_object_type = metadata
        .object_types
        .get(&model.model.data_type)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "object type {} not found in metadata",
                model.model.data_type
            ))
        })?;

    let data_connector = &model_source.data_connector;
    let ndc_version = data_connector.capabilities.supported_ndc_version;

    let mut fields = IndexMap::new();

    for (field_alias, aggregate) in selection {
        let resolved_column = match aggregate.operand.as_ref() {
            None => Ok(None),
            Some(Operand::Field(operand)) => {
                let column = to_resolved_column(
                    metadata,
                    &model_source.type_mappings,
                    &model.model.data_type,
                    model_object_type,
                    operand,
                )?;
                Ok(Some(column))
            }
            Some(_) => Err(PlanError::Internal("unsupported aggregate operand".into())),
        }?;

        let column_path = match resolved_column.clone() {
            None => vec![],
            Some(column) => [vec![column.column_name], column.field_path].concat(),
        };

        let ndc_aggregate = match &aggregate.function {
            AggregationFunction::Count {} => {
                Ok::<_, PlanError>(AggregateFieldSelection::Count { column_path })
            }
            AggregationFunction::CountDistinct {} => {
                Ok::<_, PlanError>(AggregateFieldSelection::CountDistinct { column_path })
            }
            AggregationFunction::Sum
            | AggregationFunction::Min
            | AggregationFunction::Max
            | AggregationFunction::Average => {
                let resolved_column = resolved_column.ok_or(
                    PlanError::Internal(
                        "column shouldn't be empty for aggregation function {aggregation_function_name}"
                            .into(),
                    ))?;
                let Some(column_path) = NonEmpty::from_vec(column_path) else {
                    Err(PlanError::Internal(
                                        "column path shouldn't be empty for aggregation function {aggregation_function_name}"
                                            .into(),
                                    ))?
                };
                let aggregate_functions = resolved_column.field_mapping.aggregate_functions.ok_or(
                    PlanError::Internal(format!(
                        "no aggregate functions defined for field {}",
                        field_alias.as_str()
                    )),
                )?;
                let data_connector_function_name = match &aggregate.function {
                    AggregationFunction::Sum => aggregate_functions
                        .get_sum_function(ndc_version)
                        .ok_or_else(|| {
                            PlanError::Internal(format!(
                                "no 'sum' operator found for type: {:?}",
                                resolved_column.field_mapping.column_type
                            ))
                        }),

                    AggregationFunction::Min => aggregate_functions
                        .get_min_function(ndc_version)
                        .ok_or_else(|| {
                            PlanError::Internal(format!(
                                "no 'min' operator found for type: {:?}",
                                resolved_column.field_mapping.column_type
                            ))
                        }),

                    AggregationFunction::Max => aggregate_functions
                        .get_max_function(ndc_version)
                        .ok_or_else(|| {
                            PlanError::Internal(format!(
                                "no 'max' operator found for type: {:?}",
                                resolved_column.field_mapping.column_type
                            ))
                        }),

                    AggregationFunction::Average => aggregate_functions
                        .get_avg_function(ndc_version)
                        .ok_or_else(|| {
                            PlanError::Internal(format!(
                                "no 'average' operator found for type: {:?}",
                                resolved_column.field_mapping.column_type
                            ))
                        }),

                    _ => {
                        panic!(
                            "invalid pattern match in from_model_aggregate_selection: {:?}",
                            aggregate.function
                        )
                    }
                }?;

                Ok(AggregateFieldSelection::AggregationFunction {
                    function_name: data_connector_function_name.clone(),
                    column_path,
                })
            }
            AggregationFunction::Custom {
                name: aggregation_function_name,
                expression: aggregate_expression,
            } => {
                let ndc_aggregation_function_name = get_ndc_aggregation_function(
                    metadata,
                    &model_target.subgraph,
                    &model_source.data_connector,
                    aggregation_function_name,
                    aggregate_expression,
                )?;
                let Some(column_path) = NonEmpty::from_vec(column_path) else {
                    Err(PlanError::Internal(
                                    "column path shouldn't be empty for aggregation function {aggregation_function_name}"
                                        .into(),
                                ))?
                };
                Ok(AggregateFieldSelection::AggregationFunction {
                    function_name: ndc_aggregation_function_name,
                    column_path,
                })
            }
        }?;

        fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_aggregate);
    }

    let query = model_target::model_target_to_ndc_query(
        model_target,
        session,
        metadata,
        request_headers,
        model,
        model_source,
        model_object_type,
        unique_number,
    )?;

    let query_aggregate_fields = if fields.is_empty() {
        None
    } else {
        Some(AggregateSelectionSet {
            fields: fields.clone(),
        })
    };

    // only send an ordering if there are actually elements
    let order_by = if query.order_by.is_empty() {
        None
    } else {
        Some(query.order_by.clone())
    };

    let query_execution_plan = QueryExecutionPlan {
        query_node: QueryNodeNew {
            fields: None,
            aggregates: query_aggregate_fields,
            limit: query.limit,
            offset: query.offset,
            order_by,
            predicate: query.filter.clone(),
        },
        collection: query.collection_name.clone(),
        arguments: query.arguments.clone(),
        collection_relationships: query.collection_relationships.clone(),
        variables: None,
        data_connector: query.data_connector,
    };

    Ok(ExecutionTree {
        query_execution_plan,
        remote_predicates: PredicateQueryTrees::new(),
        remote_join_executions: JoinLocations::new(),
    })
}

fn get_ndc_aggregation_function(
    metadata: &Metadata,
    subgraph: &SubgraphName,
    data_connector: &metadata_resolve::DataConnectorLink,
    aggregate_function: &AggregationFunctionName,
    aggregate_expression: &AggregateExpressionName,
) -> Result<DataConnectorAggregationFunctionName, PlanError> {
    let aggregate_expression_name = Qualified::new(subgraph.clone(), aggregate_expression.clone());
    let ndc_aggregation_function_info = metadata
        .aggregate_expressions
        .get(&aggregate_expression_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "aggregate expression {aggregate_expression} not found in metadata"
            ))
        })?
        .operand.aggregation_functions.iter().find(|info| info.name == *aggregate_function).ok_or_else(|| {
            PlanError::Internal(format!(
                "aggregation function {aggregate_function} not found in aggregate expression {aggregate_expression}"
            ))
        })?
        .data_connector_functions.iter().find(|info| {
            info.data_connector_name == data_connector.name
        } ).ok_or_else(|| {
            PlanError::Internal(format!(
                "aggregation function {aggregate_function} is missing a data connector mapping for {}", data_connector.name
            ))
        })?;
    Ok(ndc_aggregation_function_info.function_name.clone())
}

pub fn from_model_selection(
    model_selection: &ModelSelection,
    metadata: &Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<ExecutionTree, PlanError> {
    let mut remote_predicates = PredicateQueryTrees::new();
    let mut remote_join_executions = JoinLocations::new();

    let model_target = &model_selection.target;
    let qualified_model_name = metadata_resolve::Qualified::new(
        model_target.subgraph.clone(),
        model_target.model_name.clone(),
    );

    let model = metadata.models.get(&qualified_model_name).ok_or_else(|| {
        PlanError::Internal(format!(
            "model {qualified_model_name} not found in metadata"
        ))
    })?;

    let model_source = model.model.source.as_ref().ok_or_else(|| {
        PlanError::Internal(format!("model {qualified_model_name} has no source"))
    })?;

    let model_object_type = metadata
        .object_types
        .get(&model.model.data_type)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "object type {} not found in metadata",
                model.model.data_type
            ))
        })?;

    let mut relationships = BTreeMap::new();

    let ndc_fields = field_selection::resolve_field_selection(
        metadata,
        session,
        request_headers,
        &model.model.data_type,
        model_object_type,
        &model_source.type_mappings,
        &model_source.data_connector,
        &model_selection.selection,
        &mut relationships,
        &mut remote_join_executions,
        &mut remote_predicates,
        unique_number,
    )?;

    let mut query = model_target::model_target_to_ndc_query(
        model_target,
        session,
        metadata,
        request_headers,
        model,
        model_source,
        model_object_type,
        unique_number,
    )?;

    // collect relationships accummulated in this scope.
    query.collection_relationships.append(&mut relationships);

    let query_fields: Option<FieldsSelection> = if ndc_fields.is_empty() {
        None
    } else {
        Some(FieldsSelection { fields: ndc_fields })
    };

    // only send an ordering if there are actually elements
    let order_by = if query.order_by.is_empty() {
        None
    } else {
        Some(query.order_by.clone())
    };

    let query_execution_plan = QueryExecutionPlan {
        query_node: QueryNodeNew {
            fields: query_fields,
            aggregates: None,
            limit: query.limit,
            offset: query.offset,
            order_by,
            predicate: query.filter.clone(),
        },
        collection: query.collection_name.clone(),
        arguments: query.arguments.clone(),
        collection_relationships: query.collection_relationships.clone(),
        variables: None,
        data_connector: query.data_connector.clone(),
    };

    Ok(ExecutionTree {
        query_execution_plan,
        remote_predicates,
        remote_join_executions,
    })
}
