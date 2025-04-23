use super::{field_selection, model_target};

use crate::types::PlanError;
use crate::{OutputObjectTypeView, column::to_resolved_column};
use indexmap::IndexMap;
use nonempty::NonEmpty;
use open_dds::{
    aggregates::{
        AggregateExpressionName, AggregationFunctionName, DataConnectorAggregationFunctionName,
    },
    models::ModelName,
};
use std::collections::BTreeMap;

use hasura_authn_core::Session;
use metadata_resolve::{FieldMapping, Metadata, NdcVersion, Qualified};
use open_dds::query::{
    Aggregate, AggregationFunction, ExtractionFunction, ModelDimensions, ModelSelection,
    ModelTarget, Name, ObjectFieldOperand, Operand,
};
use plan_types::{
    AggregateFieldSelection, AggregateSelectionSet, FieldsSelection, Grouping, JoinLocations,
    NdcFieldAlias, PredicateQueryTrees, QueryExecutionPlan, QueryExecutionTree, QueryNode,
    UniqueNumber,
};

pub fn from_model_group_by(
    model_target: &ModelTarget,
    selection: &IndexMap<Name, Aggregate>,
    model_dimensions: &ModelDimensions,
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<QueryExecutionTree, PlanError> {
    let mut remote_predicates = PredicateQueryTrees::new();

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

    let model_object_type = crate::metadata_accessor::get_output_object_type(
        metadata,
        &model.model.data_type,
        &session.role,
    )?;

    let data_connector = &model_source.data_connector;
    let ndc_version = data_connector.capabilities.supported_ndc_version;

    let mut dimensions: IndexMap<NdcFieldAlias, plan_types::Dimension> = IndexMap::new();

    for (
        field_alias,
        open_dds::query::Dimension::Field {
            column: operand,
            extraction,
        },
    ) in &model_dimensions.dimensions
    {
        let dimension = match operand {
            Operand::Field(operand) => {
                let column = to_resolved_column(
                    &session.role,
                    metadata,
                    &model_source.type_mappings,
                    &model_object_type,
                    operand,
                )?;
                let field_mapping: FieldMapping = column.field_mapping;
                let extraction_functions = field_mapping
                    .extraction_functions
                    .ok_or_else(|| PlanError::Internal("no extraction functions".to_string()))?;
                let extraction = match extraction {
                    None => None,
                    Some(extraction) => Some(match extraction {
                        ExtractionFunction::Year => {
                            extraction_functions.year_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "year extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Month => {
                            extraction_functions.month_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "month extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Day => {
                            extraction_functions.day_function.ok_or_else(|| {
                                PlanError::Internal("day extraction function not found".to_string())
                            })
                        }
                        ExtractionFunction::Nanosecond => {
                            extraction_functions.nanosecond_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "nanosecond extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Microsecond => {
                            extraction_functions.microsecond_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "microsecond extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Millisecond => {
                            extraction_functions.millisecond_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "millisecond extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Second => {
                            extraction_functions.second_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "second extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Minute => {
                            extraction_functions.minute_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "minute extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Hour => {
                            extraction_functions.hour_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "hour extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Week => {
                            extraction_functions.week_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "week extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Quarter => {
                            extraction_functions.quarter_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "quarter extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::DayOfWeek => {
                            extraction_functions.day_of_week_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "day_of_week extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::DayOfYear => {
                            extraction_functions.day_of_year_function.ok_or_else(|| {
                                PlanError::Internal(
                                    "day_of_year extraction function not found".to_string(),
                                )
                            })
                        }
                        ExtractionFunction::Custom { name } => extraction_functions
                            .other_functions
                            .into_iter()
                            .find(|f| f.as_str() == name.as_str())
                            .ok_or_else(|| {
                                PlanError::Internal(format!(
                                    "unsupported extraction function {name:?}"
                                ))
                            }),
                    }?),
                };
                Ok(plan_types::Dimension::Column {
                    column_path: nonempty::NonEmpty {
                        head: column.column_name,
                        tail: column.field_path,
                    },
                    extraction,
                })
            }
            _ => Err(PlanError::Internal(format!(
                "Unsupported dimension in from_model_group_by: {operand:?}"
            ))),
        }?;

        dimensions.insert(NdcFieldAlias::from(field_alias.as_str()), dimension);
    }

    let mut aggregates = IndexMap::new();

    for (field_alias, aggregate) in selection {
        let ndc_aggregate = to_ndc_aggregate(
            metadata,
            session,
            &model.model.name,
            &model_object_type,
            model_source,
            model.model.aggregate_expression.as_ref(),
            aggregate,
            field_alias,
            ndc_version,
        )?;
        aggregates.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_aggregate);
    }

    let query = model_target::model_target_to_ndc_query(
        model_target,
        session,
        metadata,
        request_headers,
        model,
        model_source,
        &model_object_type,
        &mut remote_predicates,
        unique_number,
    )?;

    // only send an ordering if there are actually elements
    let order_by = if query.order_by.is_empty() {
        None
    } else {
        Some(query.order_by.clone())
    };

    let limit = model_dimensions
        .limit
        .map(u32::try_from)
        .transpose()
        .map_err(|_| PlanError::Internal("limit out of range".into()))?;

    let offset: Option<u32> = model_dimensions
        .offset
        .map(u32::try_from)
        .transpose()
        .map_err(|_| PlanError::Internal("offset out of range".into()))?;

    let query_execution_plan = QueryExecutionPlan {
        query_node: QueryNode {
            fields: None,
            aggregates: None,
            limit: query.limit,
            offset: query.offset,
            order_by,
            predicate: query.filter.clone(),
            group_by: Some(Grouping {
                aggregates,
                dimensions,
                limit,
                offset,
            }),
        },
        collection: query.collection_name.clone(),
        arguments: query.arguments.clone(),
        collection_relationships: query.collection_relationships.clone(),
        variables: None,
        data_connector: query.data_connector,
    };

    Ok(QueryExecutionTree {
        query_execution_plan,
        remote_predicates,
        remote_join_executions: JoinLocations::new(),
    })
}

pub fn from_model_aggregate_selection(
    model_target: &ModelTarget,
    selection: &IndexMap<Name, Aggregate>,
    metadata: &Metadata,
    session: &Session,
    relationship_aggregate_expression: Option<&Qualified<AggregateExpressionName>>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<QueryExecutionTree, PlanError> {
    let mut remote_predicates = PredicateQueryTrees::new();

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

    //... and use them to check permissoins when fetching the object type
    let model_object_type = crate::metadata_accessor::get_output_object_type(
        metadata,
        &model.model.data_type,
        &session.role,
    )?;

    let data_connector = &model_source.data_connector;
    let ndc_version = data_connector.capabilities.supported_ndc_version;

    let mut fields = IndexMap::new();

    // Use relationship aggregate expression if provided, otherwise use the model's
    // aggregate expression.
    let aggregate_expression_name =
        relationship_aggregate_expression.or(model.model.aggregate_expression.as_ref());

    for (field_alias, aggregate) in selection {
        let ndc_aggregate = to_ndc_aggregate(
            metadata,
            session,
            &model.model.name,
            &model_object_type,
            model_source,
            aggregate_expression_name,
            aggregate,
            field_alias,
            ndc_version,
        )?;

        fields.insert(NdcFieldAlias::from(field_alias.as_str()), ndc_aggregate);
    }

    let query = model_target::model_target_to_ndc_query(
        model_target,
        session,
        metadata,
        request_headers,
        model,
        model_source,
        &model_object_type,
        &mut remote_predicates,
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
        query_node: QueryNode {
            fields: None,
            aggregates: query_aggregate_fields,
            limit: query.limit,
            offset: query.offset,
            order_by,
            predicate: query.filter.clone(),
            group_by: None,
        },
        collection: query.collection_name.clone(),
        arguments: query.arguments.clone(),
        collection_relationships: query.collection_relationships.clone(),
        variables: None,
        data_connector: query.data_connector,
    };

    Ok(QueryExecutionTree {
        query_execution_plan,
        remote_predicates,
        remote_join_executions: JoinLocations::new(),
    })
}

/// Only field operands are supported for aggregation
fn extract_field_operand_for_aggregation(
    operand: &Operand,
) -> Result<ObjectFieldOperand, PlanError> {
    match operand {
        Operand::Field(operand) => Ok(operand.clone()),
        Operand::Relationship(_) | Operand::RelationshipAggregate(_) => {
            Err(PlanError::Internal("unsupported aggregate operand".into()))
        }
    }
}

fn to_ndc_aggregate(
    metadata: &Metadata,
    session: &Session,
    model_name: &Qualified<ModelName>,
    model_object_type: &OutputObjectTypeView,
    model_source: &metadata_resolve::ModelSource,
    aggregate_expression: Option<&Qualified<AggregateExpressionName>>,
    aggregate: &Aggregate,
    field_alias: &Name,
    ndc_version: NdcVersion,
) -> Result<AggregateFieldSelection, PlanError> {
    let resolved_column = aggregate
        .operand
        .as_ref()
        .map(|operand| {
            let field_operand = extract_field_operand_for_aggregation(operand)?;
            to_resolved_column(
                &session.role,
                metadata,
                &model_source.type_mappings,
                model_object_type,
                &field_operand,
            )
        })
        .transpose()?;
    let column_path = match resolved_column.clone() {
        None => vec![],
        Some(column) => [vec![column.column_name], column.field_path].concat(),
    };

    match &aggregate.function {
        AggregationFunction::Count {} => Ok(AggregateFieldSelection::Count { column_path }),
        AggregationFunction::CountDistinct {} => {
            Ok(AggregateFieldSelection::CountDistinct { column_path })
        }
        AggregationFunction::Sum
        | AggregationFunction::Min
        | AggregationFunction::Max
        | AggregationFunction::Average => {
            let resolved_column = resolved_column.ok_or_else(|| {
                PlanError::Internal(
                "column shouldn't be empty for aggregation function {aggregation_function_name}"
                    .into(),
            )
            })?;
            let Some(column_path) = NonEmpty::from_vec(column_path) else {
                Err(PlanError::Internal(
                                    "column path shouldn't be empty for aggregation function {aggregation_function_name}"
                                        .into(),
                                ))?
            };
            let aggregate_functions = resolved_column
                .field_mapping
                .aggregate_functions
                .ok_or_else(|| {
                    PlanError::Internal(format!(
                        "no aggregate functions defined for field {}",
                        field_alias.as_str()
                    ))
                })?;
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
        } => {
            let aggregate_expression_name = aggregate_expression.ok_or_else( || {
                PlanError::Internal(format!(
                    "Custom aggregation function '{aggregation_function_name}' requires an aggregate expression defined on the model {model_name}",
                ))
            })?;
            let ndc_aggregation_function_name = get_ndc_aggregation_function(
                metadata,
                &model_source.data_connector,
                aggregation_function_name,
                aggregate_expression_name,
                aggregate.operand.as_ref(),
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
    }
}

// Traverse the operand tree to find the NDC aggregation function name
// from the aggregate expression mappings.
fn get_ndc_aggregation_function(
    metadata: &Metadata,
    data_connector: &metadata_resolve::DataConnectorLink,
    aggregate_function: &AggregationFunctionName,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    operand: Option<&Operand>,
) -> Result<DataConnectorAggregationFunctionName, PlanError> {
    // Fetch the aggregate expression
    let aggregate_expression = metadata
        .aggregate_expressions
        .get(aggregate_expression_name)
        .ok_or_else(|| {
            PlanError::Internal(format!(
                "aggregate expression {aggregate_expression_name} not found in metadata"
            ))
        })?;
    match operand {
        None => {
            // No operand found. This is a leaf scalar expression. Find the NDC aggregate function name from the aggregate expression definition.
            let ndc_aggregation_function_info = aggregate_expression.operand.aggregation_functions.iter().find(|info| info.name == *aggregate_function).ok_or_else(|| {
                    PlanError::Internal(format!(
                        "aggregation function {aggregate_function} not found in aggregate expression {aggregate_expression_name}"
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
        Some(operand) => {
            // Extract the field operand
            let ObjectFieldOperand { target, nested } =
                extract_field_operand_for_aggregation(operand)?;
            // Get the field's referenced aggregate expression
            let field_aggregate_expression_name = &aggregate_expression
                .operand
                .aggregatable_fields
                .iter()
                .find(|field| field.field_name == target.field_name)
                .ok_or_else(|| {
                    PlanError::Internal(format!(
                        "field {} not found in aggregate expression {aggregate_expression_name}",
                        &target.field_name
                    ))
                })?
                .aggregate_expression;
            // Recursively resolve the NDC aggregation function name
            get_ndc_aggregation_function(
                metadata,
                data_connector,
                aggregate_function,
                field_aggregate_expression_name,
                nested.as_ref().map(std::convert::AsRef::as_ref), // &Box<T> -> &T in this closure
            )
        }
    }
}

pub fn from_model_selection(
    model_selection: &ModelSelection,
    metadata: &Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<QueryExecutionTree, PlanError> {
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

    let model_object_type = crate::metadata_accessor::get_output_object_type(
        metadata,
        &model.model.data_type,
        &session.role,
    )?;

    let mut relationships = BTreeMap::new();

    let ndc_fields = field_selection::resolve_field_selection(
        metadata,
        session,
        request_headers,
        &model_object_type,
        &model_source.type_mappings,
        &model_source.data_connector,
        &model_selection.selection,
        metadata_resolve::FieldNestedness::NotNested,
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
        &model_object_type,
        &mut remote_predicates,
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
        query_node: QueryNode {
            fields: query_fields,
            aggregates: None,
            limit: query.limit,
            offset: query.offset,
            order_by,
            predicate: query.filter.clone(),
            group_by: None,
        },
        collection: query.collection_name.clone(),
        arguments: query.arguments.clone(),
        collection_relationships: query.collection_relationships.clone(),
        variables: None,
        data_connector: query.data_connector.clone(),
    };

    Ok(QueryExecutionTree {
        query_execution_plan,
        remote_predicates,
        remote_join_executions,
    })
}
