use std::{
    cmp::{Ord, Ordering},
    collections::{BTreeMap, HashSet},
};

use axum::{http::StatusCode, Json};
use indexmap::IndexMap;
use ndc_models;
use regex::Regex;

use crate::{
    arguments::apply_arguments,
    collections::get_collection_by_name,
    state::{AppState, Row},
};

pub type Result<A> = std::result::Result<A, (StatusCode, Json<ndc_models::ErrorResponse>)>;

const DEFAULT_VARIABLE_SETS: &[BTreeMap<ndc_models::VariableName, serde_json::Value>] =
    &[BTreeMap::new()];

pub fn execute_query_request(
    state: &AppState,
    request: &ndc_models::QueryRequest,
) -> Result<ndc_models::QueryResponse> {
    let variable_sets: &[BTreeMap<ndc_models::VariableName, serde_json::Value>] = request
        .variables
        .as_deref()
        .unwrap_or(DEFAULT_VARIABLE_SETS);
    let mut row_sets = vec![];
    for variables in variable_sets {
        let row_set = execute_query_with_variables(
            &request.collection,
            &request.arguments,
            &request.collection_relationships,
            &request.query,
            variables,
            state,
        )?;
        row_sets.push(row_set);
    }

    Ok(ndc_models::QueryResponse(row_sets))
}

fn execute_query_with_variables(
    collection: &ndc_models::CollectionName,
    arguments: &BTreeMap<ndc_models::ArgumentName, ndc_models::Argument>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    query: &ndc_models::Query,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
) -> Result<ndc_models::RowSet> {
    let mut argument_values = BTreeMap::new();

    for (argument_name, argument_value) in arguments {
        if argument_values
            .insert(
                argument_name.clone(),
                eval_argument(variables, argument_value)?,
            )
            .is_some()
        {
            return Err((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "duplicate argument names".into(),
                    details: serde_json::Value::Null,
                }),
            ));
        }
    }

    let collection = get_collection_by_name(collection, &argument_values, state)?;

    execute_query(
        collection_relationships,
        variables,
        state,
        query,
        Root::CurrentRow,
        collection,
    )
}

#[derive(Clone, Copy)]
enum Root<'a> {
    /// References to the root collection actually
    /// refer to the current row, because the path to
    /// the nearest enclosing [`ndc_models::Query`] does not pass
    /// an [`ndc_models::Expression::Exists`] node.
    CurrentRow,
    /// References to the root collection refer to the
    /// explicitly-identified row, which is the row
    /// being evaluated in the context of the nearest enclosing
    /// [`ndc_models::Query`].
    ExplicitRow(&'a Row),
}

fn execute_query(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    query: &ndc_models::Query,
    root: Root,
    collection: Vec<Row>,
) -> Result<ndc_models::RowSet> {
    let sorted = sort(
        collection_relationships,
        variables,
        state,
        collection,
        query.order_by.as_ref(),
    )?;

    let filtered: Vec<Row> = (match &query.predicate {
        None => Ok(sorted),
        Some(expr) => {
            let mut filtered: Vec<Row> = vec![];
            for item in sorted {
                let root = match root {
                    Root::CurrentRow => &item,
                    Root::ExplicitRow(root) => root,
                };
                if eval_expression(
                    collection_relationships,
                    variables,
                    state,
                    expr,
                    root,
                    &item,
                )? {
                    filtered.push(item);
                }
            }
            Ok(filtered)
        }
    })?;

    let paginated: Vec<Row> = paginate(filtered.into_iter(), query.limit, query.offset);

    let aggregates = query
        .aggregates
        .as_ref()
        .map(|aggregates| {
            let mut row: IndexMap<ndc_models::FieldName, serde_json::Value> = IndexMap::new();
            for (aggregate_name, aggregate) in aggregates {
                row.insert(
                    aggregate_name.clone(),
                    eval_aggregate(aggregate, &paginated)?,
                );
            }
            Ok(row)
        })
        .transpose()?;

    let rows = query
        .fields
        .as_ref()
        .map(|fields| {
            let mut rows: Vec<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>> = vec![];
            for item in &paginated {
                let row = eval_row(fields, collection_relationships, variables, state, item)?;
                rows.push(row);
            }
            Ok(rows)
        })
        .transpose()?;

    Ok(ndc_models::RowSet {
        aggregates,
        rows,
        groups: None,
    })
}

fn eval_row(
    fields: &IndexMap<ndc_models::FieldName, ndc_models::Field>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    item: &BTreeMap<ndc_models::FieldName, serde_json::Value>,
) -> Result<IndexMap<ndc_models::FieldName, ndc_models::RowFieldValue>> {
    let mut row = IndexMap::new();
    for (field_name, field) in fields {
        row.insert(
            field_name.clone(),
            eval_field(collection_relationships, variables, state, field, item)?,
        );
    }
    Ok(row)
}

fn eval_aggregate(
    aggregate: &ndc_models::Aggregate,
    paginated: &[BTreeMap<ndc_models::FieldName, serde_json::Value>],
) -> Result<serde_json::Value> {
    match aggregate {
        ndc_models::Aggregate::StarCount {} => Ok(serde_json::Value::from(paginated.len())),
        ndc_models::Aggregate::ColumnCount {
            column,
            arguments: _,
            field_path,
            distinct,
        } => {
            let values = paginated
                .iter()
                .map(|row| {
                    let column_value = row.get(column).ok_or((
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: "invalid column name".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))?;
                    let field_path_slice = field_path
                        .as_ref()
                        .map_or_else(|| [].as_ref(), |p| p.as_slice());
                    extract_nested_field(column_value, field_path_slice)
                })
                .collect::<Result<Vec<_>>>()?;

            let non_null_values = values.iter().filter(|value| !value.is_null());

            let agg_value = if *distinct {
                non_null_values
                    .map(|value| {
                        serde_json::to_string(value).map_err(|_| {
                            (
                                StatusCode::INTERNAL_SERVER_ERROR,
                                Json(ndc_models::ErrorResponse {
                                    message: "unable to encode value".into(),
                                    details: serde_json::Value::Null,
                                }),
                            )
                        })
                    })
                    .collect::<Result<HashSet<_>>>()?
                    .len()
            } else {
                non_null_values.count()
            };
            serde_json::to_value(agg_value).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(ndc_models::ErrorResponse {
                        message: " ".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })
        }
        ndc_models::Aggregate::SingleColumn {
            column,
            arguments: _,
            field_path,
            function,
        } => {
            let values = paginated
                .iter()
                .map(|row| {
                    let column_value = row.get(column).ok_or((
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: "invalid column name".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))?;
                    let field_path_slice = field_path
                        .as_ref()
                        .map_or_else(|| [].as_ref(), |p| p.as_slice());
                    extract_nested_field(column_value, field_path_slice)
                })
                .collect::<Result<Vec<_>>>()?;
            eval_aggregate_function(function, &values)
        }
    }
}

fn extract_nested_field<'a>(
    value: &'a serde_json::Value,
    field_path: &[ndc_models::FieldName],
) -> Result<&'a serde_json::Value> {
    if let Some((field, remaining_field_path)) = field_path.split_first() {
        // Short circuit on null values
        if value.is_null() {
            return Ok(value);
        }

        let object_value = value.as_object().ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message:
                        "expected object value when extracting a nested field from a field path"
                            .into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

        let field_value = object_value.get(field.as_str()).ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("could not find field {field} in nested object"),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

        extract_nested_field(field_value, remaining_field_path)
    } else {
        Ok(value)
    }
}

fn eval_aggregate_function(
    function: &ndc_models::AggregateFunctionName,
    values: &[&serde_json::Value],
) -> Result<serde_json::Value> {
    if let Some((first_value, _)) = values.split_first() {
        if first_value.is_i64() {
            eval_aggregate_function_i64(function, values)
        } else if first_value.is_string() {
            eval_aggregate_function_string(function, values)
        } else {
            Err((
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: "Can only aggregate over i64 or string values".into(),
                    details: serde_json::Value::Null,
                }),
            ))
        }
    } else {
        // This is a bit of a hack, as technically what this value is should be dependent
        // on what type the aggregate function operand is, but it is valid while we only
        // support min and max aggregate functions.
        Ok(serde_json::Value::Null)
    }
}

fn eval_aggregate_function_i64(
    function: &ndc_models::AggregateFunctionName,
    values: &[&serde_json::Value],
) -> Result<serde_json::Value> {
    let int_values = values
        .iter()
        .filter_map(|value| {
            if value.is_null() {
                None
            } else {
                Some(value.as_i64().ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: "aggregate value is not an integer".into(),
                            details: (*value).clone(),
                        }),
                    )
                }))
            }
        })
        .collect::<Result<Vec<_>>>()?;

    let agg_value = match function.as_str() {
        "min" => Ok(int_values.into_iter().min()),
        "max" => Ok(int_values.into_iter().max()),
        _ => Err((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "invalid integer aggregation function".into(),
                details: serde_json::Value::String(function.to_string()),
            }),
        )),
    }?;

    serde_json::to_value(agg_value).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })
}

fn eval_aggregate_function_string(
    function: &ndc_models::AggregateFunctionName,
    values: &[&serde_json::Value],
) -> Result<serde_json::Value> {
    let str_values = values
        .iter()
        .filter_map(|value| {
            if value.is_null() {
                None
            } else {
                Some(value.as_str().ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: "aggregate value is not a string".into(),
                            details: (*value).clone(),
                        }),
                    )
                }))
            }
        })
        .collect::<Result<Vec<_>>>()?;

    let agg_value = match function.as_str() {
        "min" => Ok(str_values.into_iter().min()),
        "max" => Ok(str_values.into_iter().max()),
        _ => Err((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "invalid string aggregation function".into(),
                details: serde_json::Value::String(function.to_string()),
            }),
        )),
    }?;

    serde_json::to_value(agg_value).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })
}

fn sort(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    collection: Vec<Row>,
    order_by: Option<&ndc_models::OrderBy>,
) -> Result<Vec<Row>> {
    match order_by {
        None => Ok(collection),
        Some(order_by) => {
            let mut copy = vec![];
            for item_to_insert in collection {
                let mut index = 0;
                for other in &copy {
                    if let Ordering::Greater = eval_order_by(
                        collection_relationships,
                        variables,
                        state,
                        order_by,
                        other,
                        &item_to_insert,
                    )? {
                        break;
                    }
                    index += 1;
                }
                copy.insert(index, item_to_insert);
            }
            Ok(copy)
        }
    }
}

fn paginate<I: Iterator<Item = Row>>(
    collection: I,
    limit: Option<u32>,
    offset: Option<u32>,
) -> Vec<Row> {
    let start = offset.unwrap_or(0).try_into().unwrap();
    match limit {
        Some(n) => collection.skip(start).take(n.try_into().unwrap()).collect(),
        None => collection.skip(start).collect(),
    }
}

fn eval_order_by(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    order_by: &ndc_models::OrderBy,
    t1: &Row,
    t2: &Row,
) -> Result<Ordering> {
    let mut result = Ordering::Equal;

    for element in &order_by.elements {
        let v1 = eval_order_by_element(collection_relationships, variables, state, element, t1)?;
        let v2 = eval_order_by_element(collection_relationships, variables, state, element, t2)?;
        let x = match element.order_direction {
            ndc_models::OrderDirection::Asc => compare(v1, v2)?,
            ndc_models::OrderDirection::Desc => compare(v2, v1)?,
        };
        result = result.then(x);
    }

    Ok(result)
}

fn compare(v1: serde_json::Value, v2: serde_json::Value) -> Result<Ordering> {
    match (v1, v2) {
        (serde_json::Value::Null, serde_json::Value::Null) => Ok(Ordering::Equal),
        (serde_json::Value::Null, _) => Ok(Ordering::Less),
        (_, serde_json::Value::Null) => Ok(Ordering::Greater),

        (serde_json::Value::Bool(b1), serde_json::Value::Bool(b2)) => Ok(b1.cmp(&b2)),
        (serde_json::Value::Number(n1), serde_json::Value::Number(n2)) => {
            if n1.as_f64().unwrap() < n2.as_f64().unwrap() {
                Ok(Ordering::Less)
            } else {
                Ok(Ordering::Greater)
            }
        }
        (serde_json::Value::String(s1), serde_json::Value::String(s2)) => Ok(s1.cmp(&s2)),
        _ => Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "cannot compare values".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}

fn eval_order_by_element(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    element: &ndc_models::OrderByElement,
    item: &Row,
) -> Result<serde_json::Value> {
    match &element.target {
        ndc_models::OrderByTarget::Column {
            name,
            arguments: _,
            field_path,
            path,
        } => eval_order_by_column(
            collection_relationships,
            variables,
            state,
            item,
            path,
            name,
            field_path.as_ref(),
        ),
        ndc_models::OrderByTarget::Aggregate { aggregate, path } => match aggregate {
            ndc_models::Aggregate::ColumnCount {
                column,
                arguments: _,
                field_path: _,
                distinct,
            } => eval_order_by_column_count_aggregate(
                collection_relationships,
                variables,
                state,
                item,
                path,
                column,
                *distinct,
            ),
            ndc_models::Aggregate::SingleColumn {
                column,
                arguments: _,
                field_path: _,
                function,
            } => eval_order_by_single_column_aggregate(
                collection_relationships,
                variables,
                state,
                item,
                path,
                column,
                function,
            ),
            ndc_models::Aggregate::StarCount {} => eval_order_by_star_count_aggregate(
                collection_relationships,
                variables,
                state,
                item,
                path,
            ),
        },
    }
}

fn eval_order_by_star_count_aggregate(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    item: &Row,
    path: &[ndc_models::PathElement],
) -> Result<serde_json::Value> {
    let rows: Vec<Row> = eval_path(collection_relationships, variables, state, path, item)?;
    Ok(rows.len().into())
}

fn eval_order_by_column_count_aggregate(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    item: &Row,
    path: &[ndc_models::PathElement],
    column: &ndc_models::FieldName,
    distinct: bool,
) -> Result<serde_json::Value> {
    let rows: Vec<Row> = eval_path(collection_relationships, variables, state, path, item)?;
    let values = rows
        .iter()
        .map(|row| {
            row.get(column).ok_or((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "invalid column name".into(),
                    details: serde_json::Value::Null,
                }),
            ))
        })
        .collect::<Result<Vec<_>>>()?;
    let non_null_value_count = if distinct {
        values
            .iter()
            .filter(|column_value| !column_value.is_null())
            .collect::<HashSet<_>>()
            .len()
    } else {
        values
            .iter()
            .filter(|column_value| !column_value.is_null())
            .count()
    };
    Ok(serde_json::Value::from(non_null_value_count))
}

fn eval_order_by_single_column_aggregate(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    item: &Row,
    path: &[ndc_models::PathElement],
    column: &ndc_models::FieldName,
    function: &ndc_models::AggregateFunctionName,
) -> Result<serde_json::Value> {
    let rows: Vec<Row> = eval_path(collection_relationships, variables, state, path, item)?;
    let values = rows
        .iter()
        .map(|row| {
            row.get(column).ok_or((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "invalid column name".into(),
                    details: serde_json::Value::Null,
                }),
            ))
        })
        .collect::<Result<Vec<_>>>()?;
    eval_aggregate_function(function, &values)
}

fn eval_order_by_column(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    item: &Row,
    path: &[ndc_models::PathElement],
    name: &ndc_models::FieldName,
    field_path: Option<&Vec<ndc_models::FieldName>>,
) -> Result<serde_json::Value> {
    let rows: Vec<Row> = eval_path(collection_relationships, variables, state, path, item)?;
    if rows.len() > 1 {
        return Err((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        ));
    }
    match rows.first() {
        Some(row) => eval_column_field_path(row, name, field_path),
        None => Ok(serde_json::Value::Null),
    }
}

fn eval_path(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    path: &[ndc_models::PathElement],
    item: &Row,
) -> Result<Vec<Row>> {
    let mut result: Vec<Row> = vec![item.clone()];

    for path_element in path {
        if !state.enable_relationship_support {
            return Err((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "Relationships are not supported".into(),
                    details: serde_json::Value::Null,
                }),
            ));
        }

        let relationship_name = path_element.relationship.as_str();
        let relationship = collection_relationships.get(relationship_name).ok_or((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "invalid relationship name in path".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        result = eval_path_element(
            collection_relationships,
            variables,
            state,
            relationship,
            &path_element.arguments,
            &result,
            path_element.predicate.as_deref(),
        )?;
    }

    Ok(result)
}

fn eval_path_element(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    relationship: &ndc_models::Relationship,
    arguments: &BTreeMap<ndc_models::ArgumentName, ndc_models::RelationshipArgument>,
    source: &[Row],
    predicate: Option<&ndc_models::Expression>,
) -> Result<Vec<Row>> {
    let mut matching_rows: Vec<Row> = vec![];

    // Note: Join strategy
    //
    // Rows can be related in two ways: 1) via a column mapping, and
    // 2) via collection arguments. Because collection arguments can be computed
    // using the columns on the source side of a relationship, in general
    // we need to compute the target collection once for each source row.
    // This join strategy can result in some target rows appearing in the
    // resulting row set more than once, if two source rows are both related
    // to the same target row.
    //
    // In practice, this is not an issue, either because a) the relationship
    // is computed in the course of evaluating a predicate, and all predicates are
    // implicitly or explicitly existentially quantified, or b) if the
    // relationship is computed in the course of evaluating an ordering, the path
    // should consist of all object relationships, and possibly terminated by a
    // single array relationship, so there should be no double counting.

    for src_row in source {
        let mut all_arguments = BTreeMap::new();

        for (argument_name, argument_value) in &relationship.arguments {
            if all_arguments
                .insert(
                    argument_name.clone(),
                    eval_relationship_argument(variables, src_row, argument_value)?,
                )
                .is_some()
            {
                return Err((
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "duplicate argument names".into(),
                        details: serde_json::Value::Null,
                    }),
                ));
            }
        }

        for (argument_name, argument_value) in arguments {
            if all_arguments
                .insert(
                    argument_name.clone(),
                    eval_relationship_argument(variables, src_row, argument_value)?,
                )
                .is_some()
            {
                return Err((
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "duplicate argument names".into(),
                        details: serde_json::Value::Null,
                    }),
                ));
            }
        }

        let target =
            get_collection_by_name(&relationship.target_collection, &all_arguments, state)?;

        for tgt_row in &target {
            if let Some(predicate) = predicate {
                if eval_column_mapping(relationship, src_row, tgt_row)?
                    && eval_expression(
                        collection_relationships,
                        variables,
                        state,
                        predicate,
                        tgt_row,
                        tgt_row,
                    )?
                {
                    matching_rows.push(tgt_row.clone());
                }
            } else if eval_column_mapping(relationship, src_row, tgt_row)? {
                matching_rows.push(tgt_row.clone());
            }
        }
    }

    Ok(matching_rows)
}

fn eval_argument(
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    argument: &ndc_models::Argument,
) -> Result<serde_json::Value> {
    match argument {
        ndc_models::Argument::Variable { name } => {
            let value = variables
                .get(name)
                .ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "invalid variable name".into(),
                        details: serde_json::Value::Null,
                    }),
                ))
                .cloned()?;
            Ok(value)
        }
        ndc_models::Argument::Literal { value } => Ok(value.clone()),
    }
}

fn eval_relationship_argument(
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    row: &Row,
    argument: &ndc_models::RelationshipArgument,
) -> Result<serde_json::Value> {
    match argument {
        ndc_models::RelationshipArgument::Variable { name } => {
            let value = variables
                .get(name)
                .ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "invalid variable name".into(),
                        details: serde_json::Value::Null,
                    }),
                ))
                .cloned()?;
            Ok(value)
        }
        ndc_models::RelationshipArgument::Literal { value } => Ok(value.clone()),
        ndc_models::RelationshipArgument::Column { name } => eval_column(row, name),
    }
}

fn eval_expression(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    expr: &ndc_models::Expression,
    root: &Row,
    item: &Row,
) -> Result<bool> {
    match expr {
        ndc_models::Expression::And { expressions } => {
            for expr in expressions {
                if !eval_expression(collection_relationships, variables, state, expr, root, item)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        ndc_models::Expression::Or { expressions } => {
            for expr in expressions {
                if eval_expression(collection_relationships, variables, state, expr, root, item)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
        ndc_models::Expression::Not { expression } => {
            let b = eval_expression(
                collection_relationships,
                variables,
                state,
                expression,
                root,
                item,
            )?;
            Ok(!b)
        }

        ndc_models::Expression::UnaryComparisonOperator { column, operator } => match operator {
            ndc_models::UnaryComparisonOperator::IsNull => {
                let val = eval_comparison_target(column, item)?;
                Ok(val.is_null())
            }
        },

        ndc_models::Expression::BinaryComparisonOperator {
            column,
            operator,
            value,
        } => match operator.as_str() {
            "_eq" => {
                let left_val = eval_comparison_target(column, item)?;
                let right_vals =
                    eval_comparison_value(collection_relationships, variables, state, value, item)?;
                for right_val in &right_vals {
                    if left_val == *right_val {
                        return Ok(true);
                    }
                }

                Ok(false)
            }
            "like" => {
                let column_val = eval_comparison_target(column, item)?;
                let regex_vals =
                    eval_comparison_value(collection_relationships, variables, state, value, item)?;
                for regex_val in &regex_vals {
                    let column_str = column_val.as_str().ok_or((
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: "column is not a string".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))?;
                    let regex_str = regex_val.as_str().ok_or((
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: " ".into(),
                            details: serde_json::Value::Null,
                        }),
                    ))?;
                    let regex = Regex::new(regex_str).map_err(|_| {
                        (
                            StatusCode::BAD_REQUEST,
                            Json(ndc_models::ErrorResponse {
                                message: "invalid regular expression".into(),
                                details: serde_json::Value::Null,
                            }),
                        )
                    })?;
                    if regex.is_match(column_str) {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            _op => Err((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: format!("operator '{_op}' not supported"),
                    details: serde_json::Value::Null,
                }),
            )),
        },

        ndc_models::Expression::ArrayComparison { .. } => Err((
            StatusCode::NOT_IMPLEMENTED,
            Json(ndc_models::ErrorResponse {
                message: "array comparison in expression is not supported".to_string(),
                details: serde_json::Value::Null,
            }),
        )),

        ndc_models::Expression::Exists {
            in_collection,
            predicate,
        } => {
            let query = ndc_models::Query {
                aggregates: None,
                groups: None,
                fields: Some(IndexMap::new()),
                limit: None,
                offset: None,
                order_by: None,
                predicate: predicate.clone().map(|exp| *exp),
            };
            let collection = eval_in_collection(
                collection_relationships,
                item,
                variables,
                state,
                in_collection,
            )?;
            let row_set = execute_query(
                collection_relationships,
                variables,
                state,
                &query,
                Root::ExplicitRow(root),
                collection,
            )?;
            let rows: Vec<IndexMap<_, _>> = row_set.rows.ok_or((
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: " ".into(),
                    details: serde_json::Value::Null,
                }),
            ))?;
            Ok(!rows.is_empty())
        }
    }
}

fn eval_in_collection(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    item: &Row,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    in_collection: &ndc_models::ExistsInCollection,
) -> Result<Vec<Row>> {
    match in_collection {
        ndc_models::ExistsInCollection::Related {
            relationship,
            field_path: _,
            arguments,
        } => {
            if !state.enable_relationship_support {
                return Err((
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "Relationships are not supported".into(),
                        details: serde_json::Value::Null,
                    }),
                ));
            }

            let relationship = collection_relationships.get(relationship.as_str()).ok_or((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: " ".into(),
                    details: serde_json::Value::Null,
                }),
            ))?;
            let source = vec![item.clone()];
            eval_path_element(
                collection_relationships,
                variables,
                state,
                relationship,
                arguments,
                &source,
                Some(Box::new(ndc_models::Expression::And {
                    expressions: vec![],
                }))
                .as_deref(),
            )
        }
        ndc_models::ExistsInCollection::Unrelated {
            collection,
            arguments,
        } => {
            let arguments = arguments
                .iter()
                .map(|(k, v)| Ok((k.clone(), eval_relationship_argument(variables, item, v)?)))
                .collect::<Result<BTreeMap<_, _>>>()?;

            get_collection_by_name(collection, &arguments, state)
        }
        ndc_models::ExistsInCollection::NestedCollection { .. } => Err((
            StatusCode::NOT_IMPLEMENTED,
            Json(ndc_models::ErrorResponse {
                message: "ExistsInCollection::NestedCollection is not supported".to_string(),
                details: serde_json::Value::Null,
            }),
        )),
        ndc_models::ExistsInCollection::NestedScalarCollection { .. } => Err((
            StatusCode::NOT_IMPLEMENTED,
            Json(ndc_models::ErrorResponse {
                message: "ExistsInCollection::NestedScalarCollection is not supported".to_string(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}

fn eval_comparison_target(
    target: &ndc_models::ComparisonTarget,
    item: &Row,
) -> Result<serde_json::Value> {
    match target {
        ndc_models::ComparisonTarget::Column {
            name,
            arguments: _,
            field_path,
        } => Ok(eval_column_field_path(item, name, field_path.as_ref())?),
        ndc_models::ComparisonTarget::Aggregate {
            aggregate: _,
            path: _,
        } => Err((
            StatusCode::NOT_IMPLEMENTED,
            Json(ndc_models::ErrorResponse {
                message: "aggregate predicates not supported".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}

fn eval_column_field_path(
    row: &Row,
    column_name: &ndc_models::FieldName,
    field_path: Option<&Vec<ndc_models::FieldName>>,
) -> Result<serde_json::Value> {
    let column_value = eval_column(row, column_name)?;
    Ok(match field_path {
        None => column_value,
        Some(path) => path
            .iter()
            .try_fold(&column_value, |value, field_name| {
                value.get(field_name.as_str())
            })
            .cloned()
            .unwrap_or(serde_json::Value::Null),
    })
}

fn eval_column(row: &Row, column_name: &ndc_models::FieldName) -> Result<serde_json::Value> {
    row.get(column_name).cloned().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "invalid column name".into(),
            details: serde_json::Value::Null,
        }),
    ))
}

fn eval_comparison_value(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    comparison_value: &ndc_models::ComparisonValue,
    item: &Row,
) -> Result<Vec<serde_json::Value>> {
    match comparison_value {
        ndc_models::ComparisonValue::Column {
            name,
            arguments: _,
            field_path,
            path,
            scope: _,
        } => {
            let rows = eval_path(collection_relationships, variables, state, path, item)?;
            let mut values = vec![];
            for row in &rows {
                let value = eval_column_field_path(row, name, field_path.as_ref())?;
                values.push(value);
            }
            Ok(values)
        }
        ndc_models::ComparisonValue::Scalar { value } => Ok(vec![value.clone()]),
        ndc_models::ComparisonValue::Variable { name } => {
            let value = variables
                .get(name.as_str())
                .ok_or((
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "invalid variable name".into(),
                        details: serde_json::Value::Null,
                    }),
                ))
                .cloned()?;
            Ok(vec![value])
        }
    }
}

pub(crate) fn eval_nested_field(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    value: serde_json::Value,
    nested_field: &ndc_models::NestedField,
) -> Result<ndc_models::RowFieldValue> {
    if value.is_null() {
        return Ok(ndc_models::RowFieldValue(value));
    }
    match nested_field {
        ndc_models::NestedField::Object(nested_object) => {
            let full_row: Row = serde_json::from_value(value).map_err(|_| {
                (
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "Expected object".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
            let row = eval_row(
                &nested_object.fields,
                collection_relationships,
                variables,
                state,
                &full_row,
            )?;
            Ok(ndc_models::RowFieldValue(
                serde_json::to_value(row).map_err(|_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(ndc_models::ErrorResponse {
                            message: "Cannot encode rowset".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?,
            ))
        }
        ndc_models::NestedField::Array(ndc_models::NestedArray { fields }) => {
            let array: Vec<serde_json::Value> = serde_json::from_value(value).map_err(|_| {
                (
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "Expected array".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
            let result_array = array
                .into_iter()
                .map(|value| {
                    eval_nested_field(collection_relationships, variables, state, value, fields)
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(ndc_models::RowFieldValue(
                serde_json::to_value(result_array).map_err(|_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(ndc_models::ErrorResponse {
                            message: "Cannot encode rowset".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?,
            ))
        }
        ndc_models::NestedField::Collection(ndc_models::NestedCollection { .. }) => Err((
            StatusCode::NOT_IMPLEMENTED,
            Json(ndc_models::ErrorResponse {
                message: "Nested field collections are not supported".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}

fn eval_field(
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    variables: &BTreeMap<ndc_models::VariableName, serde_json::Value>,
    state: &AppState,
    field: &ndc_models::Field,
    item: &Row,
) -> Result<ndc_models::RowFieldValue> {
    match field {
        ndc_models::Field::Column {
            column,
            fields,
            arguments,
        } => {
            let col_val = eval_column(item, column)?;
            let result = match fields {
                None => Ok(ndc_models::RowFieldValue(col_val)),
                Some(nested_field) => eval_nested_field(
                    collection_relationships,
                    variables,
                    state,
                    col_val,
                    nested_field,
                ),
            }?;
            apply_arguments(result, arguments, variables)
        }
        ndc_models::Field::Relationship {
            relationship,
            arguments,
            query,
        } => {
            if !state.enable_relationship_support {
                return Err((
                    StatusCode::BAD_REQUEST,
                    Json(ndc_models::ErrorResponse {
                        message: "Relationships are not supported".into(),
                        details: serde_json::Value::Null,
                    }),
                ));
            }

            let relationship = collection_relationships
                .get(relationship.as_str())
                .ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: format!("Unknown relationship {relationship}"),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;
            let source = vec![item.clone()];
            let collection = eval_path_element(
                collection_relationships,
                variables,
                state,
                relationship,
                arguments,
                &source,
                Some(&Box::new(ndc_models::Expression::And {
                    expressions: vec![],
                })),
            )?;
            let rows = execute_query(
                collection_relationships,
                variables,
                state,
                query,
                Root::CurrentRow,
                collection,
            )?;
            let rows_json = serde_json::to_value(rows).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(ndc_models::ErrorResponse {
                        message: "cannot encode rowset".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
            Ok(ndc_models::RowFieldValue(rows_json))
        }
    }
}

fn eval_column_mapping(
    relationship: &ndc_models::Relationship,
    src_row: &Row,
    tgt_row: &Row,
) -> Result<bool> {
    for (src_column, tgt_column) in &relationship.column_mapping {
        let src_value = eval_column(src_row, src_column)?;
        let tgt_value = eval_column(tgt_row, tgt_column.first().unwrap())?; // tgt_column should only contain one element until relationships.nested capability is enabled
        if src_value != tgt_value {
            return Ok(false);
        }
    }
    Ok(true)
}
