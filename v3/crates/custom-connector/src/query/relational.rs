use crate::state::AppState;

use axum::{Json, http::StatusCode};
use core::unimplemented;
use datafusion::{
    arrow::datatypes::{DataType, Field, SchemaBuilder, SchemaRef},
    common::{DFSchema, ToDFSchema},
    datasource::{DefaultTableSource, MemTable, TableProvider},
    error::DataFusionError,
    execution::{SessionStateBuilder, runtime_env::RuntimeEnv},
    functions::{string::contains, unicode::substr},
    functions_aggregate::{average, count, min_max, sum},
    functions_window::{expr_fn::row_number, ntile},
    logical_expr::{ExprSchemable, Literal as _, SubqueryAlias, expr::AggregateFunctionParams},
    prelude::{
        ExprFunctionExt, SessionConfig, SessionContext, abs, array_element, btrim, ceil,
        character_length, coalesce, concat, cos, current_date, current_time, date_part, date_trunc,
        exp, floor, get_field, greatest, isnan, iszero, least, left, ln, log, log2, log10, lower,
        lpad, ltrim, now, nullif, nvl, power, random, replace, reverse, right, round, rpad, rtrim,
        sqrt, strpos, substr_index, tan, to_date, to_timestamp, trunc, upper,
    },
    scalar::ScalarValue,
    sql::TableReference,
};
use ndc_models::{
    CaseWhen, JoinType, Relation, RelationalExpression, RelationalLiteral, RelationalQuery, Sort,
};
use serde_arrow::from_record_batch;
use std::collections::BTreeMap;
use std::sync::Arc;

pub type Result<A> = std::result::Result<A, (StatusCode, Json<ndc_models::ErrorResponse>)>;

#[allow(clippy::print_stdout)]
pub async fn execute_relational_query(
    state: &AppState,
    query: &RelationalQuery,
) -> Result<Vec<Vec<serde_json::Value>>> {
    println!("[SELECT]: query={query:?}");

    let logical_plan: datafusion::logical_expr::LogicalPlan =
        convert_relation_to_logical_plan(&query.root_relation, state).map_err(|err| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: err.to_string(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

    let state = SessionStateBuilder::new()
        .with_config(SessionConfig::new())
        .with_runtime_env(Arc::new(RuntimeEnv::default()))
        .with_default_features()
        .build();

    let session_ctx = SessionContext::new();
    let task_ctx = session_ctx.task_ctx();

    let physical_plan = state
        .create_physical_plan(&logical_plan)
        .await
        .map_err(|err| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: err.to_string(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

    let results = datafusion::physical_plan::collect(physical_plan, task_ctx)
        .await
        .map_err(|err| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: err.to_string(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

    // unimplemented: stream the records back
    let mut rows: Vec<Vec<serde_json::Value>> = vec![];

    for batch in results {
        let schema = batch.schema();
        let new_rows = from_record_batch::<Vec<serde_json::Map<String, serde_json::Value>>>(&batch)
            .map_err(|err| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(ndc_models::ErrorResponse {
                        message: err.to_string(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
        for new_row in new_rows {
            let row = convert_fields_object_to_row_vec(schema.fields(), &new_row)?;
            rows.push(row);
        }
    }

    Ok(rows)
}

fn convert_fields_object_to_row_vec(
    fields: &datafusion::arrow::datatypes::Fields,
    row: &serde_json::Map<String, serde_json::Value>,
) -> Result<Vec<serde_json::Value>> {
    let mut row_vec = vec![];
    for field in fields {
        let value = row.get(field.name()).unwrap();
        let vectorified_row = vectorify_row(value.clone(), field.data_type())?;
        row_vec.push(vectorified_row);
    }
    Ok(row_vec)
}

fn vectorify_row(
    value: serde_json::Value,
    data_type: &datafusion::arrow::datatypes::DataType,
) -> Result<serde_json::Value> {
    match value {
        serde_json::Value::Null
        | serde_json::Value::Bool(_)
        | serde_json::Value::Number(_)
        | serde_json::Value::String(_) => Ok(value),
        serde_json::Value::Array(arr) => match data_type {
            datafusion::arrow::datatypes::DataType::List(field_ref) => {
                let mut list_vals = Vec::new();
                for val in arr {
                    let validate_val = vectorify_row(val, field_ref.data_type())?;
                    list_vals.push(validate_val);
                }
                Ok(serde_json::Value::Array(list_vals))
            }
            _ => Err((
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message:
                        "schema mismatch: got array data type, but expected non-array arrow type"
                            .to_string(),
                    details: serde_json::Value::Null,
                }),
            )),
        },
        serde_json::Value::Object(obj) => match data_type {
            datafusion::arrow::datatypes::DataType::Struct(fields) => {
                let struct_row_vec = convert_fields_object_to_row_vec(fields, &obj)?;
                Ok(serde_json::Value::Array(struct_row_vec))
            }
            _ => Err((
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message:
                        "schema mismatch: got object data type, but expected non-struct arrow type"
                            .to_string(),
                    details: serde_json::Value::Null,
                }),
            )),
        },
    }
}

fn convert_relation_to_logical_plan(
    relation: &Relation,
    state: &AppState,
) -> datafusion::error::Result<datafusion::logical_expr::LogicalPlan> {
    match relation {
        Relation::From {
            collection,
            columns,
            arguments,
        } => {
            let table_provider: Arc<dyn TableProvider> =
                get_table_provider(collection, arguments, state)?;

            let table_schema: SchemaRef = table_provider.as_ref().schema();

            let projection = columns
                .iter()
                .map(|column| {
                    table_schema
                        .as_ref()
                        .index_of(column.as_str())
                        .map_err(|e| DataFusionError::ArrowError(e, None))
                })
                .collect::<datafusion::error::Result<Vec<_>>>()?;

            let table_scan_plan = datafusion::logical_expr::LogicalPlan::TableScan(
                datafusion::logical_expr::TableScan::try_new(
                    TableReference::bare(collection.as_str()),
                    Arc::new(DefaultTableSource::new(table_provider)),
                    Some(projection),
                    vec![],
                    None,
                )?,
            );

            Ok(table_scan_plan)
        }
        Relation::Paginate { input, fetch, skip } => {
            let input_plan = convert_relation_to_logical_plan(input, state)?;

            let logical_plan =
                datafusion::logical_expr::LogicalPlan::Limit(datafusion::logical_expr::Limit {
                    input: Arc::new(input_plan),
                    fetch: fetch.map(|fetch| {
                        Box::new(i64::try_from(fetch).expect("cast u64 to i64").lit())
                    }),
                    skip: Some(Box::new(
                        i64::try_from(*skip).expect("cast u64 to i64").lit(),
                    )),
                });

            Ok(logical_plan)
        }
        Relation::Project { input, exprs } => {
            let input_plan = convert_relation_to_logical_plan(input, state)?;

            let mut logical_exprs: Vec<datafusion::logical_expr::Expr> = vec![];
            let mut schema_builder = SchemaBuilder::new();

            for (i, expr) in exprs.iter().enumerate() {
                // add a random number to the column name to avoid collisions
                let name = format!("column_{i}_{rand}", rand = rand::random::<u32>());
                let logical_expr: datafusion::logical_expr::Expr =
                    convert_expression_to_logical_expr(expr, input_plan.schema())?;

                let (data_type, nullable) =
                    logical_expr.data_type_and_nullable(input_plan.schema())?;

                logical_exprs.push(logical_expr.alias(&name));
                schema_builder.push(Field::new(&name, data_type, nullable));
            }

            let explicit_schema = schema_builder.finish().to_dfschema_ref()?;
            let projection_plan = datafusion::logical_expr::LogicalPlan::Projection(
                datafusion::logical_expr::Projection::try_new_with_schema(
                    logical_exprs,
                    Arc::new(input_plan),
                    explicit_schema,
                )?,
            );
            Ok(projection_plan)
        }
        Relation::Filter { input, predicate } => {
            let input_plan = convert_relation_to_logical_plan(input, state)?;
            let predicate = convert_expression_to_logical_expr(predicate, input_plan.schema())?;
            let logical_plan = datafusion::logical_expr::LogicalPlan::Filter(
                datafusion::logical_expr::Filter::try_new(predicate, Arc::new(input_plan))?,
            );
            Ok(logical_plan)
        }
        Relation::Sort { input, exprs } => {
            let input_plan = convert_relation_to_logical_plan(input, state)?;
            let expr = exprs
                .iter()
                .map(|s| convert_sort_to_logical_sort(s, input_plan.schema()))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let logical_plan =
                datafusion::logical_expr::LogicalPlan::Sort(datafusion::logical_expr::Sort {
                    expr,
                    input: Arc::new(input_plan),
                    fetch: None,
                });
            Ok(logical_plan)
        }
        Relation::Join {
            left,
            right,
            on,
            join_type,
        } => {
            let left_plan = convert_relation_to_logical_plan(left, state)?;
            let right_plan = convert_relation_to_logical_plan(right, state)?;

            let left_plan_alias = datafusion::logical_expr::LogicalPlan::SubqueryAlias(
                SubqueryAlias::try_new(Arc::new(left_plan), "left")?,
            );
            let right_plan_alias = datafusion::logical_expr::LogicalPlan::SubqueryAlias(
                SubqueryAlias::try_new(Arc::new(right_plan), "right")?,
            );

            let on_expr = on
                .iter()
                .map(|join_on| {
                    let left = convert_expression_to_logical_expr(
                        &join_on.left,
                        left_plan_alias.schema(),
                    )?;
                    let right = convert_expression_to_logical_expr(
                        &join_on.right,
                        right_plan_alias.schema(),
                    )?;
                    Ok((left, right))
                })
                .collect::<datafusion::error::Result<Vec<_>>>()?;

            let join_type = match join_type {
                JoinType::Left => datafusion::logical_expr::JoinType::Left,
                JoinType::Right => datafusion::logical_expr::JoinType::Right,
                JoinType::Inner => datafusion::logical_expr::JoinType::Inner,
                JoinType::Full => datafusion::logical_expr::JoinType::Full,
                JoinType::LeftAnti => datafusion::logical_expr::JoinType::LeftAnti,
                JoinType::LeftSemi => datafusion::logical_expr::JoinType::LeftSemi,
                JoinType::RightAnti => datafusion::logical_expr::JoinType::RightAnti,
                JoinType::RightSemi => datafusion::logical_expr::JoinType::RightSemi,
            };

            let join_schema = datafusion::logical_expr::build_join_schema(
                left_plan_alias.schema(),
                right_plan_alias.schema(),
                &join_type,
            )?;

            let logical_plan =
                datafusion::logical_expr::LogicalPlan::Join(datafusion::logical_expr::Join {
                    left: Arc::new(left_plan_alias),
                    right: Arc::new(right_plan_alias),
                    on: on_expr,
                    filter: None,
                    join_type,
                    join_constraint: datafusion::common::JoinConstraint::On,
                    schema: Arc::new(join_schema),
                    null_equals_null: false,
                });
            Ok(logical_plan)
        }
        Relation::Aggregate {
            input,
            group_by,
            aggregates,
        } => {
            let input_plan = convert_relation_to_logical_plan(input, state)?;

            let group_by = group_by
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, input_plan.schema()))
                .collect::<datafusion::error::Result<Vec<_>>>()?;

            let aggr_expr = aggregates
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, input_plan.schema()))
                .collect::<datafusion::error::Result<Vec<_>>>()?;

            let aggregate_plan = datafusion::logical_expr::LogicalPlan::Aggregate(
                datafusion::logical_expr::Aggregate::try_new(
                    Arc::new(input_plan),
                    group_by,
                    aggr_expr,
                )?,
            );
            Ok(aggregate_plan)
        }
        Relation::Window { input, exprs } => {
            let input_plan = convert_relation_to_logical_plan(input, state)?;
            let exprs = exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, input_plan.schema()))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let window_plan = datafusion::logical_expr::LogicalPlan::Window(
                datafusion::logical_expr::Window::try_new(exprs, Arc::new(input_plan))?,
            );
            Ok(window_plan)
        }
        Relation::Union { relations } => {
            let input_plans = relations
                .iter()
                .map(|relation| Ok(Arc::new(convert_relation_to_logical_plan(relation, state)?)))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let schema = input_plans[0].schema().as_ref().clone();
            let union_plan =
                datafusion::logical_expr::LogicalPlan::Union(datafusion::logical_expr::Union {
                    inputs: input_plans,
                    schema: Arc::new(schema),
                });
            Ok(union_plan)
        }
    }
}

// return types for tables, with columns / data we don't current support filtered out
fn get_table_provider(
    collection_name: &ndc_models::CollectionName,
    arguments: &BTreeMap<ndc_models::ArgumentName, RelationalLiteral>,
    state: &AppState,
) -> datafusion::error::Result<Arc<dyn TableProvider>> {
    let (rows, collection_fields) = match collection_name.as_str() {
        "actors" => (
            crate::collections::actors::rows(&BTreeMap::new(), state)
                .map_err(|e| DataFusionError::Internal(e.1.0.message))?,
            crate::types::actor::definition().fields,
        ),
        "actors_by_movie" => {
            let movie_id_int: i32 = arguments
                .get("movie_id")
                .and_then(|v| match v {
                    RelationalLiteral::Int64 { value } => {
                        Some(i32::try_from(*value).expect("movie_id out of range"))
                    }
                    _ => None,
                })
                .ok_or_else(|| {
                    DataFusionError::Internal(
                        "actors_by_movie requires a movie_id argument".to_string(),
                    )
                })?;

            (
                crate::collections::actors_by_movie::rows_inner(movie_id_int, state),
                crate::types::actor::definition().fields,
            )
        }
        "countries" => (
            crate::collections::countries::rows(&BTreeMap::new(), state)
                .map_err(|e| DataFusionError::Internal(e.1.0.message))?
                .iter()
                .map(|row| {
                    BTreeMap::from_iter([
                        (
                            "id".into(),
                            row.get(&ndc_models::FieldName::from("id"))
                                .expect("'id' field missing")
                                .clone(),
                        ),
                        (
                            "name".into(),
                            row.get(&ndc_models::FieldName::from("name"))
                                .expect("'name' field missing")
                                .clone(),
                        ),
                        (
                            "area_km2".into(),
                            row.get(&ndc_models::FieldName::from("area_km2"))
                                .expect("'area_km2' field missing")
                                .clone(),
                        ),
                        (
                            "continent_id".into(),
                            row.get(&ndc_models::FieldName::from("continent_id"))
                                .unwrap_or_else(|| &serde_json::Value::Null)
                                .clone(),
                        ),
                    ])
                })
                .collect(),
            crate::types::country::definition()
                .fields
                .into_iter()
                .filter(|(k, _)| k.as_str() != "cities")
                .collect(),
        ),
        "continents" => (
            crate::collections::continents::rows(&BTreeMap::new(), state)
                .map_err(|e| DataFusionError::Internal(e.1.0.message))?,
            crate::types::continent::definition().fields,
        ),
        "movies" => (
            crate::collections::movies::rows(&BTreeMap::new(), state)
                .map_err(|e| DataFusionError::Internal(e.1.0.message))?
                .iter()
                .map(|row| {
                    BTreeMap::from_iter([
                        (
                            "id".into(),
                            row.get(&ndc_models::FieldName::from("id"))
                                .expect("'id' field missing")
                                .clone(),
                        ),
                        (
                            "title".into(),
                            row.get(&ndc_models::FieldName::from("title"))
                                .expect("'title' field missing")
                                .clone(),
                        ),
                        (
                            "rating".into(),
                            row.get(&ndc_models::FieldName::from("rating"))
                                .expect("'rating' field missing")
                                .clone(),
                        ),
                        (
                            "release_date".into(),
                            row.get(&ndc_models::FieldName::from("release_date"))
                                .expect("'release_date' field missing")
                                .clone(),
                        ),
                    ])
                })
                .collect(),
            // Truncate the fields to avoid things we can't support
            BTreeMap::from_iter([
                (
                    "id".into(),
                    ndc_models::ObjectField {
                        description: Some("The movie's primary key".into()),
                        r#type: ndc_models::Type::Named { name: "Int".into() },
                        arguments: BTreeMap::new(),
                    },
                ),
                (
                    "title".into(),
                    ndc_models::ObjectField {
                        description: Some("The movie's title".into()),
                        r#type: ndc_models::Type::Named {
                            name: "String".into(),
                        },
                        arguments: BTreeMap::new(),
                    },
                ),
                (
                    "rating".into(),
                    ndc_models::ObjectField {
                        description: Some("The movie's rating".into()),
                        r#type: ndc_models::Type::Named { name: "Int".into() },
                        arguments: BTreeMap::new(),
                    },
                ),
                (
                    "release_date".into(),
                    ndc_models::ObjectField {
                        description: Some("The movie's release date".into()),
                        r#type: ndc_models::Type::Named {
                            name: "Date".into(),
                        },
                        arguments: BTreeMap::new(),
                    },
                ),
            ]),
        ),
        "institutions" => (
            crate::collections::institutions::rows(&BTreeMap::new(), state)
                .map_err(|e| DataFusionError::Internal(e.1.0.message))?
                .iter()
                .map(|row| {
                    BTreeMap::from_iter([
                        (
                            "id".into(),
                            row.get(&ndc_models::FieldName::from("id"))
                                .expect("'id' field missing")
                                .clone(),
                        ),
                        (
                            "name".into(),
                            row.get(&ndc_models::FieldName::from("name"))
                                .expect("'name' field missing")
                                .clone(),
                        ),
                        (
                            "location".into(),
                            row.get(&ndc_models::FieldName::from("location"))
                                .expect("'location' field missing")
                                .clone(),
                        ),
                        (
                            "staff".into(),
                            row.get(&ndc_models::FieldName::from("staff"))
                                .expect("'staff' field missing")
                                .clone(),
                        ),
                        (
                            "departments".into(),
                            row.get(&ndc_models::FieldName::from("departments"))
                                .expect("'departments' field missing")
                                .clone(),
                        ),
                    ])
                })
                .collect(),
            crate::types::institution::definition().fields,
        ),
        _ => {
            return Err(DataFusionError::Internal(format!(
                "Collection {collection_name} not found"
            )));
        }
    };
    let mut schema_builder = SchemaBuilder::new();
    for (field_name, object_field) in &collection_fields {
        let (data_type, nullable) = to_df_datatype(&object_field.r#type)?;
        schema_builder.push(Field::new(field_name.as_str(), data_type, nullable));
    }

    let schema = schema_builder.finish();
    let records = serde_arrow::to_record_batch(schema.fields(), &rows)
        .map_err(|e| DataFusionError::Internal(e.to_string()))?;

    let mem_table = MemTable::try_new(Arc::new(schema), vec![vec![records]])?;

    Ok(Arc::new(mem_table))
}

fn to_df_datatype(
    ty: &ndc_models::Type,
) -> datafusion::error::Result<(datafusion::arrow::datatypes::DataType, bool)> {
    Ok(match ty {
        ndc_models::Type::Named { name } if name.as_str() == "Int" => {
            (datafusion::arrow::datatypes::DataType::Int64, false)
        }
        ndc_models::Type::Named { name } if name.as_str() == "Int64" => {
            (datafusion::arrow::datatypes::DataType::Utf8, false)
        }
        ndc_models::Type::Named { name } if name.as_str() == "BigInt" => (
            datafusion::arrow::datatypes::DataType::Decimal128(34, 0),
            false,
        ),
        ndc_models::Type::Named { name } if name.as_str() == "String" => {
            (datafusion::arrow::datatypes::DataType::Utf8, false)
        }
        ndc_models::Type::Named { name } if name.as_str() == "Date" => {
            (datafusion::arrow::datatypes::DataType::Date32, false)
        }
        ndc_models::Type::Named { name } if name.as_str() == "location" => {
            (crate::types::location::arrow_type(), true)
        }
        ndc_models::Type::Named { name } if name.as_str() == "staff_member" => {
            (crate::types::staff_member::arrow_type(), true)
        }
        ndc_models::Type::Nullable { underlying_type } => {
            let (dt, _) = to_df_datatype(underlying_type)?;
            (dt, true)
        }
        ndc_models::Type::Array { element_type } => {
            let (dt, _) = to_df_datatype(element_type)?;
            (
                datafusion::arrow::datatypes::DataType::List(Arc::new(Field::new(
                    "item", dt, true,
                ))),
                true,
            )
        }
        _ => {
            return Err(DataFusionError::Internal(format!(
                "Unsupported type: {ty:?}"
            )));
        }
    })
}

fn convert_expression_to_logical_expr(
    expr: &RelationalExpression,
    schema: &DFSchema,
) -> datafusion::error::Result<datafusion::logical_expr::Expr> {
    match expr {
        // Data Selection
        RelationalExpression::Literal { literal } => Ok(datafusion::prelude::Expr::Literal(
            convert_literal_to_logical_expr(literal),None
        )),
        RelationalExpression::Column { index } => Ok(datafusion::prelude::Expr::Column(
            schema.columns()[usize::try_from(*index).expect("cast u64 to usize in column index")]
                .clone(),
        )),

        // Conditional operators
        RelationalExpression::Case { scrutinee, when, default } => {
            let scrutinee_expr = scrutinee
                .as_ref()
                .map(|e| -> datafusion::error::Result<_> {
                    Ok(Box::new(convert_expression_to_logical_expr(e, schema)?))
                })
                .transpose()?;
            let when_then_expr = when
                .iter()
                .map(|case_when: &CaseWhen| {
                    Ok((
                        Box::new(convert_expression_to_logical_expr(&case_when.when, schema)?),
                        Box::new(convert_expression_to_logical_expr(&case_when.then, schema)?),
                    ))
                })
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let else_expr = default
                .as_ref()
                .map(|e| -> datafusion::error::Result<_> {
                    Ok(Box::new(convert_expression_to_logical_expr(e, schema)?))
                })
                .transpose()?;
            Ok(datafusion::prelude::Expr::Case(
                datafusion::logical_expr::Case::new(scrutinee_expr, when_then_expr, else_expr),
            ))
        }

        // Logical operators
        RelationalExpression::And { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::And,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::Or { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Or,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::Not { expr } => Ok(datafusion::prelude::Expr::Not(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),

        // Comparison operators
        RelationalExpression::Eq { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Eq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::NotEq { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::NotEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::Lt { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Lt,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::LtEq { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::LtEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::Gt { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Gt,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::GtEq { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::GtEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::IsNotNull { expr } => Ok(datafusion::prelude::Expr::IsNotNull(
            Box::new(convert_expression_to_logical_expr(expr, schema)?),
        )),
        RelationalExpression::IsNull { expr } => Ok(datafusion::prelude::Expr::IsNull(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        RelationalExpression::IsTrue { expr } => Ok(datafusion::prelude::Expr::IsTrue(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        RelationalExpression::IsFalse { expr } => Ok(datafusion::prelude::Expr::IsFalse(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        RelationalExpression::IsNotTrue { expr } => Ok(datafusion::prelude::Expr::IsNotTrue(
            Box::new(convert_expression_to_logical_expr(expr, schema)?),
        )),
        RelationalExpression::IsNotFalse { expr } => Ok(datafusion::prelude::Expr::IsNotFalse(
            Box::new(convert_expression_to_logical_expr(expr, schema)?),
        )),
        RelationalExpression::IsDistinctFrom { left, right } => Ok(
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::IsDistinctFrom,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            }),
        ),
        RelationalExpression::IsNotDistinctFrom { left, right } => Ok(
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::IsNotDistinctFrom,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            }),
        ),
        RelationalExpression::In { expr, list } => {
            let list = list
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            Ok(convert_expression_to_logical_expr(expr, schema)?.in_list(list, false))
        }
        RelationalExpression::NotIn { expr, list } => {
            let list = list
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            Ok(convert_expression_to_logical_expr(expr, schema)?.in_list(list, true))
        }
        RelationalExpression::Like { expr, pattern } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)?),
                op: datafusion::logical_expr::Operator::LikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)?),
            },
        )),
        RelationalExpression::NotLike { expr, pattern } => Ok(
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)?),
                op: datafusion::logical_expr::Operator::NotLikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)?),
            }),
        ),
        RelationalExpression::ILike { expr, pattern } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)?),
                op: datafusion::logical_expr::Operator::ILikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)?),
            },
        )),
        RelationalExpression::NotILike { expr, pattern } => Ok(
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)?),
                op: datafusion::logical_expr::Operator::NotILikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)?),
            }),
        ),
        RelationalExpression::Between { low, expr, high } => {
            Ok(datafusion::prelude::Expr::between(
                convert_expression_to_logical_expr(expr, schema)?,
                convert_expression_to_logical_expr(low, schema)?,
                convert_expression_to_logical_expr(high, schema)?,
            ))
        }
        RelationalExpression::NotBetween { low, expr, high } => {
            Ok(datafusion::prelude::Expr::not_between(
                convert_expression_to_logical_expr(expr, schema)?,
                convert_expression_to_logical_expr(low, schema)?,
                convert_expression_to_logical_expr(high, schema)?,
            ))
        }
        RelationalExpression::Contains { str, search_str } => {
            Ok(datafusion::prelude::Expr::ScalarFunction(
                datafusion::logical_expr::expr::ScalarFunction {
                    func: contains(),
                    args: vec![
                        convert_expression_to_logical_expr(str, schema)?,
                        convert_expression_to_logical_expr(search_str, schema)?,
                    ],
                },
            ))
        }
        RelationalExpression::BinaryConcat { left, right } => Ok(
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::StringConcat,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            }),
        ),
        RelationalExpression::IsNaN { expr } => {
            Ok(isnan(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::IsZero { expr } => {
            Ok(iszero(convert_expression_to_logical_expr(expr, schema)?))
        }

        // Arithmetic operators
        RelationalExpression::Plus { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Plus,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::Minus { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Minus,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::Multiply { left, right } => Ok(
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Multiply,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            }),
        ),
        RelationalExpression::Divide { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Divide,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::Modulo { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Modulo,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        RelationalExpression::Negate { expr } => Ok(datafusion::prelude::Expr::Negative(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),

        // Scalar functions
        RelationalExpression::Cast { expr, from_type: _, as_type } => {
            convert_expression_to_logical_expr(expr, schema)?
                .cast_to(&convert_cast_type_to_data_type(as_type), schema)
        }
        RelationalExpression::TryCast {
            expr: _,
            from_type: _,
            as_type: _,
        } => unimplemented!(),
        RelationalExpression::Abs { expr } => {
            Ok(abs(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::BTrim { str, trim_str } => Ok(match trim_str {
            None => btrim(vec![convert_expression_to_logical_expr(str, schema)?]),
            Some(trim_str) => btrim(vec![
                convert_expression_to_logical_expr(str, schema)?,
                convert_expression_to_logical_expr(trim_str, schema)?,
            ]),
        }),
        RelationalExpression::Ceil { expr } => {
            Ok(ceil(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::CharacterLength { str } => Ok(character_length(
            convert_expression_to_logical_expr(str, schema)?,
        )),
        RelationalExpression::Coalesce { exprs } => Ok(coalesce(
            exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?,
        )),
        RelationalExpression::Concat { exprs } => Ok(concat(
            exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?,
        )),

        RelationalExpression::Cos { expr } => {
            Ok(cos(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::CurrentDate => Ok(current_date()),
        RelationalExpression::CurrentTime => Ok(current_time()),
        RelationalExpression::CurrentTimestamp => Ok(now()),
        RelationalExpression::DatePart { expr, part } => Ok(date_part(
            convert_date_part_unit_to_literal_expr(*part),
            convert_expression_to_logical_expr(expr, schema)?,
        )),
        RelationalExpression::DateTrunc { expr, part } => Ok(date_trunc(
            convert_expression_to_logical_expr(part, schema)?,
            convert_expression_to_logical_expr(expr, schema)?,
        )),
        RelationalExpression::Exp { expr } => {
            Ok(exp(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::Floor { expr } => {
            Ok(floor(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::ArrayElement { column, index } => Ok(array_element(
            convert_expression_to_logical_expr(column, schema)?,
            datafusion::logical_expr::Expr::Cast(datafusion::logical_expr::Cast {
                data_type: DataType::Int64,
                expr: Box::new(datafusion::logical_expr::Expr::Literal(
                    convert_literal_to_logical_expr(&RelationalLiteral::UInt64 {
                        value: *index as u64,
                    }),None
                )),
            }),
        )),
        RelationalExpression::GetField { column, field } => Ok(get_field(
            convert_expression_to_logical_expr(column, schema)?,
            convert_literal_to_logical_expr(&RelationalLiteral::String {
                value: field.to_string(),
            }),
        )),
        RelationalExpression::Greatest { exprs } => Ok(greatest(
            exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?,
        )),

        RelationalExpression::Least { exprs } => Ok(least(
            exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?,
        )),
        RelationalExpression::Left { str, n } => Ok(left(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(n, schema)?,
        )),
        RelationalExpression::Ln { expr } => {
            Ok(ln(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::Log { expr, base } => Ok(log(
            convert_expression_to_logical_expr(
                base.as_ref()
                    .unwrap_or(&Box::new(RelationalExpression::Literal {
                        literal: RelationalLiteral::Int64 { value: 10 },
                    })),
                schema,
            )?,
            convert_expression_to_logical_expr(expr, schema)?,
        )),
        RelationalExpression::Log10 { expr } => {
            Ok(log10(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::Log2 { expr } => {
            Ok(log2(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::LPad {
            str,
            n,
            padding_str,
        } => Ok(lpad(match padding_str {
            None => vec![
                convert_expression_to_logical_expr(str, schema)?,
                convert_expression_to_logical_expr(n, schema)?,
            ],
            Some(padding_str) => vec![
                convert_expression_to_logical_expr(str, schema)?,
                convert_expression_to_logical_expr(n, schema)?,
                convert_expression_to_logical_expr(padding_str, schema)?,
            ],
        })),
        RelationalExpression::LTrim { str, trim_str } => Ok(ltrim(match trim_str {
            None => vec![convert_expression_to_logical_expr(str, schema)?],
            Some(trim_str) => {
                vec![
                    convert_expression_to_logical_expr(str, schema)?,
                    convert_expression_to_logical_expr(trim_str, schema)?,
                ]
            }
        })),
        RelationalExpression::NullIf { expr1, expr2 } => Ok(nullif(
            convert_expression_to_logical_expr(expr1, schema)?,
            convert_expression_to_logical_expr(expr2, schema)?,
        )),
        RelationalExpression::Nvl { expr1, expr2 } => Ok(nvl(
            convert_expression_to_logical_expr(expr1, schema)?,
            convert_expression_to_logical_expr(expr2, schema)?,
        )),
        RelationalExpression::Power { base, exp } => Ok(power(
            convert_expression_to_logical_expr(base, schema)?,
            convert_expression_to_logical_expr(exp, schema)?,
        )),
        RelationalExpression::Random => Ok(random()),
        RelationalExpression::Replace {
            str,
            substr,
            replacement,
        } => Ok(replace(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(substr, schema)?,
            convert_expression_to_logical_expr(replacement, schema)?,
        )),
        RelationalExpression::Reverse { str } => {
            Ok(reverse(convert_expression_to_logical_expr(str, schema)?))
        }
        RelationalExpression::Right { str, n } => Ok(right(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(n, schema)?,
        )),
        RelationalExpression::Round { expr, prec } => Ok(round(match prec {
            None => vec![convert_expression_to_logical_expr(expr, schema)?],
            Some(prec) => vec![
                convert_expression_to_logical_expr(expr, schema)?,
                convert_expression_to_logical_expr(prec, schema)?,
            ],
        })),
        RelationalExpression::RPad {
            str,
            n,
            padding_str,
        } => Ok(rpad(match padding_str {
            None => vec![
                convert_expression_to_logical_expr(str, schema)?,
                convert_expression_to_logical_expr(n, schema)?,
            ],
            Some(padding_str) => vec![
                convert_expression_to_logical_expr(str, schema)?,
                convert_expression_to_logical_expr(n, schema)?,
                convert_expression_to_logical_expr(padding_str, schema)?,
            ],
        })),
        RelationalExpression::RTrim { str, trim_str } => Ok(rtrim(match trim_str {
            None => vec![convert_expression_to_logical_expr(str, schema)?],
            Some(trim_str) => {
                vec![
                    convert_expression_to_logical_expr(str, schema)?,
                    convert_expression_to_logical_expr(trim_str, schema)?,
                ]
            }
        })),
        RelationalExpression::Sqrt { expr } => {
            Ok(sqrt(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::StrPos { str, substr } => Ok(strpos(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(substr, schema)?,
        )),
        RelationalExpression::Substr {
            str,
            start_pos,
            len,
        } => Ok(datafusion::prelude::Expr::ScalarFunction(
            datafusion::logical_expr::expr::ScalarFunction {
                func: substr(),
                args: match len {
                    None => vec![
                        convert_expression_to_logical_expr(str, schema)?,
                        convert_expression_to_logical_expr(start_pos, schema)?,
                    ],
                    Some(len) => {
                        vec![
                            convert_expression_to_logical_expr(str, schema)?,
                            convert_expression_to_logical_expr(start_pos, schema)?,
                            convert_expression_to_logical_expr(len, schema)?,
                        ]
                    }
                },
            },
        )),
        RelationalExpression::SubstrIndex { str, delim, count } => Ok(substr_index(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(delim, schema)?,
            convert_expression_to_logical_expr(count, schema)?,
        )),
        RelationalExpression::Tan { expr } => {
            Ok(tan(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::ToDate { expr } => {
            Ok(to_date(vec![convert_expression_to_logical_expr(
                expr, schema,
            )?]))
        }
        RelationalExpression::ToLower { expr } => {
            Ok(lower(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::ToTimestamp { expr } => {
            Ok(to_timestamp(vec![convert_expression_to_logical_expr(
                expr, schema,
            )?]))
        }
        RelationalExpression::ToUpper { expr } => {
            Ok(upper(convert_expression_to_logical_expr(expr, schema)?))
        }
        RelationalExpression::Trunc { expr, prec } => Ok(trunc(match prec {
            None => vec![convert_expression_to_logical_expr(expr, schema)?],
            Some(prec) => vec![
                convert_expression_to_logical_expr(expr, schema)?,
                convert_expression_to_logical_expr(prec, schema)?,
            ],
        })),

        // Aggregate functions
        RelationalExpression::Average { expr } => {
            Ok(datafusion::prelude::Expr::AggregateFunction(
                datafusion::logical_expr::expr::AggregateFunction {
                    func: average::avg_udaf(),
                    params: AggregateFunctionParams {
                        args: vec![convert_expression_to_logical_expr(expr, schema)?],
                        distinct: false,
                        filter: None,
                        order_by: None,
                        null_treatment: None,
                    },
                },
            ))
        }
        RelationalExpression::BoolAnd { expr: _ } => unimplemented!(),
        RelationalExpression::BoolOr { expr: _ } => unimplemented!(),
        RelationalExpression::Count { expr, distinct } => {
            Ok(datafusion::prelude::Expr::AggregateFunction(
                datafusion::logical_expr::expr::AggregateFunction {
                    func: count::count_udaf(),
                    params: AggregateFunctionParams {
                        args: vec![convert_expression_to_logical_expr(expr, schema)?],
                        distinct: *distinct,
                        filter: None,
                        order_by: None,
                        null_treatment: None,
                    },
                },
            ))
        }
        RelationalExpression::FirstValue { expr: _ } => unimplemented!(),
        RelationalExpression::LastValue { expr: _ } => unimplemented!(),
        RelationalExpression::Max { expr  } => Ok(datafusion::prelude::Expr::AggregateFunction(
            datafusion::logical_expr::expr::AggregateFunction {
                func: min_max::max_udaf(),
                params: AggregateFunctionParams {
                    args: vec![convert_expression_to_logical_expr(expr, schema)?],
                    distinct: false,
                    filter: None,
                    order_by: None,
                    null_treatment: None,
                },
            },
        )),
        RelationalExpression::Median { expr: _ } => unimplemented!(),
        RelationalExpression::Min { expr } => Ok(datafusion::prelude::Expr::AggregateFunction(
            datafusion::logical_expr::expr::AggregateFunction {
                func: min_max::min_udaf(),
                params: AggregateFunctionParams {
                    args: vec![convert_expression_to_logical_expr(expr, schema)?],
                    distinct: false,
                    filter: None,
                    order_by: None,
                    null_treatment: None,
                },
            },
        )),
        RelationalExpression::StringAgg { expr, separator, distinct, order_by } => {
            let order_by = order_by.as_ref()
                .map(|order_by| {
                    order_by
                        .iter()
                        .map(|s| convert_sort_to_logical_sort(s, schema))
                        .collect::<datafusion::error::Result<Vec<_>>>()
                })
                .transpose()?;
            Ok(datafusion::prelude::Expr::AggregateFunction(
                datafusion::logical_expr::expr::AggregateFunction {
                    func: datafusion::functions_aggregate::string_agg::string_agg_udaf(),
                    params: AggregateFunctionParams {
                        args: vec![convert_expression_to_logical_expr(expr, schema)?, separator.lit()],
                        distinct: *distinct,
                        filter: None,
                        order_by,
                        null_treatment: None,
                    },
                },
            ))
        }
        RelationalExpression::Sum { expr } => Ok(datafusion::prelude::Expr::AggregateFunction(
            datafusion::logical_expr::expr::AggregateFunction {
                func: sum::sum_udaf(),
                params: AggregateFunctionParams {
                    args: vec![convert_expression_to_logical_expr(expr, schema)?],
                    distinct: false,
                    filter: None,
                    order_by: None,
                    null_treatment: None,
                },
            },
        )),
        RelationalExpression::Var { expr: _ } => unimplemented!(),
        RelationalExpression::Stddev { expr } => Ok(datafusion::prelude::Expr::AggregateFunction(
            datafusion::logical_expr::expr::AggregateFunction {
                func: datafusion::functions_aggregate::stddev::stddev_udaf(),
                params: AggregateFunctionParams {
                    args: vec![convert_expression_to_logical_expr(expr, schema)?],
                    distinct: false,
                    filter: None,
                    order_by: None,
                    null_treatment: None,
                },
            },
        )),
        RelationalExpression::StddevPop { expr } => Ok(datafusion::prelude::Expr::AggregateFunction(
            datafusion::logical_expr::expr::AggregateFunction {
                func: datafusion::functions_aggregate::stddev::stddev_pop_udaf(),
                params: AggregateFunctionParams {
                    args: vec![convert_expression_to_logical_expr(expr, schema)?],
                    distinct: false,
                    filter: None,
                    order_by: None,
                    null_treatment: None,
                },
            },
        )),
        RelationalExpression::ApproxPercentileCont { expr, percentile } => {
            Ok(datafusion::prelude::Expr::AggregateFunction(
                datafusion::logical_expr::expr::AggregateFunction {
                    func: datafusion::functions_aggregate::approx_percentile_cont::approx_percentile_cont_udaf(),
                    params: AggregateFunctionParams {
                        args: vec![
                            convert_expression_to_logical_expr(expr, schema)?,
                            percentile.0.lit(),
                        ],
                        distinct: false,
                        filter: None,
                        order_by: None,
                        null_treatment: None,
                    },
                },
            ))
        },
        RelationalExpression::ApproxDistinct { expr } => {
            Ok(datafusion::prelude::Expr::AggregateFunction(
                datafusion::logical_expr::expr::AggregateFunction {
                    func: datafusion::functions_aggregate::approx_distinct::approx_distinct_udaf(),
                    params: AggregateFunctionParams {
                        args: vec![convert_expression_to_logical_expr(expr, schema)?],
                        distinct: false,
                        filter: None,
                        order_by: None,
                        null_treatment: None,
                    },
                },
            ))
        },
        RelationalExpression::ArrayAgg { expr, distinct, order_by } => {
            let order_by = order_by.as_ref()
                .map(|order_by| {
                    order_by
                        .iter()
                        .map(|s| convert_sort_to_logical_sort(s, schema))
                        .collect::<datafusion::error::Result<Vec<_>>>()
                })
                .transpose()?;
            Ok(datafusion::prelude::Expr::AggregateFunction(
                datafusion::logical_expr::expr::AggregateFunction {
                    func: datafusion::functions_aggregate::array_agg::array_agg_udaf(),
                    params: AggregateFunctionParams {
                        args: vec![convert_expression_to_logical_expr(expr, schema)?],
                        distinct: *distinct,
                        filter: None,
                        order_by,
                        null_treatment: None,
                    },
                },
            ))
        },
        // Window functions
        RelationalExpression::RowNumber {
            order_by,
            partition_by,
        } => {
            let order_by = order_by
                .iter()
                .map(|s| convert_sort_to_logical_sort(s, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let partition_by = partition_by
                .iter()
                .map(|s| convert_expression_to_logical_expr(s, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?;

            row_number()
                .order_by(order_by)
                .partition_by(partition_by)
                .build()
        }
        RelationalExpression::DenseRank {
            order_by: _,
            partition_by: _,
        } => unimplemented!(),
        RelationalExpression::NTile {
            order_by,
            partition_by,
            n,
        } => {
            let order_by = order_by
                .iter()
                .map(|s| convert_sort_to_logical_sort(s, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let partition_by = partition_by
                .iter()
                .map(|s| convert_expression_to_logical_expr(s, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?;

            ntile::ntile(n.lit())
                .order_by(order_by)
                .partition_by(partition_by)
                .build()
        }
        RelationalExpression::Rank {
            order_by: _,
            partition_by: _,
        } => unimplemented!(),
        RelationalExpression::CumeDist {
            order_by: _,
            partition_by: _,
        } => unimplemented!(),
        RelationalExpression::PercentRank {
            order_by: _,
            partition_by: _,
        } => unimplemented!(),
    }
}

fn convert_cast_type_to_data_type(
    scalar_type: &ndc_models::CastType,
) -> datafusion::arrow::datatypes::DataType {
    match scalar_type {
        ndc_models::CastType::Boolean => datafusion::arrow::datatypes::DataType::Boolean,
        ndc_models::CastType::Float32 => datafusion::arrow::datatypes::DataType::Float32,
        ndc_models::CastType::Float64 => datafusion::arrow::datatypes::DataType::Float64,
        ndc_models::CastType::Int8 => datafusion::arrow::datatypes::DataType::Int8,
        ndc_models::CastType::Int16 => datafusion::arrow::datatypes::DataType::Int16,
        ndc_models::CastType::Int32 => datafusion::arrow::datatypes::DataType::Int32,
        ndc_models::CastType::Int64 => datafusion::arrow::datatypes::DataType::Int64,
        ndc_models::CastType::UInt8 => datafusion::arrow::datatypes::DataType::UInt8,
        ndc_models::CastType::UInt16 => datafusion::arrow::datatypes::DataType::UInt16,
        ndc_models::CastType::UInt32 => datafusion::arrow::datatypes::DataType::UInt32,
        ndc_models::CastType::UInt64 => datafusion::arrow::datatypes::DataType::UInt64,
        ndc_models::CastType::Decimal128 { scale, prec } => {
            datafusion::arrow::datatypes::DataType::Decimal128(*prec, *scale)
        }
        ndc_models::CastType::Decimal256 { scale, prec } => {
            datafusion::arrow::datatypes::DataType::Decimal256(*prec, *scale)
        }
        ndc_models::CastType::Utf8 => datafusion::arrow::datatypes::DataType::Utf8,
        ndc_models::CastType::Date => datafusion::arrow::datatypes::DataType::Date64,
        ndc_models::CastType::Time => datafusion::arrow::datatypes::DataType::Time64(
            datafusion::arrow::datatypes::TimeUnit::Nanosecond,
        ),
        ndc_models::CastType::Timestamp => datafusion::arrow::datatypes::DataType::Timestamp(
            datafusion::arrow::datatypes::TimeUnit::Nanosecond,
            None,
        ),
        ndc_models::CastType::Duration => datafusion::arrow::datatypes::DataType::Duration(
            datafusion::arrow::datatypes::TimeUnit::Nanosecond,
        ),
        ndc_models::CastType::Interval => datafusion::arrow::datatypes::DataType::Interval(
            datafusion::arrow::datatypes::IntervalUnit::MonthDayNano,
        ),
    }
}

fn convert_literal_to_logical_expr(literal: &RelationalLiteral) -> ScalarValue {
    match literal {
        RelationalLiteral::Null => ScalarValue::Null,
        RelationalLiteral::Boolean { value } => ScalarValue::Boolean(Some(*value)),
        RelationalLiteral::Float32 { value } => ScalarValue::Float32(Some(value.0)),
        RelationalLiteral::Float64 { value } => ScalarValue::Float64(Some(value.0)),
        RelationalLiteral::Int8 { value } => ScalarValue::Int8(Some(*value)),
        RelationalLiteral::Int16 { value } => ScalarValue::Int16(Some(*value)),
        RelationalLiteral::Int32 { value } => ScalarValue::Int32(Some(*value)),
        RelationalLiteral::Int64 { value } => ScalarValue::Int64(Some(*value)),
        RelationalLiteral::UInt8 { value } => ScalarValue::UInt8(Some(*value)),
        RelationalLiteral::UInt16 { value } => ScalarValue::UInt16(Some(*value)),
        RelationalLiteral::UInt32 { value } => ScalarValue::UInt32(Some(*value)),
        RelationalLiteral::UInt64 { value } => ScalarValue::UInt64(Some(*value)),
        RelationalLiteral::Decimal128 { value, scale, prec } => {
            ScalarValue::Decimal128(Some(*value), *prec, *scale)
        }
        RelationalLiteral::Decimal256 { value, scale, prec } => ScalarValue::Decimal256(
            datafusion::arrow::datatypes::i256::from_string(value),
            *prec,
            *scale,
        ),
        RelationalLiteral::String { value } => ScalarValue::Utf8(Some(value.clone())),
        RelationalLiteral::Date32 { value } => ScalarValue::Date32(Some(*value)),
        RelationalLiteral::Date64 { value } => ScalarValue::Date64(Some(*value)),
        RelationalLiteral::Time32Second { value } => ScalarValue::Time32Second(Some(*value)),
        RelationalLiteral::Time32Millisecond { value } => {
            ScalarValue::Time32Millisecond(Some(*value))
        }
        RelationalLiteral::Time64Microsecond { value } => {
            ScalarValue::Time64Microsecond(Some(*value))
        }
        RelationalLiteral::Time64Nanosecond { value } => {
            ScalarValue::Time64Nanosecond(Some(*value))
        }
        RelationalLiteral::TimestampSecond { value } => {
            ScalarValue::TimestampSecond(Some(*value), None)
        }
        RelationalLiteral::TimestampMillisecond { value } => {
            ScalarValue::TimestampMillisecond(Some(*value), None)
        }
        RelationalLiteral::TimestampMicrosecond { value } => {
            ScalarValue::TimestampMicrosecond(Some(*value), None)
        }
        RelationalLiteral::TimestampNanosecond { value } => {
            ScalarValue::TimestampNanosecond(Some(*value), None)
        }
        RelationalLiteral::DurationSecond { value } => ScalarValue::DurationSecond(Some(*value)),
        RelationalLiteral::DurationMillisecond { value } => {
            ScalarValue::DurationMillisecond(Some(*value))
        }
        RelationalLiteral::DurationMicrosecond { value } => {
            ScalarValue::DurationMicrosecond(Some(*value))
        }
        RelationalLiteral::DurationNanosecond { value } => {
            ScalarValue::DurationNanosecond(Some(*value))
        }
        RelationalLiteral::Interval {
            months,
            days,
            nanoseconds,
        } => ScalarValue::IntervalMonthDayNano(Some(
            datafusion::arrow::datatypes::IntervalMonthDayNano::new(*months, *days, *nanoseconds),
        )),
    }
}

fn convert_sort_to_logical_sort(
    sort: &Sort,
    schema: &DFSchema,
) -> datafusion::error::Result<datafusion::logical_expr::SortExpr> {
    Ok(datafusion::logical_expr::SortExpr {
        expr: convert_expression_to_logical_expr(&sort.expr, schema)?,
        asc: match sort.direction {
            ndc_models::OrderDirection::Asc => true,
            ndc_models::OrderDirection::Desc => false,
        },
        nulls_first: match sort.nulls_sort {
            ndc_models::NullsSort::NullsFirst => true,
            ndc_models::NullsSort::NullsLast => false,
        },
    })
}

fn convert_date_part_unit_to_literal_expr(
    part: ndc_models::DatePartUnit,
) -> datafusion::logical_expr::Expr {
    datafusion::logical_expr::Expr::Literal(
        ScalarValue::Utf8(Some(String::from(match part {
            ndc_models::DatePartUnit::Year => "year",
            ndc_models::DatePartUnit::Quarter => "quarter",
            ndc_models::DatePartUnit::Month => "month",
            ndc_models::DatePartUnit::Week => "week",
            ndc_models::DatePartUnit::DayOfWeek => "dow",
            ndc_models::DatePartUnit::DayOfYear => "doy",
            ndc_models::DatePartUnit::Day => "day",
            ndc_models::DatePartUnit::Hour => "hour",
            ndc_models::DatePartUnit::Minute => "minute",
            ndc_models::DatePartUnit::Second => "second",
            ndc_models::DatePartUnit::Microsecond => "microsecond",
            ndc_models::DatePartUnit::Millisecond => "millisecond",
            ndc_models::DatePartUnit::Nanosecond => "nanosecond",
            ndc_models::DatePartUnit::Epoch => "epoch",
        }))),
        None,
    )
}
