use crate::state::AppState;

use axum::{http::StatusCode, Json};
use core::unimplemented;
use datafusion::{
    arrow::datatypes::{Field, SchemaBuilder, SchemaRef},
    common::{DFSchema, ToDFSchema},
    datasource::{DefaultTableSource, MemTable, TableProvider},
    error::DataFusionError,
    execution::{runtime_env::RuntimeEnv, SessionStateBuilder},
    functions::{string::contains, unicode::substr},
    functions_aggregate::{count::Count, sum::Sum},
    functions_window::{expr_fn::row_number, ntile},
    logical_expr::{build_join_schema, AggregateUDF, ExprSchemable, Literal as _, SubqueryAlias},
    prelude::{
        abs, btrim, ceil, character_length, coalesce, concat, cos, current_date, current_time,
        date_part, date_trunc, exp, floor, greatest, isnan, iszero, least, left, ln, log, log10,
        log2, lower, lpad, ltrim, now, nullif, nvl, power, random, replace, reverse, right, round,
        rpad, rtrim, sqrt, strpos, substr_index, tan, to_date, to_timestamp, trunc, upper,
        ExprFunctionExt, SessionConfig, SessionContext,
    },
    scalar::ScalarValue,
    sql::TableReference,
};
use plan_pushdown_types::{CaseWhen, Expression, JoinType, Literal, Rel, Sort};
use serde_arrow::from_record_batch;
use std::collections::BTreeMap;
use std::sync::Arc;

pub type Result<A> = std::result::Result<A, (StatusCode, Json<ndc_models::ErrorResponse>)>;

pub async fn execute_query_rel(
    state: &AppState,
    plan: &Rel,
) -> Result<Vec<Vec<serde_json::Value>>> {
    let logical_plan: datafusion::logical_expr::LogicalPlan =
        convert_plan_to_logical_plan(plan, state).map_err(|err| {
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
        let new_rows = from_record_batch::<Vec<BTreeMap<String, serde_json::Value>>>(&batch)
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
            let mut row = vec![];
            for field in schema.fields() {
                row.push(new_row.get(field.name()).unwrap().clone());
            }
            rows.push(row);
        }
    }

    Ok(rows)
}

fn convert_plan_to_logical_plan(
    plan: &Rel,
    state: &AppState,
) -> datafusion::error::Result<datafusion::logical_expr::LogicalPlan> {
    match plan {
        Rel::From {
            collection,
            columns,
        } => {
            let table_provider: Arc<dyn TableProvider> = get_table_provider(collection, state)?;
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
            let logical_plan = datafusion::logical_expr::LogicalPlan::TableScan(
                datafusion::logical_expr::TableScan::try_new(
                    TableReference::bare(collection.as_str()),
                    Arc::new(DefaultTableSource::new(table_provider)),
                    Some(projection),
                    vec![],
                    None,
                )?,
            );

            Ok(logical_plan)
        }
        Rel::Limit { input, fetch, skip } => {
            let input_plan = convert_plan_to_logical_plan(input, state)?;
            let logical_plan =
                datafusion::logical_expr::LogicalPlan::Limit(datafusion::logical_expr::Limit {
                    input: Arc::new(input_plan),
                    fetch: fetch.map(|fetch| {
                        Box::new(i64::try_from(fetch).expect("cast usize to i64").lit())
                    }),
                    skip: Some(Box::new(
                        i64::try_from(*skip).expect("cast usize to i64").lit(),
                    )),
                });
            Ok(logical_plan)
        }
        Rel::Project { input, exprs } => {
            let input_plan = convert_plan_to_logical_plan(input, state)?;

            let mut logical_exprs: Vec<datafusion::logical_expr::Expr> = vec![];
            let mut schema_builder = SchemaBuilder::new();

            for (i, expr) in exprs.iter().enumerate() {
                let name = format!("column_{i}");
                let logical_expr: datafusion::logical_expr::Expr =
                    convert_expression_to_logical_expr(expr, input_plan.schema())?;
                let (data_type, nullable) =
                    logical_expr.data_type_and_nullable(input_plan.schema())?;
                logical_exprs.push(logical_expr.alias(&name));
                schema_builder.push(Field::new(&name, data_type, nullable));
            }

            let explicit_schema = schema_builder.finish().to_dfschema_ref()?;
            let logical_plan = datafusion::logical_expr::LogicalPlan::Projection(
                datafusion::logical_expr::Projection::try_new_with_schema(
                    logical_exprs,
                    Arc::new(input_plan),
                    explicit_schema,
                )?,
            );
            Ok(logical_plan)
        }
        Rel::Filter { input, predicate } => {
            let input_plan = convert_plan_to_logical_plan(input, state)?;
            let predicate = convert_expression_to_logical_expr(predicate, input_plan.schema())?;
            let logical_plan = datafusion::logical_expr::LogicalPlan::Filter(
                datafusion::logical_expr::Filter::try_new(predicate, Arc::new(input_plan))?,
            );
            Ok(logical_plan)
        }
        Rel::Sort { input, exprs } => {
            let input_plan = convert_plan_to_logical_plan(input, state)?;
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
        Rel::Join {
            left,
            right,
            on,
            join_type,
        } => {
            let left_plan = convert_plan_to_logical_plan(left, state)?;
            let right_plan = convert_plan_to_logical_plan(right, state)?;

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
            };

            let join_schema = build_join_schema(
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
        Rel::Aggregate {
            input,
            group_by,
            aggregates,
        } => {
            let input_plan = convert_plan_to_logical_plan(input, state)?;
            let group_by = group_by
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, input_plan.schema()))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let aggr_expr = aggregates
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, input_plan.schema()))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let logical_plan = datafusion::logical_expr::LogicalPlan::Aggregate(
                datafusion::logical_expr::Aggregate::try_new(
                    Arc::new(input_plan),
                    group_by,
                    aggr_expr,
                )?,
            );
            Ok(logical_plan)
        }
        Rel::Window { input, exprs } => {
            let input_plan = convert_plan_to_logical_plan(input, state)?;
            let exprs = exprs
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, input_plan.schema()))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            let logical_plan = datafusion::logical_expr::LogicalPlan::Window(
                datafusion::logical_expr::Window::try_new(exprs, Arc::new(input_plan))?,
            );
            Ok(logical_plan)
        }
    }
}

fn get_table_provider(
    collection_name: &ndc_models::CollectionName,
    state: &AppState,
) -> datafusion::error::Result<Arc<dyn TableProvider>> {
    let (rows, collection_fields) = match collection_name.as_str() {
        "actors" => (
            crate::collections::actors::rows(&BTreeMap::new(), state)
                .map_err(|e| DataFusionError::Internal(e.1 .0.message))?,
            crate::types::actor::definition().fields,
        ),
        "movies" => (
            crate::collections::movies::rows(&BTreeMap::new(), state)
                .map_err(|e| DataFusionError::Internal(e.1 .0.message))?
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
        _ => unimplemented!(),
    };
    let mut schema_builder = SchemaBuilder::new();
    for (field_name, object_field) in &collection_fields {
        let (data_type, nullable) = to_df_datatype(&object_field.r#type);
        schema_builder.push(Field::new(field_name.as_str(), data_type, nullable));
    }

    let schema = schema_builder.finish();
    let records = serde_arrow::to_record_batch(schema.fields(), &rows)
        .map_err(|e| DataFusionError::Internal(e.to_string()))?;

    let mem_table = MemTable::try_new(Arc::new(schema), vec![vec![records]])?;

    Ok(Arc::new(mem_table))
}

fn to_df_datatype(ty: &ndc_models::Type) -> (datafusion::arrow::datatypes::DataType, bool) {
    match ty {
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
            (datafusion::arrow::datatypes::DataType::Utf8, false)
        }
        ndc_models::Type::Nullable { underlying_type } => {
            let (dt, _) = to_df_datatype(underlying_type);
            (dt, true)
        }
        _ => unimplemented!(),
    }
}

fn convert_expression_to_logical_expr(
    expr: &Expression,
    schema: &DFSchema,
) -> datafusion::error::Result<datafusion::logical_expr::Expr> {
    match expr {
        Expression::Literal { literal } => Ok(datafusion::prelude::Expr::Literal(
            convert_literal_to_logical_expr(literal),
        )),
        Expression::Column { index } => Ok(datafusion::prelude::Expr::Column(
            schema.columns()[*index].clone(),
        )),
        Expression::Cast { expr, as_type } => convert_expression_to_logical_expr(expr, schema)?
            .cast_to(&convert_data_type(as_type), schema),
        Expression::TryCast {
            expr: _,
            as_type: _,
        } => unimplemented!(),
        Expression::Case { when, default } => {
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
                datafusion::logical_expr::Case::new(None, when_then_expr, else_expr),
            ))
        }
        Expression::And { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::And,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Or { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Or,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Eq { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Eq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::NotEq { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::NotEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Lt { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Lt,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::LtEq { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::LtEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Gt { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Gt,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::GtEq { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::GtEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Plus { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Plus,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Minus { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Minus,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Multiply { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Multiply,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Divide { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Divide,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Modulo { left, right } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)?),
                op: datafusion::logical_expr::Operator::Modulo,
                right: Box::new(convert_expression_to_logical_expr(right, schema)?),
            },
        )),
        Expression::Like { expr, pattern } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)?),
                op: datafusion::logical_expr::Operator::LikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)?),
            },
        )),
        Expression::ILike { expr, pattern } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)?),
                op: datafusion::logical_expr::Operator::ILikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)?),
            },
        )),
        Expression::NotLike { expr, pattern } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)?),
                op: datafusion::logical_expr::Operator::NotLikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)?),
            },
        )),
        Expression::NotILike { expr, pattern } => Ok(datafusion::prelude::Expr::BinaryExpr(
            datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)?),
                op: datafusion::logical_expr::Operator::NotILikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)?),
            },
        )),
        Expression::Not { expr } => Ok(datafusion::prelude::Expr::Not(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::IsNotNull { expr } => Ok(datafusion::prelude::Expr::IsNotNull(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::IsNull { expr } => Ok(datafusion::prelude::Expr::IsNull(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::IsTrue { expr } => Ok(datafusion::prelude::Expr::IsTrue(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::IsFalse { expr } => Ok(datafusion::prelude::Expr::IsFalse(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::IsUnknown { expr } => Ok(datafusion::prelude::Expr::IsUnknown(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::IsNotTrue { expr } => Ok(datafusion::prelude::Expr::IsNotTrue(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::IsNotFalse { expr } => Ok(datafusion::prelude::Expr::IsNotFalse(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::IsNotUnknown { expr } => Ok(datafusion::prelude::Expr::IsNotUnknown(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::Negative { expr } => Ok(datafusion::prelude::Expr::Negative(Box::new(
            convert_expression_to_logical_expr(expr, schema)?,
        ))),
        Expression::Between { low, expr, high } => Ok(datafusion::prelude::Expr::between(
            convert_expression_to_logical_expr(expr, schema)?,
            convert_expression_to_logical_expr(low, schema)?,
            convert_expression_to_logical_expr(high, schema)?,
        )),
        Expression::NotBetween { low, expr, high } => Ok(datafusion::prelude::Expr::not_between(
            convert_expression_to_logical_expr(expr, schema)?,
            convert_expression_to_logical_expr(low, schema)?,
            convert_expression_to_logical_expr(high, schema)?,
        )),
        Expression::In { expr, list } => {
            let list = list
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            Ok(convert_expression_to_logical_expr(expr, schema)?.in_list(list, false))
        }
        Expression::NotIn { expr, list } => {
            let list = list
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?;
            Ok(convert_expression_to_logical_expr(expr, schema)?.in_list(list, true))
        }
        Expression::Average { expr: _ } => unimplemented!(),
        Expression::BoolAnd { expr: _ } => unimplemented!(),
        Expression::BoolOr { expr: _ } => unimplemented!(),
        Expression::Count { expr, distinct } => Ok(datafusion::prelude::Expr::AggregateFunction(
            datafusion::logical_expr::expr::AggregateFunction {
                func: Arc::new(AggregateUDF::from(Count::new())),
                args: vec![convert_expression_to_logical_expr(expr, schema)?],
                distinct: *distinct,
                filter: None,
                order_by: None,
                null_treatment: None,
            },
        )),
        Expression::FirstValue { expr: _ } => unimplemented!(),
        Expression::LastValue { expr: _ } => unimplemented!(),
        Expression::Max { expr: _ } => unimplemented!(),
        Expression::Mean { expr: _ } => unimplemented!(),
        Expression::Median { expr: _ } => unimplemented!(),
        Expression::Min { expr: _ } => unimplemented!(),
        Expression::StringAgg { expr: _ } => unimplemented!(),
        Expression::Sum { expr } => Ok(datafusion::prelude::Expr::AggregateFunction(
            datafusion::logical_expr::expr::AggregateFunction {
                func: Arc::new(AggregateUDF::from(Sum::new())),
                args: vec![convert_expression_to_logical_expr(expr, schema)?],
                distinct: false,
                filter: None,
                order_by: None,
                null_treatment: None,
            },
        )),
        Expression::Var { expr: _ } => unimplemented!(),
        Expression::RowNumber {
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
        Expression::DenseRank {
            order_by: _,
            partition_by: _,
        } => unimplemented!(),
        Expression::NTile {
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
        Expression::Rank {
            order_by: _,
            partition_by: _,
        } => unimplemented!(),
        Expression::CumeDist {
            order_by: _,
            partition_by: _,
        } => unimplemented!(),
        Expression::PercentRank {
            order_by: _,
            partition_by: _,
        } => unimplemented!(),

        // scalar functions
        Expression::Abs { expr } => Ok(abs(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::BTrim { str, trim_str } => Ok(match trim_str {
            None => btrim(vec![convert_expression_to_logical_expr(str, schema)?]),
            Some(trim_str) => btrim(vec![
                convert_expression_to_logical_expr(str, schema)?,
                convert_expression_to_logical_expr(trim_str, schema)?,
            ]),
        }),
        Expression::Ceil { expr } => Ok(ceil(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::CharacterLength { str } => Ok(character_length(
            convert_expression_to_logical_expr(str, schema)?,
        )),
        Expression::Coalesce { exprs } => Ok(coalesce(
            exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?,
        )),
        Expression::Concat { exprs } => Ok(concat(
            exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?,
        )),
        Expression::Contains { str, search_str } => Ok(datafusion::prelude::Expr::ScalarFunction(
            datafusion::logical_expr::expr::ScalarFunction {
                func: contains(),
                args: vec![
                    convert_expression_to_logical_expr(str, schema)?,
                    convert_expression_to_logical_expr(search_str, schema)?,
                ],
            },
        )),
        Expression::Cos { expr } => Ok(cos(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::CurrentDate => Ok(current_date()),
        Expression::CurrentTime => Ok(current_time()),
        Expression::CurrentTimestamp => Ok(now()),
        Expression::DatePart { expr, part } => Ok(date_part(
            convert_expression_to_logical_expr(part, schema)?,
            convert_expression_to_logical_expr(expr, schema)?,
        )),
        Expression::DateTrunc { expr, part } => Ok(date_trunc(
            convert_expression_to_logical_expr(part, schema)?,
            convert_expression_to_logical_expr(expr, schema)?,
        )),
        Expression::Exp { expr } => Ok(exp(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::Floor { expr } => Ok(floor(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::Greatest { exprs } => Ok(greatest(
            exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?,
        )),
        Expression::IsNaN { expr } => Ok(isnan(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::IsZero { expr } => {
            Ok(iszero(convert_expression_to_logical_expr(expr, schema)?))
        }
        Expression::Least { exprs } => Ok(least(
            exprs
                .iter()
                .map(|expr| convert_expression_to_logical_expr(expr, schema))
                .collect::<datafusion::error::Result<Vec<_>>>()?,
        )),
        Expression::Left { str, n } => Ok(left(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(n, schema)?,
        )),
        Expression::Ln { expr } => Ok(ln(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::Log { expr, base } => Ok(log(
            convert_expression_to_logical_expr(
                base.as_ref()
                    .unwrap_or(&Box::new(plan_pushdown_types::Expression::Literal {
                        literal: Literal::Int64 { value: Some(10) },
                    })),
                schema,
            )?,
            convert_expression_to_logical_expr(expr, schema)?,
        )),
        Expression::Log10 { expr } => Ok(log10(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::Log2 { expr } => Ok(log2(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::LPad {
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
        Expression::LTrim { str, trim_str } => Ok(ltrim(match trim_str {
            None => vec![convert_expression_to_logical_expr(str, schema)?],
            Some(trim_str) => {
                vec![
                    convert_expression_to_logical_expr(str, schema)?,
                    convert_expression_to_logical_expr(trim_str, schema)?,
                ]
            }
        })),
        Expression::NullIf { expr1, expr2 } => Ok(nullif(
            convert_expression_to_logical_expr(expr1, schema)?,
            convert_expression_to_logical_expr(expr2, schema)?,
        )),
        Expression::Nvl { expr1, expr2 } => Ok(nvl(
            convert_expression_to_logical_expr(expr1, schema)?,
            convert_expression_to_logical_expr(expr2, schema)?,
        )),
        Expression::Power { base, exp } => Ok(power(
            convert_expression_to_logical_expr(base, schema)?,
            convert_expression_to_logical_expr(exp, schema)?,
        )),
        Expression::Random => Ok(random()),
        Expression::Replace {
            str,
            substr,
            replacement,
        } => Ok(replace(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(substr, schema)?,
            convert_expression_to_logical_expr(replacement, schema)?,
        )),
        Expression::Reverse { str } => {
            Ok(reverse(convert_expression_to_logical_expr(str, schema)?))
        }
        Expression::Right { str, n } => Ok(right(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(n, schema)?,
        )),
        Expression::Round { expr, prec } => Ok(round(match prec {
            None => vec![convert_expression_to_logical_expr(expr, schema)?],
            Some(prec) => vec![
                convert_expression_to_logical_expr(expr, schema)?,
                convert_expression_to_logical_expr(prec, schema)?,
            ],
        })),
        Expression::RPad {
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
        Expression::RTrim { str, trim_str } => Ok(rtrim(match trim_str {
            None => vec![convert_expression_to_logical_expr(str, schema)?],
            Some(trim_str) => {
                vec![
                    convert_expression_to_logical_expr(str, schema)?,
                    convert_expression_to_logical_expr(trim_str, schema)?,
                ]
            }
        })),
        Expression::Sqrt { expr } => Ok(sqrt(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::StrPos { str, substr } => Ok(strpos(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(substr, schema)?,
        )),
        Expression::Substr {
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
        Expression::SubstrIndex { str, delim, count } => Ok(substr_index(
            convert_expression_to_logical_expr(str, schema)?,
            convert_expression_to_logical_expr(delim, schema)?,
            convert_expression_to_logical_expr(count, schema)?,
        )),
        Expression::Tan { expr } => Ok(tan(convert_expression_to_logical_expr(expr, schema)?)),
        Expression::ToDate { expr } => Ok(to_date(vec![convert_expression_to_logical_expr(
            expr, schema,
        )?])),
        Expression::ToLower { expr } => {
            Ok(lower(convert_expression_to_logical_expr(expr, schema)?))
        }
        Expression::ToTimestamp { expr } => {
            Ok(to_timestamp(vec![convert_expression_to_logical_expr(
                expr, schema,
            )?]))
        }
        Expression::ToUpper { expr } => {
            Ok(upper(convert_expression_to_logical_expr(expr, schema)?))
        }
        Expression::Trunc { expr, prec } => Ok(trunc(match prec {
            None => vec![convert_expression_to_logical_expr(expr, schema)?],
            Some(prec) => vec![
                convert_expression_to_logical_expr(expr, schema)?,
                convert_expression_to_logical_expr(prec, schema)?,
            ],
        })),
    }
}

fn convert_data_type(
    scalar_type: &plan_pushdown_types::ScalarType,
) -> datafusion::arrow::datatypes::DataType {
    match scalar_type {
        plan_pushdown_types::ScalarType::Null => datafusion::arrow::datatypes::DataType::Null,
        plan_pushdown_types::ScalarType::Boolean => datafusion::arrow::datatypes::DataType::Boolean,
        plan_pushdown_types::ScalarType::Float32 => datafusion::arrow::datatypes::DataType::Float32,
        plan_pushdown_types::ScalarType::Float64 => datafusion::arrow::datatypes::DataType::Float64,
        plan_pushdown_types::ScalarType::Int8 => datafusion::arrow::datatypes::DataType::Int8,
        plan_pushdown_types::ScalarType::Int16 => datafusion::arrow::datatypes::DataType::Int16,
        plan_pushdown_types::ScalarType::Int32 => datafusion::arrow::datatypes::DataType::Int32,
        plan_pushdown_types::ScalarType::Int64 => datafusion::arrow::datatypes::DataType::Int64,
        plan_pushdown_types::ScalarType::UInt8 => datafusion::arrow::datatypes::DataType::UInt8,
        plan_pushdown_types::ScalarType::UInt16 => datafusion::arrow::datatypes::DataType::UInt16,
        plan_pushdown_types::ScalarType::UInt32 => datafusion::arrow::datatypes::DataType::UInt32,
        plan_pushdown_types::ScalarType::UInt64 => datafusion::arrow::datatypes::DataType::UInt64,
        plan_pushdown_types::ScalarType::Decimal128 { scale, prec } => {
            datafusion::arrow::datatypes::DataType::Decimal128(*prec, *scale)
        }
        plan_pushdown_types::ScalarType::Decimal256 { scale, prec } => {
            datafusion::arrow::datatypes::DataType::Decimal256(*prec, *scale)
        }
        plan_pushdown_types::ScalarType::Utf8 => datafusion::arrow::datatypes::DataType::Utf8,
        plan_pushdown_types::ScalarType::Date32 => datafusion::arrow::datatypes::DataType::Date32,
        plan_pushdown_types::ScalarType::Date64 => datafusion::arrow::datatypes::DataType::Date64,
        plan_pushdown_types::ScalarType::Time32Second => {
            datafusion::arrow::datatypes::DataType::Time32(
                datafusion::arrow::datatypes::TimeUnit::Second,
            )
        }
        plan_pushdown_types::ScalarType::Time32Millisecond => {
            datafusion::arrow::datatypes::DataType::Time32(
                datafusion::arrow::datatypes::TimeUnit::Millisecond,
            )
        }
        plan_pushdown_types::ScalarType::Time64Microsecond => {
            datafusion::arrow::datatypes::DataType::Time64(
                datafusion::arrow::datatypes::TimeUnit::Microsecond,
            )
        }
        plan_pushdown_types::ScalarType::Time64Nanosecond => {
            datafusion::arrow::datatypes::DataType::Time64(
                datafusion::arrow::datatypes::TimeUnit::Nanosecond,
            )
        }
        plan_pushdown_types::ScalarType::TimestampSecond => {
            datafusion::arrow::datatypes::DataType::Timestamp(
                datafusion::arrow::datatypes::TimeUnit::Second,
                None,
            )
        }
        plan_pushdown_types::ScalarType::TimestampMillisecond => {
            datafusion::arrow::datatypes::DataType::Timestamp(
                datafusion::arrow::datatypes::TimeUnit::Millisecond,
                None,
            )
        }
        plan_pushdown_types::ScalarType::TimestampMicrosecond => {
            datafusion::arrow::datatypes::DataType::Timestamp(
                datafusion::arrow::datatypes::TimeUnit::Microsecond,
                None,
            )
        }
        plan_pushdown_types::ScalarType::TimestampNanosecond => {
            datafusion::arrow::datatypes::DataType::Timestamp(
                datafusion::arrow::datatypes::TimeUnit::Nanosecond,
                None,
            )
        }
        plan_pushdown_types::ScalarType::DurationSecond => {
            datafusion::arrow::datatypes::DataType::Duration(
                datafusion::arrow::datatypes::TimeUnit::Second,
            )
        }
        plan_pushdown_types::ScalarType::DurationMillisecond => {
            datafusion::arrow::datatypes::DataType::Duration(
                datafusion::arrow::datatypes::TimeUnit::Millisecond,
            )
        }
        plan_pushdown_types::ScalarType::DurationMicrosecond => {
            datafusion::arrow::datatypes::DataType::Duration(
                datafusion::arrow::datatypes::TimeUnit::Microsecond,
            )
        }
        plan_pushdown_types::ScalarType::DurationNanosecond => {
            datafusion::arrow::datatypes::DataType::Duration(
                datafusion::arrow::datatypes::TimeUnit::Nanosecond,
            )
        }
    }
}

fn convert_literal_to_logical_expr(literal: &Literal) -> ScalarValue {
    match literal {
        Literal::Null => ScalarValue::Null,
        Literal::Boolean { value } => ScalarValue::Boolean(*value),
        Literal::Float32 { value } => ScalarValue::Float32(*value),
        Literal::Float64 { value } => ScalarValue::Float64(*value),
        Literal::Int8 { value } => ScalarValue::Int8(*value),
        Literal::Int16 { value } => ScalarValue::Int16(*value),
        Literal::Int32 { value } => ScalarValue::Int32(*value),
        Literal::Int64 { value } => ScalarValue::Int64(*value),
        Literal::UInt8 { value } => ScalarValue::UInt8(*value),
        Literal::UInt16 { value } => ScalarValue::UInt16(*value),
        Literal::UInt32 { value } => ScalarValue::UInt32(*value),
        Literal::UInt64 { value } => ScalarValue::UInt64(*value),
        Literal::Decimal128 { value, scale, prec } => {
            ScalarValue::Decimal128(*value, *prec, *scale)
        }
        Literal::Decimal256 { value, scale, prec } => ScalarValue::Decimal256(
            value
                .as_ref()
                .and_then(|s| datafusion::arrow::datatypes::i256::from_string(s)),
            *prec,
            *scale,
        ),
        Literal::Utf8 { value } => ScalarValue::Utf8(value.clone()),
        Literal::Date32 { value } => ScalarValue::Date32(*value),
        Literal::Date64 { value } => ScalarValue::Date64(*value),
        Literal::Time32Second { value } => ScalarValue::Time32Second(*value),
        Literal::Time32Millisecond { value } => ScalarValue::Time32Millisecond(*value),
        Literal::Time64Microsecond { value } => ScalarValue::Time64Microsecond(*value),
        Literal::Time64Nanosecond { value } => ScalarValue::Time64Nanosecond(*value),
        Literal::TimestampSecond { value } => ScalarValue::TimestampSecond(*value, None),
        Literal::TimestampMillisecond { value } => ScalarValue::TimestampMillisecond(*value, None),
        Literal::TimestampMicrosecond { value } => ScalarValue::TimestampMicrosecond(*value, None),
        Literal::TimestampNanosecond { value } => ScalarValue::TimestampNanosecond(*value, None),
        Literal::DurationSecond { value } => ScalarValue::DurationSecond(*value),
        Literal::DurationMillisecond { value } => ScalarValue::DurationMillisecond(*value),
        Literal::DurationMicrosecond { value } => ScalarValue::DurationMicrosecond(*value),
        Literal::DurationNanosecond { value } => ScalarValue::DurationNanosecond(*value),
    }
}

fn convert_sort_to_logical_sort(
    sort: &Sort,
    schema: &DFSchema,
) -> datafusion::error::Result<datafusion::logical_expr::SortExpr> {
    Ok(datafusion::logical_expr::SortExpr {
        expr: convert_expression_to_logical_expr(&sort.expr, schema)?,
        asc: sort.asc,
        nulls_first: sort.nulls_first,
    })
}
