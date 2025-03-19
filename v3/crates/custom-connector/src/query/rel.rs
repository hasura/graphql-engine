use crate::state::AppState;

use axum::{http::StatusCode, Json};
use core::unimplemented;
use datafusion::{
    arrow::datatypes::{Field, SchemaBuilder, SchemaRef},
    common::DFSchema,
    datasource::{DefaultTableSource, MemTable, TableProvider},
    error::DataFusionError,
    execution::{runtime_env::RuntimeEnv, SessionStateBuilder},
    functions_aggregate::count::Count,
    logical_expr::{build_join_schema, AggregateUDF, Literal as _, SubqueryAlias},
    prelude::{SessionConfig, SessionContext},
    scalar::ScalarValue,
    sql::TableReference,
};
use plan_pushdown_types::{Expression, JoinType, Literal, Rel, Sort};
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

    // TODO: stream the records back
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
            let exprs = exprs
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, input_plan.schema()))
                .collect();
            let logical_plan = datafusion::logical_expr::LogicalPlan::Projection(
                datafusion::logical_expr::Projection::try_new(exprs, Arc::new(input_plan))?,
            );
            Ok(logical_plan)
        }
        Rel::Filter { input, predicate } => {
            let input_plan = convert_plan_to_logical_plan(input, state)?;
            let predicate = convert_expression_to_logical_expr(predicate, input_plan.schema());
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
                .collect();
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
                    let left =
                        convert_expression_to_logical_expr(&join_on.left, left_plan_alias.schema());
                    let right = convert_expression_to_logical_expr(
                        &join_on.right,
                        right_plan_alias.schema(),
                    );
                    (left, right)
                })
                .collect();

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
                .collect();
            let aggr_expr = aggregates
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, input_plan.schema()))
                .collect();
            let logical_plan = datafusion::logical_expr::LogicalPlan::Aggregate(
                datafusion::logical_expr::Aggregate::try_new(
                    Arc::new(input_plan),
                    group_by,
                    aggr_expr,
                )?,
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
        ndc_models::Type::Named { name } if name.as_str() == "BigInt" => {
            (datafusion::arrow::datatypes::DataType::Utf8, false)
        }
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
) -> datafusion::logical_expr::Expr {
    match expr {
        Expression::Literal { literal } => {
            datafusion::prelude::Expr::Literal(convert_literal_to_logical_expr(literal))
        }
        Expression::Column { index } => {
            datafusion::prelude::Expr::Column(schema.columns()[*index].clone())
        }
        Expression::Cast {
            expr: _,
            as_type: _,
        } => unimplemented!(),
        Expression::TryCast {
            expr: _,
            as_type: _,
        } => unimplemented!(),
        Expression::Case {
            when: _,
            default: _,
        } => unimplemented!(),
        Expression::And { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::And,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Or { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Or,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Eq { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Eq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::NotEq { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::NotEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Lt { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Lt,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::LtEq { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::LtEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Gt { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Gt,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::GtEq { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::GtEq,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Plus { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Plus,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Minus { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Minus,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Multiply { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Multiply,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Divide { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Divide,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Modulo { left, right } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(left, schema)),
                op: datafusion::logical_expr::Operator::Modulo,
                right: Box::new(convert_expression_to_logical_expr(right, schema)),
            })
        }
        Expression::Like { expr, pattern } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)),
                op: datafusion::logical_expr::Operator::LikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)),
            })
        }
        Expression::ILike { expr, pattern } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)),
                op: datafusion::logical_expr::Operator::ILikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)),
            })
        }
        Expression::NotLike { expr, pattern } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)),
                op: datafusion::logical_expr::Operator::NotLikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)),
            })
        }
        Expression::NotILike { expr, pattern } => {
            datafusion::prelude::Expr::BinaryExpr(datafusion::logical_expr::BinaryExpr {
                left: Box::new(convert_expression_to_logical_expr(expr, schema)),
                op: datafusion::logical_expr::Operator::NotILikeMatch,
                right: Box::new(convert_expression_to_logical_expr(pattern, schema)),
            })
        }
        Expression::Not { expr } => datafusion::prelude::Expr::Not(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::IsNotNull { expr } => datafusion::prelude::Expr::IsNotNull(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::IsNull { expr } => datafusion::prelude::Expr::IsNull(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::IsTrue { expr } => datafusion::prelude::Expr::IsTrue(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::IsFalse { expr } => datafusion::prelude::Expr::IsFalse(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::IsUnknown { expr } => datafusion::prelude::Expr::IsUnknown(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::IsNotTrue { expr } => datafusion::prelude::Expr::IsNotTrue(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::IsNotFalse { expr } => datafusion::prelude::Expr::IsNotFalse(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::IsNotUnknown { expr } => datafusion::prelude::Expr::IsNotUnknown(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::Negative { expr } => datafusion::prelude::Expr::Negative(Box::new(
            convert_expression_to_logical_expr(expr, schema),
        )),
        Expression::Between { low, expr, high } => datafusion::prelude::Expr::between(
            convert_expression_to_logical_expr(expr, schema),
            convert_expression_to_logical_expr(low, schema),
            convert_expression_to_logical_expr(high, schema),
        ),
        Expression::NotBetween { low, expr, high } => datafusion::prelude::Expr::not_between(
            convert_expression_to_logical_expr(expr, schema),
            convert_expression_to_logical_expr(low, schema),
            convert_expression_to_logical_expr(high, schema),
        ),
        Expression::In { expr, list } => {
            let list = list
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, schema))
                .collect();
            convert_expression_to_logical_expr(expr, schema).in_list(list, false)
        }
        Expression::NotIn { expr, list } => {
            let list = list
                .iter()
                .map(|e| convert_expression_to_logical_expr(e, schema))
                .collect();
            convert_expression_to_logical_expr(expr, schema).in_list(list, true)
        }
        Expression::ToLower { expr: _ } => unimplemented!(),
        Expression::ToUpper { expr: _ } => unimplemented!(),
        Expression::Average { expr: _ } => unimplemented!(),
        Expression::BoolAnd { expr: _ } => unimplemented!(),
        Expression::BoolOr { expr: _ } => unimplemented!(),
        Expression::Count { expr } => datafusion::prelude::Expr::AggregateFunction(
            datafusion::logical_expr::expr::AggregateFunction {
                func: Arc::new(AggregateUDF::from(Count::new())),
                args: vec![convert_expression_to_logical_expr(expr, schema)],
                distinct: false,
                filter: None,
                order_by: None,
                null_treatment: None,
            },
        ),
        Expression::FirstValue { expr: _ } => unimplemented!(),
        Expression::LastValue { expr: _ } => unimplemented!(),
        Expression::Max { expr: _ } => unimplemented!(),
        Expression::Mean { expr: _ } => unimplemented!(),
        Expression::Median { expr: _ } => unimplemented!(),
        Expression::Min { expr: _ } => unimplemented!(),
        Expression::StringAgg { expr: _ } => unimplemented!(),
        Expression::Sum { expr: _ } => unimplemented!(),
        Expression::Var { expr: _ } => unimplemented!(),
    }
}

fn convert_literal_to_logical_expr(literal: &Literal) -> ScalarValue {
    match literal {
        Literal::Null => ScalarValue::Null,
        Literal::Boolean { value } => ScalarValue::Boolean(*value),
        Literal::Float32 { value } => ScalarValue::Float32(*value),
        Literal::Float64 { value } => ScalarValue::Float64(*value),
        Literal::Int32 { value } => ScalarValue::Int32(*value),
        Literal::Int64 { value } => ScalarValue::Int64(*value),
        // Handle other literal types here
        _ => unimplemented!(),
    }
}

fn convert_sort_to_logical_sort(
    sort: &Sort,
    schema: &DFSchema,
) -> datafusion::logical_expr::SortExpr {
    datafusion::logical_expr::SortExpr {
        expr: convert_expression_to_logical_expr(&sort.expr, schema),
        asc: sort.asc,
        nulls_first: sort.nulls_first,
    }
}
