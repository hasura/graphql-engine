use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_actors_by_bool_exp".into(),
        description: Some("Get all actors with a boolean expression".into()),
        arguments: BTreeMap::from_iter([(
            "actor_bool_exp".into(),
            ndc_models::ArgumentInfo {
                description: Some("boolean expression over actor".into()),
                argument_type: ndc_models::Type::Predicate {
                    object_type_name: "actor".into(),
                },
            },
        )]),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Array {
                element_type: Box::new(ndc_models::Type::Named {
                    name: "actor".into(),
                }),
            }),
        },
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let actor_bool_exp_value = arguments.get("actor_bool_exp").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "missing argument actor_bool_exp".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let decoded_bool_exp: ndc_models::Expression =
        serde_json::from_value(actor_bool_exp_value.clone()).map_err(|_| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "could not decode argument actor_bool_exp".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

    // the only thing we are able to do is match on a single binary expression
    let (column, operator, value) = match decoded_bool_exp {
        ndc_models::Expression::BinaryComparisonOperator {
            column: ndc_models::ComparisonTarget::Column { name, .. },
            operator,
            value: ndc_models::ComparisonValue::Scalar { value },
        } => Ok((name, operator, value)),
        _ => Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "expression shape unsupported. we only support a single binary comparison"
                    .to_string(),
                details: serde_json::Value::Null,
            }),
        )),
    }?;

    let mut actors = vec![];

    // this is very limited, we can only compare id columns with "_eq"
    if column.as_str() == "id" && operator.as_str() == "_eq" {
        for actor in state.actors.values() {
            let actor_value = serde_json::to_value(actor).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(ndc_models::ErrorResponse {
                        message: "unable to encode value".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;

            // if it matches our boolean expression, include it
            if actor_value.get("id") == Some(&value) {
                actors.push(actor_value);
            }
        }
    } else {
        return Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "can only compare 'id' with _eq".to_string(),
                details: serde_json::Value::Null,
            }),
        ));
    }

    let actors_value = serde_json::to_value(actors).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "unable to encode value".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        actors_value,
    )])])
}
