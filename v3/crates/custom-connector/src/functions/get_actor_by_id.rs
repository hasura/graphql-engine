use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_actor_by_id".into(),
        description: Some("Get actor by ID".into()),
        arguments: BTreeMap::from_iter([(
            "id".into(),
            ndc_models::ArgumentInfo {
                description: Some("the id of the actor to fetch".into()),
                argument_type: ndc_models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Named {
                name: "actor".into(),
            }),
        },
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let id_value = arguments.get("id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "missing argument id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let id = id_value
        .as_i64()
        .ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "argument 'id' is not an integer".into(),
                details: serde_json::Value::Null,
            }),
        ))?
        .try_into()
        .map_err(|_| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "argument 'id' is out of range".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    let actor = state.actors.get(&id);

    match actor {
        None => Ok(vec![BTreeMap::from_iter([(
            "__value".into(),
            serde_json::Value::Null,
        )])]),
        Some(actor) => {
            let actor_value = serde_json::to_value(actor).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(ndc_models::ErrorResponse {
                        message: "unable to encode value".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;

            Ok(vec![BTreeMap::from_iter([("__value".into(), actor_value)])])
        }
    }
}
