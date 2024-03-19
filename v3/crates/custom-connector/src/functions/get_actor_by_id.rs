use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> models::FunctionInfo {
    models::FunctionInfo {
        name: "get_actor_by_id".into(),
        description: Some("Get actor by ID".into()),
        arguments: BTreeMap::from_iter([(
            "id".into(),
            models::ArgumentInfo {
                description: Some("the id of the actor to fetch".into()),
                argument_type: models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
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
        Json(models::ErrorResponse {
            message: "missing argument id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    if let Some(id) = id_value.as_i64() {
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
                        Json(models::ErrorResponse {
                            message: "unable to encode value".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;

                Ok(vec![BTreeMap::from_iter([("__value".into(), actor_value)])])
            }
        }
    } else {
        Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "incorrect type for id".into(),
                details: serde_json::Value::Null,
            }),
        ))
    }
}
