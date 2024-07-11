use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_actors_by_movie_id_bounds".into(),
        description: Some("Get all actors within a given lower and upper movie id bound".into()),
        arguments: BTreeMap::from_iter([
            (
                "lower_bound".into(),
                ndc_models::ArgumentInfo {
                    description: Some("the lower bound for movie id".into()),
                    argument_type: ndc_models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "upper_bound".into(),
                ndc_models::ArgumentInfo {
                    description: Some("the upper bound for movie id".into()),
                    argument_type: ndc_models::Type::Named { name: "Int".into() },
                },
            ),
        ]),
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
    let lower_bound_value = arguments.get("lower_bound").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "missing argument movie id lower bound".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let lower_bound = lower_bound_value.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "movie id bound must be an integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let upper_bound_value = arguments.get("upper_bound").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "missing argument movie id upper bound".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let upper_bound = upper_bound_value.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "movie id bound must be an integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let mut actors_by_movie_id_bounds = vec![];

    for actor in state.actors.values() {
        let movie_id = actor.get("movie_id").ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "movie id not found".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let movie_id_int = movie_id.as_i64().ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "movie id not an integer".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        if movie_id_int >= lower_bound && movie_id_int <= upper_bound {
            let actor_value = serde_json::to_value(actor).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(ndc_models::ErrorResponse {
                        message: "unable to encode value".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;
            actors_by_movie_id_bounds.push(actor_value);
        }
    }

    let actors_by_movie_id_bounds_value =
        serde_json::to_value(actors_by_movie_id_bounds).map_err(|_| {
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
        actors_by_movie_id_bounds_value,
    )])])
}
