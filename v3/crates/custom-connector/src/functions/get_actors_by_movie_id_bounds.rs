use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn rows(
    arguments: &BTreeMap<String, serde_json::Value>,
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

    for (_id, actor) in state.actors.iter() {
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
