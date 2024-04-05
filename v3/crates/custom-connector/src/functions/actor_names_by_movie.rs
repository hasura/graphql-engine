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
    let movie_id = arguments.get("movie_id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "missing argument movie_id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let movie_id_int = movie_id.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "movie_id must be a string".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let mut actor_names_by_movie = vec![];

    for (_id, actor) in state.actors.iter() {
        let actor_movie_id = actor.get("movie_id").ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "movie_id not found".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let actor_movie_id_int = actor_movie_id.as_i64().ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let actor_name = actor.get("name").ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "name not found".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        if actor_movie_id_int == movie_id_int {
            actor_names_by_movie.push(actor_name.clone())
        }
    }
    let actor_names_by_movie_value = serde_json::to_value(actor_names_by_movie).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        actor_names_by_movie_value,
    )])])
}
