use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
    types::actor::get_actor_movie_id,
};

pub(crate) fn collection_info() -> ndc_models::CollectionInfo {
    ndc_models::CollectionInfo {
        name: "actors_by_movie".into(),
        description: Some("Actors parameterized by movie".into()),
        collection_type: "actor".into(),
        arguments: BTreeMap::from_iter([(
            "movie_id".into(),
            ndc_models::ArgumentInfo {
                argument_type: ndc_models::Type::Named { name: "Int".into() },
                description: None,
            },
        )]),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::new(),
    }
}

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
    let movie_id_int: i32 = movie_id
        .as_i64()
        .ok_or((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "movie_id must be a integer".into(),
                details: serde_json::Value::Null,
            }),
        ))?
        .try_into()
        .map_err(|_| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "movie_id is out of range".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

    let mut actors_by_movie = vec![];

    for (_id, actor) in state.actors.iter() {
        let actor_movie_id_int = get_actor_movie_id(actor)?;
        if actor_movie_id_int == movie_id_int {
            actors_by_movie.push(actor.clone())
        }
    }

    Ok(actors_by_movie)
}
