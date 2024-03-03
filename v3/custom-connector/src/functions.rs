use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub mod actor_names_by_movie;
pub mod get_actor_by_id;
pub mod get_actors_by_movie_id;
pub mod get_actors_by_movie_id_bounds;
pub mod get_actors_by_name;
pub mod get_all_actors;
pub mod get_all_movies;
pub mod get_movie_by_id;
pub mod latest_actor;
pub mod latest_actor_id;
pub mod latest_actor_name;

pub(crate) fn get_functions() -> Vec<models::FunctionInfo> {
    vec![
        latest_actor_id::function_info(),
        latest_actor_name::function_info(),
        latest_actor::function_info(),
        get_actor_by_id::function_info(),
        get_movie_by_id::function_info(),
        get_actors_by_name::function_info(),
        get_actors_by_movie_id::function_info(),
        get_all_actors::function_info(),
        get_all_movies::function_info(),
        // TODO: Looks like the other functions where never added to the schema?
    ]
}

pub(crate) fn get_function_by_name(
    collection_name: &str,
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    match collection_name {
        "latest_actor_id" => latest_actor_id::rows(state),
        "latest_actor_name" => latest_actor_name::rows(state),
        "latest_actor" => latest_actor::rows(state),
        "get_actor_by_id" => get_actor_by_id::rows(arguments, state),
        "get_movie_by_id" => get_movie_by_id::rows(arguments, state),
        "get_actors_by_name" => get_actors_by_name::rows(arguments, state),
        "actor_names_by_movie" => actor_names_by_movie::rows(arguments, state),
        "get_all_actors" => get_all_actors::rows(state),
        "get_all_movies" => get_all_movies::rows(state),
        "get_actors_by_movie_id_bounds" => get_actors_by_movie_id_bounds::rows(arguments, state),
        "get_actors_by_movie_id" => get_actors_by_movie_id::rows(arguments, state),
        _ => Err((
            StatusCode::BAD_REQUEST,
            Json(models::ErrorResponse {
                message: "invalid collection name".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}
