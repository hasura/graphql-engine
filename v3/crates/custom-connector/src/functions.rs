use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub mod actor_names_by_movie;
pub mod eval_institutions;
pub mod eval_location;
pub mod get_actor_by_id;
pub mod get_actors_by_bool_exp;
pub mod get_actors_by_movie_id;
pub mod get_actors_by_movie_id_bounds;
pub mod get_actors_by_name;
pub mod get_all_actors;
pub mod get_all_movies;
pub mod get_institutions_by_institution_query;
pub mod get_movie_by_id;
pub mod get_session_details;
pub mod latest_actor;
pub mod latest_actor_id;
pub mod latest_actor_name;

pub(crate) fn get_functions() -> Vec<ndc_models::FunctionInfo> {
    vec![
        eval_institutions::function_info(),
        eval_location::function_info(),
        latest_actor_id::function_info(),
        latest_actor_name::function_info(),
        latest_actor::function_info(),
        get_actor_by_id::function_info(),
        get_movie_by_id::function_info(),
        get_actors_by_name::function_info(),
        actor_names_by_movie::function_info(),
        get_all_actors::function_info(),
        get_all_movies::function_info(),
        get_actors_by_movie_id_bounds::function_info(),
        get_actors_by_bool_exp::function_info(),
        get_actors_by_movie_id::function_info(),
        get_institutions_by_institution_query::function_info(),
        get_session_details::function_info(),
    ]
}

pub(crate) fn get_function_by_name(
    collection_name: &ndc_models::CollectionName,
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    state: &AppState,
) -> Result<Vec<Row>> {
    match collection_name.as_str() {
        "eval_institutions" => eval_institutions::rows(arguments, collection_relationships, state),
        "eval_location" => eval_location::rows(arguments, collection_relationships, state),
        "latest_actor_id" => latest_actor_id::rows(arguments, state),
        "latest_actor_name" => latest_actor_name::rows(arguments, state),
        "latest_actor" => latest_actor::rows(arguments, state),
        "get_actor_by_id" => get_actor_by_id::rows(arguments, state),
        "get_movie_by_id" => get_movie_by_id::rows(arguments, state),
        "get_actors_by_name" => get_actors_by_name::rows(arguments, state),
        "actor_names_by_movie" => actor_names_by_movie::rows(arguments, state),
        "get_all_actors" => get_all_actors::rows(arguments, state),
        "get_all_movies" => get_all_movies::rows(arguments, state),
        "get_actors_by_movie_id_bounds" => get_actors_by_movie_id_bounds::rows(arguments, state),
        "get_actors_by_bool_exp" => get_actors_by_bool_exp::rows(arguments, state),
        "get_actors_by_movie_id" => get_actors_by_movie_id::rows(arguments, state),
        "get_institutions_by_institution_query" => {
            get_institutions_by_institution_query::rows(arguments, state)
        }
        "get_session_details" => get_session_details::rows(arguments),
        _ => Err((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "invalid collection name".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}
