use std::collections::BTreeMap;

use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub mod actors;
pub mod actors_by_movie;
pub mod institutions;
pub mod movies;
pub mod movies_by_actor_name;

pub(crate) fn get_collections() -> Vec<ndc_models::CollectionInfo> {
    vec![
        actors::collection_info(),
        movies::collection_info(),
        institutions::collection_info(),
        actors_by_movie::collection_info(),
        movies_by_actor_name::collection_info(),
    ]
}

pub(crate) fn get_collection_by_name(
    collection_name: &str,
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    match collection_name {
        "actors" => actors::rows(state),
        "movies" => movies::rows(state),
        "institutions" => institutions::rows(state),
        "actors_by_movie" => actors_by_movie::rows(arguments, state),
        "movies_by_actor_name" => movies_by_actor_name::rows(arguments, state),
        _ => super::functions::get_function_by_name(collection_name, arguments, state),
    }
}
