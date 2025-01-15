use std::collections::BTreeMap;

use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub mod actors;
pub mod actors_by_movie;
pub mod countries;
pub mod institutions;
pub mod movies;
pub mod movies_by_actor_name;

pub(crate) fn get_collections() -> Vec<ndc_models::CollectionInfo> {
    vec![
        actors::collection_info(),
        movies::collection_info(),
        countries::collection_info(),
        institutions::collection_info(),
        actors_by_movie::collection_info(),
        movies_by_actor_name::collection_info(),
    ]
}

pub(crate) fn get_collection_by_name(
    collection_name: &ndc_models::CollectionName,
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    state: &AppState,
) -> Result<Vec<Row>> {
    match collection_name.as_str() {
        "actors" => actors::rows(arguments, state),
        "countries" => countries::rows(arguments, state),
        "movies" => movies::rows(arguments, state),
        "institutions" => institutions::rows(arguments, state),
        "actors_by_movie" => actors_by_movie::rows(arguments, state),
        "movies_by_actor_name" => movies_by_actor_name::rows(arguments, state),
        _ => super::functions::get_function_by_name(
            collection_name,
            arguments,
            collection_relationships,
            state,
        ),
    }
}
