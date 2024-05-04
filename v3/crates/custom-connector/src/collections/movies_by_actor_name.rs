use std::collections::{BTreeMap, HashSet};

use ndc_models;

use crate::{
    query::{parse_object_argument, Result},
    state::{AppState, Row},
    types::{actor::get_actor_movie_id, name_query::parse_name_query_object},
};

use super::actors::filter_actors_by_name;

pub(crate) fn collection_info() -> ndc_models::CollectionInfo {
    ndc_models::CollectionInfo {
        name: "movies_by_actor_name".into(),
        description: Some("Movies filtered by actor name search parameters".into()),
        collection_type: "movie".into(),
        arguments: BTreeMap::from_iter([(
            "actor_name".into(),
            ndc_models::ArgumentInfo {
                description: Some("the actor name components to search by".into()),
                argument_type: ndc_models::Type::Named {
                    name: "name_query".into(),
                },
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
    let name_object = parse_object_argument("actor_name", arguments)?;
    let (filter_first_name, filter_last_name) = parse_name_query_object(name_object)?;

    let movie_ids = filter_actors_by_name(state, filter_first_name, filter_last_name)
        .map(|result| result.and_then(get_actor_movie_id))
        .collect::<Result<HashSet<i32>>>()?;

    let movies_by_actor = movie_ids
        .iter()
        .filter_map(|movie_id| state.movies.get(movie_id).cloned())
        .collect::<Vec<_>>();

    Ok(movies_by_actor)
}
