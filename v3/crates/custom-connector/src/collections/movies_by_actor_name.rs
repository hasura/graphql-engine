use std::collections::{BTreeMap, HashSet};

use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_object_argument},
    query::Result,
    state::{AppState, Row},
    types::{actor::get_actor_movie_id, name_query::parse_name_query_object},
};

use super::actors::filter_actors_by_name;

pub(crate) fn collection_info() -> ndc_models::CollectionInfo {
    ndc_models::CollectionInfo {
        name: "movies_by_actor_name".into(),
        description: Some("Movies filtered by actor name search parameters".into()),
        collection_type: "movie".into(),
        arguments: BTreeMap::from_iter([
            (
                "_headers".into(),
                ndc_models::ArgumentInfo {
                    description: Some("headers".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "HeaderMap".into(),
                    },
                },
            ),
            (
                "actor_name".into(),
                ndc_models::ArgumentInfo {
                    description: Some("the actor name components to search by".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "name_query".into(),
                    },
                },
            ),
        ]),
        uniqueness_constraints: BTreeMap::new(),
        relational_mutations: None,
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let _headers = parse_object_argument("_headers", &mut arguments)?;
    let name_object = parse_object_argument("actor_name", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

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
