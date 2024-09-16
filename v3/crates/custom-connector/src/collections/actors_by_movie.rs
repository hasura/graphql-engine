use std::collections::BTreeMap;

use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_i32_argument},
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
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let movie_id_int = parse_i32_argument("movie_id", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let mut actors_by_movie = vec![];

    for actor in state.actors.values() {
        let actor_movie_id_int = get_actor_movie_id(actor)?;
        if actor_movie_id_int == movie_id_int {
            actors_by_movie.push(actor.clone());
        }
    }

    Ok(actors_by_movie)
}
