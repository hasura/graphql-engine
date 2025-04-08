use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_i32_argument},
    query::Result,
    state::{AppState, Row},
    types::actor::get_actor_movie_id,
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_actors_by_movie_id".into(),
        description: Some("Get all actors from a movie by movie ID".into()),
        arguments: BTreeMap::from_iter([(
            "movie_id".into(),
            ndc_models::ArgumentInfo {
                description: Some("the id of the movie to fetch the actors from".into()),
                argument_type: ndc_models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: ndc_models::Type::Array {
            element_type: Box::new(ndc_models::Type::Named {
                name: "actor".into(),
            }),
        },
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
            actors_by_movie.push(actor);
        }
    }

    let actors_by_movie_value = serde_json::to_value(actors_by_movie).map_err(|_| {
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
        actors_by_movie_value,
    )])])
}
