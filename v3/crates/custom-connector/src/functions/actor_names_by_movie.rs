use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_i64_argument},
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "actor_names_by_movie".into(),
        description: Some("Get actor names by movie ID".into()),
        arguments: BTreeMap::from_iter([(
            "movie_id".into(),
            ndc_models::ArgumentInfo {
                description: Some("the id of the movie to fetch".into()),
                argument_type: ndc_models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Array {
                element_type: Box::new(ndc_models::Type::Named {
                    name: "String".into(),
                }),
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
    let movie_id_int = parse_i64_argument("movie_id", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let mut actor_names_by_movie = vec![];

    for actor in state.actors.values() {
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
            actor_names_by_movie.push(actor_name.clone());
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
