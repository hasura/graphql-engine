use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_movie_by_id".into(),
        description: Some("Get movie by ID".into()),
        arguments: BTreeMap::from_iter([(
            "movie_id".into(),
            ndc_models::ArgumentInfo {
                description: Some("the id of the movie to fetch".into()),
                argument_type: ndc_models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Named {
                name: "movie".into(),
            }),
        },
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let id_value = arguments.get("movie_id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "missing argument movie_id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let id = id_value
        .as_i64()
        .ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "incorrect type for movie_id".into(),
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
    let movie = state.movies.get(&id);

    match movie {
        None => Ok(vec![BTreeMap::from_iter([(
            "__value".into(),
            serde_json::Value::Null,
        )])]),
        Some(movie) => {
            let movie_value = serde_json::to_value(movie).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(ndc_models::ErrorResponse {
                        message: "unable to encode value".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;

            Ok(vec![BTreeMap::from_iter([("__value".into(), movie_value)])])
        }
    }
}
