use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> models::FunctionInfo {
    models::FunctionInfo {
        name: "get_movie_by_id".into(),
        description: Some("Get movie by ID".into()),
        arguments: BTreeMap::from_iter([(
            "movie_id".into(),
            models::ArgumentInfo {
                description: Some("the id of the movie to fetch".into()),
                argument_type: models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
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
        Json(models::ErrorResponse {
            message: "missing argument movie_id".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    if let Some(id) = id_value.as_i64() {
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
                        Json(models::ErrorResponse {
                            message: "unable to encode value".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;

                Ok(vec![BTreeMap::from_iter([("__value".into(), movie_value)])])
            }
        }
    } else {
        Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "incorrect type for id".into(),
                details: serde_json::Value::Null,
            }),
        ))
    }
}
