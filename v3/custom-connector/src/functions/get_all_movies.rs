use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> models::FunctionInfo {
    models::FunctionInfo {
        name: "get_all_movies".into(),
        description: Some("Get all the movies".into()),
        result_type: models::Type::Array {
            element_type: Box::new(models::Type::Named {
                name: "movie".into(),
            }),
        },
        arguments: BTreeMap::new(),
    }
}

pub(crate) fn rows(state: &AppState) -> Result<Vec<Row>> {
    let mut movies = vec![];
    for (_id, movie) in state.movies.iter() {
        let movie_value = serde_json::to_value(movie).map_err(|_| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(models::ErrorResponse {
                    message: "unable to encode value".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
        movies.push(movie_value);
    }
    let movies_value = serde_json::to_value(movies).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "unable to encode value".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        movies_value,
    )])])
}
