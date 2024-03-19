use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> models::FunctionInfo {
    models::FunctionInfo {
        name: "get_all_actors".into(),
        description: Some("Get all the actors".into()),
        result_type: models::Type::Array {
            element_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
        arguments: BTreeMap::new(),
    }
}

pub(crate) fn rows(state: &AppState) -> Result<Vec<Row>> {
    let mut actors = vec![];
    for (_id, actor) in state.actors.iter() {
        let actor_value = serde_json::to_value(actor).map_err(|_| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(models::ErrorResponse {
                    message: "unable to encode value".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
        actors.push(actor_value);
    }
    let actors_value = serde_json::to_value(actors).map_err(|_| {
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
        actors_value,
    )])])
}
