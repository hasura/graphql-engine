use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> models::FunctionInfo {
    models::FunctionInfo {
        name: "latest_actor_name".into(),
        description: Some("Get the name of the most recent actor".into()),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "Actor_Name".into(),
            }),
        },
        arguments: BTreeMap::new(),
    }
}

pub(crate) fn rows(state: &AppState) -> Result<Vec<Row>> {
    let latest_name = state
        .actors
        .iter()
        .filter_map(|(_id, a)| a.get("name").and_then(|v| v.as_str()))
        .max();
    let latest_name_value = serde_json::to_value(latest_name).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: " ".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        latest_name_value,
    )])])
}
