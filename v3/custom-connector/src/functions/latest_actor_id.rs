use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> models::FunctionInfo {
    models::FunctionInfo {
        name: "latest_actor_id".into(),
        description: Some("Get the ID of the most recent actor".into()),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named { name: "Int".into() }),
        },
        arguments: BTreeMap::new(),
    }
}

pub(crate) fn rows(state: &AppState) -> Result<Vec<Row>> {
    let latest_id = state
        .actors
        .iter()
        .filter_map(|(_id, a)| a.get("id").and_then(|v| v.as_i64()))
        .max();
    let latest_id_value = serde_json::to_value(latest_id).map_err(|_| {
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
        latest_id_value,
    )])])
}
