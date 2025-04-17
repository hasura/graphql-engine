use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    arguments::check_all_arguments_used,
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "latest_actor_id".into(),
        description: Some("Get the ID of the most recent actor".into()),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Named { name: "Int".into() }),
        },
        arguments: BTreeMap::new(),
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    check_all_arguments_used(arguments)?;

    let latest_id = state
        .actors
        .iter()
        .filter_map(|(_id, a)| a.get("id").and_then(serde_json::Value::as_i64))
        .max();
    let latest_id_value = serde_json::to_value(latest_id).map_err(|_| {
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
        latest_id_value,
    )])])
}
