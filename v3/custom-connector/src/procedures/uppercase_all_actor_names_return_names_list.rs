use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{query::Result, state::AppState};

pub(crate) fn execute(state: &mut AppState) -> Result<serde_json::Value> {
    let mut actors_list = vec![];
    let current_state = state.actors.clone();
    for (actor_id, actor) in current_state.iter() {
        let id_int = *actor_id;
        let actor_name = actor.get("name").ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "name not found".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let actor_name_str = actor_name.as_str().ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(models::ErrorResponse {
                message: "name is not a string".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let actor_name_uppercase = actor_name_str.to_uppercase();
        let actor_name_uppercase_value = serde_json::Value::String(actor_name_uppercase);

        let old_row = actor;
        let mut new_row = BTreeMap::from_iter(old_row.iter().map(|(k, v)| (k.clone(), v.clone())));
        new_row.insert("name".into(), actor_name_uppercase_value.clone());
        state.actors.insert(id_int, new_row);
        let output_row = state.actors.get(actor_id);
        let returning_value = output_row.map_or(Ok(serde_json::Value::Null), |new_row| {
            let name = new_row.get("name").ok_or((
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(models::ErrorResponse {
                    message: "name not found".into(),
                    details: serde_json::Value::Null,
                }),
            ))?;
            Ok(name.clone())
        })?;
        actors_list.push(returning_value);
    }
    Ok(serde_json::Value::Array(actors_list))
}
