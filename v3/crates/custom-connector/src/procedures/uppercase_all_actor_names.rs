use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    query::{eval_nested_field, Result},
    state::AppState,
};

pub(crate) fn execute(
    fields: &Option<ndc_models::NestedField>,
    collection_relationships: &BTreeMap<String, ndc_models::Relationship>,
    state: &mut AppState,
) -> Result<serde_json::Value> {
    let mut actors_list = vec![];
    let current_state = state.actors.clone();
    for (actor_id, actor) in current_state.iter() {
        let id_int = *actor_id;
        let actor_name = actor.get("name").ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "name not found".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let actor_name_str = actor_name.as_str().ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "name is not a string".into(),
                details: serde_json::Value::Null,
            }),
        ))?;
        let actor_name_uppercase = actor_name_str.to_uppercase();
        let actor_name_uppercase_value = serde_json::Value::String(actor_name_uppercase);

        let old_row = actor;
        let mut new_row = BTreeMap::from_iter(old_row.iter().map(|(k, v)| (k.clone(), v.clone())));
        new_row.insert("name".into(), actor_name_uppercase_value.clone());
        state.actors.insert(id_int, new_row.clone());
        let actor_json = serde_json::to_value(new_row).map_err(|_| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(ndc_models::ErrorResponse {
                    message: "cannot encode response".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
        actors_list.push(actor_json);
    }

    let actors_list_as_json = serde_json::Value::Array(actors_list);
    let return_value = if let Some(nested_field) = fields {
        eval_nested_field(
            collection_relationships,
            &BTreeMap::new(),
            state,
            actors_list_as_json,
            nested_field,
        )?
        .0
    } else {
        actors_list_as_json
    };
    Ok(return_value)
}
