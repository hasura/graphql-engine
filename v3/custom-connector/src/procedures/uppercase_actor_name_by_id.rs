use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::{eval_nested_field, Result},
    state::AppState,
};

pub(crate) fn execute(
    arguments: &BTreeMap<String, serde_json::Value>,
    fields: &Option<models::NestedField>,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    state: &mut AppState,
) -> Result<serde_json::Value> {
    let id = arguments.get("id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "required argument field 'id' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let id_int = id.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: "argument 'id' is not an integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let current_state = state.actors.clone();
    let old_row = current_state.get(&id_int);
    match &old_row {
        Some(actor_obj) => {
            let actor_name = actor_obj.get("name").ok_or((
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
            let mut new_row =
                BTreeMap::from_iter(actor_obj.iter().map(|(k, v)| (k.clone(), v.clone())));
            new_row.insert("name".into(), actor_name_uppercase_value.clone());
            state.actors.insert(id_int, new_row);
            let old_row = state.actors.get(&id_int);
            Ok(old_row.map_or(Ok(serde_json::Value::Null), |old_row| {
                let old_row_value = serde_json::to_value(old_row).map_err(|_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(models::ErrorResponse {
                            message: "cannot encode response".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;

                let old_row_fields = match fields {
                    None => Ok(models::RowFieldValue(old_row_value)),
                    Some(nested_field) => eval_nested_field(
                        collection_relationships,
                        &BTreeMap::new(),
                        state,
                        old_row_value,
                        nested_field,
                    ),
                }?;

                Ok(old_row_fields.0)
            })?)
        }
        None => Ok(serde_json::Value::Null),
    }
}
