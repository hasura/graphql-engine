use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::{eval_nested_field, Result},
    state::AppState,
};

pub(crate) fn procedure_info() -> models::ProcedureInfo {
    models::ProcedureInfo {
        name: "upsert_actor".into(),
        description: Some("Insert or update an actor".into()),
        arguments: BTreeMap::from_iter([(
            "actor".into(),
            models::ArgumentInfo {
                description: Some("The actor to insert or update".into()),
                argument_type: models::Type::Named {
                    name: "actor".into(),
                },
            },
        )]),
        result_type: models::Type::Nullable {
            underlying_type: Box::new(models::Type::Named {
                name: "actor".into(),
            }),
        },
    }
}

pub(crate) fn execute(
    arguments: &BTreeMap<String, serde_json::Value>,
    fields: &Option<models::NestedField>,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    state: &mut AppState,
) -> Result<serde_json::Value> {
    let actor = arguments.get("actor").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: " ".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let actor_obj = actor.as_object().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: " ".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let id = actor_obj.get("id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: " ".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let id_int = id.as_i64().ok_or((
        StatusCode::BAD_REQUEST,
        Json(models::ErrorResponse {
            message: " ".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let new_row = BTreeMap::from_iter(actor_obj.iter().map(|(k, v)| (k.clone(), v.clone())));
    let old_row = state.actors.insert(id_int, new_row);
    old_row.map_or(Ok(serde_json::Value::Null), |old_row| {
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
    })
}
