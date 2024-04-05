use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    query::{eval_nested_field, Result},
    state::AppState,
};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "update_actor_name_by_id".into(),
        description: Some("Update an actor name given the ID and new name".into()),
        arguments: BTreeMap::from_iter([
            (
                "id".into(),
                ndc_models::ArgumentInfo {
                    description: Some("the id of the actor to update".into()),
                    argument_type: ndc_models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "name".into(),
                ndc_models::ArgumentInfo {
                    description: Some("the new name of the actor".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
        ]),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Named {
                name: "actor".into(),
            }),
        },
    }
}

pub(crate) fn execute(
    arguments: &BTreeMap<String, serde_json::Value>,
    fields: &Option<ndc_models::NestedField>,
    collection_relationships: &BTreeMap<String, ndc_models::Relationship>,
    state: &mut AppState,
) -> Result<serde_json::Value> {
    let id = arguments.get("id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'id' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let name = arguments.get("name").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'name' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let id_int = id
        .as_i64()
        .ok_or((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "argument 'id' is not an integer".into(),
                details: serde_json::Value::Null,
            }),
        ))?
        .try_into()
        .map_err(|_| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "argument 'id' is out of range".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;

    let current_state = state.actors.clone();
    let old_row = current_state.get(&id_int);
    match &old_row {
        Some(actor_obj) => {
            let mut new_row =
                BTreeMap::from_iter(actor_obj.iter().map(|(k, v)| (k.clone(), v.clone())));
            new_row.insert("name".into(), name.clone());
            state.actors.insert(id_int, new_row);
            let output_row = state.actors.get(&id_int);
            output_row.map_or(Ok(serde_json::Value::Null), |output_row| {
                let output_row_value = serde_json::to_value(output_row).map_err(|_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(ndc_models::ErrorResponse {
                            message: "cannot encode response".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;

                let output_row_fields = match fields {
                    None => Ok(ndc_models::RowFieldValue(output_row_value)),
                    Some(nested_field) => eval_nested_field(
                        collection_relationships,
                        &BTreeMap::new(),
                        state,
                        output_row_value,
                        nested_field,
                    ),
                }?;

                Ok(output_row_fields.0)
            })
        }
        None => Ok(serde_json::Value::Null),
    }
}
