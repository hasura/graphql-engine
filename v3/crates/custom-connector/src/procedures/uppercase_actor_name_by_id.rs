use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_i32_argument},
    query::{Result, eval_nested_field},
    state::AppState,
};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "uppercase_actor_name_by_id".into(),
        description: Some("Uppercase an actor name given the ID".into()),
        arguments: BTreeMap::from_iter([(
            "id".into(),
            ndc_models::ArgumentInfo {
                description: Some("the id of the actor to update".into()),
                argument_type: ndc_models::Type::Named { name: "Int".into() },
            },
        )]),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Named {
                name: "actor".into(),
            }),
        },
    }
}

pub(crate) fn execute(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    fields: Option<&ndc_models::NestedField>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    state: &mut AppState,
) -> Result<serde_json::Value> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let id_int = parse_i32_argument("id", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let current_state = state.actors.clone();
    let old_row = current_state.get(&id_int);
    match &old_row {
        Some(actor_obj) => {
            let actor_name = actor_obj.get("name").ok_or((
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
            let mut new_row = (*actor_obj).clone();
            new_row.insert("name".into(), actor_name_uppercase_value);
            state.actors.insert(id_int, new_row);
            let old_row = state.actors.get(&id_int);
            Ok(old_row.map_or(Ok(serde_json::Value::Null), |old_row| {
                let old_row_value = serde_json::to_value(old_row).map_err(|_| {
                    (
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Json(ndc_models::ErrorResponse {
                            message: "cannot encode response".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;

                let old_row_fields = match fields {
                    None => Ok(ndc_models::RowFieldValue(old_row_value)),
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
