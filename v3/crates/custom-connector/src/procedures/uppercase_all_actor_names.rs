use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    arguments::check_all_arguments_used,
    query::{Result, eval_nested_field},
    state::AppState,
};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "uppercase_all_actor_names".into(),
        description: Some("Uppercase all actor names".into()),
        arguments: BTreeMap::new(),
        result_type: ndc_models::Type::Nullable {
            underlying_type: Box::new(ndc_models::Type::Array {
                element_type: Box::new(ndc_models::Type::Named {
                    name: "actor".into(),
                }),
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
    check_all_arguments_used(arguments)?;

    let mut actors_list = vec![];
    let current_state = state.actors.clone();
    for (actor_id, actor) in &current_state {
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

        let mut new_row = actor.clone();
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
