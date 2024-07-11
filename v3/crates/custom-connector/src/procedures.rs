use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{query::Result, state::AppState};

pub mod add_movie_with_genres;
pub mod login;
pub mod noop_procedure;
pub mod update_actor_name_by_id;
pub mod uppercase_actor_name_by_id;
pub mod uppercase_all_actor_names;
pub mod uppercase_all_actor_names_return_names_list;
pub mod upsert_actor;

pub(crate) fn get_procedures() -> Vec<ndc_models::ProcedureInfo> {
    vec![
        upsert_actor::procedure_info(),
        update_actor_name_by_id::procedure_info(),
        uppercase_actor_name_by_id::procedure_info(),
        uppercase_all_actor_names::procedure_info(),
        uppercase_all_actor_names_return_names_list::procedure_info(),
        login::procedure_info(),
        noop_procedure::procedure_info(),
        add_movie_with_genres::procedure_info(),
    ]
}

pub(crate) fn execute_procedure(
    name: &ndc_models::ProcedureName,
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    fields: &Option<ndc_models::NestedField>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    state: &mut AppState,
) -> Result<serde_json::Value> {
    match name.as_str() {
        "upsert_actor" => upsert_actor::execute(arguments, fields, collection_relationships, state),
        "update_actor_name_by_id" => {
            update_actor_name_by_id::execute(arguments, fields, collection_relationships, state)
        }
        "uppercase_actor_name_by_id" => {
            uppercase_actor_name_by_id::execute(arguments, fields, collection_relationships, state)
        }
        "uppercase_all_actor_names" => {
            uppercase_all_actor_names::execute(fields, collection_relationships, state)
        }
        "uppercase_all_actor_names_return_names_list" => {
            uppercase_all_actor_names_return_names_list::execute(state)
        }
        "login" => login::execute(arguments),
        "noop_procedure" => Ok(noop_procedure::execute()),
        "add_movie_with_genres" => {
            add_movie_with_genres::execute(arguments, fields, collection_relationships, state)
        }
        _ => Err((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "invalid procedure name".into(),
                details: serde_json::Value::Null,
            }),
        )),
    }
}
