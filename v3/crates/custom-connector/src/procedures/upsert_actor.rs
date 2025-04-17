use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_expression_argument, parse_object_argument},
    query::{Result, eval_expression, eval_nested_field},
    state::AppState,
};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "upsert_actor".into(),
        description: Some("Insert or update an actor".into()),
        arguments: BTreeMap::from_iter([
            (
                "actor".into(),
                ndc_models::ArgumentInfo {
                    description: Some("The actor to insert or update".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "actor".into(),
                    },
                },
            ),
            (
                "pre_check".into(),
                ndc_models::ArgumentInfo {
                    description: Some("Validate if the actor can be upserted".into()),
                    argument_type: ndc_models::Type::Nullable {
                        underlying_type: Box::new(ndc_models::Type::Predicate {
                            object_type_name: "actor".into(),
                        }),
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
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    fields: Option<&ndc_models::NestedField>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    state: &mut AppState,
) -> Result<serde_json::Value> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let actor_obj = parse_object_argument("actor", &mut arguments)?;
    let pre_check = if arguments.get("pre_check").is_some_and(|v| !v.is_null()) {
        Some(parse_expression_argument("pre_check", &mut arguments)?)
    } else {
        None
    };
    check_all_arguments_used(&arguments)?;

    let id = actor_obj.get("id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'id' is missing".into(),
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
    let new_row = actor_obj
        .iter()
        .map(|(k, v)| (ndc_models::FieldName::from(k.as_str()), v.clone()))
        .collect::<BTreeMap<_, _>>();

    if let Some(pre_check) = pre_check {
        if !eval_expression(
            collection_relationships,
            &BTreeMap::new(),
            state,
            &pre_check,
            &new_row,
            &new_row,
        )? {
            return Err((
                StatusCode::CONFLICT,
                Json(ndc_models::ErrorResponse {
                    message: "pre_check failed".into(),
                    details: serde_json::Value::Null,
                }),
            ));
        }
    }

    let old_row = state.actors.insert(id_int, new_row);
    old_row.map_or(Ok(serde_json::Value::Null), |old_row| {
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
    })
}
