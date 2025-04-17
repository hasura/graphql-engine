use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_i32_argument},
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_actor_by_id".into(),
        description: Some("Get actor by ID".into()),
        arguments: BTreeMap::from_iter([(
            "id".into(),
            ndc_models::ArgumentInfo {
                description: Some("the id of the actor to fetch".into()),
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

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let id = parse_i32_argument("id", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let actor = state.actors.get(&id);

    match actor {
        None => Ok(vec![BTreeMap::from_iter([(
            "__value".into(),
            serde_json::Value::Null,
        )])]),
        Some(actor) => {
            let actor_value = serde_json::to_value(actor).map_err(|_| {
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Json(ndc_models::ErrorResponse {
                        message: "unable to encode value".into(),
                        details: serde_json::Value::Null,
                    }),
                )
            })?;

            Ok(vec![BTreeMap::from_iter([("__value".into(), actor_value)])])
        }
    }
}
