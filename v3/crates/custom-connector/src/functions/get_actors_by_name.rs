use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    collections::actors::filter_actors_by_name,
    query::{parse_object_argument, Result},
    state::{AppState, Row},
    types::name_query::parse_name_query_object,
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_actors_by_name".into(),
        description: Some("Get actors by name".into()),
        arguments: BTreeMap::from_iter([(
            "name".into(),
            ndc_models::ArgumentInfo {
                description: Some("the name components to search by".into()),
                argument_type: ndc_models::Type::Named {
                    name: "name_query".into(),
                },
            },
        )]),
        result_type: ndc_models::Type::Array {
            element_type: Box::new(ndc_models::Type::Named {
                name: "actor".into(),
            }),
        },
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let name_object = parse_object_argument("name", arguments)?;
    let (filter_first_name, filter_last_name) = parse_name_query_object(name_object)?;

    let filtered_actors = filter_actors_by_name(state, filter_first_name, filter_last_name)
        .collect::<Result<Vec<_>>>()?;

    let actors_value = serde_json::to_value(filtered_actors).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "unable to encode value".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        actors_value,
    )])])
}
