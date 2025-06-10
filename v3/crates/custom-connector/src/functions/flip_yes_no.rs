use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "flip_yes_no_function".into(),
        description: Some("Flip a yes/no enum".into()),
        result_type: ndc_models::Type::Named {
            name: "YesNo".into(),
        },
        arguments: BTreeMap::from_iter([(
            ndc_models::ArgumentName::new("yes_no".into()),
            ndc_models::ArgumentInfo {
                description: Some("The yes/no enum to flip".into()),
                argument_type: ndc_models::Type::Named {
                    name: "YesNo".into(),
                },
            },
        )]),
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    _state: &AppState,
) -> Result<Vec<Row>> {
    let yes_no = arguments.get("yes_no").ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "Missing argument: yes_no".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    let yes_no_string = yes_no.as_str().ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "Expected string for argument 'yes_no'".into(),
                details: yes_no.clone(),
            }),
        )
    })?;

    let flipped = match yes_no_string {
        "yes" => "no",
        "no" => "yes",
        _ => {
            return Err((
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "Invalid argument: yes_no".into(),
                    details: yes_no.clone(),
                }),
            ));
        }
    };

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        serde_json::Value::String(flipped.to_string()),
    )])])
}
