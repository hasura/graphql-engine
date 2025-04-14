use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{arguments::argument_string, query::Result};

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("parameters for querying by name".into()),
        fields: BTreeMap::from_iter([
            (
                "first_name".into(),
                ndc_models::ObjectField {
                    description: Some(
                        "The actor's first name or null to match any first name".into(),
                    ),
                    r#type: ndc_models::Type::Nullable {
                        underlying_type: ndc_models::Type::Named {
                            name: "String".into(),
                        }
                        .into(),
                    },
                    arguments: argument_string(),
                },
            ),
            (
                "last_name".into(),
                ndc_models::ObjectField {
                    description: Some("The actor's last name or null to match any last".into()),
                    r#type: ndc_models::Type::Nullable {
                        underlying_type: ndc_models::Type::Named {
                            name: "String".into(),
                        }
                        .into(),
                    },
                    arguments: argument_string(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}

pub(crate) fn parse_name_query_object(
    name_object: &serde_json::Map<String, serde_json::Value>,
) -> Result<(Option<&str>, Option<&str>)> {
    let first_name_value = name_object.get("first_name").ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "missing name argument property first_name".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    let first_name = if first_name_value.is_null() {
        None
    } else {
        Some(first_name_value.as_str().ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "name argument property first_name must be a string".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?)
    };
    let last_name_value = name_object.get("last_name").ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "missing name argument property last_name".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    let last_name = if last_name_value.is_null() {
        None
    } else {
        Some(last_name_value.as_str().ok_or_else(|| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "name argument property last_name must be a string".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?)
    };
    Ok((first_name, last_name))
}
