use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{query::Result, state::Row};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "eval_where".into(),
        description: Some("Returns fields described in a where clause".into()),
        arguments: BTreeMap::from_iter([(
            "where".into(),
            ndc_models::ArgumentInfo {
                description: Some("The where clause to evaluate".into()),
                argument_type: ndc_models::Type::Array {
                    element_type: Box::new(ndc_models::Type::Named {
                        name: "where".into(),
                    }),
                },
            },
        )]),
        result_type: ndc_models::Type::Array {
            element_type: Box::new(ndc_models::Type::Named {
                name: "String".into(),
            }),
        },
    }
}

// `where` looks like
// {
//   name: where_string,
//   age: where_int
// }

// `where_string` looks like
// {
//   _eq: string,
//   _neq: string
// }

// `where_int` looks like
// {
//   _eq: int,
//   _gt: int,
//   _lt: int,
// }

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
) -> Result<Vec<Row>> {
    let arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();

    let where_arg = arguments.get("where").ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "Missing argument: where".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    let where_clause = where_arg.as_object().ok_or_else(|| {
        (
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "Argument 'where' should be an object".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    let mut found_filters: Vec<String> = vec![];

    // collect all the filters we've found
    for (k, v) in where_clause {
        if let Some(v) = v.as_object() {
            for (k2, v2) in v {
                found_filters.push(format!("{k}: {{{k2}: {v2}}}"));
            }
        }
    }

    let return_value = serde_json::Value::Array(
        found_filters
            .into_iter()
            .map(std::convert::Into::into)
            .collect(),
    );

    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        return_value,
    )])])
}
