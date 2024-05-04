use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    collections::institutions::{self, parse_institution_query},
    query::{parse_object_argument, Result},
    state::{AppState, Row},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_institutions_by_institution_query".into(),
        description: Some("Get institutions by specifying parts of institution object. For example by 'location.city'. All fields are optional.".into()),
        arguments: BTreeMap::from_iter([(
            "institution_query".into(),
            ndc_models::ArgumentInfo {
                description: Some("The institution query object. All fields are optional".into()),
                argument_type: ndc_models::Type::Named {
                    name: "institution".into(),
                },
            },
        )]),
        result_type: ndc_models::Type::Array {
            element_type: Box::new(ndc_models::Type::Named {
                name: "institution".into(),
            }),
        },
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<String, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    let query_object = parse_object_argument("institution_query", arguments)?;
    let institution_query = parse_institution_query(query_object)?;
    let filtered_institutions =
        institutions::filter_by_institution_object_input(state, institution_query)
            .collect::<Result<Vec<_>>>()?;

    let result_value = serde_json::to_value(filtered_institutions).map_err(|_| {
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
        result_value,
    )])])
}
