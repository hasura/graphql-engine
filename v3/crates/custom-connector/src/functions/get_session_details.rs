use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;
use uuid::Uuid;

use crate::{
    arguments::{check_all_arguments_used, parse_i64_argument, parse_object_argument},
    query::Result,
    state::Row,
    types::login::{ResponseHeaders, SessionInfo, SessionResponse},
};

pub(crate) fn function_info() -> ndc_models::FunctionInfo {
    ndc_models::FunctionInfo {
        name: "get_session_details".into(),
        description: Some("Get session details of a user".into()),
        arguments: BTreeMap::from_iter([
            (
                "_headers".into(),
                ndc_models::ArgumentInfo {
                    description: Some("headers required for session details".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "HeaderMap".into(),
                    },
                },
            ),
            (
                "user_id".into(),
                ndc_models::ArgumentInfo {
                    description: Some("user id of the user".into()),
                    argument_type: ndc_models::Type::Named { name: "Int".into() },
                },
            ),
        ]),
        result_type: ndc_models::Type::Named {
            name: "session_response".into(),
        },
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
) -> Result<Vec<Row>> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let _headers = parse_object_argument("_headers", &mut arguments)?;
    let _user_id = parse_i64_argument("user_id", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let cookie_val1 = Uuid::new_v4();
    let cookie_val2 = Uuid::new_v4();
    let token = "64a6c518-4a5b-4067-a99f-3abc11eeeacf".to_string();
    let session_response = SessionResponse {
        response: SessionInfo {
            expiry: "2025-12-12T05:48:33+0000".to_string(),
            token: token.clone(),
        },
        headers: ResponseHeaders {
            cookie: format!("foo={cookie_val1:}; bar={cookie_val2:};"),
            session_token: token,
        },
    };
    let response_value = serde_json::to_value(session_response).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "cannot encode response".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;
    Ok(vec![BTreeMap::from_iter([(
        "__value".into(),
        response_value,
    )])])
}
