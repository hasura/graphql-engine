use axum::{http::StatusCode, Json};
use ndc_models;
use std::collections::BTreeMap;

use crate::{
    query::Result,
    types::login::{LoginResponse, ResponseHeaders},
};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "login".into(),
        description: Some("Perform a user login".into()),
        arguments: BTreeMap::from_iter([
            (
                "headers".into(),
                ndc_models::ArgumentInfo {
                    description: Some("headers required for authentication".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "HeaderMap".into(),
                    },
                },
            ),
            (
                "username".into(),
                ndc_models::ArgumentInfo {
                    description: Some("username of the user".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "password".into(),
                ndc_models::ArgumentInfo {
                    description: Some("password of the user".into()),
                    argument_type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
        ]),
        result_type: ndc_models::Type::Named {
            name: "login_response".into(),
        },
    }
}

pub(crate) fn execute(
    arguments: &BTreeMap<String, serde_json::Value>,
) -> Result<serde_json::Value> {
    let _headers = arguments.get("headers").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'headers' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let _username = arguments.get("username").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'username' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let _password = arguments.get("password").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'password' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;

    let login_response = LoginResponse {
        response: true,
        headers: ResponseHeaders {
            cookie: "Set-Cookie: cookie_name=cookie_val;".to_string(),
            session_token: "abcdefghi".to_string(),
        },
    };
    serde_json::to_value(login_response).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "cannot encode response".into(),
                details: serde_json::Value::Null,
            }),
        )
    })
}
