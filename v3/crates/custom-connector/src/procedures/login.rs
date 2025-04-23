use axum::{Json, http::StatusCode};
use ndc_models;
use std::collections::BTreeMap;
use uuid::Uuid;

use crate::{
    arguments::{check_all_arguments_used, parse_object_argument, parse_string_argument},
    query::Result,
    types::login::{LoginResponse, ResponseHeaders},
};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "login".into(),
        description: Some("Perform a user login".into()),
        arguments: BTreeMap::from_iter([
            (
                "_headers".into(),
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
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
) -> Result<serde_json::Value> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let _headers = parse_object_argument("_headers", &mut arguments)?;
    let _username = parse_string_argument("username", &mut arguments)?;
    let _password = parse_string_argument("password", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let cookie_val1 = Uuid::new_v4();
    let cookie_val2 = Uuid::new_v4();
    let login_response = LoginResponse {
        response: true,
        headers: ResponseHeaders {
            cookie: format!("foo={cookie_val1:}; bar={cookie_val2:};"),
            session_token: Uuid::new_v4().to_string(),
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
