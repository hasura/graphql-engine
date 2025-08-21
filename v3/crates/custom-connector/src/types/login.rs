use ndc_models;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct LoginResponse {
    pub headers: ResponseHeaders,
    pub response: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SessionResponse {
    pub headers: ResponseHeaders,
    pub response: SessionInfo,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SessionInfo {
    pub token: String,
    pub expiry: String,
    pub session_id: i32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ResponseHeaders {
    #[serde(rename = "Set-Cookie")]
    pub cookie: String,
    pub session_token: String,
}

pub(crate) fn definition_login_response() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("Response to a login action".into()),
        fields: BTreeMap::from_iter([
            (
                "headers".into(),
                ndc_models::ObjectField {
                    description: Some("Response headers to be forwarded".into()),
                    r#type: ndc_models::Type::Named {
                        name: "HeaderMap".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "response".into(),
                ndc_models::ObjectField {
                    description: Some("Authentication successful or not".into()),
                    r#type: ndc_models::Type::Named {
                        name: "Bool".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}

pub(crate) fn definition_session_response() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("Response of session details".into()),
        fields: BTreeMap::from_iter([
            (
                "headers".into(),
                ndc_models::ObjectField {
                    description: Some("Response headers to be forwarded".into()),
                    r#type: ndc_models::Type::Named {
                        name: "HeaderMap".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "response".into(),
                ndc_models::ObjectField {
                    description: Some("Session details".into()),
                    r#type: ndc_models::Type::Named {
                        name: "session_info".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}

pub(crate) fn definition_session_info() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("Session details".into()),
        fields: BTreeMap::from_iter([
            (
                "token".into(),
                ndc_models::ObjectField {
                    description: Some("Session token".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "expiry".into(),
                ndc_models::ObjectField {
                    description: Some("Token expiry".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: BTreeMap::new(),
                },
            ),
            (
                "session_id".into(),
                ndc_models::ObjectField {
                    description: Some("Session ID".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: BTreeMap::new(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}
