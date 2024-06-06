use ndc_models;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct LoginResponse {
    pub headers: ResponseHeaders,
    pub response: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ResponseHeaders {
    pub cookie: String,
    pub session_token: String,
}

pub(crate) fn definition() -> ndc_models::ObjectType {
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
    }
}
