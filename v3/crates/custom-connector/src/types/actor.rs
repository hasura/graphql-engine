use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    arguments::{argument_any, argument_string},
    query::Result,
};

pub(crate) fn definition() -> ndc_models::ObjectType {
    ndc_models::ObjectType {
        description: Some("An actor".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                ndc_models::ObjectField {
                    description: Some("The actor's primary key".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: argument_any(),
                },
            ),
            (
                "name".into(),
                ndc_models::ObjectField {
                    description: Some("The actor's name".into()),
                    r#type: ndc_models::Type::Named {
                        name: "String".into(),
                    },
                    arguments: argument_string(),
                },
            ),
            (
                "movie_id".into(),
                ndc_models::ObjectField {
                    description: Some("The actor's movie ID".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: argument_any(),
                },
            ),
            (
                "favourite_author_id".into(),
                ndc_models::ObjectField {
                    description: Some("The actor's favourite author ID".into()),
                    r#type: ndc_models::Type::Named { name: "Int".into() },
                    arguments: argument_any(),
                },
            ),
        ]),
        foreign_keys: BTreeMap::new(),
    }
}

pub(crate) fn get_actor_movie_id(
    actor: &BTreeMap<ndc_models::FieldName, serde_json::Value>,
) -> Result<i32> {
    let actor_movie_id = actor.get("movie_id").ok_or((
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(ndc_models::ErrorResponse {
            message: "actor movie_id not found".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let actor_movie_id_int = actor_movie_id
        .as_i64()
        .ok_or((
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "actor movie_id is not an integer".into(),
                details: serde_json::Value::Null,
            }),
        ))?
        .try_into()
        .map_err(|_| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "actor movie_id is out of range".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    Ok(actor_movie_id_int)
}
