use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::query::Result;

pub(crate) fn definition() -> models::ObjectType {
    models::ObjectType {
        description: Some("An actor".into()),
        fields: BTreeMap::from_iter([
            (
                "id".into(),
                models::ObjectField {
                    description: Some("The actor's primary key".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "name".into(),
                models::ObjectField {
                    description: Some("The actor's name".into()),
                    r#type: models::Type::Named {
                        name: "String".into(),
                    },
                },
            ),
            (
                "movie_id".into(),
                models::ObjectField {
                    description: Some("The actor's movie ID".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
            (
                "favourite_author_id".into(),
                models::ObjectField {
                    description: Some("The actor's favourite author ID".into()),
                    r#type: models::Type::Named { name: "Int".into() },
                },
            ),
        ]),
    }
}

pub(crate) fn get_actor_movie_id(actor: &BTreeMap<String, serde_json::Value>) -> Result<i64> {
    let actor_movie_id = actor.get("movie_id").ok_or((
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(models::ErrorResponse {
            message: "actor movie_id not found".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let actor_movie_id_int = actor_movie_id.as_i64().ok_or((
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(models::ErrorResponse {
            message: "actor movie_id is not an integer".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    Ok(actor_movie_id_int)
}
