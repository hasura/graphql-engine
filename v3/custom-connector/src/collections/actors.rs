use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn collection_info() -> models::CollectionInfo {
    models::CollectionInfo {
        name: "actors".into(),
        description: Some("A collection of actors".into()),
        collection_type: "actor".into(),
        arguments: BTreeMap::new(),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "ActorByID".into(),
            models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
    }
}

pub(crate) fn rows(state: &AppState) -> Result<Vec<Row>> {
    Ok(state.actors.values().cloned().collect())
}

pub(crate) fn filter_actors_by_name<'a>(
    state: &'a AppState,
    filter_first_name: Option<&'a str>,
    filter_last_name: Option<&'a str>,
) -> impl std::iter::Iterator<Item = Result<&'a BTreeMap<String, serde_json::Value>>> {
    state
        .actors
        .values()
        .map(|actor| {
            let actor_name = actor
                .get("name")
                .ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(models::ErrorResponse {
                            message: "actor missing name".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?
                .as_str()
                .ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(models::ErrorResponse {
                            message: "actor name must be a string".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;
            let names = actor_name.split(' ').collect::<Vec<_>>();
            let actor_first_name = names
                .first()
                .map(|str| str.to_string())
                .unwrap_or(String::new());
            let actor_last_name = names.into_iter().skip(1).collect::<Vec<_>>().join(" ");

            Ok((actor_first_name, actor_last_name, actor))
        })
        .filter_map(move |result| match result {
            Ok((actor_first_name, actor_last_name, actor)) => {
                if filter_first_name
                    .map(|first_name| first_name == actor_first_name)
                    .unwrap_or(true)
                    && filter_last_name
                        .map(|last_name| last_name == actor_last_name)
                        .unwrap_or(true)
                {
                    Some(Ok(actor))
                } else {
                    None
                }
            }
            Err(err) => Some(Err(err)),
        })
}
