use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{
    arguments::check_all_arguments_used,
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn collection_info() -> ndc_models::CollectionInfo {
    ndc_models::CollectionInfo {
        name: "actors".into(),
        description: Some("A collection of actors".into()),
        collection_type: "actor".into(),
        arguments: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "ActorByID".into(),
            ndc_models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    check_all_arguments_used(arguments)?;
    Ok(state.actors.values().cloned().collect())
}

pub(crate) fn filter_actors_by_name<'a>(
    state: &'a AppState,
    filter_first_name: Option<&'a str>,
    filter_last_name: Option<&'a str>,
) -> impl std::iter::Iterator<Item = Result<&'a Row>> {
    state
        .actors
        .values()
        .map(|actor| {
            let actor_name = actor
                .get("name")
                .ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: "actor missing name".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?
                .as_str()
                .ok_or_else(|| {
                    (
                        StatusCode::BAD_REQUEST,
                        Json(ndc_models::ErrorResponse {
                            message: "actor name must be a string".into(),
                            details: serde_json::Value::Null,
                        }),
                    )
                })?;
            let names = actor_name.split(' ').collect::<Vec<_>>();
            let actor_first_name = names
                .first()
                .map_or(String::new(), std::string::ToString::to_string);
            let actor_last_name = names.into_iter().skip(1).collect::<Vec<_>>().join(" ");

            Ok((actor_first_name, actor_last_name, actor))
        })
        .filter_map(move |result| match result {
            Ok((actor_first_name, actor_last_name, actor)) => {
                if filter_first_name.is_none_or(|first_name| first_name == actor_first_name)
                    && filter_last_name.is_none_or(|last_name| last_name == actor_last_name)
                {
                    Some(Ok(actor))
                } else {
                    None
                }
            }
            Err(err) => Some(Err(err)),
        })
}
