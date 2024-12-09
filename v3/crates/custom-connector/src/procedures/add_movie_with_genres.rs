use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{
    arguments::{check_all_arguments_used, parse_object_argument},
    query::{eval_nested_field, Result},
    state::AppState,
};

pub(crate) fn procedure_info() -> ndc_models::ProcedureInfo {
    ndc_models::ProcedureInfo {
        name: "add_movie_with_genres".into(),
        description: Some("Add a movie with genres".into()),
        arguments: BTreeMap::from_iter([(
            "movie".into(),
            ndc_models::ArgumentInfo {
                description: Some("The movie to add".into()),
                argument_type: ndc_models::Type::Named {
                    name: "movie".into(),
                },
            },
        )]),
        result_type: ndc_models::Type::Named {
            name: "movie".into(),
        },
    }
}

pub(crate) fn execute(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    fields: Option<&ndc_models::NestedField>,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    state: &mut AppState,
) -> Result<serde_json::Value> {
    let mut arguments = arguments
        .iter()
        .map(|(k, v)| (k.clone(), v))
        .collect::<BTreeMap<_, _>>();
    let movie_obj = parse_object_argument("movie", &mut arguments)?;
    check_all_arguments_used(&arguments)?;

    let id = movie_obj.get("id").ok_or((
        StatusCode::BAD_REQUEST,
        Json(ndc_models::ErrorResponse {
            message: "required argument field 'id' is missing".into(),
            details: serde_json::Value::Null,
        }),
    ))?;
    let id_int: i32 = id
        .as_i64()
        .ok_or((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "argument 'id' is not an integer".into(),
                details: serde_json::Value::Null,
            }),
        ))?
        .try_into()
        .map_err(|_| {
            (
                StatusCode::BAD_REQUEST,
                Json(ndc_models::ErrorResponse {
                    message: "argument 'id' is out of range".into(),
                    details: serde_json::Value::Null,
                }),
            )
        })?;
    let movie_row = movie_obj
        .iter()
        .map(|(k, v)| (ndc_models::FieldName::from(k.as_str()), v.clone()))
        .collect::<BTreeMap<_, _>>();

    if state.movies.insert(id_int, movie_row).is_some() {
        return Err((
            StatusCode::BAD_REQUEST,
            Json(ndc_models::ErrorResponse {
                message: "movie with id already exists".into(),
                details: serde_json::Value::Null,
            }),
        ));
    }

    let row_value = serde_json::to_value(movie_obj).map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(ndc_models::ErrorResponse {
                message: "cannot encode response".into(),
                details: serde_json::Value::Null,
            }),
        )
    })?;

    let row_fields = match fields {
        None => Ok(ndc_models::RowFieldValue(row_value)),
        Some(nested_field) => eval_nested_field(
            collection_relationships,
            &BTreeMap::new(),
            state,
            row_value,
            nested_field,
        ),
    }?;

    Ok(row_fields.0)
}
