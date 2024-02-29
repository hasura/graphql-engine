use std::collections::BTreeMap;

use ndc_client::models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn collection_info() -> models::CollectionInfo {
    models::CollectionInfo {
        name: "movies".into(),
        description: Some("A collection of movies".into()),
        collection_type: "movie".into(),
        arguments: BTreeMap::new(),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "MovieByID".into(),
            models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
    }
}

pub(crate) fn rows(state: &AppState) -> Result<Vec<Row>> {
    Ok(state.movies.values().cloned().collect())
}
