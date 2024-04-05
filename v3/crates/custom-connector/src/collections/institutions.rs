use std::collections::BTreeMap;

use ndc_models;

use crate::{
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn collection_info() -> ndc_models::CollectionInfo {
    ndc_models::CollectionInfo {
        name: "institutions".into(),
        description: Some("A collection of institutions".into()),
        collection_type: "institution".into(),
        arguments: BTreeMap::new(),
        foreign_keys: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "InstitutionByID".into(),
            ndc_models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
    }
}

pub(crate) fn rows(state: &AppState) -> Result<Vec<Row>> {
    Ok(state.institutions.values().cloned().collect())
}
