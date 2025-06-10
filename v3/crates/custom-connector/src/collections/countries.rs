use std::collections::BTreeMap;

use ndc_models;

use crate::{
    arguments::check_all_arguments_used,
    query::Result,
    state::{AppState, Row},
};

pub(crate) fn collection_info() -> ndc_models::CollectionInfo {
    ndc_models::CollectionInfo {
        name: "countries".into(),
        description: Some("A collection of countries".into()),
        collection_type: "country".into(),
        arguments: BTreeMap::new(),
        uniqueness_constraints: BTreeMap::from_iter([(
            "CountryByID".into(),
            ndc_models::UniquenessConstraint {
                unique_columns: vec!["id".into()],
            },
        )]),
        relational_mutations: Some(ndc_models::RelationalMutationInfo {
            insertable: true,
            updatable: true,
            deletable: true,
        }),
    }
}

pub(crate) fn rows(
    arguments: &BTreeMap<ndc_models::ArgumentName, serde_json::Value>,
    state: &AppState,
) -> Result<Vec<Row>> {
    check_all_arguments_used(arguments)?;
    Ok(state.countries.values().cloned().collect())
}
