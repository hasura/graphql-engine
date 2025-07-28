use std::collections::BTreeMap;

use axum::{Json, http::StatusCode};
use ndc_models;

use crate::{procedures, state::AppState};

type Result<A> = std::result::Result<A, (StatusCode, Json<ndc_models::ErrorResponse>)>;

// Add this new function to handle relational inserts
#[allow(clippy::print_stdout)]
pub fn execute_relational_insert(
    _state: &AppState,
    request: &ndc_models::RelationalInsertRequest,
) -> Result<ndc_models::RelationalInsertResponse> {
    println!(
        "[INSERT]: collection={}, columns={}, rows={:?}",
        request.collection.as_str(),
        request
            .columns
            .iter()
            .map(ndc_models::FieldName::as_str)
            .collect::<Vec<_>>()
            .join(","),
        request.rows
    );

    Ok(ndc_models::RelationalInsertResponse { affected_rows: 0 })
}

// Add this new function to handle relational updates
#[allow(clippy::print_stdout)]
pub fn execute_relational_update(
    _state: &AppState,
    request: &ndc_models::RelationalUpdateRequest,
) -> Result<ndc_models::RelationalUpdateResponse> {
    println!(
        "[UPDATE]: collection={}, relation={:?}",
        request.collection.as_str(),
        request.relation
    );

    Ok(ndc_models::RelationalUpdateResponse { affected_rows: 0 })
}

// Add this new function to handle relational deletes
#[allow(clippy::print_stdout)]
pub fn execute_relational_delete(
    _state: &AppState,
    request: &ndc_models::RelationalDeleteRequest,
) -> Result<ndc_models::RelationalDeleteResponse> {
    println!(
        "[DELETE]: collection={}, relation={:?}",
        request.collection.as_str(),
        request.relation
    );

    Ok(ndc_models::RelationalDeleteResponse { affected_rows: 0 })
}

pub fn execute_mutation_request(
    state: &AppState,
    request: &ndc_models::MutationRequest,
) -> Result<ndc_models::MutationResponse> {
    let mut operation_results = vec![];

    for operation in &request.operations {
        let operation_result =
            execute_mutation_operation(state, &request.collection_relationships, operation)?;
        operation_results.push(operation_result);
    }

    Ok(ndc_models::MutationResponse { operation_results })
}

fn execute_mutation_operation(
    state: &AppState,
    collection_relationships: &BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
    operation: &ndc_models::MutationOperation,
) -> Result<ndc_models::MutationOperationResults> {
    // We don't actually want to mutate the app state since the connector is used in tests.
    let mut state = state.clone();

    match operation {
        ndc_models::MutationOperation::Procedure {
            name,
            arguments,
            fields,
        } => procedures::execute_procedure(
            name,
            arguments,
            fields.as_ref(),
            collection_relationships,
            &mut state,
        )
        .map(|result| ndc_models::MutationOperationResults::Procedure { result }),
    }
}
