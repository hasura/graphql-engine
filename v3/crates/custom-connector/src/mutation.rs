use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_models;

use crate::{procedures, state::AppState};

type Result<A> = std::result::Result<A, (StatusCode, Json<ndc_models::ErrorResponse>)>;

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
