use std::collections::BTreeMap;

use axum::{http::StatusCode, Json};
use ndc_client::models;

use crate::{procedures, state::AppState};

type Result<A> = std::result::Result<A, (StatusCode, Json<models::ErrorResponse>)>;

pub fn execute_mutation_request(
    state: &AppState,
    request: models::MutationRequest,
) -> Result<models::MutationResponse> {
    let mut operation_results = vec![];

    for operation in request.operations.iter() {
        let operation_result =
            execute_mutation_operation(state, &request.collection_relationships, operation)?;
        operation_results.push(operation_result);
    }

    Ok(models::MutationResponse { operation_results })
}

fn execute_mutation_operation(
    state: &AppState,
    collection_relationships: &BTreeMap<String, models::Relationship>,
    operation: &models::MutationOperation,
) -> Result<models::MutationOperationResults> {
    // We don't actually want to mutate the app state since the connector is used in tests.
    let mut state = state.clone();

    match operation {
        models::MutationOperation::Procedure {
            name,
            arguments,
            fields,
        } => procedures::execute_procedure(
            name,
            arguments,
            fields,
            collection_relationships,
            &mut state,
        )
        .map(|result| models::MutationOperationResults::Procedure { result }),
    }
}
