use ndc_models;
use ndc_models_v01;

pub fn upgrade_rowset_to_v02(rowset: ndc_models_v01::RowSet) -> ndc_models::RowSet {
    ndc_models::RowSet {
        aggregates: rowset.aggregates.map(|aggregates| {
            aggregates
                .into_iter()
                .map(|(name, value)| (ndc_models::FieldName::new(name.into_inner()), value))
                .collect()
        }),
        rows: rowset.rows.map(|rows| {
            rows.into_iter()
                .map(|row| {
                    row.into_iter()
                        .map(|(name, ndc_models_v01::RowFieldValue(v))| {
                            (
                                ndc_models::FieldName::new(name.into_inner()),
                                ndc_models::RowFieldValue(v),
                            )
                        })
                        .collect()
                })
                .collect()
        }),
        groups: None, // v0.1.x does not have groups
    }
}

pub fn upgrade_mutation_response_to_v02(
    mutation_response: ndc_models_v01::MutationResponse,
) -> ndc_models::MutationResponse {
    ndc_models::MutationResponse {
        operation_results: mutation_response
            .operation_results
            .into_iter()
            .map(upgrade_mutation_operation_result_to_latest)
            .collect(),
    }
}

fn upgrade_mutation_operation_result_to_latest(
    mutation_operation_result: ndc_models_v01::MutationOperationResults,
) -> ndc_models::MutationOperationResults {
    match mutation_operation_result {
        ndc_models_v01::MutationOperationResults::Procedure { result } => {
            ndc_models::MutationOperationResults::Procedure { result }
        }
    }
}
