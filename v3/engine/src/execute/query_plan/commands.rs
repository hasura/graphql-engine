use ndc_client as ndc;
use std::collections::BTreeMap;

use super::selection_set;
use crate::execute::error;
use crate::execute::ir::commands::CommandInfo;
use crate::execute::ir::commands::FunctionBasedCommand;
use crate::execute::ir::commands::ProcedureBasedCommand;
use crate::execute::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};
use open_dds::commands::ProcedureName;

pub(crate) fn ndc_query<'s, 'ir>(
    ir: &'ir CommandInfo<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc::models::Query, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    let (ndc_fields, jl) = selection_set::process_selection_set_ir(&ir.selection, join_id_counter)?;
    let query = ndc::models::Query {
        aggregates: None,
        fields: Some(ndc_fields),
        limit: None,
        offset: None,
        order_by: None,
        predicate: None,
    };
    Ok((query, jl))
}

pub(crate) fn ndc_query_ir<'s, 'ir>(
    ir: &'ir FunctionBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        ndc::models::QueryRequest,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let mut arguments: BTreeMap<String, ndc_client::models::Argument> = ir
        .command_info
        .arguments
        .iter()
        .map(|(argument_name, argument_value)| {
            (
                argument_name.clone(),
                ndc_client::models::Argument::Literal {
                    value: argument_value.clone(),
                },
            )
        })
        .collect();

    // Add the variable arguments which are used for remote joins
    for (variable_name, variable_argument) in ir.variable_arguments.iter() {
        arguments.insert(
            variable_name.clone(),
            ndc_client::models::Argument::Variable {
                name: variable_argument.clone(),
            },
        );
    }

    let (query, jl) = ndc_query(&ir.command_info, join_id_counter)?;
    let mut collection_relationships = BTreeMap::new();
    selection_set::collect_relationships_from_selection(
        &ir.command_info.selection,
        &mut collection_relationships,
    )?;
    let query_request = ndc::models::QueryRequest {
        query,
        collection: ir.function_name.to_string(),
        arguments,
        collection_relationships,
        variables: None,
    };
    Ok((query_request, jl))
}

pub(crate) fn ndc_mutation_ir<'s, 'ir>(
    procedure_name: &ProcedureName,
    ir: &'ir ProcedureBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        ndc::models::MutationRequest,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let (ndc_fields, jl) =
        selection_set::process_selection_set_ir(&ir.command_info.selection, join_id_counter)?;
    let mutation_operation = ndc::models::MutationOperation::Procedure {
        name: procedure_name.to_string(),
        arguments: ir.command_info.arguments.clone(),
        fields: Some(ndc::models::NestedField::Object(
            ndc::models::NestedObject { fields: ndc_fields },
        )),
    };
    let mut collection_relationships = BTreeMap::new();
    selection_set::collect_relationships_from_selection(
        &ir.command_info.selection,
        &mut collection_relationships,
    )?;
    let mutation_request = ndc::models::MutationRequest {
        operations: vec![mutation_operation],
        collection_relationships,
    };
    Ok((mutation_request, jl))
}
