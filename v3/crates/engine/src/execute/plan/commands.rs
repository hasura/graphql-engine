use indexmap::IndexMap;
use ndc_models;
use std::collections::BTreeMap;

use super::selection_set;
use crate::execute::error;
use crate::execute::ir::commands::CommandInfo;
use crate::execute::ir::commands::FunctionBasedCommand;
use crate::execute::ir::commands::ProcedureBasedCommand;
use crate::execute::ndc::FUNCTION_IR_VALUE_COLUMN_NAME;
use crate::execute::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};
use open_dds::commands::ProcedureName;

pub(crate) fn ndc_query<'s, 'ir>(
    ir: &'ir CommandInfo<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc_models::Query, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    let (ndc_nested_field, jl) = ir
        .selection
        .as_ref()
        .map(|nested_selection| {
            selection_set::process_nested_selection(nested_selection, join_id_counter)
        })
        .transpose()?
        .unzip();
    let query = ndc_models::Query {
        aggregates: None,
        fields: Some(IndexMap::from([(
            FUNCTION_IR_VALUE_COLUMN_NAME.to_string(),
            ndc_models::Field::Column {
                column: FUNCTION_IR_VALUE_COLUMN_NAME.to_string(),
                fields: ndc_nested_field,
            },
        )])),
        limit: None,
        offset: None,
        order_by: None,
        predicate: None,
    };
    Ok((query, jl.unwrap_or_default()))
}

pub(crate) fn ndc_query_ir<'s, 'ir>(
    ir: &'ir FunctionBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<(ndc_models::QueryRequest, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    let mut arguments: BTreeMap<String, ndc_models::Argument> = ir
        .command_info
        .arguments
        .iter()
        .map(|(argument_name, argument_value)| {
            (
                argument_name.clone(),
                ndc_models::Argument::Literal {
                    value: argument_value.clone(),
                },
            )
        })
        .collect();

    // Add the variable arguments which are used for remote joins
    for (variable_name, variable_argument) in ir.variable_arguments.iter() {
        arguments.insert(
            variable_name.clone(),
            ndc_models::Argument::Variable {
                name: variable_argument.clone(),
            },
        );
    }

    let (query, jl) = ndc_query(&ir.command_info, join_id_counter)?;
    let mut collection_relationships = BTreeMap::new();
    if let Some(nested_selection) = &ir.command_info.selection {
        selection_set::collect_relationships_from_nested_selection(
            nested_selection,
            &mut collection_relationships,
        )?;
    }
    let query_request = ndc_models::QueryRequest {
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
        ndc_models::MutationRequest,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let (ndc_nested_field, jl) = ir
        .command_info
        .selection
        .as_ref()
        .map(|nested_selection| {
            selection_set::process_nested_selection(nested_selection, join_id_counter)
        })
        .transpose()?
        .unzip();
    let mutation_operation = ndc_models::MutationOperation::Procedure {
        name: procedure_name.to_string(),
        arguments: ir.command_info.arguments.clone(),
        fields: ndc_nested_field,
    };
    let mut collection_relationships = BTreeMap::new();
    if let Some(nested_selection) = &ir.command_info.selection {
        selection_set::collect_relationships_from_nested_selection(
            nested_selection,
            &mut collection_relationships,
        )?;
    }
    let mutation_request = ndc_models::MutationRequest {
        operations: vec![mutation_operation],
        collection_relationships,
    };
    Ok((mutation_request, jl.unwrap_or_default()))
}
