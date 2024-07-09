use indexmap::IndexMap;
use std::collections::BTreeMap;

use super::common;
use super::error;
use super::selection_set;
use crate::ir::commands::CommandInfo;
use crate::ir::commands::FunctionBasedCommand;
use crate::ir::commands::ProcedureBasedCommand;
use crate::ndc::FUNCTION_IR_VALUE_COLUMN_NAME;
use crate::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};
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
            ndc_models::FieldName::from(FUNCTION_IR_VALUE_COLUMN_NAME),
            ndc_models::Field::Column {
                column: ndc_models::FieldName::from(FUNCTION_IR_VALUE_COLUMN_NAME),
                fields: ndc_nested_field,
                arguments: BTreeMap::new(),
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
    let mut arguments = common::ndc_arguments(&ir.command_info.arguments)?;

    // Add the variable arguments which are used for remote joins
    for (variable_name, variable_argument) in &ir.variable_arguments {
        arguments.insert(
            ndc_models::ArgumentName::from(variable_name.as_str()),
            ndc_models::Argument::Variable {
                name: ndc_models::VariableName::from(variable_argument.as_str()),
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
        collection: ndc_models::CollectionName::from(ir.function_name.as_str()),
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
        name: ndc_models::ProcedureName::from(procedure_name.as_str()),
        arguments: common::ndc_raw_arguments(&ir.command_info.arguments)?,
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
