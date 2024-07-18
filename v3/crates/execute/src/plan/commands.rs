use indexmap::IndexMap;
use std::collections::BTreeMap;

use super::common;
use super::error;
use super::selection_set;
use super::types;
use crate::ir::commands::CommandInfo;
use crate::ir::commands::FunctionBasedCommand;
use crate::ir::commands::ProcedureBasedCommand;
use crate::ndc::FUNCTION_IR_VALUE_COLUMN_NAME;
use crate::remote_joins::types::{JoinLocations, MonotonicCounter, RemoteJoin};
use open_dds::commands::ProcedureName;

pub(crate) fn plan_query_node<'s, 'ir>(
    ir: &'ir CommandInfo<'s>,
    join_id_counter: &mut MonotonicCounter,
    relationships: &mut BTreeMap<ndc_models::RelationshipName, ndc_models::Relationship>,
) -> Result<(types::QueryNode<'s>, JoinLocations<RemoteJoin<'s, 'ir>>), error::Error> {
    let mut ndc_nested_field = None;
    let mut jl = JoinLocations::new();
    if let Some(nested_selection) = &ir.selection {
        let (fields, locations) = selection_set::plan_nested_selection(
            nested_selection,
            join_id_counter,
            ir.data_connector.capabilities.supported_ndc_version,
            relationships,
        )?;
        ndc_nested_field = Some(fields);
        jl = locations;
    }
    let query = types::QueryNode {
        aggregates: None,
        groups: None,
        fields: Some(IndexMap::from([(
            ndc_models::FieldName::from(FUNCTION_IR_VALUE_COLUMN_NAME),
            types::Field::Column {
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
    Ok((query, jl))
}

pub(crate) fn plan_query_execution<'s, 'ir>(
    ir: &'ir FunctionBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        types::QueryExecutionPlan<'s>,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let mut collection_relationships = BTreeMap::new();
    let mut arguments = common::plan_ndc_arguments(
        &ir.command_info.arguments,
        ir.command_info
            .data_connector
            .capabilities
            .supported_ndc_version,
        &mut collection_relationships,
    )?;

    // Add the variable arguments which are used for remote joins
    for (variable_name, variable_argument) in &ir.variable_arguments {
        arguments.insert(
            ndc_models::ArgumentName::from(variable_name.as_str()),
            types::Argument::Variable {
                name: ndc_models::VariableName::from(variable_argument.as_str()),
            },
        );
    }

    let (query_node, jl) = plan_query_node(
        &ir.command_info,
        join_id_counter,
        &mut collection_relationships,
    )?;
    if let Some(nested_selection) = &ir.command_info.selection {
        selection_set::collect_relationships_from_nested_selection(
            nested_selection,
            &mut collection_relationships,
        )?;
    }

    let query_request = types::QueryExecutionPlan {
        query_node,
        collection: ndc_models::CollectionName::from(ir.function_name.as_str()),
        arguments: arguments.clone(),
        collection_relationships,
        variables: None,
        data_connector: ir.command_info.data_connector,
    };
    Ok((query_request, jl))
}

pub(crate) fn plan_mutation_execution<'s, 'ir>(
    procedure_name: &'ir ProcedureName,
    ir: &'ir ProcedureBasedCommand<'s>,
    join_id_counter: &mut MonotonicCounter,
) -> Result<
    (
        types::MutationExecutionPlan<'s>,
        JoinLocations<RemoteJoin<'s, 'ir>>,
    ),
    error::Error,
> {
    let mut ndc_nested_field = None;
    let mut jl = JoinLocations::new();
    let mut collection_relationships = BTreeMap::new();
    if let Some(nested_selection) = &ir.command_info.selection {
        let (fields, locations) = selection_set::plan_nested_selection(
            nested_selection,
            join_id_counter,
            ir.command_info
                .data_connector
                .capabilities
                .supported_ndc_version,
            &mut collection_relationships,
        )?;
        ndc_nested_field = Some(fields);
        jl = locations;
    }
    if let Some(nested_selection) = &ir.command_info.selection {
        selection_set::collect_relationships_from_nested_selection(
            nested_selection,
            &mut collection_relationships,
        )?;
    }
    let mutation_request = types::MutationExecutionPlan {
        procedure_name: ndc_models::ProcedureName::from(procedure_name.as_str()),
        procedure_arguments: common::plan_ndc_arguments(
            &ir.command_info.arguments,
            ir.command_info
                .data_connector
                .capabilities
                .supported_ndc_version,
            &mut collection_relationships,
        )?,
        procedure_fields: ndc_nested_field,
        collection_relationships,
        data_connector: ir.command_info.data_connector,
    };
    Ok((mutation_request, jl))
}
