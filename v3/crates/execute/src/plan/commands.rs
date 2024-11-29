use indexmap::IndexMap;
use open_dds::data_connector::CollectionName;
use open_dds::data_connector::DataConnectorColumnName;
use std::collections::BTreeMap;

use super::arguments;
use super::error;
use super::field;
use super::mutation;
use super::query;
use super::selection_set;
use crate::remote_joins::types::JoinLocations;
use graphql_ir::{CommandInfo, FunctionBasedCommand, ProcedureBasedCommand};
use open_dds::commands::ProcedureName;
use plan_types::FUNCTION_IR_VALUE_COLUMN_NAME;
use plan_types::{NdcFieldAlias, NdcRelationshipName, Relationship, VariableName};

pub(crate) fn plan_query_node<'s>(
    ir: &CommandInfo<'s>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
) -> Result<(query::UnresolvedQueryNode<'s>, JoinLocations<'s>), error::Error> {
    let mut ndc_nested_field = None;
    let mut jl = JoinLocations::new();
    if let Some(nested_selection) = &ir.selection {
        let (fields, locations) = selection_set::plan_nested_selection(
            nested_selection,
            ir.data_connector.capabilities.supported_ndc_version,
            relationships,
        )?;
        ndc_nested_field = Some(fields);
        jl = locations;
    }
    let query = query::QueryNode {
        aggregates: None,
        fields: Some(IndexMap::from([(
            NdcFieldAlias::from(FUNCTION_IR_VALUE_COLUMN_NAME),
            field::Field::Column {
                column: DataConnectorColumnName::from(FUNCTION_IR_VALUE_COLUMN_NAME),
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

pub(crate) fn plan_query_execution<'s>(
    ir: &FunctionBasedCommand<'s>,
) -> Result<(query::UnresolvedQueryExecutionPlan<'s>, JoinLocations<'s>), error::Error> {
    let mut collection_relationships = BTreeMap::new();
    let mut arguments =
        arguments::plan_arguments(&ir.command_info.arguments, &mut collection_relationships)?;

    // Add the variable arguments which are used for remote joins
    for (variable_name, variable_argument) in &ir.variable_arguments {
        arguments.insert(
            variable_name.clone(),
            arguments::Argument::Variable {
                name: VariableName(variable_argument.clone()),
            },
        );
    }

    let (query_node, jl) = plan_query_node(&ir.command_info, &mut collection_relationships)?;

    let query_request = query::UnresolvedQueryExecutionPlan {
        query_node,
        collection: CollectionName::from(ir.function_name.as_str()),
        arguments: arguments.clone(),
        collection_relationships,
        variables: None,
        data_connector: ir.command_info.data_connector.clone(),
    };
    Ok((query_request, jl))
}

pub(crate) fn plan_mutation_execution<'s, 'ir>(
    procedure_name: &'ir ProcedureName,
    ir: &'ir ProcedureBasedCommand<'s>,
) -> Result<
    (
        mutation::UnresolvedMutationExecutionPlan<'s>,
        JoinLocations<'s>,
    ),
    error::Error,
> {
    let mut ndc_nested_field = None;
    let mut jl = JoinLocations::new();
    let mut collection_relationships = BTreeMap::new();
    if let Some(nested_selection) = &ir.command_info.selection {
        let (fields, locations) = selection_set::plan_nested_selection(
            nested_selection,
            ir.command_info
                .data_connector
                .capabilities
                .supported_ndc_version,
            &mut collection_relationships,
        )?;
        ndc_nested_field = Some(fields);
        jl = locations;
    }
    let mutation_request = mutation::MutationExecutionPlan {
        procedure_name: procedure_name.clone(),
        procedure_arguments: arguments::plan_mutation_arguments(
            &ir.command_info.arguments,
            &mut collection_relationships,
        )?,
        procedure_fields: ndc_nested_field,
        collection_relationships,
        data_connector: ir.command_info.data_connector.clone(),
    };
    Ok((mutation_request, jl))
}
