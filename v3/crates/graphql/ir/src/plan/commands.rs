use indexmap::IndexMap;
use open_dds::data_connector::CollectionName;
use open_dds::data_connector::DataConnectorColumnName;
use std::collections::BTreeMap;

use super::arguments;
use super::error;
use super::selection_set;
use crate::plan::Plan;
use crate::{CommandInfo, FunctionBasedCommand, ProcedureBasedCommand};
use open_dds::commands::ProcedureName;
use plan_types::{
    Argument, ExecutionTree, Field, FieldsSelection, JoinLocations, MutationExecutionPlan,
    NdcFieldAlias, NdcRelationshipName, PredicateQueryTrees, QueryExecutionPlan, QueryNodeNew,
    Relationship, UniqueNumber, VariableName, FUNCTION_IR_VALUE_COLUMN_NAME,
};

pub(crate) fn plan_query_node(
    ir: &CommandInfo<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    unique_number: &mut UniqueNumber,
) -> Result<Plan<QueryNodeNew>, error::Error> {
    let mut ndc_nested_field = None;
    let mut join_locations = JoinLocations::new();
    let mut remote_predicates = PredicateQueryTrees::new();

    if let Some(nested_selection) = &ir.selection {
        let Plan {
            inner: fields,
            join_locations: nested_join_locations,
            remote_predicates: nested_remote_predicates,
        } = selection_set::plan_nested_selection(
            nested_selection,
            ir.data_connector.capabilities.supported_ndc_version,
            relationships,
            unique_number,
        )?;
        ndc_nested_field = Some(fields);
        join_locations = nested_join_locations;
        remote_predicates = nested_remote_predicates;
    }
    let query = QueryNodeNew {
        aggregates: None,
        fields: Some(FieldsSelection {
            fields: IndexMap::from([(
                NdcFieldAlias::from(FUNCTION_IR_VALUE_COLUMN_NAME),
                Field::Column {
                    column: DataConnectorColumnName::from(FUNCTION_IR_VALUE_COLUMN_NAME),
                    fields: ndc_nested_field,
                    arguments: BTreeMap::new(),
                },
            )]),
        }),
        limit: None,
        offset: None,
        order_by: None,
        predicate: None,
    };
    Ok(Plan {
        inner: query,
        join_locations,
        remote_predicates,
    })
}

pub(crate) fn plan_query_execution(
    ir: &FunctionBasedCommand<'_>,
    unique_number: &mut UniqueNumber,
) -> Result<ExecutionTree, error::Error> {
    let mut collection_relationships = BTreeMap::new();
    let (mut arguments, mut remote_predicates) = arguments::plan_arguments(
        &ir.command_info.arguments,
        &mut collection_relationships,
        unique_number,
    )?;

    // Add the variable arguments which are used for remote joins
    for (variable_name, variable_argument) in &ir.variable_arguments {
        arguments.insert(
            variable_name.clone(),
            Argument::Variable {
                name: VariableName(variable_argument.clone()),
            },
        );
    }

    let Plan {
        inner: query_node,
        join_locations: remote_join_executions,
        remote_predicates: query_remote_predicates,
    } = plan_query_node(
        &ir.command_info,
        &mut collection_relationships,
        unique_number,
    )?;

    remote_predicates.0.extend(query_remote_predicates.0);

    let query_execution_plan = QueryExecutionPlan {
        query_node,
        collection: CollectionName::from(ir.function_name.as_str()),
        arguments: arguments.clone(),
        collection_relationships,
        variables: None,
        data_connector: ir.command_info.data_connector.clone(),
    };
    Ok(ExecutionTree {
        query_execution_plan,
        remote_join_executions,
        remote_predicates,
    })
}

pub(crate) fn plan_mutation_execution(
    procedure_name: &ProcedureName,
    ir: &ProcedureBasedCommand<'_>,
    unique_number: &mut UniqueNumber,
) -> Result<Plan<MutationExecutionPlan>, error::Error> {
    let mut ndc_nested_field = None;
    let mut join_locations = JoinLocations::new();
    let mut remote_predicates = PredicateQueryTrees::new();
    let mut collection_relationships = BTreeMap::new();

    if let Some(nested_selection) = &ir.command_info.selection {
        let Plan {
            inner: fields,
            join_locations: nested_join_locations,
            remote_predicates: nested_remote_predicates,
        } = selection_set::plan_nested_selection(
            nested_selection,
            ir.command_info
                .data_connector
                .capabilities
                .supported_ndc_version,
            &mut collection_relationships,
            unique_number,
        )?;
        ndc_nested_field = Some(fields);
        join_locations = nested_join_locations;
        remote_predicates = nested_remote_predicates;
    }
    let mutation_request = MutationExecutionPlan {
        procedure_name: procedure_name.clone(),
        procedure_arguments: arguments::plan_mutation_arguments(
            &ir.command_info.arguments,
            &mut collection_relationships,
            unique_number,
        )?,
        procedure_fields: ndc_nested_field,
        collection_relationships,
        data_connector: ir.command_info.data_connector.clone(),
    };
    Ok(Plan {
        inner: mutation_request,
        join_locations,
        remote_predicates,
    })
}
