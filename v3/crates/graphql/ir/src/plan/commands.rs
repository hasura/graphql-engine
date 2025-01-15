use indexmap::IndexMap;
use open_dds::data_connector::CollectionName;
use open_dds::data_connector::DataConnectorColumnName;
use std::collections::BTreeMap;

use super::arguments;
use super::error;
use super::selection_set;
use crate::plan::Plan;
use crate::{CommandInfo, CommandSelection, FunctionBasedCommand, ProcedureBasedCommand};
use hasura_authn_core::Session;
use metadata_resolve::Metadata;
use open_dds::commands::ProcedureName;
use plan_types::{
    Argument, ExecutionTree, Field, FieldsSelection, JoinLocations, MutationExecutionPlan,
    NdcFieldAlias, NdcRelationshipName, PredicateQueryTrees, QueryExecutionPlan, QueryNodeNew,
    Relationship, UniqueNumber, VariableName, FUNCTION_IR_VALUE_COLUMN_NAME,
};
use std::sync::Arc;

pub(crate) fn plan_query_node(
    ir: &CommandInfo<'_>,
    relationships: &mut BTreeMap<NdcRelationshipName, Relationship>,
    metadata: &'_ Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<Plan<QueryNodeNew>, error::Error> {
    let mut ndc_nested_field = None;
    let mut join_locations = JoinLocations::new();
    let mut remote_predicates = PredicateQueryTrees::new();

    match &ir.selection {
        CommandSelection::OpenDd { .. } => todo!("plan_query_node OpenDd"),
        CommandSelection::Ir {
            selection,
            arguments: _, // TODO: why aren't these used? I think it's because this function is used by
                          // relationships
        } => {
            if let Some(nested_selection) = &selection {
                let Plan {
                    inner: fields,
                    join_locations: nested_join_locations,
                    remote_predicates: nested_remote_predicates,
                } = selection_set::plan_nested_selection(
                    nested_selection,
                    ir.data_connector.capabilities.supported_ndc_version,
                    relationships,
                    metadata,
                    session,
                    request_headers,
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
    }
}

pub(crate) fn plan_query_execution(
    ir: &FunctionBasedCommand<'_>,
    metadata: &'_ Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<ExecutionTree, error::Error> {
    let mut collection_relationships = BTreeMap::new();
    match &ir.command_info.selection {
        CommandSelection::OpenDd { selection } => {
            // TODO: expose more specific function in `plan` for command selections
            let single_node_execution_plan = plan::query_to_plan(
                &open_dds::query::Query::Command(selection.clone()),
                metadata,
                session,
                request_headers,
                unique_number,
            )?;
            match single_node_execution_plan {
                plan::SingleNodeExecutionPlan::Query(execution_tree) => Ok(execution_tree),
                plan::SingleNodeExecutionPlan::Mutation(_) => {
                    // this may be unavoidable as we don't know ahead of time which kind of function we're
                    // invoking yet
                    Err(error::Error::PlanExpectedQueryGotMutation)
                }
            }
        }
        CommandSelection::Ir {
            selection: _,
            arguments,
        } => {
            let (mut arguments, mut remote_predicates) =
                arguments::plan_arguments(arguments, &mut collection_relationships, unique_number)?;

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
                metadata,
                session,
                request_headers,
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
    }
}

pub(crate) fn plan_mutation_execution(
    procedure_name: &ProcedureName,
    ir: &ProcedureBasedCommand<'_>,
    metadata: &'_ Metadata,
    session: &Arc<Session>,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<Plan<MutationExecutionPlan>, error::Error> {
    let mut ndc_nested_field = None;
    let mut join_locations = JoinLocations::new();
    let mut remote_predicates = PredicateQueryTrees::new();
    let mut collection_relationships = BTreeMap::new();

    match &ir.command_info.selection {
        CommandSelection::OpenDd { .. } => todo!("plan_mutation_execution OpenDd"),
        CommandSelection::Ir {
            selection,
            arguments,
        } => {
            if let Some(nested_selection) = &selection {
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
                    metadata,
                    session,
                    request_headers,
                    unique_number,
                )?;
                ndc_nested_field = Some(fields);
                join_locations = nested_join_locations;
                remote_predicates = nested_remote_predicates;
            }
            let mutation_request = MutationExecutionPlan {
                procedure_name: procedure_name.clone(),
                procedure_arguments: arguments::plan_mutation_arguments(
                    arguments,
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
    }
}
