mod arguments;
mod commands;
mod error;
mod filter;
mod model_selection;
mod order_by;
mod relationships;
mod selection_set;
mod types;
use crate::{
    ApolloFederationRootFields, MutationRootField, ProcedureBasedCommand, QueryRootField,
    SubscriptionRootField, IR,
};
pub use error::Error;
pub use filter::plan_expression;
use graphql_schema::{GDSRoleNamespaceGetter, GDS};
use indexmap::IndexMap;
use lang_graphql as gql;
use plan_types::{
    ExecutionTree, NDCMutationExecution, NDCQueryExecution, NDCSubscriptionExecution,
    ProcessResponseAs, QueryExecutionPlan, UniqueNumber,
};
pub use relationships::process_model_relationship_definition;
pub use types::{
    ApolloFederationSelect, MutationPlan, MutationSelect, NodeQueryPlan, Plan, QueryPlan,
    RequestPlan, SubscriptionSelect,
};

// in the new world, this is where we'll create execution plans in GraphQL
// it's here because
// a) it marks old and new more clearly
// b) it removes graphql concepts from `execute`

/// Build a plan to handle a given request. This plan will either be a mutation plan or a query
/// plan, but currently can't be both. This may change when we support protocols other than
/// GraphQL.
/// This should really live in `graphql_ir`
pub fn generate_request_plan<'n, 's, 'ir>(
    ir: &'ir IR<'n, 's>,
) -> Result<RequestPlan<'n, 's, 'ir>, error::Error> {
    let mut unique_number = UniqueNumber::new();

    match ir {
        IR::Query(ir) => {
            let mut query_plan = IndexMap::new();
            for (alias, field) in ir {
                query_plan.insert(alias.clone(), plan_query(field, &mut unique_number)?);
            }
            Ok(RequestPlan::QueryPlan(query_plan))
        }
        IR::Mutation(ir) => {
            let mut mutation_plan = MutationPlan {
                nodes: IndexMap::new(),
                type_names: IndexMap::new(),
            };
            for (alias, field) in ir {
                match field {
                    MutationRootField::TypeName { type_name } => {
                        mutation_plan
                            .type_names
                            .insert(alias.clone(), type_name.clone());
                    }
                    MutationRootField::ProcedureBasedCommand { selection_set, ir } => {
                        let plan = plan_mutation(selection_set, ir, &mut unique_number)?;
                        mutation_plan
                            .nodes
                            .entry(plan.mutation_execution.data_connector.clone())
                            .or_default()
                            .insert(alias.clone(), plan);
                    }
                };
            }
            Ok(RequestPlan::MutationPlan(mutation_plan))
        }
        IR::Subscription(alias, ir) => Ok(RequestPlan::SubscriptionPlan(
            alias.clone(),
            plan_subscription(ir, &mut unique_number)?,
        )),
    }
}

// Given a singular root field of a mutation, plan the execution of that root field.
fn plan_mutation<'n, 's>(
    selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
    ir: &ProcedureBasedCommand<'s>,
    unique_number: &mut UniqueNumber,
) -> Result<MutationSelect<'n, 's>, error::Error> {
    let Plan {
        inner: ndc_ir,
        join_locations,
        remote_predicates,
    } = commands::plan_mutation_execution(ir.procedure_name, ir, unique_number)?;

    // _should not_ happen but let's fail rather than do a query with missing filters
    if !remote_predicates.0.is_empty() {
        return Err(error::Error::RemotePredicatesAreNotSupportedInMutations);
    }

    Ok(MutationSelect {
        selection_set,
        mutation_execution: NDCMutationExecution {
            execution_node: ndc_ir,
            join_locations,
            data_connector: ir.command_info.data_connector.clone(),
            execution_span_attribute: "execute_command".into(),
            field_span_attribute: ir.command_info.field_name.to_string(),
            process_response_as: ProcessResponseAs::CommandResponse {
                command_name: ir.command_info.command_name.clone(),
                type_container: ir.command_info.type_container.clone(),
                response_config: ir.command_info.data_connector.response_config.clone(),
            },
        },
    })
}

fn plan_subscription<'s, 'ir>(
    root_field: &'ir SubscriptionRootField<'_, 's>,
    unique_number: &mut UniqueNumber,
) -> Result<SubscriptionSelect<'s, 'ir>, error::Error> {
    match root_field {
        SubscriptionRootField::ModelSelectOne {
            ir,
            selection_set,
            polling_interval_ms,
        } => {
            let execution_tree =
                model_selection::plan_query_execution(&ir.model_selection, unique_number)?;
            let query_execution_plan = reject_remote_joins(execution_tree)?;
            Ok(SubscriptionSelect {
                selection_set,
                subscription_execution: NDCSubscriptionExecution {
                    query_execution_plan,
                    polling_interval_ms: *polling_interval_ms,
                    execution_span_attribute: "execute_model_select_one",
                    field_span_attribute: ir.field_name.to_string(),
                    process_response_as: ProcessResponseAs::Object {
                        is_nullable: ir.type_container.nullable.to_owned(),
                    },
                },
            })
        }

        SubscriptionRootField::ModelSelectMany {
            ir,
            selection_set,
            polling_interval_ms,
        } => {
            let execution_tree =
                model_selection::plan_query_execution(&ir.model_selection, unique_number)?;
            let query_execution_plan = reject_remote_joins(execution_tree)?;
            Ok(SubscriptionSelect {
                selection_set,
                subscription_execution: NDCSubscriptionExecution {
                    query_execution_plan,
                    polling_interval_ms: *polling_interval_ms,
                    execution_span_attribute: "execute_model_select_many",
                    field_span_attribute: ir.field_name.to_string(),
                    process_response_as: ProcessResponseAs::Array {
                        is_nullable: ir.type_container.nullable.to_owned(),
                    },
                },
            })
        }

        SubscriptionRootField::ModelSelectAggregate {
            ir,
            selection_set,
            polling_interval_ms,
        } => {
            let execution_tree =
                model_selection::plan_query_execution(&ir.model_selection, unique_number)?;
            let query_execution_plan = reject_remote_joins(execution_tree)?;
            Ok(SubscriptionSelect {
                selection_set,
                subscription_execution: NDCSubscriptionExecution {
                    query_execution_plan,
                    polling_interval_ms: *polling_interval_ms,
                    execution_span_attribute: "execute_model_select_aggregate",
                    field_span_attribute: ir.field_name.to_string(),
                    process_response_as: ProcessResponseAs::Aggregates,
                },
            })
        }
    }
}

fn reject_remote_joins(tree: ExecutionTree) -> Result<QueryExecutionPlan, error::Error> {
    if !tree.remote_join_executions.is_empty() {
        return Err(error::Error::RemoteJoinsAreNotSupportedSubscriptions);
    }
    Ok(tree.query_execution_plan)
}

// Given a singular root field of a query, plan the execution of that root field.
fn plan_query<'n, 's, 'ir>(
    ir: &'ir QueryRootField<'n, 's>,
    unique_number: &mut UniqueNumber,
) -> Result<NodeQueryPlan<'n, 's, 'ir>, error::Error> {
    let query_plan = match ir {
        QueryRootField::TypeName { type_name } => NodeQueryPlan::TypeName {
            type_name: type_name.clone(),
        },
        QueryRootField::TypeField {
            selection_set,
            schema,
            type_name,
            role: namespace,
        } => NodeQueryPlan::TypeField {
            selection_set,
            schema,
            type_name: type_name.clone(),
            role: namespace.clone(),
        },
        QueryRootField::SchemaField {
            role: namespace,
            selection_set,
            schema,
        } => NodeQueryPlan::SchemaField {
            role: namespace.clone(),
            selection_set,
            schema,
        },
        QueryRootField::ModelSelectOne { ir, selection_set } => {
            let execution_tree =
                model_selection::plan_query_execution(&ir.model_selection, unique_number)?;
            NodeQueryPlan::NDCQueryExecution {
                selection_set,
                query_execution: NDCQueryExecution {
                    execution_tree,
                    execution_span_attribute: "execute_model_select_one",
                    field_span_attribute: ir.field_name.to_string(),
                    process_response_as: ProcessResponseAs::Object {
                        is_nullable: ir.type_container.nullable.to_owned(),
                    },
                },
            }
        }

        QueryRootField::ModelSelectMany { ir, selection_set } => {
            let execution_tree =
                model_selection::plan_query_execution(&ir.model_selection, unique_number)?;
            NodeQueryPlan::NDCQueryExecution {
                selection_set,
                query_execution: NDCQueryExecution {
                    execution_tree,
                    execution_span_attribute: "execute_model_select_many",
                    field_span_attribute: ir.field_name.to_string(),
                    process_response_as: ProcessResponseAs::Array {
                        is_nullable: ir.type_container.nullable.to_owned(),
                    },
                },
            }
        }
        QueryRootField::ModelSelectAggregate { ir, selection_set } => {
            let execution_tree =
                model_selection::plan_query_execution(&ir.model_selection, unique_number)?;
            NodeQueryPlan::NDCQueryExecution {
                query_execution: NDCQueryExecution {
                    execution_tree,
                    execution_span_attribute: "execute_model_select_aggregate",
                    field_span_attribute: ir.field_name.to_string(),
                    process_response_as: ProcessResponseAs::Aggregates,
                },
                selection_set,
            }
        }
        QueryRootField::NodeSelect(optional_ir) => match optional_ir {
            Some(ir) => {
                let execution_tree =
                    model_selection::plan_query_execution(&ir.model_selection, unique_number)?;
                NodeQueryPlan::RelayNodeSelect(Some((
                    NDCQueryExecution {
                        execution_tree,
                        execution_span_attribute: "execute_node",
                        field_span_attribute: "node".into(),
                        process_response_as: ProcessResponseAs::Object { is_nullable: true }, // node(id: ID!): Node; the node field is nullable,
                    },
                    &ir.selection_set,
                )))
            }
            None => NodeQueryPlan::RelayNodeSelect(None),
        },
        QueryRootField::FunctionBasedCommand { ir, selection_set } => {
            let execution_tree = commands::plan_query_execution(ir, unique_number)?;

            NodeQueryPlan::NDCQueryExecution {
                selection_set,
                query_execution: NDCQueryExecution {
                    execution_tree,
                    execution_span_attribute: "execute_command",
                    field_span_attribute: ir.command_info.field_name.to_string(),
                    process_response_as: ProcessResponseAs::CommandResponse {
                        command_name: ir.command_info.command_name.clone(),
                        type_container: ir.command_info.type_container.clone(),
                        response_config: ir.command_info.data_connector.response_config.clone(),
                    },
                },
            }
        }
        QueryRootField::ApolloFederation(ApolloFederationRootFields::EntitiesSelect(irs)) => {
            let mut ndc_query_executions = Vec::new();
            for ir in irs {
                let execution_tree =
                    model_selection::plan_query_execution(&ir.model_selection, unique_number)?;
                ndc_query_executions.push((
                    NDCQueryExecution {
                        execution_tree,
                        execution_span_attribute: "execute_entity",
                        field_span_attribute: "entity".into(),
                        process_response_as: ProcessResponseAs::Object { is_nullable: true },
                    },
                    &ir.selection_set,
                ));
            }
            NodeQueryPlan::ApolloFederationSelect(ApolloFederationSelect::EntitiesSelect(
                ndc_query_executions,
            ))
        }
        QueryRootField::ApolloFederation(ApolloFederationRootFields::ServiceField {
            schema,
            selection_set,
            role,
        }) => {
            let sdl = schema.generate_sdl(&GDSRoleNamespaceGetter {
                scope: role.clone(),
            });
            NodeQueryPlan::ApolloFederationSelect(ApolloFederationSelect::ServiceField {
                sdl,
                selection_set,
            })
        }
    };
    Ok(query_plan)
}
