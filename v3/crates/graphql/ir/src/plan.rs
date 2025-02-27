mod arguments;
mod commands;
mod error;
mod filter;
mod model_selection;
mod order_by;
mod relationships;
mod selection_set;
mod types;
use crate::query_root::apollo_federation::ModelEntitySelection;
use crate::query_root::node_field::ModelNodeSelection;
use crate::query_root::select_aggregate::ModelSelectAggregateSelection;
use crate::query_root::select_many::ModelSelectManySelection;
use crate::query_root::select_one::ModelSelectOneSelection;
use crate::{
    ApolloFederationRootFields, MutationRootField, ProcedureBasedCommand, QueryRootField,
    SubscriptionRootField, IR,
};
pub use error::Error;
use graphql_schema::{GDSRoleNamespaceGetter, GDS};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql as gql;
pub use metadata_resolve::Metadata;
use plan_types::{
    CommandReturnKind, NDCMutationExecution, NDCQueryExecution, NDCSubscriptionExecution,
    ProcessResponseAs, QueryExecutionPlan, QueryExecutionTree, UniqueNumber,
};
pub use types::{
    ApolloFederationSelect, MutationPlan, MutationSelect, NodeQueryPlan, Plan, QueryPlan,
    RequestPlan, SubscriptionSelect,
};

/// Build a plan to handle a given GraphQL request. This plan will either be a mutation plan or a query
/// plan, but currently can't be both.
pub fn generate_request_plan<'n, 's, 'ir>(
    ir: &'ir IR<'n, 's>,
    metadata: &'s Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<RequestPlan<'n, 's, 'ir>, error::Error> {
    let mut unique_number = UniqueNumber::new();

    match ir {
        IR::Query(ir) => {
            let mut query_plan = IndexMap::new();
            for (alias, field) in ir {
                query_plan.insert(
                    alias.clone(),
                    plan_query(
                        field,
                        metadata,
                        session,
                        request_headers,
                        &mut unique_number,
                    )?,
                );
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
                        let plan = plan_mutation(
                            selection_set,
                            ir,
                            metadata,
                            session,
                            request_headers,
                            &mut unique_number,
                        )?;
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
            plan_subscription(ir, metadata, session, request_headers, &mut unique_number)?,
        )),
    }
}

// Given a singular root field of a mutation, plan the execution of that root field.
fn plan_mutation<'n, 's>(
    selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
    ir: &ProcedureBasedCommand<'s>,
    metadata: &'s Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<MutationSelect<'n, 's>, error::Error> {
    let execution_tree = commands::plan_mutation_execution(
        ir.procedure_name,
        ir,
        metadata,
        session,
        request_headers,
        unique_number,
    )?;

    Ok(MutationSelect {
        selection_set,
        mutation_execution: NDCMutationExecution {
            execution_tree,
            data_connector: ir.command_info.data_connector.clone(),
            execution_span_attribute: "execute_command",
            field_span_attribute: ir.command_info.field_name.to_string(),
            process_response_as: ProcessResponseAs::CommandResponse {
                command_name: ir.command_info.command_name.clone(),
                is_nullable: ir.command_info.type_container.nullable,
                return_kind: if ir.command_info.type_container.is_list() {
                    CommandReturnKind::Array
                } else {
                    CommandReturnKind::Object
                },
                response_config: ir.command_info.data_connector.response_config.clone(),
            },
        },
    })
}

fn plan_subscription<'s, 'ir>(
    root_field: &'ir SubscriptionRootField<'_, 's>,
    metadata: &'s Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    unique_number: &mut UniqueNumber,
) -> Result<SubscriptionSelect<'s, 'ir>, error::Error> {
    match root_field {
        SubscriptionRootField::ModelSelectOne {
            ir,
            selection_set,
            polling_interval_ms,
        } => {
            let execution_tree = match ir.model_selection {
                ModelSelectOneSelection::Ir(ref model_selection) => {
                    model_selection::plan_query_execution(
                        model_selection,
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )
                }
                ModelSelectOneSelection::OpenDd(ref model_selection) => {
                    // TODO: expose more specific function in `plan` for just model selections
                    let single_node_execution_plan = plan::query_to_plan(
                        &open_dds::query::Query::Model(model_selection.clone()),
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )?;
                    match single_node_execution_plan {
                        plan::SingleNodeExecutionPlan::Query(execution_tree) => Ok(execution_tree),
                        plan::SingleNodeExecutionPlan::Mutation(_) => {
                            // we should use a more specific planning function to avoid
                            // this as it _should not_ happen
                            Err(error::Error::PlanExpectedQueryGotMutation)
                        }
                    }
                }
            }?;

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
            let execution_tree = match ir.model_selection {
                ModelSelectManySelection::Ir(ref model_selection) => {
                    model_selection::plan_query_execution(
                        model_selection,
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )
                }
                ModelSelectManySelection::OpenDd(ref model_selection) => {
                    // TODO: expose more specific function in `plan` for just model selections
                    let single_node_execution_plan = plan::query_to_plan(
                        &open_dds::query::Query::Model(model_selection.clone()),
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )?;
                    match single_node_execution_plan {
                        plan::SingleNodeExecutionPlan::Query(execution_tree) => Ok(execution_tree),
                        plan::SingleNodeExecutionPlan::Mutation(_) => {
                            // we should use a more specific planning function to avoid
                            // this as it _should not_ happen
                            Err(error::Error::PlanExpectedQueryGotMutation)
                        }
                    }
                }
            }?;

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
            let execution_tree = match &ir.model_selection {
                ModelSelectAggregateSelection::Ir(model_selection) => {
                    model_selection::plan_query_execution(
                        model_selection,
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )
                }
                ModelSelectAggregateSelection::OpenDd(model_aggregate_selection) => {
                    // TODO: expose more specific function in `plan` for just model selections
                    let single_node_execution_plan = plan::query_to_plan(
                        &open_dds::query::Query::ModelAggregate(model_aggregate_selection.clone()),
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )?;
                    match single_node_execution_plan {
                        plan::SingleNodeExecutionPlan::Query(execution_tree) => Ok(execution_tree),
                        plan::SingleNodeExecutionPlan::Mutation(_) => {
                            // we should use a more specific planning function to avoid
                            // this as it _should not_ happen
                            Err(error::Error::PlanExpectedQueryGotMutation)
                        }
                    }
                }
            }?;
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

fn reject_remote_joins(tree: QueryExecutionTree) -> Result<QueryExecutionPlan, error::Error> {
    if !tree.remote_join_executions.is_empty() {
        return Err(error::Error::RemoteJoinsAreNotSupportedSubscriptions);
    }
    Ok(tree.query_execution_plan)
}

// Given a singular root field of a query, plan the execution of that root field.
fn plan_query<'n, 's, 'ir>(
    ir: &'ir QueryRootField<'n, 's>,
    metadata: &'s Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
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
            let execution_tree = match ir.model_selection {
                ModelSelectOneSelection::Ir(ref model_selection) => {
                    model_selection::plan_query_execution(
                        model_selection,
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )
                }
                ModelSelectOneSelection::OpenDd(ref model_selection) => {
                    // TODO: expose more specific function in `plan` for just model selections
                    let single_node_execution_plan = plan::query_to_plan(
                        &open_dds::query::Query::Model(model_selection.clone()),
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )?;
                    match single_node_execution_plan {
                        plan::SingleNodeExecutionPlan::Query(execution_tree) => Ok(execution_tree),
                        plan::SingleNodeExecutionPlan::Mutation(_) => {
                            // we should use a more specific planning function to avoid
                            // this as it _should not_ happen
                            Err(error::Error::PlanExpectedQueryGotMutation)
                        }
                    }
                }
            }?;

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
            let execution_tree = match ir.model_selection {
                ModelSelectManySelection::Ir(ref model_selection) => {
                    model_selection::plan_query_execution(
                        model_selection,
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )
                }
                ModelSelectManySelection::OpenDd(ref model_selection) => {
                    // TODO: expose more specific function in `plan` for just model selections
                    let single_node_execution_plan = plan::query_to_plan(
                        &open_dds::query::Query::Model(model_selection.clone()),
                        metadata,
                        session,
                        request_headers,
                        unique_number,
                    )?;
                    match single_node_execution_plan {
                        plan::SingleNodeExecutionPlan::Query(execution_tree) => Ok(execution_tree),
                        plan::SingleNodeExecutionPlan::Mutation(_) => {
                            // we should use a more specific planning function to avoid
                            // this as it _should not_ happen
                            Err(error::Error::PlanExpectedQueryGotMutation)
                        }
                    }
                }
            }?;

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
            let execution_tree = {
                match &ir.model_selection {
                    ModelSelectAggregateSelection::Ir(model_selection) => {
                        model_selection::plan_query_execution(
                            model_selection,
                            metadata,
                            session,
                            request_headers,
                            unique_number,
                        )
                    }
                    ModelSelectAggregateSelection::OpenDd(model_aggregate_selection) => {
                        // TODO: expose more specific function in `plan` for just model selections
                        let single_node_execution_plan = plan::query_to_plan(
                            &open_dds::query::Query::ModelAggregate(
                                model_aggregate_selection.clone(),
                            ),
                            metadata,
                            session,
                            request_headers,
                            unique_number,
                        )?;
                        match single_node_execution_plan {
                            plan::SingleNodeExecutionPlan::Query(execution_tree) => {
                                Ok(execution_tree)
                            }
                            plan::SingleNodeExecutionPlan::Mutation(_) => {
                                // we should use a more specific planning function to avoid
                                // this as it _should not_ happen
                                Err(error::Error::PlanExpectedQueryGotMutation)
                            }
                        }
                    }
                }
            }?;
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
                let execution_tree = match ir.model_selection {
                    ModelNodeSelection::Ir(ref model_selection) => {
                        model_selection::plan_query_execution(
                            model_selection,
                            metadata,
                            session,
                            request_headers,
                            unique_number,
                        )
                    }
                    ModelNodeSelection::OpenDd(ref model_selection) => {
                        // TODO: expose more specific function in `plan` for just model selections
                        let single_node_execution_plan = plan::query_to_plan(
                            &open_dds::query::Query::Model(model_selection.clone()),
                            metadata,
                            session,
                            request_headers,
                            unique_number,
                        )?;
                        match single_node_execution_plan {
                            plan::SingleNodeExecutionPlan::Query(execution_tree) => {
                                Ok(execution_tree)
                            }
                            plan::SingleNodeExecutionPlan::Mutation(_) => {
                                // we should use a more specific planning function to avoid
                                // this as it _should not_ happen
                                Err(error::Error::PlanExpectedQueryGotMutation)
                            }
                        }
                    }
                }?;

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
            let execution_tree = commands::plan_query_execution(
                ir,
                metadata,
                session,
                request_headers,
                unique_number,
            )?;

            NodeQueryPlan::NDCQueryExecution {
                selection_set,
                query_execution: NDCQueryExecution {
                    execution_tree,
                    execution_span_attribute: "execute_command",
                    field_span_attribute: ir.command_info.field_name.to_string(),
                    process_response_as: ProcessResponseAs::CommandResponse {
                        command_name: ir.command_info.command_name.clone(),
                        is_nullable: ir.command_info.type_container.nullable,
                        return_kind: if ir.command_info.type_container.is_list() {
                            CommandReturnKind::Array
                        } else {
                            CommandReturnKind::Object
                        },
                        response_config: ir.command_info.data_connector.response_config.clone(),
                    },
                },
            }
        }
        QueryRootField::ApolloFederation(ApolloFederationRootFields::EntitiesSelect(irs)) => {
            let mut ndc_query_executions = Vec::new();
            for ir in irs {
                let execution_tree = match ir.model_selection {
                    ModelEntitySelection::Ir(ref model_selection) => {
                        model_selection::plan_query_execution(
                            model_selection,
                            metadata,
                            session,
                            request_headers,
                            unique_number,
                        )
                    }
                    ModelEntitySelection::OpenDd(ref model_selection) => {
                        // TODO: expose more specific function in `plan` for just model selections
                        let single_node_execution_plan = plan::query_to_plan(
                            &open_dds::query::Query::Model(model_selection.clone()),
                            metadata,
                            session,
                            request_headers,
                            unique_number,
                        )?;
                        match single_node_execution_plan {
                            plan::SingleNodeExecutionPlan::Query(execution_tree) => {
                                Ok(execution_tree)
                            }
                            plan::SingleNodeExecutionPlan::Mutation(_) => {
                                // we should use a more specific planning function to avoid
                                // this as it _should not_ happen
                                Err(error::Error::PlanExpectedQueryGotMutation)
                            }
                        }
                    }
                }?;
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
