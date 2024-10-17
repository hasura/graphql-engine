use std::sync::Arc;

mod arguments;
mod commands;
pub mod error;
pub mod field;
pub(crate) mod filter;
mod model_selection;
mod mutation;
pub mod ndc_request;
pub(crate) mod query;
mod relationships;
pub(crate) mod selection_set;

pub use arguments::{Argument, MutationArgument, ResolvedArgument};
pub use field::{ResolvedField, ResolvedNestedField};
pub use filter::{
    plan_expression, resolve_expression, ResolveFilterExpressionContext, ResolvedFilterExpression,
};
pub use mutation::ResolvedMutationExecutionPlan;
pub use query::{ResolvedQueryExecutionPlan, ResolvedQueryNode, UnresolvedQueryNode};
pub use relationships::Relationship;

use gql::normalized_ast;
use gql::schema::NamespacedGetter;
use hasura_authn_core::Role;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use serde_json as json;
use tracing_util::{set_attribute_on_active_span, AttributeVisibility, Traceable};

use super::ndc;
use super::process_response::process_response;
use super::remote_joins::execute_join_locations;
use super::remote_joins::types::JoinLocations;
use super::{HttpContext, ProjectId};
use crate::error::FieldError;
use crate::process_response::{process_mutation_response, ProcessedResponse};
use graphql_ir::ModelSelection;
use graphql_schema::GDSRoleNamespaceGetter;
use graphql_schema::GDS;

pub type QueryPlan<'n, 's, 'ir> = IndexMap<ast::Alias, NodeQueryPlan<'n, 's, 'ir>>;

/// Unlike a query, the root nodes of a mutation aren't necessarily independent. Specifically, the
/// GraphQL specification says that each root mutation must be executed sequentially. Moreover, if
/// we want to, say, insert a parent _and_ children in one query, we want the ability to make
/// transactional requests. In a mutation plan, we group nodes by connector, allowing us to issue
/// transactional commands to connectors whose capabilities allow for transactional mutations.
/// Otherwise, we can just send them one-by-one (though still sequentially).
pub struct MutationPlan<'n, 's> {
    pub nodes: IndexMap<
        Arc<metadata_resolve::DataConnectorLink>,
        IndexMap<ast::Alias, NDCMutationExecution<'n, 's>>,
    >,
    pub type_names: IndexMap<ast::Alias, ast::TypeName>,
}

// At least for now, requests are _either_ queries or mutations, and a mix of the two can be
// treated as an invalid request. We may want to change this in the future.
pub enum RequestPlan<'n, 's, 'ir> {
    QueryPlan(QueryPlan<'n, 's, 'ir>),
    MutationPlan(MutationPlan<'n, 's>),
    SubscriptionPlan(ast::Alias, NDCSubscriptionExecution<'s, 'ir>),
}

/// Query plan of individual root field or node
#[derive(Debug)]
pub enum NodeQueryPlan<'n, 's, 'ir> {
    /// __typename field on query root
    TypeName { type_name: ast::TypeName },
    /// __schema field
    SchemaField {
        role: Role,
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        schema: &'s gql::schema::Schema<GDS>,
    },
    /// __type field
    TypeField {
        selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
        schema: &'s gql::schema::Schema<GDS>,
        type_name: ast::TypeName,
        role: Role,
    },
    /// NDC query to be executed
    NDCQueryExecution {
        query_execution: NDCQueryExecution<'s>,
        selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
    },
    /// NDC query for Relay 'node' to be executed
    RelayNodeSelect(
        Option<(
            NDCQueryExecution<'s>,
            &'ir normalized_ast::SelectionSet<'s, GDS>,
        )>,
    ),
    /// Apollo Federation query to be executed
    ApolloFederationSelect(ApolloFederationSelect<'n, 's, 'ir>),
}

#[derive(Debug)]
pub struct NDCQueryExecution<'s> {
    pub execution_tree: ExecutionTree<'s>,
    pub execution_span_attribute: &'static str,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs,
}

pub struct NDCSubscriptionExecution<'s, 'ir> {
    pub query_execution_plan: query::UnresolvedQueryExecutionPlan<'s>,
    pub polling_interval_ms: u64,
    pub execution_span_attribute: &'static str,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs,
    pub selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
}

#[derive(Debug)]
pub enum ApolloFederationSelect<'n, 's, 'ir> {
    /// NDC queries for Apollo Federation '_entities' to be executed
    EntitiesSelect(
        Vec<(
            NDCQueryExecution<'s>,
            &'ir normalized_ast::SelectionSet<'s, GDS>,
        )>,
    ),
    ServiceField {
        sdl: String,
        selection_set: &'n normalized_ast::SelectionSet<'s, GDS>,
    },
}

#[derive(Debug)]
pub struct NDCMutationExecution<'n, 's> {
    pub execution_node: mutation::UnresolvedMutationExecutionPlan<'s>,
    pub join_locations: JoinLocations<'s>,
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
    pub execution_span_attribute: String,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs,
    pub selection_set: &'n normalized_ast::SelectionSet<'s, GDS>,
}

#[derive(Debug)]
pub struct ExecutionTree<'s> {
    pub query_execution_plan: query::UnresolvedQueryExecutionPlan<'s>,
    pub remote_join_executions: JoinLocations<'s>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ProcessResponseAs {
    Object {
        is_nullable: bool,
    },
    Array {
        is_nullable: bool,
    },
    CommandResponse {
        command_name: Arc<metadata_resolve::Qualified<open_dds::commands::CommandName>>,
        type_container: ast::TypeContainer<ast::TypeName>,
        // how to process a command response
        response_config: Option<Arc<metadata_resolve::data_connectors::CommandsResponseConfig>>,
    },
    Aggregates,
}

impl ProcessResponseAs {
    pub fn is_nullable(&self) -> bool {
        match self {
            ProcessResponseAs::Object { is_nullable }
            | ProcessResponseAs::Array { is_nullable } => *is_nullable,
            ProcessResponseAs::CommandResponse { type_container, .. } => type_container.nullable,
            ProcessResponseAs::Aggregates { .. } => false,
        }
    }
}

/// Build a plan to handle a given request. This plan will either be a mutation plan or a query
/// plan, but currently can't be both. This may change when we support protocols other than
/// GraphQL.
pub fn generate_request_plan<'n, 's, 'ir>(
    ir: &'ir graphql_ir::IR<'n, 's>,
) -> Result<RequestPlan<'n, 's, 'ir>, error::Error> {
    match ir {
        graphql_ir::IR::Query(ir) => {
            let mut query_plan = IndexMap::new();
            for (alias, field) in ir {
                query_plan.insert(alias.clone(), plan_query(field)?);
            }
            Ok(RequestPlan::QueryPlan(query_plan))
        }
        graphql_ir::IR::Mutation(ir) => {
            let mut mutation_plan = MutationPlan {
                nodes: IndexMap::new(),
                type_names: IndexMap::new(),
            };
            for (alias, field) in ir {
                match field {
                    graphql_ir::MutationRootField::TypeName { type_name } => {
                        mutation_plan
                            .type_names
                            .insert(alias.clone(), type_name.clone());
                    }
                    graphql_ir::MutationRootField::ProcedureBasedCommand { selection_set, ir } => {
                        let plan = plan_mutation(selection_set, ir)?;
                        mutation_plan
                            .nodes
                            .entry(plan.data_connector.clone())
                            .or_default()
                            .insert(alias.clone(), plan);
                    }
                };
            }
            Ok(RequestPlan::MutationPlan(mutation_plan))
        }
        graphql_ir::IR::Subscription(alias, ir) => Ok(RequestPlan::SubscriptionPlan(
            alias.clone(),
            plan_subscription(ir)?,
        )),
    }
}

// Given a singular root field of a mutation, plan the execution of that root field.
fn plan_mutation<'n, 's>(
    selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
    ir: &graphql_ir::ProcedureBasedCommand<'s>,
) -> Result<NDCMutationExecution<'n, 's>, error::Error> {
    let (ndc_ir, join_locations) = commands::plan_mutation_execution(ir.procedure_name, ir)?;
    Ok(NDCMutationExecution {
        execution_node: ndc_ir,
        join_locations,
        data_connector: ir.command_info.data_connector.clone(),
        selection_set,
        execution_span_attribute: "execute_command".into(),
        field_span_attribute: ir.command_info.field_name.to_string(),
        process_response_as: ProcessResponseAs::CommandResponse {
            command_name: ir.command_info.command_name.clone(),
            type_container: ir.command_info.type_container.clone(),
            response_config: ir.command_info.data_connector.response_config.clone(),
        },
    })
}

fn plan_subscription<'s, 'ir>(
    root_field: &'ir graphql_ir::SubscriptionRootField<'_, 's>,
) -> Result<NDCSubscriptionExecution<'s, 'ir>, error::Error> {
    match root_field {
        graphql_ir::SubscriptionRootField::ModelSelectOne {
            ir,
            selection_set,
            polling_interval_ms,
        } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
            let query_execution_plan = reject_remote_joins(execution_tree)?;
            Ok(NDCSubscriptionExecution {
                query_execution_plan,
                polling_interval_ms: *polling_interval_ms,
                selection_set,
                execution_span_attribute: "execute_model_select_one",
                field_span_attribute: ir.field_name.to_string(),
                process_response_as: ProcessResponseAs::Object {
                    is_nullable: ir.type_container.nullable.to_owned(),
                },
            })
        }

        graphql_ir::SubscriptionRootField::ModelSelectMany {
            ir,
            selection_set,
            polling_interval_ms,
        } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
            let query_execution_plan = reject_remote_joins(execution_tree)?;
            Ok(NDCSubscriptionExecution {
                query_execution_plan,
                polling_interval_ms: *polling_interval_ms,
                selection_set,
                execution_span_attribute: "execute_model_select_many",
                field_span_attribute: ir.field_name.to_string(),
                process_response_as: ProcessResponseAs::Array {
                    is_nullable: ir.type_container.nullable.to_owned(),
                },
            })
        }

        graphql_ir::SubscriptionRootField::ModelSelectAggregate {
            ir,
            selection_set,
            polling_interval_ms,
        } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
            let query_execution_plan = reject_remote_joins(execution_tree)?;
            Ok(NDCSubscriptionExecution {
                query_execution_plan,
                polling_interval_ms: *polling_interval_ms,
                selection_set,
                execution_span_attribute: "execute_model_select_aggregate",
                field_span_attribute: ir.field_name.to_string(),
                process_response_as: ProcessResponseAs::Aggregates,
            })
        }
    }
}

fn reject_remote_joins(
    tree: ExecutionTree,
) -> Result<query::UnresolvedQueryExecutionPlan, error::Error> {
    if !tree.remote_join_executions.is_empty() {
        return Err(error::Error::RemoteJoinsAreNotSupportedSubscriptions);
    }
    Ok(tree.query_execution_plan)
}

// Given a singular root field of a query, plan the execution of that root field.
fn plan_query<'n, 's, 'ir>(
    ir: &'ir graphql_ir::QueryRootField<'n, 's>,
) -> Result<NodeQueryPlan<'n, 's, 'ir>, error::Error> {
    let query_plan = match ir {
        graphql_ir::QueryRootField::TypeName { type_name } => NodeQueryPlan::TypeName {
            type_name: type_name.clone(),
        },
        graphql_ir::QueryRootField::TypeField {
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
        graphql_ir::QueryRootField::SchemaField {
            role: namespace,
            selection_set,
            schema,
        } => NodeQueryPlan::SchemaField {
            role: namespace.clone(),
            selection_set,
            schema,
        },
        graphql_ir::QueryRootField::ModelSelectOne { ir, selection_set } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
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

        graphql_ir::QueryRootField::ModelSelectMany { ir, selection_set } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
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
        graphql_ir::QueryRootField::ModelSelectAggregate { ir, selection_set } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
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
        graphql_ir::QueryRootField::NodeSelect(optional_ir) => match optional_ir {
            Some(ir) => {
                let execution_tree = generate_execution_tree(&ir.model_selection)?;
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
        graphql_ir::QueryRootField::FunctionBasedCommand { ir, selection_set } => {
            let (query_execution_plan, join_locations) = commands::plan_query_execution(ir)?;
            let execution_tree = ExecutionTree {
                query_execution_plan,
                remote_join_executions: join_locations,
            };
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
        graphql_ir::QueryRootField::ApolloFederation(
            graphql_ir::ApolloFederationRootFields::EntitiesSelect(irs),
        ) => {
            let mut ndc_query_executions = Vec::new();
            for ir in irs {
                let execution_tree = generate_execution_tree(&ir.model_selection)?;
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
        graphql_ir::QueryRootField::ApolloFederation(
            graphql_ir::ApolloFederationRootFields::ServiceField {
                schema,
                selection_set,
                role,
            },
        ) => {
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

fn generate_execution_tree<'s>(ir: &ModelSelection<'s>) -> Result<ExecutionTree<'s>, error::Error> {
    let (query_execution_plan, join_locations) = model_selection::plan_query_execution(ir)?;
    Ok(ExecutionTree {
        query_execution_plan,
        remote_join_executions: join_locations,
    })
}

#[derive(Debug)]
pub struct RootFieldResult {
    pub is_nullable: bool,
    pub result: Result<json::Value, FieldError>,
    pub headers: Option<reqwest::header::HeaderMap>,
}

impl Traceable for RootFieldResult {
    type ErrorType<'a> = <Result<json::Value, FieldError> as Traceable>::ErrorType<'a>;

    fn get_error(&self) -> Option<Self::ErrorType<'_>> {
        Traceable::get_error(&self.result)
    }
}

impl RootFieldResult {
    pub fn new(is_nullable: bool, result: Result<json::Value, FieldError>) -> Self {
        Self {
            is_nullable,
            result,
            headers: None,
        }
    }
    pub fn from_processed_response(
        is_nullable: bool,
        result: Result<ProcessedResponse, FieldError>,
    ) -> Self {
        match result {
            Ok(processed_response) => Self {
                is_nullable,
                result: Ok(processed_response.response),
                headers: processed_response.response_headers.map(|h| h.0),
            },
            Err(field_error) => Self {
                is_nullable,
                result: Err(field_error),
                headers: None,
            },
        }
    }
}

#[derive(Debug)]
pub struct ExecuteQueryResult {
    pub root_fields: IndexMap<ast::Alias, RootFieldResult>,
}

const SET_COOKIE_HEADER_NAME: axum::http::HeaderName =
    axum::http::HeaderName::from_static("set-cookie");

impl ExecuteQueryResult {
    /// Converts the result into a GraphQL response
    #[allow(clippy::wrong_self_convention)]
    pub fn to_graphql_response(
        self,
        expose_internal_errors: crate::ExposeInternalErrors,
    ) -> gql::http::Response {
        let mut data = IndexMap::new();
        let mut errors = Vec::new();
        let mut headers = Vec::new();
        for (alias, field_result) in self.root_fields {
            let result = match field_result.result {
                Ok(value) => value,
                Err(e) => {
                    let path = vec![gql::http::PathSegment::field(alias.clone().0)];
                    // When error occur, check if the field is nullable
                    if field_result.is_nullable {
                        // If field is nullable, collect error and mark the field as null
                        errors.push(e.to_graphql_error(expose_internal_errors, Some(path)));
                        json::Value::Null
                    } else {
                        // If the field is not nullable, return `null` data response with the error.
                        // We return whatever headers we have collected until this point.
                        return gql::http::Response::error(
                            e.to_graphql_error(expose_internal_errors, Some(path)),
                            Self::merge_headers(headers),
                        );
                    }
                }
            };
            data.insert(alias, result);

            // if this root field result has headers, collect it
            if let Some(header_map) = field_result.headers {
                headers.push(header_map);
            }
        }

        gql::http::Response::partial(data, errors, Self::merge_headers(headers))
    }

    // merge all the headers of all root fields
    fn merge_headers(headers: Vec<axum::http::HeaderMap>) -> axum::http::HeaderMap {
        let mut result_map = axum::http::HeaderMap::new();
        for header_map in headers {
            for (name, val) in header_map {
                if let Some(name) = name {
                    match result_map.entry(&name) {
                        axum::http::header::Entry::Vacant(vacant) => {
                            vacant.insert(val);
                        }
                        axum::http::header::Entry::Occupied(mut occupied) => {
                            if name == SET_COOKIE_HEADER_NAME {
                                let prev_val = occupied.get();
                                if prev_val != val {
                                    occupied.append(val);
                                }
                            } else {
                                occupied.insert(val);
                            }
                        }
                    }
                }
            }
        }
        result_map
    }
}

/// Execute a single root field's query plan to produce a result.
async fn execute_query_field_plan<'n, 's, 'ir>(
    field_alias: &ast::Alias,
    http_context: &HttpContext,
    query_plan: NodeQueryPlan<'n, 's, 'ir>,
    project_id: Option<&ProjectId>,
) -> RootFieldResult {
    let tracer = tracing_util::global_tracer();

    tracer
        .in_span_async(
            "execute_query_field_plan",
            format!("{field_alias} field planning"),
            tracing_util::SpanVisibility::User,
            || {
                Box::pin(async {
                    match query_plan {
                        NodeQueryPlan::TypeName { type_name } => {
                            set_attribute_on_active_span(
                                AttributeVisibility::Default,
                                "field",
                                "__typename",
                            );
                            RootFieldResult::new(
                                false, // __typename: String! ; the __typename field is not nullable
                                resolve_type_name(type_name),
                            )
                        }
                        NodeQueryPlan::TypeField {
                            selection_set,
                            schema,
                            type_name,
                            role: namespace,
                        } => {
                            set_attribute_on_active_span(
                                AttributeVisibility::Default,
                                "field",
                                "__type",
                            );
                            RootFieldResult::new(
                                true, // __type(name: String!): __Type ; the type field is nullable
                                resolve_type_field(selection_set, schema, &type_name, &GDSRoleNamespaceGetter{scope:namespace}),
                            )
                        }
                        NodeQueryPlan::SchemaField {
                            role: namespace,
                            selection_set,
                            schema,
                        } => {
                            set_attribute_on_active_span(
                                AttributeVisibility::Default,
                                "field",
                                "__schema",
                            );
                            RootFieldResult::new(
                                false, // __schema: __Schema! ; the schema field is not nullable
                                resolve_schema_field(selection_set, schema, &GDSRoleNamespaceGetter{scope:namespace}),
                            )
                        }
                        NodeQueryPlan::NDCQueryExecution { query_execution: ndc_query, selection_set} => RootFieldResult::from_processed_response(
                            ndc_query.process_response_as.is_nullable(),
                            resolve_ndc_query_execution(http_context, ndc_query, selection_set, project_id).await,
                        ),
                        NodeQueryPlan::RelayNodeSelect(optional_query) => RootFieldResult::from_processed_response(
                            optional_query.as_ref().map_or(true, |(ndc_query,_selection_set)| {
                                ndc_query.process_response_as.is_nullable()
                            }),
                            resolve_optional_ndc_select(http_context, optional_query, project_id)
                                .await,
                        ),
                        NodeQueryPlan::ApolloFederationSelect(
                            ApolloFederationSelect::EntitiesSelect(entity_execution_plans),
                        ) => {
                            let mut tasks: Vec<_> =
                                Vec::with_capacity(entity_execution_plans.capacity());
                            for query in entity_execution_plans {
                                // We are not running the field plans parallely here, we are just running them concurrently on a single thread.
                                // To run the field plans parallely, we will need to use tokio::spawn for each field plan.
                                let task = async {
                                    (resolve_optional_ndc_select(
                                        http_context,
                                        Some(query),
                                        project_id,
                                    )
                                    .await,)
                                };

                                tasks.push(task);
                            }

                            let executed_entities = futures_util::future::join_all(tasks).await;
                            let mut entities_result = Vec::new();
                            for result in executed_entities {
                                match result {
                                    // for apollo federation, we ignore any response headers we get
                                    (Ok(value),) => entities_result.push(value.response),
                                    (Err(e),) => {
                                        return RootFieldResult::new(true, Err(e));
                                    }
                                }
                            }

                            RootFieldResult::new(true, Ok(json::Value::Array(entities_result)))
                        }
                        NodeQueryPlan::ApolloFederationSelect(
                            ApolloFederationSelect::ServiceField { sdl, selection_set },
                        ) => {
                            let result = selection_set.as_object_selection_set( |_type_name, _field, field_call| {
                                match field_call.info.generic {
                                    graphql_schema::Annotation::Output(graphql_schema::OutputAnnotation::SDL) => {
                                        let extended_sdl = "extend schema\n  @link(url: \"https://specs.apollo.dev/federation/v2.0\", import: [\"@key\", \"@extends\", \"@external\", \"@shareable\"])\n\n".to_string() + &sdl;
                                        Ok(json::Value::String(extended_sdl))
                                    },
                                    _ => {
                                        Err(FieldError::FieldNotFoundInService {
                                            field_name: field_call.name.to_string(),
                                        })
                                    }
                                }

                            }).and_then(|v| json::to_value(v).map_err(FieldError::from));
                            match result {
                                Ok(value) => RootFieldResult::new(true, Ok(value)),
                                Err(e) => RootFieldResult::new(true, Err(e))
                            }
                        }
                    }
                })
            },
        )
        .await
}

/// Execute a single root field's mutation plan to produce a result.
async fn execute_mutation_field_plan<'n, 's>(
    http_context: &HttpContext,
    mutation_plan: NDCMutationExecution<'n, 's>,
    project_id: Option<&ProjectId>,
) -> RootFieldResult {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "execute_mutation_field_plan",
            "Execute request plan for mutation field",
            tracing_util::SpanVisibility::User,
            || {
                Box::pin(async {
                    RootFieldResult::from_processed_response(
                        mutation_plan.process_response_as.is_nullable(),
                        resolve_ndc_mutation_execution(http_context, mutation_plan, project_id)
                            .await,
                    )
                })
            },
        )
        .await
}

/// Given an entire plan for a mutation, produce a result. We do this by executing the singular
/// root fields of the mutation sequentially rather than concurrently, in the order defined by the
/// `IndexMap`'s keys.
pub async fn execute_mutation_plan<'n, 's>(
    http_context: &HttpContext,
    mutation_plan: MutationPlan<'n, 's>,
    project_id: Option<&ProjectId>,
) -> ExecuteQueryResult {
    let mut root_fields = IndexMap::new();
    let mut executed_root_fields = Vec::new();

    for (alias, type_name) in mutation_plan.type_names {
        set_attribute_on_active_span(AttributeVisibility::Default, "field", "__typename");

        executed_root_fields.push((
            alias,
            RootFieldResult::new(
                false, // __typename: String! ; the __typename field is not nullable
                resolve_type_name(type_name),
            ),
        ));
    }

    for (_, mutation_group) in mutation_plan.nodes {
        for (alias, field_plan) in mutation_group {
            executed_root_fields.push((
                alias,
                execute_mutation_field_plan(http_context, field_plan, project_id).await,
            ));
        }
    }

    for (alias, root_field) in executed_root_fields {
        root_fields.insert(alias, root_field);
    }

    ExecuteQueryResult { root_fields }
}

/// Given an entire plan for a query, produce a result. We do this by executing all the singular
/// root fields of the query in parallel, and joining the results back together.
pub async fn execute_query_plan<'n, 's, 'ir>(
    http_context: &HttpContext,
    query_plan: QueryPlan<'n, 's, 'ir>,
    project_id: Option<&ProjectId>,
) -> ExecuteQueryResult {
    let mut root_fields = IndexMap::new();

    // We are not running the field plans parallely here, we are just running them concurrently on a single thread.
    // To run the field plans parallely, we will need to use tokio::spawn for each field plan.
    let executed_root_fields =
        futures_ext::execute_concurrently(query_plan.into_iter(), |(alias, field_plan)| async {
            let plan_result =
                execute_query_field_plan(&alias, http_context, field_plan, project_id).await;
            (alias, plan_result)
        })
        .await;

    for (alias, root_field) in executed_root_fields {
        root_fields.insert(alias, root_field);
    }

    ExecuteQueryResult { root_fields }
}

fn resolve_type_name(type_name: ast::TypeName) -> Result<json::Value, FieldError> {
    Ok(json::to_value(type_name)?)
}

fn resolve_type_field<NSGet: NamespacedGetter<GDS>>(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    schema: &gql::schema::Schema<GDS>,
    type_name: &ast::TypeName,
    namespaced_getter: &NSGet,
) -> Result<json::Value, FieldError> {
    match schema.get_type(type_name) {
        Some(type_info) => Ok(json::to_value(gql::introspection::named_type(
            schema,
            namespaced_getter,
            type_info,
            selection_set,
        )?)?),
        None => Ok(json::Value::Null),
    }
}

fn resolve_schema_field<NSGet: NamespacedGetter<GDS>>(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    schema: &gql::schema::Schema<GDS>,
    namespaced_getter: &NSGet,
) -> Result<json::Value, FieldError> {
    Ok(json::to_value(gql::introspection::schema_type(
        schema,
        namespaced_getter,
        selection_set,
    )?)?)
}

/// A subscription NDC query.
/// Contains required information to execute a NDC query for a subscription in a polling loop.
pub struct NDCSubscriptionQuery<'s, 'ir> {
    pub query_request: ndc::NdcQueryRequest,
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
    pub process_response_as: ProcessResponseAs,
    pub selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
    pub polling_interval_ms: u64,
}

/// Resolve a subscription execution plan to a NDC query.
pub async fn resolve_ndc_subscription_execution<'s, 'ir>(
    execution: NDCSubscriptionExecution<'s, 'ir>,
) -> Result<NDCSubscriptionQuery<'s, 'ir>, FieldError> {
    let NDCSubscriptionExecution {
        query_execution_plan,
        selection_set,
        execution_span_attribute: _,
        field_span_attribute: _,
        process_response_as,
        polling_interval_ms,
    } = execution;
    // Remote relationships and relationships without NDC comparison capability are not allowed in predicates for subscriptions.
    // Only allow local relationships and fields that can be pushed down to NDC.
    let resolve_context = ResolveFilterExpressionContext::new_only_allow_ndc_pushdown_expressions();
    let resolved_execution_plan = query_execution_plan.resolve(&resolve_context).await?;
    let data_connector = resolved_execution_plan.data_connector.clone();
    let query_request = ndc_request::make_ndc_query_request(resolved_execution_plan)?;
    Ok(NDCSubscriptionQuery {
        query_request,
        data_connector,
        process_response_as,
        selection_set,
        polling_interval_ms,
    })
}

// run ndc query, do any joins, and process result
async fn resolve_ndc_query_execution<'s, 'ir>(
    http_context: &HttpContext,
    ndc_query: NDCQueryExecution<'s>,
    selection_set: &normalized_ast::SelectionSet<'ir, GDS>,
    project_id: Option<&ProjectId>,
) -> Result<ProcessedResponse, FieldError> {
    let NDCQueryExecution {
        execution_tree,
        execution_span_attribute,
        ref field_span_attribute,
        process_response_as,
    } = ndc_query;

    let response_rowsets = execute_ndc_query(
        http_context,
        execution_tree.query_execution_plan,
        field_span_attribute,
        execution_span_attribute,
        project_id,
    )
    .await?;

    process_ndc_query_response(
        http_context,
        execution_tree.remote_join_executions,
        execution_span_attribute,
        selection_set,
        process_response_as,
        project_id,
        response_rowsets,
    )
    .await
}

async fn execute_ndc_query<'s, 'ir>(
    http_context: &HttpContext,
    query_execution_plan: query::UnresolvedQueryExecutionPlan<'s>,
    field_span_attribute: &str,
    execution_span_attribute: &'static str,
    project_id: Option<&ProjectId>,
) -> Result<Vec<ndc_models::RowSet>, FieldError> {
    let resolve_context =
        ResolveFilterExpressionContext::new_allow_in_engine_resolution(http_context);
    let resolved_execution_plan = query_execution_plan.resolve(&resolve_context).await?;

    let data_connector = resolved_execution_plan.data_connector.clone();
    let query_request = ndc_request::make_ndc_query_request(resolved_execution_plan)?;

    let response = ndc::execute_ndc_query(
        http_context,
        &query_request,
        &data_connector,
        execution_span_attribute,
        field_span_attribute.to_owned(),
        project_id,
    )
    .await?;

    Ok(response.as_latest_rowsets())
}

// given results of ndc query, do any joins, and process result
async fn process_ndc_query_response<'s, 'ir>(
    http_context: &HttpContext,
    remote_join_executions: JoinLocations<'s>,
    execution_span_attribute: &'static str,
    selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
    process_response_as: ProcessResponseAs,
    project_id: Option<&ProjectId>,
    mut response_rowsets: Vec<ndc_models::RowSet>,
) -> Result<ProcessedResponse, FieldError> {
    // TODO: Failures in remote joins should result in partial response
    // https://github.com/hasura/v3-engine/issues/229
    execute_join_locations(
        http_context,
        execution_span_attribute,
        &mut response_rowsets,
        &process_response_as,
        &remote_join_executions,
        project_id,
    )
    .await?;

    process_response(selection_set, response_rowsets, &process_response_as)
}

async fn resolve_ndc_mutation_execution(
    http_context: &HttpContext,
    ndc_mutation_execution: NDCMutationExecution<'_, '_>,
    project_id: Option<&ProjectId>,
) -> Result<ProcessedResponse, FieldError> {
    let NDCMutationExecution {
        execution_node,
        data_connector,
        selection_set,
        execution_span_attribute,
        field_span_attribute,
        process_response_as,
        // TODO: remote joins are not handled for mutations
        join_locations: _,
    } = ndc_mutation_execution;

    let resolve_context =
        ResolveFilterExpressionContext::new_allow_in_engine_resolution(http_context);
    let resolved_execution_plan = execution_node.resolve(&resolve_context).await?;

    let mutation_request = ndc_request::make_ndc_mutation_request(resolved_execution_plan)?;

    let response = ndc::execute_ndc_mutation(
        http_context,
        &mutation_request,
        &data_connector,
        execution_span_attribute,
        field_span_attribute,
        project_id,
    )
    .await?
    .as_latest();

    process_mutation_response(selection_set, response, &process_response_as)
}

async fn resolve_optional_ndc_select(
    http_context: &HttpContext,
    optional_query: Option<(
        NDCQueryExecution<'_>,
        &normalized_ast::SelectionSet<'_, GDS>,
    )>,
    project_id: Option<&ProjectId>,
) -> Result<ProcessedResponse, FieldError> {
    match optional_query {
        None => Ok(ProcessedResponse {
            response_headers: None,
            response: json::Value::Null,
        }),
        Some((ndc_query, selection_set)) => {
            resolve_ndc_query_execution(http_context, ndc_query, selection_set, project_id).await
        }
    }
}
