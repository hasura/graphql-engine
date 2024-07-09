mod commands;
mod common;
pub(crate) mod error;
mod model_selection;
mod ndc_request;
mod relationships;
pub(crate) mod selection_set;

pub use common::ndc_expression;
pub use relationships::process_model_relationship_definition;

use gql::normalized_ast;
use gql::schema::NamespacedGetter;
use hasura_authn_core::Role;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use serde_json as json;
use tracing_util::{set_attribute_on_active_span, AttributeVisibility, Traceable};

use super::ir;
use super::ir::model_selection::ModelSelection;
use super::ir::root_field;
use super::ndc;
use super::process_response::process_response;
use super::remote_joins::execute_join_locations;
use super::remote_joins::types::{
    JoinId, JoinLocations, JoinNode, Location, LocationKind, MonotonicCounter, RemoteJoin,
};
use super::{HttpContext, ProjectId};
use crate::error::FieldError;
use crate::process_response::{process_mutation_response, ProcessedResponse};
use schema::GDSRoleNamespaceGetter;
use schema::GDS;

pub type QueryPlan<'n, 's, 'ir> = IndexMap<ast::Alias, NodeQueryPlan<'n, 's, 'ir>>;

/// Unlike a query, the root nodes of a mutation aren't necessarily independent. Specifically, the
/// GraphQL specification says that each root mutation must be executed sequentially. Moreover, if
/// we want to, say, insert a parent _and_ children in one query, we want the ability to make
/// transactional requests. In a mutation plan, we group nodes by connector, allowing us to issue
/// transactional commands to connectors whose capabilities allow for transactional mutations.
/// Otherwise, we can just send them one-by-one (though still sequentially).
pub struct MutationPlan<'n, 's, 'ir> {
    pub nodes: IndexMap<
        metadata_resolve::DataConnectorLink,
        IndexMap<ast::Alias, NDCMutationExecution<'n, 's, 'ir>>,
    >,
    pub type_names: IndexMap<ast::Alias, ast::TypeName>,
}

// At least for now, requests are _either_ queries or mutations, and a mix of the two can be
// treated as an invalid request. We may want to change this in the future.
pub enum RequestPlan<'n, 's, 'ir> {
    QueryPlan(QueryPlan<'n, 's, 'ir>),
    MutationPlan(MutationPlan<'n, 's, 'ir>),
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
    NDCQueryExecution(NDCQueryExecution<'s, 'ir>),
    /// NDC query for Relay 'node' to be executed
    RelayNodeSelect(Option<NDCQueryExecution<'s, 'ir>>),
    /// Apollo Federation query to be executed
    ApolloFederationSelect(ApolloFederationSelect<'n, 's, 'ir>),
}

#[derive(Debug)]
pub struct NDCQueryExecution<'s, 'ir> {
    pub execution_tree: ExecutionTree<'s, 'ir>,
    pub execution_span_attribute: &'static str,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs<'ir>,
    // This selection set can either be owned by the IR structures or by the normalized query request itself.
    // We use the more restrictive lifetime `'ir` here which allows us to construct this struct using the selection
    // set either from the IR or from the normalized query request.
    pub selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
}

#[derive(Debug)]
pub enum ApolloFederationSelect<'n, 's, 'ir> {
    /// NDC queries for Apollo Federation '_entities' to be executed
    EntitiesSelect(Vec<NDCQueryExecution<'s, 'ir>>),
    ServiceField {
        sdl: String,
        selection_set: &'n normalized_ast::SelectionSet<'s, GDS>,
    },
}

#[derive(Debug)]
pub struct NDCMutationExecution<'n, 's, 'ir> {
    pub query: ndc::NdcMutationRequest,
    pub join_locations: JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
    pub execution_span_attribute: String,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs<'ir>,
    pub selection_set: &'n normalized_ast::SelectionSet<'s, GDS>,
}

#[derive(Debug)]
pub struct ExecutionTree<'s, 'ir> {
    pub root_node: ExecutionNode<'s>,
    pub remote_executions: JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
}

#[derive(Debug)]
pub struct ExecutionNode<'s> {
    pub query: ndc::NdcQueryRequest,
    pub data_connector: &'s metadata_resolve::DataConnectorLink,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ProcessResponseAs<'ir> {
    Object {
        is_nullable: bool,
    },
    Array {
        is_nullable: bool,
    },
    CommandResponse {
        command_name: &'ir metadata_resolve::Qualified<open_dds::commands::CommandName>,
        type_container: &'ir ast::TypeContainer<ast::TypeName>,
        // how to process a command response
        response_config: &'ir Option<metadata_resolve::data_connectors::CommandsResponseConfig>,
    },
    Aggregates,
}

impl<'ir> ProcessResponseAs<'ir> {
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
    ir: &'ir ir::IR<'n, 's>,
) -> Result<RequestPlan<'n, 's, 'ir>, error::Error> {
    match ir {
        ir::IR::Query(ir) => {
            let mut query_plan = IndexMap::new();
            for (alias, field) in ir {
                query_plan.insert(alias.clone(), plan_query(field)?);
            }
            Ok(RequestPlan::QueryPlan(query_plan))
        }
        ir::IR::Mutation(ir) => {
            let mut mutation_plan = MutationPlan {
                nodes: IndexMap::new(),
                type_names: IndexMap::new(),
            };
            for (alias, field) in ir {
                match field {
                    root_field::MutationRootField::TypeName { type_name } => {
                        mutation_plan
                            .type_names
                            .insert(alias.clone(), type_name.clone());
                    }
                    root_field::MutationRootField::ProcedureBasedCommand { selection_set, ir } => {
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
    }
}

// Given a singular root field of a mutation, plan the execution of that root field.
fn plan_mutation<'n, 's, 'ir>(
    selection_set: &'n gql::normalized_ast::SelectionSet<'s, GDS>,
    ir: &'ir super::ir::commands::ProcedureBasedCommand<'s>,
) -> Result<NDCMutationExecution<'n, 's, 'ir>, error::Error> {
    let mut join_id_counter = MonotonicCounter::new();
    let (ndc_ir, join_locations) =
        ndc_request::ndc_procedure_mutation_request(ir.procedure_name, ir, &mut join_id_counter)?;
    let join_locations_ids = assign_with_join_ids(join_locations)?;
    Ok(NDCMutationExecution {
        query: ndc_ir,
        join_locations: join_locations_ids,
        data_connector: ir.command_info.data_connector,
        selection_set,
        execution_span_attribute: "execute_command".into(),
        field_span_attribute: ir.command_info.field_name.to_string(),
        process_response_as: ProcessResponseAs::CommandResponse {
            command_name: &ir.command_info.command_name,
            type_container: &ir.command_info.type_container,
            response_config: &ir.command_info.data_connector.response_config,
        },
    })
}

// Given a singular root field of a query, plan the execution of that root field.
fn plan_query<'n, 's, 'ir>(
    ir: &'ir root_field::QueryRootField<'n, 's>,
) -> Result<NodeQueryPlan<'n, 's, 'ir>, error::Error> {
    let mut counter = MonotonicCounter::new();
    let query_plan = match ir {
        root_field::QueryRootField::TypeName { type_name } => NodeQueryPlan::TypeName {
            type_name: type_name.clone(),
        },
        root_field::QueryRootField::TypeField {
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
        root_field::QueryRootField::SchemaField {
            role: namespace,
            selection_set,
            schema,
        } => NodeQueryPlan::SchemaField {
            role: namespace.clone(),
            selection_set,
            schema,
        },
        root_field::QueryRootField::ModelSelectOne { ir, selection_set } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
            NodeQueryPlan::NDCQueryExecution(NDCQueryExecution {
                execution_tree,
                selection_set,
                execution_span_attribute: "execute_model_select_one",
                field_span_attribute: ir.field_name.to_string(),
                process_response_as: ProcessResponseAs::Object {
                    is_nullable: ir.type_container.nullable.to_owned(),
                },
            })
        }

        root_field::QueryRootField::ModelSelectMany { ir, selection_set } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
            NodeQueryPlan::NDCQueryExecution(NDCQueryExecution {
                execution_tree,
                selection_set,
                execution_span_attribute: "execute_model_select_many",
                field_span_attribute: ir.field_name.to_string(),
                process_response_as: ProcessResponseAs::Array {
                    is_nullable: ir.type_container.nullable.to_owned(),
                },
            })
        }
        root_field::QueryRootField::ModelSelectAggregate { ir, selection_set } => {
            let execution_tree = generate_execution_tree(&ir.model_selection)?;
            NodeQueryPlan::NDCQueryExecution(NDCQueryExecution {
                execution_tree,
                selection_set,
                execution_span_attribute: "execute_model_select_aggregate",
                field_span_attribute: ir.field_name.to_string(),
                process_response_as: ProcessResponseAs::Aggregates,
            })
        }
        root_field::QueryRootField::NodeSelect(optional_ir) => match optional_ir {
            Some(ir) => {
                let execution_tree = generate_execution_tree(&ir.model_selection)?;
                NodeQueryPlan::RelayNodeSelect(Some(NDCQueryExecution {
                    execution_tree,
                    selection_set: &ir.selection_set,
                    execution_span_attribute: "execute_node",
                    field_span_attribute: "node".into(),
                    process_response_as: ProcessResponseAs::Object { is_nullable: true }, // node(id: ID!): Node; the node field is nullable,
                }))
            }
            None => NodeQueryPlan::RelayNodeSelect(None),
        },
        root_field::QueryRootField::FunctionBasedCommand { ir, selection_set } => {
            let (ndc_ir, join_locations) =
                ndc_request::make_ndc_function_query_request(ir, &mut counter)?;
            let join_locations_ids = assign_with_join_ids(join_locations)?;
            let execution_tree = ExecutionTree {
                root_node: ExecutionNode {
                    query: ndc_ir,
                    data_connector: ir.command_info.data_connector,
                },
                remote_executions: join_locations_ids,
            };
            NodeQueryPlan::NDCQueryExecution(NDCQueryExecution {
                execution_tree,
                selection_set,
                execution_span_attribute: "execute_command",
                field_span_attribute: ir.command_info.field_name.to_string(),
                process_response_as: ProcessResponseAs::CommandResponse {
                    command_name: &ir.command_info.command_name,
                    type_container: &ir.command_info.type_container,
                    response_config: &ir.command_info.data_connector.response_config,
                },
            })
        }
        root_field::QueryRootField::ApolloFederation(
            root_field::ApolloFederationRootFields::EntitiesSelect(irs),
        ) => {
            let mut ndc_query_executions = Vec::new();
            for ir in irs {
                let execution_tree = generate_execution_tree(&ir.model_selection)?;
                ndc_query_executions.push(NDCQueryExecution {
                    execution_tree,
                    selection_set: &ir.selection_set,
                    execution_span_attribute: "execute_entity",
                    field_span_attribute: "entity".into(),
                    process_response_as: ProcessResponseAs::Object { is_nullable: true },
                });
            }
            NodeQueryPlan::ApolloFederationSelect(ApolloFederationSelect::EntitiesSelect(
                ndc_query_executions,
            ))
        }
        root_field::QueryRootField::ApolloFederation(
            root_field::ApolloFederationRootFields::ServiceField {
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

fn generate_execution_tree<'s, 'ir>(
    ir: &'ir ModelSelection<'s>,
) -> Result<ExecutionTree<'s, 'ir>, error::Error> {
    let mut counter = MonotonicCounter::new();
    let (ndc_ir, join_locations) = ndc_request::make_ndc_model_query_request(ir, &mut counter)?;
    let join_locations_with_ids = assign_with_join_ids(join_locations)?;
    Ok(ExecutionTree {
        root_node: ExecutionNode {
            query: ndc_ir,
            data_connector: ir.data_connector,
        },
        remote_executions: join_locations_with_ids,
    })
}

fn assign_with_join_ids<'s, 'ir>(
    join_locations: JoinLocations<RemoteJoin<'s, 'ir>>,
) -> Result<JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>, error::Error> {
    let mut state = RemoteJoinCounter::new();
    let join_ids = assign_join_ids(&join_locations, &mut state);
    zip_with_join_ids(join_locations, join_ids)
}

fn zip_with_join_ids<'s, 'ir>(
    join_locations: JoinLocations<RemoteJoin<'s, 'ir>>,
    mut join_ids: JoinLocations<JoinId>,
) -> Result<JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>, error::Error> {
    let mut new_locations = IndexMap::new();
    for (key, location) in join_locations.locations {
        let join_id_location =
            join_ids
                .locations
                .swap_remove(&key)
                .ok_or(error::InternalError::InternalGeneric {
                    description: "unexpected; could not find {key} in join ids tree".to_string(),
                })?;
        let new_node = match (location.join_node, join_id_location.join_node) {
            (JoinNode::Remote(rj), JoinNode::Remote(join_id)) => {
                Ok(JoinNode::Remote((rj, join_id)))
            }
            (
                JoinNode::Local(LocationKind::NestedData),
                JoinNode::Local(LocationKind::NestedData),
            ) => Ok(JoinNode::Local(LocationKind::NestedData)),
            (
                JoinNode::Local(LocationKind::LocalRelationship),
                JoinNode::Local(LocationKind::LocalRelationship),
            ) => Ok(JoinNode::Local(LocationKind::LocalRelationship)),
            _ => Err(error::InternalError::InternalGeneric {
                description: "unexpected join node mismatch".to_string(),
            }),
        }?;
        let new_rest = zip_with_join_ids(location.rest, join_id_location.rest)?;
        new_locations.insert(
            key,
            Location {
                join_node: new_node,
                rest: new_rest,
            },
        );
    }
    Ok(JoinLocations {
        locations: new_locations,
    })
}

/// Once `JoinLocations<RemoteJoin>` is generated, traverse the tree and assign
/// join ids. All the join nodes (`RemoteJoin`) that are equal, are assigned the
/// same join id.
fn assign_join_ids<'s, 'ir>(
    join_locations: &'s JoinLocations<RemoteJoin<'s, 'ir>>,
    state: &mut RemoteJoinCounter<'s, 'ir>,
) -> JoinLocations<JoinId> {
    let new_locations = join_locations
        .locations
        .iter()
        .map(|(key, location)| {
            let new_node = match &location.join_node {
                JoinNode::Local(location_kind) => JoinNode::Local(*location_kind),
                JoinNode::Remote(remote_join) => {
                    JoinNode::Remote(assign_join_id(remote_join, state))
                }
            };
            let new_location = Location {
                join_node: new_node,
                rest: assign_join_ids(&location.rest, state),
            };
            (key.to_string(), new_location)
        })
        .collect::<IndexMap<_, _>>();
    JoinLocations {
        locations: new_locations,
    }
}

/// We use an associative list and check for equality of `RemoteJoin` to
/// generate it's `JoinId`. This is because `Hash` trait is not implemented for
/// `ndc_models::QueryRequest`
fn assign_join_id<'s, 'ir>(
    remote_join: &'s RemoteJoin<'s, 'ir>,
    state: &mut RemoteJoinCounter<'s, 'ir>,
) -> JoinId {
    let found = state
        .remote_joins
        .iter()
        .find(|(rj, _id)| rj == &remote_join);

    match found {
        None => {
            let next_id = JoinId(state.counter.get_next());
            state.remote_joins.push((remote_join, next_id));
            next_id
        }
        Some((_rj, id)) => *id,
    }
}

struct RemoteJoinCounter<'s, 'ir> {
    remote_joins: Vec<(&'s RemoteJoin<'s, 'ir>, JoinId)>,
    counter: MonotonicCounter,
}

impl<'s, 'ir> RemoteJoinCounter<'s, 'ir> {
    pub fn new() -> RemoteJoinCounter<'s, 'ir> {
        RemoteJoinCounter {
            remote_joins: Vec::new(),
            counter: MonotonicCounter::new(),
        }
    }
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
                        NodeQueryPlan::NDCQueryExecution(ndc_query) => RootFieldResult::from_processed_response(
                            ndc_query.process_response_as.is_nullable(),
                            resolve_ndc_query_execution(http_context, &ndc_query, project_id).await,
                        ),
                        NodeQueryPlan::RelayNodeSelect(optional_query) => RootFieldResult::from_processed_response(
                            optional_query.as_ref().map_or(true, |ndc_query| {
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
                            let service_result = {
                                let mut object_fields = Vec::new();
                                for (alias, field) in &selection_set.fields {
                                    let field_call = match field.field_call() {
                                        Ok(field_call) => field_call,
                                        Err(e) => {
                                            return RootFieldResult::new(true, Err(e.into()))
                                        }
                                    };
                                    match field_call.name.as_str() {
                                        "sdl" => {
                                            let extended_sdl = "extend schema\n  @link(url: \"https://specs.apollo.dev/federation/v2.0\", import: [\"@key\", \"@extends\", \"@external\", \"@shareable\"])\n\n".to_string() + &sdl;
                                            object_fields.push((
                                                alias.to_string(),
                                                json::Value::String(extended_sdl),
                                            ));
                                        }
                                        "__typename" => {
                                            object_fields.push((
                                                alias.to_string(),
                                                json::Value::String("_Service".to_string()),
                                            ));
                                        }
                                        field_name => {
                                            return RootFieldResult::new(
                                                true,
                                                Err(FieldError::FieldNotFoundInService {
                                                    field_name: field_name.to_string(),
                                                }),
                                            )
                                        }
                                    };
                                }
                                Ok(json::Value::Object(object_fields.into_iter().collect()))
                            };
                            RootFieldResult::new(true, service_result)
                        }
                    }
                })
            },
        )
        .await
}

/// Execute a single root field's mutation plan to produce a result.
async fn execute_mutation_field_plan<'n, 's, 'ir>(
    http_context: &HttpContext,
    mutation_plan: NDCMutationExecution<'n, 's, 'ir>,
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
pub async fn execute_mutation_plan<'n, 's, 'ir>(
    http_context: &HttpContext,
    mutation_plan: MutationPlan<'n, 's, 'ir>,
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

async fn resolve_ndc_query_execution(
    http_context: &HttpContext,
    ndc_query: &NDCQueryExecution<'_, '_>,
    project_id: Option<&ProjectId>,
) -> Result<ProcessedResponse, FieldError> {
    let NDCQueryExecution {
        execution_tree,
        selection_set,
        execution_span_attribute,
        field_span_attribute,
        process_response_as,
    } = ndc_query;

    let response = ndc::execute_ndc_query(
        http_context,
        &execution_tree.root_node.query,
        execution_tree.root_node.data_connector,
        execution_span_attribute,
        field_span_attribute.clone(),
        project_id,
    )
    .await?;

    let mut response_rowsets = response.as_latest_rowsets();

    // TODO: Failures in remote joins should result in partial response
    // https://github.com/hasura/v3-engine/issues/229
    execute_join_locations(
        http_context,
        execution_span_attribute,
        &mut response_rowsets,
        process_response_as,
        &execution_tree.remote_executions,
        project_id,
    )
    .await?;

    process_response(selection_set, response_rowsets, process_response_as)
}

async fn resolve_ndc_mutation_execution(
    http_context: &HttpContext,
    ndc_query: NDCMutationExecution<'_, '_, '_>,
    project_id: Option<&ProjectId>,
) -> Result<ProcessedResponse, FieldError> {
    let NDCMutationExecution {
        query,
        data_connector,
        selection_set,
        execution_span_attribute,
        field_span_attribute,
        process_response_as,
        // TODO: remote joins are not handled for mutations
        join_locations: _,
    } = ndc_query;

    let response = ndc::execute_ndc_mutation(
        http_context,
        &query,
        data_connector,
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
    optional_query: Option<NDCQueryExecution<'_, '_>>,
    project_id: Option<&ProjectId>,
) -> Result<ProcessedResponse, FieldError> {
    match optional_query {
        None => Ok(ProcessedResponse {
            response_headers: None,
            response: json::Value::Null,
        }),
        Some(ndc_query) => resolve_ndc_query_execution(http_context, &ndc_query, project_id).await,
    }
}
