mod commands;
mod model_selection;
mod relationships;
pub(crate) mod selection_set;

use gql::normalized_ast;
use hasura_authn_core::Role;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use ndc_client;
use serde_json as json;
use tracing_util::{set_attribute_on_active_span, AttributeVisibility, Traceable};

use super::error;
use super::ir::model_selection::ModelSelection;
use super::ir::root_field;
use super::ndc;
use super::process_response::process_response;
use super::remote_joins::execute_join_locations;
use super::remote_joins::types::{JoinId, JoinLocations, Location, MonotonicCounter, RemoteJoin};
use super::ProjectId;
use crate::metadata::resolved::{self, subgraph};
use crate::schema::GDS;

pub type QueryPlan<'n, 's, 'ir> = IndexMap<ast::Alias, NodeQueryPlan<'n, 's, 'ir>>;

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
    /// NDC mutation to be executed
    NDCMutationExecution(NDCMutationExecution<'n, 's, 'ir>),
}

#[derive(Debug)]
pub struct NDCQueryExecution<'s, 'ir> {
    pub execution_tree: ExecutionTree<'s, 'ir>,
    pub execution_span_attribute: String,
    pub field_span_attribute: String,
    pub process_response_as: ProcessResponseAs<'ir>,
    // This selection set can either be owned by the IR structures or by the normalized query request itself.
    // We use the more restrictive lifetime `'ir` here which allows us to construct this struct using the selection
    // set either from the IR or from the normalized query request.
    pub selection_set: &'ir normalized_ast::SelectionSet<'s, GDS>,
}

#[derive(Debug)]
pub struct NDCMutationExecution<'n, 's, 'ir> {
    pub query: ndc_client::models::MutationRequest,
    pub join_locations: JoinLocations<(RemoteJoin<'s, 'ir>, JoinId)>,
    pub data_connector: &'s resolved::data_connector::DataConnectorLink,
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
    pub query: ndc_client::models::QueryRequest,
    pub data_connector: &'s resolved::data_connector::DataConnectorLink,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ProcessResponseAs<'ir> {
    Object {
        is_nullable: bool,
    },
    Array {
        is_nullable: bool,
    },
    CommandResponse {
        command_name: &'ir subgraph::Qualified<open_dds::commands::CommandName>,
        type_container: &'ir ast::TypeContainer<ast::TypeName>,
    },
}

impl<'ir> ProcessResponseAs<'ir> {
    pub fn is_nullable(&self) -> bool {
        match self {
            ProcessResponseAs::Object { is_nullable } => *is_nullable,
            ProcessResponseAs::Array { is_nullable } => *is_nullable,
            ProcessResponseAs::CommandResponse { type_container, .. } => type_container.nullable,
        }
    }
}

pub fn generate_query_plan<'n, 's, 'ir>(
    ir: &'ir IndexMap<ast::Alias, root_field::RootField<'n, 's>>,
) -> Result<QueryPlan<'n, 's, 'ir>, error::Error> {
    let mut query_plan = IndexMap::new();
    for (alias, field) in ir.into_iter() {
        let field_plan = match field {
            root_field::RootField::QueryRootField(field_ir) => plan_query(field_ir),
            root_field::RootField::MutationRootField(field_ir) => plan_mutation(field_ir),
        }?;
        query_plan.insert(alias.clone(), field_plan);
    }
    Ok(query_plan)
}

fn plan_mutation<'n, 's, 'ir>(
    ir: &'ir root_field::MutationRootField<'n, 's>,
) -> Result<NodeQueryPlan<'n, 's, 'ir>, error::Error> {
    let plan = match ir {
        root_field::MutationRootField::TypeName { type_name } => NodeQueryPlan::TypeName {
            type_name: type_name.clone(),
        },
        root_field::MutationRootField::ProcedureBasedCommand { ir, selection_set } => {
            let mut join_id_counter = MonotonicCounter::new();
            let (ndc_ir, join_locations) =
                commands::ndc_mutation_ir(ir.procedure_name, ir, &mut join_id_counter)?;
            let join_locations_ids = assign_with_join_ids(join_locations)?;
            NodeQueryPlan::NDCMutationExecution(NDCMutationExecution {
                query: ndc_ir,
                join_locations: join_locations_ids,
                data_connector: ir.command_info.data_connector,
                selection_set,
                execution_span_attribute: "execute_command".into(),
                field_span_attribute: ir.command_info.field_name.to_string(),
                process_response_as: ProcessResponseAs::CommandResponse {
                    command_name: &ir.command_info.command_name,
                    type_container: &ir.command_info.type_container,
                },
            })
        }
    };
    Ok(plan)
}

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
                execution_span_attribute: "execute_model_select_one".into(),
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
                execution_span_attribute: "execute_model_select_many".into(),
                field_span_attribute: ir.field_name.to_string(),
                process_response_as: ProcessResponseAs::Array {
                    is_nullable: ir.type_container.nullable.to_owned(),
                },
            })
        }
        root_field::QueryRootField::NodeSelect(optional_ir) => match optional_ir {
            Some(ir) => {
                let execution_tree = generate_execution_tree(&ir.model_selection)?;
                NodeQueryPlan::RelayNodeSelect(Some(NDCQueryExecution {
                    execution_tree,
                    selection_set: &ir.selection_set,
                    execution_span_attribute: "execute_node".into(),
                    field_span_attribute: "node".into(),
                    process_response_as: ProcessResponseAs::Object { is_nullable: true }, // node(id: ID!): Node; the node field is nullable,
                }))
            }
            None => NodeQueryPlan::RelayNodeSelect(None),
        },
        root_field::QueryRootField::FunctionBasedCommand { ir, selection_set } => {
            let (ndc_ir, join_locations) = commands::ndc_query_ir(ir, &mut counter)?;
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
                execution_span_attribute: "execute_command".into(),
                field_span_attribute: ir.command_info.field_name.to_string(),
                process_response_as: ProcessResponseAs::CommandResponse {
                    command_name: &ir.command_info.command_name,
                    type_container: &ir.command_info.type_container,
                },
            })
        }
    };
    Ok(query_plan)
}

fn generate_execution_tree<'s, 'ir>(
    ir: &'ir ModelSelection<'s>,
) -> Result<ExecutionTree<'s, 'ir>, error::Error> {
    let mut counter = MonotonicCounter::new();
    let (ndc_ir, join_locations) = model_selection::ndc_ir(ir, &mut counter)?;
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
                .remove(&key)
                .ok_or(error::InternalEngineError::InternalGeneric {
                    description: "unexpected; could not find {key} in join ids tree".to_string(),
                })?;
        let new_node = match (location.join_node, join_id_location.join_node) {
            (Some(rj), Some(join_id)) => Some((rj, join_id)),
            _ => None,
        };
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
            let new_node = location
                .join_node
                .as_ref()
                .map(|join_node| assign_join_id(join_node, state));
            let new_location = Location {
                join_node: new_node.to_owned(),
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
/// `ndc::models::QueryRequest`
fn assign_join_id<'s, 'ir>(
    join_node: &'s RemoteJoin<'s, 'ir>,
    state: &mut RemoteJoinCounter<'s, 'ir>,
) -> JoinId {
    let found = state.remote_joins.iter().find(|(rj, _id)| rj == &join_node);

    match found {
        None => {
            let next_id = state.counter.get_next();
            state.remote_joins.push((join_node, next_id));
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
    pub result: Result<json::Value, error::Error>,
}

impl Traceable for RootFieldResult {
    type ErrorType<'a> = <Result<json::Value, error::Error> as Traceable>::ErrorType<'a>;

    fn get_error(&self) -> Option<Self::ErrorType<'_>> {
        Traceable::get_error(&self.result)
    }
}

impl RootFieldResult {
    pub fn new(is_nullable: &bool, result: Result<json::Value, error::Error>) -> Self {
        Self {
            is_nullable: *is_nullable,
            result,
        }
    }
}

#[derive(Debug)]
pub struct ExecuteQueryResult {
    pub root_fields: IndexMap<ast::Alias, RootFieldResult>,
}

impl ExecuteQueryResult {
    /// Converts the result into a GraphQL response
    pub fn to_graphql_response(self) -> gql::http::Response {
        let mut data = IndexMap::new();
        let mut errors = Vec::new();
        for (alias, field_result) in self.root_fields.into_iter() {
            let result = match field_result.result {
                Ok(value) => value,
                Err(e) => {
                    let path = vec![gql::http::PathSegment::field(alias.clone().0)];
                    // When error occur, check if the field is nullable
                    if field_result.is_nullable {
                        // If field is nullable, collect error and mark the field as null
                        errors.push(e.to_graphql_error(Some(path)));
                        json::Value::Null
                    } else {
                        // If the field is not nullable, return `null` data response with the error
                        return gql::http::Response::error(e.to_graphql_error(Some(path)));
                    }
                }
            };
            data.insert(alias, result);
        }
        gql::http::Response::partial(data, errors)
    }
}

async fn execute_field_plan<'n, 's, 'ir>(
    http_client: &reqwest::Client,
    field_plan: NodeQueryPlan<'n, 's, 'ir>,
    project_id: Option<ProjectId>,
) -> RootFieldResult {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "execute_field_plan",
            tracing_util::SpanVisibility::User,
            || {
                Box::pin(async {
                    match field_plan {
                        NodeQueryPlan::TypeName { type_name } => {
                            set_attribute_on_active_span(
                                AttributeVisibility::Default,
                                "field",
                                "__typename",
                            );
                            RootFieldResult::new(
                                &false, // __typename: String! ; the __typename field is not nullable
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
                                &true, // __type(name: String!): __Type ; the type field is nullable
                                resolve_type_field(selection_set, schema, &type_name, &namespace),
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
                                &false, // __schema: __Schema! ; the schema field is not nullable
                                resolve_schema_field(selection_set, schema, &namespace),
                            )
                        }
                        NodeQueryPlan::NDCQueryExecution(ndc_query) => RootFieldResult::new(
                            &ndc_query.process_response_as.is_nullable(),
                            resolve_ndc_query_execution(http_client, ndc_query, project_id).await,
                        ),
                        NodeQueryPlan::NDCMutationExecution(ndc_query) => RootFieldResult::new(
                            &ndc_query.process_response_as.is_nullable(),
                            resolve_ndc_mutation_execution(http_client, ndc_query, project_id)
                                .await,
                        ),
                        NodeQueryPlan::RelayNodeSelect(optional_query) => RootFieldResult::new(
                            &optional_query.as_ref().map_or(true, |ndc_query| {
                                ndc_query.process_response_as.is_nullable()
                            }),
                            resolve_relay_node_select(http_client, optional_query, project_id)
                                .await,
                        ),
                    }
                })
            },
        )
        .await
}

pub async fn execute_query_plan<'n, 's, 'ir>(
    http_client: &reqwest::Client,
    query_plan: QueryPlan<'n, 's, 'ir>,
    project_id: Option<ProjectId>,
) -> ExecuteQueryResult {
    let mut root_fields = IndexMap::new();

    let mut tasks: Vec<_> = Vec::with_capacity(query_plan.capacity());

    for (alias, field_plan) in query_plan.into_iter() {
        // We are not running the field plans parallely here, we are just running them concurrently on a single thread.
        // To run the field plans parallely, we will need to use tokio::spawn for each field plan.
        let task = async {
            (
                alias,
                execute_field_plan(http_client, field_plan, project_id.clone()).await,
            )
        };

        tasks.push(task);
    }

    let executed_root_fields = futures::future::join_all(tasks).await;

    for executed_root_field in executed_root_fields.into_iter() {
        let (alias, root_field) = executed_root_field;
        root_fields.insert(alias, root_field);
    }

    ExecuteQueryResult { root_fields }
}

fn resolve_type_name(type_name: ast::TypeName) -> Result<json::Value, error::Error> {
    Ok(json::to_value(type_name)?)
}

fn resolve_type_field(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    schema: &gql::schema::Schema<GDS>,
    type_name: &ast::TypeName,
    namespace: &Role,
) -> Result<json::Value, error::Error> {
    match schema.get_type(type_name) {
        Some(type_info) => Ok(json::to_value(gql::introspection::named_type(
            schema,
            namespace,
            type_info,
            selection_set,
        )?)?),
        None => Ok(json::Value::Null),
    }
}

fn resolve_schema_field(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    schema: &gql::schema::Schema<GDS>,
    namespace: &Role,
) -> Result<json::Value, error::Error> {
    Ok(json::to_value(gql::introspection::schema_type(
        schema,
        namespace,
        selection_set,
    )?)?)
}

async fn resolve_ndc_query_execution(
    http_client: &reqwest::Client,
    ndc_query: NDCQueryExecution<'_, '_>,
    project_id: Option<ProjectId>,
) -> Result<json::Value, error::Error> {
    let NDCQueryExecution {
        execution_tree,
        selection_set,
        execution_span_attribute,
        field_span_attribute,
        process_response_as,
    } = ndc_query;
    let mut response = ndc::execute_ndc_query(
        http_client,
        execution_tree.root_node.query,
        execution_tree.root_node.data_connector,
        execution_span_attribute.clone(),
        field_span_attribute.clone(),
        project_id.clone(),
    )
    .await?;
    // TODO: Failures in remote joins should result in partial response
    // https://github.com/hasura/v3-engine/issues/229
    execute_join_locations(
        http_client,
        execution_span_attribute,
        field_span_attribute,
        &mut response,
        &process_response_as,
        execution_tree.remote_executions,
        project_id,
    )
    .await?;
    let result = process_response(selection_set, response, process_response_as)?;
    Ok(json::to_value(result)?)
}

async fn resolve_ndc_mutation_execution(
    http_client: &reqwest::Client,
    ndc_query: NDCMutationExecution<'_, '_, '_>,
    project_id: Option<ProjectId>,
) -> Result<json::Value, error::Error> {
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
        http_client,
        query,
        data_connector,
        selection_set,
        execution_span_attribute,
        field_span_attribute,
        process_response_as,
        project_id,
    )
    .await?;
    Ok(json::to_value(response)?)
}

async fn resolve_relay_node_select(
    http_client: &reqwest::Client,
    optional_query: Option<NDCQueryExecution<'_, '_>>,
    project_id: Option<ProjectId>,
) -> Result<json::Value, error::Error> {
    match optional_query {
        None => Ok(json::Value::Null),
        Some(ndc_query) => resolve_ndc_query_execution(http_client, ndc_query, project_id).await,
    }
}
