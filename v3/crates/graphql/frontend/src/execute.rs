mod types;

use crate::process_response::{ProcessedResponse, process_mutation_response, process_response};
use engine_types::{HttpContext, ProjectId};
use execute::FieldError;
use execute::{resolve_ndc_mutation_execution, resolve_ndc_query_execution};
use gql::normalized_ast;
use gql::schema::NamespacedGetter;
use graphql_ir::MutationPlan;
use graphql_ir::{ApolloFederationSelect, NodeQueryPlan, QueryPlan};
use graphql_schema::GDS;
use graphql_schema::GDSRoleNamespaceGetter;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use metadata_resolve::LifecyclePluginConfigs;
use plan_types::{NDCMutationExecution, NDCQueryExecution};
use tracing_util::{AttributeVisibility, set_attribute_on_active_span};
pub use types::{ExecuteQueryResult, RootFieldResult};

/// Given an entire plan for a query, produce a result. We do this by executing all the singular
/// root fields of the query in parallel, and joining the results back together.
pub async fn execute_query_plan(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    query_plan: QueryPlan<'_, '_, '_>,
    project_id: Option<&ProjectId>,
) -> ExecuteQueryResult {
    let mut root_fields = IndexMap::new();

    // We are not running the field plans parallely here, we are just running them concurrently on a single thread.
    // To run the field plans parallely, we will need to use tokio::spawn for each field plan.
    let executed_root_fields =
        futures_ext::execute_concurrently(query_plan.into_iter(), |(alias, field_plan)| async {
            let plan_result = execute_query_field_plan(
                &alias,
                http_context,
                plugins,
                session,
                request_headers,
                field_plan,
                project_id,
            )
            .await;
            (alias, plan_result)
        })
        .await;

    for (alias, root_field) in executed_root_fields {
        root_fields.insert(alias, root_field);
    }

    ExecuteQueryResult { root_fields }
}

/// Execute a single root field's query plan to produce a result.
async fn execute_query_field_plan(
    field_alias: &ast::Alias,
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    query_plan: NodeQueryPlan<'_, '_, '_>,
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
                        NodeQueryPlan::NDCQueryExecution {
                            query_execution: ndc_query,
                            selection_set,
                        } => {
                            let process_response_as = &ndc_query.process_response_as.clone();
                            let processed_response = execute::resolve_ndc_query_execution(
                                http_context,
                                plugins,
                                session,
                                request_headers,
                                ndc_query,
                                project_id,
                            )
                            .await
                            .and_then(|vec_sets| {
                                process_response(
                                    selection_set,
                                    vec_sets,
                                    process_response_as,
                                )
                            });

                            RootFieldResult::from_processed_response(
                                process_response_as.is_nullable(),
                                processed_response,
                            )
                        }

                        NodeQueryPlan::RelayNodeSelect(optional_query) => RootFieldResult::from_processed_response(
                            optional_query.as_ref().is_none_or( |(ndc_query,_selection_set)| {
                                ndc_query.process_response_as.is_nullable()
                            }),
                            resolve_optional_ndc_select(http_context, plugins, session, request_headers, optional_query, project_id)
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
                                        plugins,
                                        session,
                                        request_headers,
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

                            RootFieldResult::new(true, Ok(serde_json::Value::Array(entities_result)))
                        }
                        NodeQueryPlan::ApolloFederationSelect(
                            ApolloFederationSelect::ServiceField { sdl, selection_set },
                        ) => {
                            let result = selection_set.as_object_selection_set( |_type_name, _field, field_call| {
                                match field_call.info.generic {
                                    graphql_schema::Annotation::Output(graphql_schema::OutputAnnotation::SDL) => {
                                        let extended_sdl = "extend schema\n  @link(url: \"https://specs.apollo.dev/federation/v2.0\", import: [\"@key\", \"@extends\", \"@external\", \"@shareable\"])\n\n".to_string() + &sdl;
                                        Ok(serde_json::Value::String(extended_sdl))
                                    },
                                    _ => {
                                        Err(FieldError::FieldNotFoundInService {
                                            field_name: field_call.name.to_string(),
                                        })
                                    }
                                }

                            }).and_then(|v| serde_json::to_value(v).map_err(FieldError::from));
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

fn resolve_type_name(type_name: ast::TypeName) -> Result<serde_json::Value, FieldError> {
    Ok(serde_json::to_value(type_name)?)
}

/// Execute a single root field's mutation plan to produce a result.
async fn execute_mutation_field_plan(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    mutation_plan: NDCMutationExecution,
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
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
                    let process_response_as = &mutation_plan.process_response_as.clone();
                    let processed_response = resolve_ndc_mutation_execution(
                        http_context,
                        plugins,
                        session,
                        request_headers,
                        mutation_plan,
                        project_id,
                    )
                    .await
                    .and_then(|mutation_response| {
                        process_mutation_response(
                            selection_set,
                            mutation_response,
                            process_response_as,
                        )
                    });

                    RootFieldResult::from_processed_response(
                        process_response_as.is_nullable(),
                        processed_response,
                    )
                })
            },
        )
        .await
}

/// Given an entire plan for a mutation, produce a result. We do this by executing the singular
/// root fields of the mutation sequentially rather than concurrently, in the order defined by the
/// `IndexMap`'s keys.
pub async fn execute_mutation_plan(
    http_context: &HttpContext,
    plugins: &LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    mutation_plan: MutationPlan<'_, '_>,
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
                execute_mutation_field_plan(
                    http_context,
                    plugins,
                    session,
                    request_headers,
                    field_plan.mutation_execution,
                    field_plan.selection_set,
                    project_id,
                )
                .await,
            ));
        }
    }

    for (alias, root_field) in executed_root_fields {
        root_fields.insert(alias, root_field);
    }

    ExecuteQueryResult { root_fields }
}

fn resolve_type_field<NSGet: NamespacedGetter<GDS>>(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    schema: &gql::schema::Schema<GDS>,
    type_name: &ast::TypeName,
    namespaced_getter: &NSGet,
) -> Result<serde_json::Value, FieldError> {
    match schema.get_type(type_name) {
        Some(type_info) => Ok(serde_json::to_value(gql::introspection::named_type(
            schema,
            namespaced_getter,
            type_info,
            selection_set,
        )?)?),
        None => Ok(serde_json::Value::Null),
    }
}

fn resolve_schema_field<NSGet: NamespacedGetter<GDS>>(
    selection_set: &normalized_ast::SelectionSet<'_, GDS>,
    schema: &gql::schema::Schema<GDS>,
    namespaced_getter: &NSGet,
) -> Result<serde_json::Value, FieldError> {
    Ok(serde_json::to_value(gql::introspection::schema_type(
        schema,
        namespaced_getter,
        selection_set,
    )?)?)
}

async fn resolve_optional_ndc_select(
    http_context: &HttpContext,
    plugins: &metadata_resolve::LifecyclePluginConfigs,
    session: &Session,
    request_headers: &http::HeaderMap,
    optional_query: Option<(NDCQueryExecution, &normalized_ast::SelectionSet<'_, GDS>)>,
    project_id: Option<&ProjectId>,
) -> Result<ProcessedResponse, FieldError> {
    match optional_query {
        None => Ok(ProcessedResponse {
            response_headers: None,
            response: serde_json::Value::Null,
        }),
        Some((ndc_query, selection_set)) => {
            let process_response_as = &ndc_query.process_response_as.clone();
            resolve_ndc_query_execution(
                http_context,
                plugins,
                session,
                request_headers,
                ndc_query,
                project_id,
            )
            .await
            .and_then(|row_sets| process_response(selection_set, row_sets, process_response_as))
        }
    }
}
