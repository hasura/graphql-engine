use execute::{ExecuteQueryResult, HttpContext, ProjectId, RootFieldResult};
use graphql_ir::{NodeQueryPlan, QueryPlan};
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;

/// This is where the GraphQL execution will live
/// We'll use the `execute` crate for running queries etc
/// but resolve stuff like type names and Apollo / Relay stuff here
/// Things here are incomplete, and being moved over / fixed as we go.

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
                        /*
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
                        }*/
                        NodeQueryPlan::NDCQueryExecution {
                            query_execution: ndc_query,
                            selection_set,
                        } => {
                            let process_response_as = &ndc_query.process_response_as.clone();
                            let processed_response = execute::resolve_ndc_query_execution(
                                http_context,
                                ndc_query,
                                project_id,
                            )
                            .await
                            .and_then(|vec_sets| {
                                execute::process_response(
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
                        /*

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
                        }*/,
                        _ => todo!("not implemented yet"),
                    }
                })
            },
        )
        .await
}
