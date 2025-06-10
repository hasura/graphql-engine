use super::steps;
use indexmap::IndexMap;

use super::types::GraphQLResponse;
use crate::execute::{
    ExecuteQueryResult, RootFieldResult, execute_mutation_plan, execute_query_plan,
};
use engine_types::{ExposeInternalErrors, HttpContext, ProjectId};
use graphql_schema::GDS;
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use lang_graphql::{http::RawRequest, schema::Schema};
use std::sync::Arc;
use tracing_util::set_status_on_current_span;
use tracing_util::{AttributeVisibility, SpanVisibility, set_attribute_on_active_span};

pub async fn execute_query(
    expose_internal_errors: ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &Schema<GDS>,
    metadata: &Arc<metadata_resolve::Metadata>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    request: RawRequest,
    project_id: Option<&ProjectId>,
) -> (Option<ast::OperationType>, GraphQLResponse) {
    execute_query_internal(
        expose_internal_errors,
        http_context,
        schema,
        metadata,
        session,
        request_headers,
        request,
        project_id,
    )
    .await
    .map_or_else(
        |e| {
            (
                None,
                GraphQLResponse::from_error(&e, expose_internal_errors),
            )
        },
        |(op_type, response)| (Some(op_type), response),
    )
}

/// Executes a GraphQL query using new pipeline
pub async fn execute_query_internal(
    expose_internal_errors: ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &gql::schema::Schema<GDS>,
    metadata: &Arc<metadata_resolve::Metadata>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    raw_request: gql::http::RawRequest,
    project_id: Option<&ProjectId>,
) -> Result<(ast::OperationType, GraphQLResponse), crate::RequestError> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "execute_query",
            "Execute query request",
            SpanVisibility::User,
            || {
                // Set GraphQL request metadata attributes on the current span
                set_request_metadata_attributes(&raw_request, session);
                Box::pin(async {
                    // parse the raw request into a GQL query
                    let query = steps::parse_query(&raw_request.query)?;

                    // normalize the parsed GQL query
                    let normalized_request = steps::normalize_request(
                        schema,
                        session,
                        query,
                        &raw_request,
                        &metadata.runtime_flags,
                    )?;

                    // generate IR
                    let ir = steps::build_ir(
                        schema,
                        metadata,
                        session,
                        request_headers,
                        &normalized_request,
                    )?;

                    // construct a plan to execute the request
                    let request_plan =
                        steps::build_request_plan(&ir, metadata, session, request_headers)?;

                    let display_name = match normalized_request.name {
                        Some(ref name) => std::borrow::Cow::Owned(format!("Execute {name}")),
                        None => std::borrow::Cow::Borrowed("Execute request plan"),
                    };

                    // execute the query plan
                    let response = tracer
                        .in_span_async("execute", display_name, SpanVisibility::User, || {
                            // Set usage analytics attributes on the current span
                            set_usage_attributes(&normalized_request, &ir);

                            Box::pin(async {
                                let execute_query_result = match request_plan {
                                    graphql_ir::RequestPlan::MutationPlan(mutation_plan) => {
                                        execute_mutation_plan(
                                            http_context,
                                            mutation_plan,
                                            project_id,
                                        )
                                        .await
                                    }
                                    graphql_ir::RequestPlan::QueryPlan(query_plan) => {
                                        execute_query_plan(http_context, query_plan, project_id)
                                            .await
                                    }
                                    graphql_ir::RequestPlan::SubscriptionPlan(
                                        alias,
                                        subscription_plan,
                                    ) => {
                                        // subscriptions are not supported over HTTP
                                        let result =
                                            Err(execute::FieldError::SubscriptionsNotSupported);
                                        let root_field_result = RootFieldResult {
                                            is_nullable: subscription_plan
                                                .subscription_execution
                                                .process_response_as
                                                .is_nullable(),
                                            result,
                                            headers: None,
                                        };
                                        ExecuteQueryResult {
                                            root_fields: IndexMap::from([(
                                                alias,
                                                root_field_result,
                                            )]),
                                        }
                                    }
                                };

                                GraphQLResponse::from_result(
                                    execute_query_result,
                                    expose_internal_errors,
                                )
                            })
                        })
                        .await;

                    // Set the response status in this (parent) span. Otherwise folks might not
                    // find the error-ing traces they are looking for. Do we want to insist all
                    // parent spans of error-ing spans are also error? Then handle in the tracing
                    // code.
                    set_status_on_current_span(&response);
                    Ok((normalized_request.ty, response))
                })
            },
        )
        .await
}

/// Set GraphQL request metadata attributes on the current span.
/// This includes the session role, operation name and the GraphQL query.
pub fn set_request_metadata_attributes(raw_request: &gql::http::RawRequest, session: &Session) {
    // Set request session role
    tracing_util::set_attribute_on_active_span(
        AttributeVisibility::Default,
        "session.role",
        session.role.to_string(),
    );
    // Set the GraphQL request operation name if exists
    if let Some(name) = &raw_request.operation_name {
        tracing_util::set_attribute_on_active_span(
            AttributeVisibility::Default,
            "operation_name",
            name.to_string(),
        );
    }
    // Set the GraphQL query
    tracing_util::set_attribute_on_active_span(
        AttributeVisibility::Default,
        "request.graphql_query",
        raw_request.query.clone(),
    );
}

/// Set usage analytics attributes on the current span
pub fn set_usage_attributes<'s, 'n>(
    normalized_request: &'n gql::normalized_ast::Operation<'s, GDS>,
    ir: &graphql_ir::IR<'n, 's>,
) {
    // Set usage counts as a span attributes
    let all_usage_counts = graphql_ir::get_all_usage_counts_in_query(ir);
    let serialized_data = serde_json::to_string(&all_usage_counts).unwrap();
    set_attribute_on_active_span(
        AttributeVisibility::Default,
        "usage_counts",
        serialized_data,
    );

    // Analyze the query usage and attach to this span as an attribute
    match steps::analyze_query_usage(normalized_request) {
        Err(analyze_error) => {
            // Set query usage analytics error as a span attribute
            set_attribute_on_active_span(
                AttributeVisibility::Internal,
                "query_usage_analytics_error",
                analyze_error.to_string(),
            );
        }
        Ok(query_usage_analytics) => {
            // Set query usage analytics as a span attribute
            set_attribute_on_active_span(
                AttributeVisibility::Internal,
                "query_usage_analytics",
                query_usage_analytics,
            );
        }
    }
}
