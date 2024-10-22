use super::steps;
use indexmap::IndexMap;

use super::types::GraphQLResponse;
use execute::{
    plan::{self, RootFieldResult},
    ExecuteQueryResult, HttpContext, ProjectId,
};

use graphql_schema::GDS;
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use lang_graphql::{http::RawRequest, schema::Schema};
use tracing_util::{set_attribute_on_active_span, AttributeVisibility, SpanVisibility};

pub async fn execute_query(
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    request: RawRequest,
    project_id: Option<&ProjectId>,
) -> (Option<ast::OperationType>, GraphQLResponse) {
    execute_query_internal(
        expose_internal_errors,
        http_context,
        schema,
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

/// Executes a GraphQL query
pub async fn execute_query_internal(
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &gql::schema::Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    raw_request: gql::http::RawRequest,
    project_id: Option<&ProjectId>,
) -> Result<(ast::OperationType, GraphQLResponse), execute::RequestError> {
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
                    let normalized_request =
                        steps::normalize_request(schema, session, query, &raw_request)?;

                    // generate IR
                    let ir =
                        steps::build_ir(schema, session, request_headers, &normalized_request)?;

                    // construct a plan to execute the request
                    let request_plan = steps::build_request_plan(&ir)?;

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
                                    plan::RequestPlan::MutationPlan(mutation_plan) => {
                                        plan::execute_mutation_plan(
                                            http_context,
                                            mutation_plan,
                                            project_id,
                                        )
                                        .await
                                    }
                                    plan::RequestPlan::QueryPlan(query_plan) => {
                                        plan::execute_query_plan(
                                            http_context,
                                            query_plan,
                                            project_id,
                                        )
                                        .await
                                    }
                                    plan::RequestPlan::SubscriptionPlan(
                                        alias,
                                        subscription_plan,
                                    ) => {
                                        // subscriptions are not supported over HTTP
                                        let result =
                                            Err(execute::FieldError::SubscriptionsNotSupported);
                                        let root_field_result = RootFieldResult {
                                            is_nullable: subscription_plan
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
