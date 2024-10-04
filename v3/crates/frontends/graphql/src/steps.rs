use std::collections::HashMap;

use super::types::{GraphQlParseError, GraphQlValidationError};

use gql::normalized_ast::Operation;
use graphql_schema::{GDSRoleNamespaceGetter, GDS};
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use tracing_util::{set_attribute_on_active_span, AttributeVisibility, SpanVisibility};

/// Parses a raw GraphQL request into a GQL query AST
pub fn parse_query(
    query: &str,
) -> Result<
    gql::ast::executable::ExecutableDocument,
    gql::ast::spanning::Positioned<gql::parser::Error>,
> {
    let tracer = tracing_util::global_tracer();
    let query = tracer
        .in_span(
            "parse",
            "Parse the raw request into a GraphQL query",
            SpanVisibility::Internal,
            || {
                gql::parser::Parser::new(query)
                    .parse_executable_document()
                    .map_err(GraphQlParseError)
            },
        )
        .map_err(|e| e.0)?;
    Ok(query)
}

/// Normalize the parsed GQL query
pub fn normalize_request<'s>(
    schema: &'s gql::schema::Schema<GDS>,
    session: &Session,
    query: gql::ast::executable::ExecutableDocument,
    raw_request: &gql::http::RawRequest,
) -> Result<Operation<'s, GDS>, gql::validation::Error> {
    let tracer = tracing_util::global_tracer();
    let normalized_request = tracer
        .in_span(
            "validate",
            "Normalize the parsed GraphQL query",
            SpanVisibility::Internal,
            || {
                // add the operation name even if validation fails
                if let Some(name) = &raw_request.operation_name {
                    set_attribute_on_active_span(
                        AttributeVisibility::Default,
                        "operation_name",
                        name.to_string(),
                    );
                }

                let request = gql::http::Request {
                    operation_name: raw_request.operation_name.clone(),
                    query,
                    variables: raw_request
                        .variables
                        .as_ref()
                        .map_or_else(HashMap::default, Clone::clone),
                };
                gql::validation::normalize_request(
                    &GDSRoleNamespaceGetter {
                        scope: session.role.clone(),
                    },
                    schema,
                    &request,
                )
                .map_err(GraphQlValidationError)
            },
        )
        .map_err(|e| e.0)?;
    Ok(normalized_request)
}

/// Generate IR for the request
pub fn build_ir<'n, 's>(
    schema: &'s gql::schema::Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<graphql_ir::IR<'n, 's>, graphql_ir::Error> {
    let tracer = tracing_util::global_tracer();
    let ir = tracer.in_span(
        "generate_ir",
        "Generate IR for the request",
        SpanVisibility::Internal,
        || generate_ir(schema, session, request_headers, normalized_request),
    )?;
    Ok(ir)
}

/// Build a plan to execute the request
pub fn build_request_plan<'n, 's, 'ir>(
    ir: &'ir graphql_ir::IR<'n, 's>,
) -> Result<execute::RequestPlan<'n, 's, 'ir>, execute::PlanError> {
    let tracer = tracing_util::global_tracer();
    let plan = tracer.in_span(
        "plan",
        "Construct a plan to execute the request",
        SpanVisibility::Internal,
        || execute::generate_request_plan(ir),
    )?;
    Ok(plan)
}

pub fn generate_ir<'n, 's>(
    schema: &'s gql::schema::Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<graphql_ir::IR<'n, 's>, graphql_ir::Error> {
    match &normalized_request.ty {
        ast::OperationType::Query => {
            let query_ir = graphql_ir::generate_query_ir(
                schema,
                session,
                request_headers,
                &normalized_request.selection_set,
            )?;
            Ok(graphql_ir::IR::Query(query_ir))
        }
        ast::OperationType::Mutation => {
            let mutation_ir = graphql_ir::generate_mutation_ir(
                &normalized_request.selection_set,
                &session.variables,
                request_headers,
            )?;
            Ok(graphql_ir::IR::Mutation(mutation_ir))
        }
        ast::OperationType::Subscription => {
            let (alias, field) = graphql_ir::generate_subscription_ir(
                session,
                request_headers,
                &normalized_request.selection_set,
            )?;
            Ok(graphql_ir::IR::Subscription(alias, field))
        }
    }
}

pub fn analyze_query_usage<'s>(
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<String, execute::QueryUsageAnalyzeError> {
    let tracer = tracing_util::global_tracer();
    tracer.in_span(
        "analyze_query_usage",
        "Analyze query usage",
        SpanVisibility::Internal,
        || {
            let query_usage_analytics = execute::analyze_query_usage(normalized_request);
            Ok(serde_json::to_string(&query_usage_analytics)?)
        },
    )
}

// run ndc query, do any joins, and process result
// we only use this in engine tests at the moment, and it will be removed as soon
// as we're able to use the new pipeline with the existing execution / process response
// for now, it allows us a cheap way to test our GraphQL -> OpenDD IR -> execute pipeline
pub async fn resolve_ndc_query_execution<'ir>(
    http_context: &execute::HttpContext,
    query_execution_plan: execute::ResolvedQueryExecutionPlan,
) -> Result<Vec<ndc_models::RowSet>, execute::FieldError> {
    let data_connector = query_execution_plan.data_connector.clone();
    let query_request = execute::plan::ndc_request::make_ndc_query_request(query_execution_plan)?;

    let response = execute::ndc::execute_ndc_query(
        http_context,
        &query_request,
        &data_connector,
        "graphql",
        "graphql".to_owned(),
        None, // TODO: plumb in project id
    )
    .await?;

    Ok(response.as_latest_rowsets())
}
