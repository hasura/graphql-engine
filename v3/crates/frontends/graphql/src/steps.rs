use super::types::{GraphQlParseError, GraphQlValidationError};

use gql::normalized_ast::Operation;
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use schema::{GDSRoleNamespaceGetter, GDS};
use tracing_util::{set_attribute_on_active_span, AttributeVisibility, SpanVisibility};

/// Parses a raw GraphQL request into a GQL query AST
pub(crate) fn parse_query(
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
pub(crate) fn normalize_request<'s>(
    schema: &'s gql::schema::Schema<GDS>,
    session: &Session,
    query: gql::ast::executable::ExecutableDocument,
    raw_request: gql::http::RawRequest,
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
                    operation_name: raw_request.operation_name,
                    query,
                    variables: raw_request.variables.unwrap_or_default(),
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
pub(crate) fn build_ir<'n, 's>(
    schema: &'s gql::schema::Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<ir::IR<'n, 's>, ir::Error> {
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
pub(crate) fn build_request_plan<'n, 's, 'ir>(
    ir: &'ir ir::IR<'n, 's>,
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
) -> Result<ir::IR<'n, 's>, ir::Error> {
    match &normalized_request.ty {
        ast::OperationType::Query => {
            let query_ir = ir::generate_query_ir(
                schema,
                session,
                request_headers,
                &normalized_request.selection_set,
            )?;
            Ok(ir::IR::Query(query_ir))
        }
        ast::OperationType::Mutation => {
            let mutation_ir = ir::generate_mutation_ir(
                &normalized_request.selection_set,
                &session.variables,
                request_headers,
            )?;
            Ok(ir::IR::Mutation(mutation_ir))
        }
        ast::OperationType::Subscription => {
            let (alias, field) = ir::generate_subscription_ir(
                session,
                request_headers,
                &normalized_request.selection_set,
            )?;
            Ok(ir::IR::Subscription(alias, field))
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
