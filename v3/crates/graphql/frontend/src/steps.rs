use std::collections::BTreeMap;

use super::types::{GraphQlParseError, GraphQlValidationError};
use crate::query_usage;
use gql::normalized_ast::Operation;
use graphql_schema::{GDS, GDSRoleNamespaceGetter};
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use std::sync::Arc;
use tracing_util::{AttributeVisibility, SpanVisibility, set_attribute_on_active_span};

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
    runtime_flags: &metadata_resolve::flags::RuntimeFlags,
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
                        .map_or_else(BTreeMap::default, Clone::clone),
                };
                let validate_non_null_graphql_variables = if runtime_flags.contains(
                    metadata_resolve::flags::ResolvedRuntimeFlag::ValidateNonNullGraphqlVariables,
                ) {
                    gql::validation::NonNullGraphqlVariablesValidation::Validate
                } else {
                    gql::validation::NonNullGraphqlVariablesValidation::DoNotValidate
                };

                gql::validation::normalize_request(
                    &GDSRoleNamespaceGetter {
                        scope: session.role.clone(),
                    },
                    schema,
                    &request,
                    validate_non_null_graphql_variables,
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
    metadata: &'s metadata_resolve::Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<graphql_ir::IR<'n, 's>, graphql_ir::Error> {
    let tracer = tracing_util::global_tracer();
    let ir = tracer.in_span(
        "generate_ir",
        "Generate IR for the request",
        SpanVisibility::Internal,
        || {
            generate_ir(
                schema,
                metadata,
                session,
                request_headers,
                normalized_request,
            )
        },
    )?;
    Ok(ir)
}

/// Build a plan to execute the request
/// using the new execution plan
pub fn build_request_plan<'n, 's, 'ir>(
    ir: &'ir graphql_ir::IR<'n, 's>,
    metadata: &'s metadata_resolve::Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
) -> Result<graphql_ir::RequestPlan<'n, 's, 'ir>, graphql_ir::GraphqlIrPlanError> {
    let tracer = tracing_util::global_tracer();
    let plan = tracer.in_span(
        "plan",
        "Construct a plan to execute the request",
        SpanVisibility::Internal,
        || {
            graphql_ir::generate_request_plan(
                ir,
                metadata,
                &Arc::new(session.clone()),
                request_headers,
            )
        },
    )?;
    Ok(plan)
}

pub fn generate_ir<'n, 's>(
    schema: &'s gql::schema::Schema<GDS>,
    metadata: &'s metadata_resolve::Metadata,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<graphql_ir::IR<'n, 's>, graphql_ir::Error> {
    match &normalized_request.ty {
        ast::OperationType::Query => {
            let query_ir = graphql_ir::generate_query_ir(
                schema,
                metadata,
                session,
                request_headers,
                &normalized_request.selection_set,
            )?;
            Ok(graphql_ir::IR::Query(query_ir))
        }
        ast::OperationType::Mutation => {
            let mutation_ir = graphql_ir::generate_mutation_ir(
                &normalized_request.selection_set,
                metadata,
                session,
                request_headers,
            )?;
            Ok(graphql_ir::IR::Mutation(mutation_ir))
        }
        ast::OperationType::Subscription => {
            let (alias, field) = graphql_ir::generate_subscription_ir(
                session,
                metadata,
                request_headers,
                &normalized_request.selection_set,
            )?;
            Ok(graphql_ir::IR::Subscription(alias, field))
        }
    }
}

pub fn analyze_query_usage<'s>(
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<String, query_usage::QueryUsageAnalyzeError> {
    let tracer = tracing_util::global_tracer();
    tracer.in_span(
        "analyze_query_usage",
        "Analyze query usage",
        SpanVisibility::Internal,
        || {
            let query_usage_analytics = query_usage::analyze_query_usage(normalized_request);
            Ok(serde_json::to_string(&query_usage_analytics)?)
        },
    )
}
