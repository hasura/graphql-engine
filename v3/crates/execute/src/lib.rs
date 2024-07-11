mod error;
mod explain;
mod global_id;
pub mod ir;
pub mod model_tracking;
pub mod ndc;
mod plan;
mod process_response;
mod query_usage;
mod remote_joins;

use plan::ExecuteQueryResult;
pub use plan::{ndc_expression, process_model_relationship_definition};

use gql::normalized_ast::Operation;
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use lang_graphql::{
    http::{RawRequest, Response},
    schema::Schema,
};
use schema::{GDSRoleNamespaceGetter, GDS};
use tracing_util::{
    set_attribute_on_active_span, AttributeVisibility, ErrorVisibility, SpanVisibility, Traceable,
    TraceableError,
};

// we explicitly export things used by other crates
pub use explain::execute_explain;
pub use explain::types::{redact_ndc_explain, ExplainResponse};
pub use ndc::fetch_from_data_connector;
pub use plan::{execute_mutation_plan, execute_query_plan, generate_request_plan, RequestPlan};

/// Context for making HTTP requests
#[derive(Debug, Clone)]
pub struct HttpContext {
    /// The HTTP client to use for making requests
    pub client: reqwest::Client,
    /// Response size limit for NDC requests
    pub ndc_response_size_limit: Option<usize>,
}

#[derive(Debug)]
/// A simple wrapper around a reference of GraphQL errors
pub struct GraphQLErrors<'a>(pub &'a nonempty::NonEmpty<gql::http::GraphQLError>);

impl<'a> std::fmt::Display for GraphQLErrors<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let messages = self
            .0
            .iter()
            .map(|e| e.message.as_str())
            .collect::<Vec<_>>();
        write!(f, "{}", messages.join(", "))
    }
}

/// Implement traceable error for GraphQL Errors
impl<'a> TraceableError for GraphQLErrors<'a> {
    fn visibility(&self) -> ErrorVisibility {
        // Traces related to GraphQL errors are always visible to the user
        ErrorVisibility::User
    }
}

/// A simple wrapper around a GraphQL HTTP response
pub struct GraphQLResponse(gql::http::Response);

impl GraphQLResponse {
    pub fn from_result(
        result: ExecuteQueryResult,
        expose_internal_errors: ExposeInternalErrors,
    ) -> Self {
        Self(result.to_graphql_response(expose_internal_errors))
    }

    pub fn from_error(
        err: &error::RequestError,
        expose_internal_errors: ExposeInternalErrors,
    ) -> Self {
        Self(Response::error(
            err.to_graphql_error(expose_internal_errors),
            axum::http::HeaderMap::default(),
        ))
    }

    pub fn from_response(response: gql::http::Response) -> Self {
        Self(response)
    }

    pub fn does_contain_error(&self) -> bool {
        self.0.does_contains_error()
    }

    pub fn inner(self) -> gql::http::Response {
        self.0
    }
}

/// Implement traceable for GraphQL Response
impl Traceable for GraphQLResponse {
    type ErrorType<'a> = GraphQLErrors<'a>;

    fn get_error(&self) -> Option<GraphQLErrors<'_>> {
        self.0.errors.as_ref().map(GraphQLErrors)
    }
}

#[derive(Clone, Debug)]
pub struct ProjectId(pub String);

pub async fn execute_query(
    expose_internal_errors: ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    request: RawRequest,
    project_id: Option<&ProjectId>,
) -> GraphQLResponse {
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
    .unwrap_or_else(|e| GraphQLResponse::from_error(&e, expose_internal_errors))
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
struct GraphQlParseError(#[from] gql::ast::spanning::Positioned<gql::parser::Error>);

impl TraceableError for GraphQlParseError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

#[derive(Debug, thiserror::Error)]
#[error("{0}")]
struct GraphQlValidationError(#[from] gql::validation::Error);
impl TraceableError for GraphQlValidationError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ExposeInternalErrors {
    Expose,
    Censor,
}

/// Executes a GraphQL query
pub async fn execute_query_internal(
    expose_internal_errors: ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &gql::schema::Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    raw_request: gql::http::RawRequest,
    project_id: Option<&ProjectId>,
) -> Result<GraphQLResponse, error::RequestError> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "execute_query",
            "Execute query request",
            SpanVisibility::User,
            || {
                tracing_util::set_attribute_on_active_span(
                    AttributeVisibility::Default,
                    "session.role",
                    session.role.to_string(),
                );
                tracing_util::set_attribute_on_active_span(
                    AttributeVisibility::Default,
                    "request.graphql_query",
                    raw_request.query.clone(),
                );
                Box::pin(async {
                    // parse the raw request into a GQL query
                    let query = parse_query(&raw_request.query)?;

                    // normalize the parsed GQL query
                    let normalized_request =
                        normalize_request(schema, session, query, raw_request)?;

                    // generate IR
                    let ir = build_ir(schema, session, request_headers, &normalized_request)?;

                    // construct a plan to execute the request
                    let request_plan = build_request_plan(&ir)?;

                    let display_name = match normalized_request.name {
                        Some(ref name) => std::borrow::Cow::Owned(format!("Execute {name}")),
                        None => std::borrow::Cow::Borrowed("Execute request plan"),
                    };

                    // execute the query plan
                    let response = tracer
                        .in_span_async("execute", display_name, SpanVisibility::User, || {
                            let all_usage_counts =
                                model_tracking::get_all_usage_counts_in_query(&ir);
                            let serialized_data = serde_json::to_string(&all_usage_counts).unwrap();

                            set_attribute_on_active_span(
                                AttributeVisibility::Default,
                                "usage_counts",
                                serialized_data,
                            );

                            let execute_response = Box::pin(async {
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
                                };

                                GraphQLResponse::from_result(
                                    execute_query_result,
                                    expose_internal_errors,
                                )
                            });

                            // Analyze the query usage
                            // It is attached to this span as an attribute
                            match analyze_query_usage(&normalized_request) {
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

                            execute_response
                        })
                        .await;
                    Ok(response)
                })
            },
        )
        .await
}

/// Explains (query plan) a GraphQL query
pub async fn explain_query_internal(
    expose_internal_errors: ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &gql::schema::Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    raw_request: gql::http::RawRequest,
) -> Result<explain::types::ExplainResponse, error::RequestError> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "explain_query",
            "Execute explain request",
            SpanVisibility::User,
            || {
                tracing_util::set_attribute_on_active_span(
                    AttributeVisibility::Default,
                    "session.role",
                    session.role.to_string(),
                );
                tracing_util::set_attribute_on_active_span(
                    AttributeVisibility::Default,
                    "request.graphql_query",
                    raw_request.query.to_string(),
                );
                Box::pin(async {
                    // parse the raw request into a GQL query
                    let query = parse_query(&raw_request.query)?;

                    // normalize the parsed GQL query
                    let normalized_request =
                        normalize_request(schema, session, query, raw_request)?;

                    // generate IR
                    let ir = build_ir(schema, session, request_headers, &normalized_request)?;

                    // construct a plan to execute the request
                    let request_plan = build_request_plan(&ir)?;

                    // explain the query plan
                    let response = tracer
                        .in_span_async(
                            "explain",
                            "Explain request plan",
                            SpanVisibility::Internal,
                            || {
                                Box::pin(async {
                                    let request_result = match request_plan {
                                        plan::RequestPlan::MutationPlan(mutation_plan) => {
                                            crate::explain::explain_mutation_plan(
                                                expose_internal_errors,
                                                http_context,
                                                mutation_plan,
                                            )
                                            .await
                                        }
                                        plan::RequestPlan::QueryPlan(query_plan) => {
                                            crate::explain::explain_query_plan(
                                                expose_internal_errors,
                                                http_context,
                                                query_plan,
                                            )
                                            .await
                                        }
                                    };
                                    // convert the query plan to explain step
                                    match request_result {
                                        Ok(step) => step.make_explain_response(),
                                        Err(e) => explain::types::ExplainResponse::error(
                                            e.to_graphql_error(expose_internal_errors),
                                        ),
                                    }
                                })
                            },
                        )
                        .await;
                    Ok(response)
                })
            },
        )
        .await
}

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
) -> Result<ir::IR<'n, 's>, ir::error::Error> {
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
) -> Result<plan::RequestPlan<'n, 's, 'ir>, plan::error::Error> {
    let tracer = tracing_util::global_tracer();
    let plan = tracer.in_span(
        "plan",
        "Construct a plan to execute the request",
        SpanVisibility::Internal,
        || plan::generate_request_plan(ir),
    )?;
    Ok(plan)
}

pub fn generate_ir<'n, 's>(
    schema: &'s gql::schema::Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<ir::IR<'n, 's>, ir::error::Error> {
    match &normalized_request.ty {
        ast::OperationType::Query => {
            let query_ir = ir::query_root::generate_ir(
                schema,
                session,
                request_headers,
                &normalized_request.selection_set,
            )?;
            Ok(ir::IR::Query(query_ir))
        }
        ast::OperationType::Mutation => {
            let mutation_ir = ir::mutation_root::generate_ir(
                &normalized_request.selection_set,
                &session.variables,
                request_headers,
            )?;
            Ok(ir::IR::Mutation(mutation_ir))
        }
        ast::OperationType::Subscription => {
            Err(ir::error::InternalEngineError::SubscriptionsNotSupported)?
        }
    }
}

fn analyze_query_usage<'s>(
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<String, error::QueryUsageAnalyzeError> {
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

#[cfg(test)]
mod tests {
    use goldenfile::{differs::text_diff, Mint};
    use hasura_authn_core::{Identity, Role, Session, SessionVariableValue};
    use lang_graphql::http::Request;
    use lang_graphql::{parser::Parser, validation::normalize_request};
    use open_dds::session_variables::{SessionVariable, SESSION_VARIABLE_ROLE};
    use serde_json as json;
    use std::{
        collections::HashMap,
        fs::{self, File},
        io::Write,
        path::PathBuf,
    };

    use super::generate_ir;
    use super::query_usage::analyze_query_usage;
    use schema::GDS;

    #[test]
    fn test_generate_ir() -> Result<(), Box<dyn std::error::Error>> {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut mint = Mint::new(&test_dir);

        let schema = fs::read_to_string(test_dir.join("schema.json"))?;

        let gds = GDS::new_with_default_flags(open_dds::Metadata::from_json_str(&schema)?)?;
        let schema = GDS::build_schema(&gds)?;

        for input_file in fs::read_dir(test_dir.join("generate_ir"))? {
            let path = input_file?.path();
            assert!(path.is_dir());

            let request_headers = reqwest::header::HeaderMap::new();
            let test_name = path
                .file_name()
                .ok_or_else(|| format!("{path:?} is not a normal file or directory"))?;

            let raw_request = fs::read_to_string(path.join("request.gql"))?;
            let expected_path = PathBuf::from("generate_ir")
                .join(test_name)
                .join("expected.json");

            let session_vars_path = path.join("session_variables.json");
            let session = resolve_session(session_vars_path);
            let query = Parser::new(&raw_request).parse_executable_document()?;

            let request = Request {
                operation_name: None,
                query,
                variables: HashMap::new(),
            };

            let normalized_request = normalize_request(
                &schema::GDSRoleNamespaceGetter {
                    scope: session.role.clone(),
                },
                &schema,
                &request,
            )?;

            let ir = generate_ir(&schema, &session, &request_headers, &normalized_request)?;
            let mut expected = mint.new_goldenfile_with_differ(
                expected_path,
                Box::new(|file1, file2| {
                    let json1: serde_json::Value =
                        serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                    let json2: serde_json::Value =
                        serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                    if json1 != json2 {
                        text_diff(file1, file2);
                    }
                }),
            )?;
            write!(expected, "{}", serde_json::to_string_pretty(&ir)?)?;
        }

        Ok(())
    }

    #[test]
    fn test_query_usage_analytics() -> Result<(), Box<dyn std::error::Error>> {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("query_usage_analytics");
        let mut mint = Mint::new(&test_dir);

        let schema = fs::read_to_string(test_dir.join("schema.json"))?;

        let gds = GDS::new_with_default_flags(open_dds::Metadata::from_json_str(&schema)?)?;
        let schema = GDS::build_schema(&gds)?;

        for test_dir in fs::read_dir(test_dir)? {
            let path = test_dir?.path();

            if !path.is_dir() {
                continue;
            }

            let test_name = path
                .file_name()
                .ok_or_else(|| format!("{path:?} is not a normal file or directory"))?;

            let raw_request = fs::read_to_string(path.join("request.gql"))?;
            let expected_path = PathBuf::from(test_name).join("expected.json");

            let session_vars_path = path.join("session_variables.json");
            let session = resolve_session(session_vars_path);
            let query = Parser::new(&raw_request).parse_executable_document()?;

            let request = Request {
                operation_name: None,
                query,
                variables: HashMap::new(),
            };

            let normalized_request = normalize_request(
                &schema::GDSRoleNamespaceGetter {
                    scope: session.role.clone(),
                },
                &schema,
                &request,
            )?;

            let query_usage = analyze_query_usage(&normalized_request);
            let mut expected = mint.new_goldenfile_with_differ(
                expected_path,
                Box::new(|file1, file2| {
                    let json1: serde_json::Value =
                        serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                    let json2: serde_json::Value =
                        serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                    if json1 != json2 {
                        text_diff(file1, file2);
                    }
                }),
            )?;
            write!(expected, "{}", serde_json::to_string_pretty(&query_usage)?)?;
        }
        Ok(())
    }

    // TODO: remove duplication between this function and 'add_session'
    fn resolve_session(session_vars_path: PathBuf) -> Session {
        let authorization = Identity::admin(Role::new("admin"));
        let session_variables: HashMap<SessionVariable, SessionVariableValue> = {
            if session_vars_path.exists() {
                json::from_str(fs::read_to_string(session_vars_path).unwrap().as_ref()).unwrap()
            } else {
                HashMap::new()
            }
        };

        let role = session_variables
            .get(&SESSION_VARIABLE_ROLE)
            .map(|v| Role::new(&v.0));
        authorization
            .get_role_authorization(role.as_ref())
            .unwrap()
            .build_session(session_variables)
    }
}
