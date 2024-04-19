use indexmap::IndexMap;

use crate::schema::types;
use crate::schema::GDS;
use gql::normalized_ast::Operation;
use hasura_authn_core::Session;
use lang_graphql as gql;
use lang_graphql::ast::common as ast;
use lang_graphql::{
    http::{RawRequest, Response},
    schema::Schema,
};
use thiserror::Error;
use tracing_util::{
    set_attribute_on_active_span, AttributeVisibility, ErrorVisibility, SpanVisibility, Traceable,
    TraceableError,
};

use self::explain::types::RequestMode;

pub mod error;
pub mod explain;
pub mod global_id;
pub mod ir;
pub mod model_tracking;
pub mod ndc;
pub mod plan;
pub mod process_response;
pub mod remote_joins;

/// Context for making HTTP requests
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
pub struct GraphQLResponse(pub gql::http::Response);

/// Implement traceable for GraphQL Response
impl Traceable for GraphQLResponse {
    type ErrorType<'a> = GraphQLErrors<'a>;

    fn get_error(&self) -> Option<GraphQLErrors<'_>> {
        self.0.errors.as_ref().map(GraphQLErrors)
    }
}

pub enum ExecuteOrExplainResponse {
    Execute(GraphQLResponse),
    Explain(explain::types::ExplainResponse),
}

impl Traceable for ExecuteOrExplainResponse {
    type ErrorType<'a> = GraphQLErrors<'a>;

    fn get_error(&self) -> Option<GraphQLErrors<'_>> {
        match self {
            ExecuteOrExplainResponse::Execute(response) => response.get_error(),
            ExecuteOrExplainResponse::Explain(response) => response.get_error(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ProjectId(pub String);

pub async fn execute_query(
    http_context: &HttpContext,
    schema: &Schema<GDS>,
    session: &Session,
    request: RawRequest,
    project_id: Option<&ProjectId>,
) -> GraphQLResponse {
    execute_query_internal(http_context, schema, session, request, project_id)
        .await
        .unwrap_or_else(|e| GraphQLResponse(Response::error(e.to_graphql_error(None))))
}

#[derive(Error, Debug)]
#[error("{0}")]
struct GraphQlParseError(#[from] gql::ast::spanning::Positioned<gql::parser::Error>);

impl TraceableError for GraphQlParseError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

#[derive(Error, Debug)]
#[error("{0}")]
struct GraphQlValidationError(#[from] gql::validation::Error);
impl TraceableError for GraphQlValidationError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

/// Executes a GraphQL query
pub async fn execute_query_internal(
    http_context: &HttpContext,
    schema: &gql::schema::Schema<GDS>,
    session: &Session,
    raw_request: gql::http::RawRequest,
    project_id: Option<&ProjectId>,
) -> Result<GraphQLResponse, error::Error> {
    let query_response = execute_request_internal(
        http_context,
        schema,
        session,
        raw_request,
        explain::types::RequestMode::Execute,
        project_id,
    )
    .await?;
    match query_response {
        ExecuteOrExplainResponse::Execute(response) => Ok(response),
        ExecuteOrExplainResponse::Explain(_response) => Err(error::Error::InternalError(
            error::InternalError::Developer(
                error::InternalDeveloperError::ExecuteReturnedExplainResponse,
            ),
        )),
    }
}

/// Executes or explains (query plan) a GraphQL query
pub async fn execute_request_internal(
    http_context: &HttpContext,
    schema: &gql::schema::Schema<GDS>,
    session: &Session,
    raw_request: gql::http::RawRequest,
    request_mode: explain::types::RequestMode,
    project_id: Option<&ProjectId>,
) -> Result<ExecuteOrExplainResponse, error::Error> {
    let tracer = tracing_util::global_tracer();
    let mode_query_span_name = match request_mode {
        RequestMode::Execute => "execute_query",
        RequestMode::Explain => "explain_query",
    };
    let mode_query_display_name = match request_mode {
        RequestMode::Execute => "Execute query request",
        RequestMode::Explain => "Execute explain request",
    };

    tracer
        .in_span_async(
            mode_query_span_name,
            mode_query_display_name.to_string(),
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
                    let query = tracer
                        .in_span(
                            "parse",
                            "Parse the raw request into a GraphQL query".into(),
                            SpanVisibility::Internal,
                            || {
                                gql::parser::Parser::new(&raw_request.query)
                                    .parse_executable_document()
                                    .map_err(GraphQlParseError)
                            },
                        )
                        .map_err(|e| e.0)?;

                    // normalize the parsed GQL query
                    let normalized_request = tracer
                        .in_span(
                            "validate",
                            "Normalize the parsed GraphQL query".into(),
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
                                gql::validation::normalize_request(&session.role, schema, &request)
                                    .map_err(GraphQlValidationError)
                            },
                        )
                        .map_err(|e| e.0)?;

                    // generate IR
                    let ir = tracer.in_span(
                        "generate_ir",
                        "Generate IR for the request".into(),
                        SpanVisibility::Internal,
                        || generate_ir(schema, session, &normalized_request),
                    )?;

                    // construct a plan to execute the request
                    let request_plan = tracer.in_span(
                        "plan",
                        "Construct a plan to execute the request".into(),
                        SpanVisibility::Internal,
                        || plan::generate_request_plan(&ir),
                    )?;

                    let display_name = match normalized_request.name {
                        Some(ref name) => format!("Execute {}", name),
                        None => "Execute request plan".to_string(),
                    };

                    // execute/explain the query plan
                    let response = match request_mode {
                        RequestMode::Execute => {
                            tracer
                                .in_span_async(
                                    "execute",
                                    display_name,
                                    SpanVisibility::User,
                                    || {
                                        let all_usage_counts =
                                            model_tracking::get_all_usage_counts_in_query(&ir);
                                        let serialized_data =
                                            serde_json::to_string(&all_usage_counts).unwrap();
                                        set_attribute_on_active_span(
                                            AttributeVisibility::Default,
                                            "usage_counts",
                                            serialized_data,
                                        );

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
                                            };

                                            ExecuteOrExplainResponse::Execute(GraphQLResponse(
                                                execute_query_result.to_graphql_response(),
                                            ))
                                        })
                                    },
                                )
                                .await
                        }
                        RequestMode::Explain => {
                            tracer
                                .in_span_async(
                                    "explain",
                                    "Explain request plan".to_string(),
                                    SpanVisibility::Internal,
                                    || {
                                        Box::pin(async {
                                            let request_result = match request_plan {
                                                plan::RequestPlan::MutationPlan(mutation_plan) => {
                                                    crate::execute::explain::explain_mutation_plan(
                                                        http_context,
                                                        mutation_plan,
                                                    )
                                                    .await
                                                }
                                                plan::RequestPlan::QueryPlan(query_plan) => {
                                                    crate::execute::explain::explain_query_plan(
                                                        http_context,
                                                        query_plan,
                                                    )
                                                    .await
                                                }
                                            };

                                            // convert the query plan to explain step
                                            let explain_response = match request_result {
                                                Ok(step) => step.make_explain_response(),
                                                Err(e) => explain::types::ExplainResponse::error(
                                                    e.to_graphql_error(None),
                                                ),
                                            };

                                            ExecuteOrExplainResponse::Explain(explain_response)
                                        })
                                    },
                                )
                                .await
                        }
                    };
                    Ok(response)
                })
            },
        )
        .await
}

pub fn generate_ir<'n, 's>(
    schema: &'s gql::schema::Schema<GDS>,
    session: &Session,
    normalized_request: &'s Operation<'s, GDS>,
) -> Result<IndexMap<ast::Alias, ir::root_field::RootField<'n, 's>>, error::Error> {
    let ir = match &normalized_request.ty {
        ast::OperationType::Query => {
            ir::query_root::generate_ir(schema, session, &normalized_request.selection_set)?
        }
        ast::OperationType::Mutation => {
            ir::mutation_root::generate_ir(&normalized_request.selection_set, &session.variables)?
        }
        ast::OperationType::Subscription => {
            Err(error::InternalEngineError::SubscriptionsNotSupported)?
        }
    };
    Ok(ir)
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
    use crate::schema::GDS;

    #[test]
    fn test_generate_ir() {
        let test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut mint = Mint::new(&test_dir);

        let schema = fs::read_to_string(test_dir.join("schema.json")).unwrap();

        let gds = GDS::new(open_dds::Metadata::from_json_str(&schema).unwrap()).unwrap();
        let schema = GDS::build_schema(&gds).unwrap();

        for input_file in fs::read_dir(test_dir.join("generate_ir")).unwrap() {
            let entry = input_file.unwrap();
            let raw_request = {
                let path = entry.path();
                assert!(path.is_dir());
                fs::read_to_string(path.join("request.gql")).unwrap()
            };
            let expected_path = {
                let path = entry.path();
                let test_name = path.file_name().unwrap().to_str().unwrap();
                PathBuf::from_iter(["generate_ir", test_name, "expected.json"])
            };

            let session = {
                let path = entry.path();
                let session_vars_path = path.join("session_variables.json");
                resolve_session(session_vars_path)
            };
            let query = Parser::new(&raw_request)
                .parse_executable_document()
                .unwrap();

            let request = Request {
                operation_name: None,
                query,
                variables: HashMap::new(),
            };

            let normalized_request = normalize_request(&session.role, &schema, &request).unwrap();

            let ir = generate_ir(&schema, &session, &normalized_request).unwrap();
            let mut expected = mint
                .new_goldenfile_with_differ(
                    expected_path,
                    Box::new(|file1, file2| {
                        let json1: serde_json::Value =
                            serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                        let json2: serde_json::Value =
                            serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                        if json1 != json2 {
                            text_diff(file1, file2)
                        }
                    }),
                )
                .unwrap();
            write!(expected, "{}", serde_json::to_string_pretty(&ir).unwrap()).unwrap();
        }
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
