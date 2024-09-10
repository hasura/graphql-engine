use axum::{
    http::{Method, Request, Uri},
    middleware::Next,
};
use indexmap::IndexMap;
use serde::Serialize;
use std::fmt::Display;
use tracing_util::{
    ErrorVisibility, SpanVisibility, Traceable, TraceableError, TraceableHttpResponse,
};

#[derive(Debug)]
pub struct State {
    pub routes: Vec<String>,
}

impl State {
    pub fn new(metadata: &metadata_resolve::Metadata) -> Self {
        let routes = metadata
            .models
            .iter()
            .map(|(model_name, _)| format!("/{}/{}", model_name.subgraph, model_name.name))
            .collect::<Vec<String>>();
        Self { routes }
    }
}

#[derive(Debug, derive_more::Display)]
pub enum RequestError {
    NotFound,
    BadRequest(String),
}

#[allow(clippy::unused_async)]
pub async fn handler_internal(
    state: &State,
    http_method: Method,
    uri: Uri,
    query_string: jsonapi_library::query::Query,
) -> Result<JsonApiDocument, RequestError> {
    // route matching/validation
    if !is_valid_route(state, &uri) {
        return Err(RequestError::NotFound);
    }
    // create the query IR
    let query_ir = create_query_ir(&http_method, &uri, &query_string)?;
    // execute the query with the query-engine
    let result = query_engine_execute(&query_ir, state)?;
    // process result to JSON:API compliant response
    Ok(process_result(&result))
}

fn is_valid_route(state: &State, uri: &Uri) -> bool {
    // TODO: to_string() maybe not optimal. Optimize later
    let uri_s = uri.to_string();
    for route in &state.routes {
        if uri_s.starts_with(route) {
            return true;
        }
    }
    false
}

#[derive(Serialize, Debug)]
pub struct JsonApiDocument {
    name: String,
    age: i32,
}

impl Display for JsonApiDocument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Hello error")
    }
}

/// Implement traceable for GraphQL Response
impl Traceable for JsonApiDocument {
    type ErrorType<'a> = RequestError;

    fn get_error(&self) -> Option<RequestError> {
        None
    }
}

impl TraceableError for RequestError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

fn process_result(_result: &QueryResult) -> JsonApiDocument {
    JsonApiDocument {
        age: 1,
        name: "foo".to_string(),
    }
}

fn query_engine_execute(
    _query_ir: &open_dds::query::QueryRequest,
    _metadata: &State,
) -> Result<QueryResult, RequestError> {
    Err(RequestError::BadRequest("Not implemented".to_string()))
}

#[allow(clippy::unnecessary_wraps)]
fn create_query_ir(
    _http_method: &Method,
    _uri: &Uri,
    _query_string: &jsonapi_library::query::Query,
) -> Result<open_dds::query::QueryRequest, RequestError> {
    let queries = IndexMap::new();
    Ok(open_dds::query::QueryRequest::V1(
        open_dds::query::QueryRequestV1 { queries },
    ))
}

struct QueryResult;

/// Middleware to start tracing of the `/v1/jsonapi` request. This middleware
/// must be active for the entire duration of the request i.e. this middleware
/// should be the entry point and the exit point of the JSON:API request.
pub async fn rest_request_tracing_middleware<B: Send>(
    request: Request<B>,
    next: Next<B>,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let path = "/v1/jsonapi";
    tracer
        .in_span_async_with_parent_context(
            path,
            path,
            SpanVisibility::User,
            &request.headers().clone(),
            || {
                Box::pin(async move {
                    let response = next.run(request).await;
                    TraceableHttpResponse::new(response, path)
                })
            },
        )
        .await
        .response
}
