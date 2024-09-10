use axum::{
    extract::Query,
    http::{HeaderMap, Method, Request, Uri},
    middleware::Next,
    response::IntoResponse,
    routing::get,
    Extension, Json, Router,
};
use execute::{ExposeInternalErrors, HttpContext, ProjectId};
use hasura_authn_core::Session;
use indexmap::IndexMap;
use serde::Serialize;
use std::{collections::HashMap, fmt::Display, sync::Arc};
use tower_http::trace::TraceLayer;
use tracing_util::{
    set_status_on_current_span, ErrorVisibility, SpanVisibility, Traceable, TraceableError,
    TraceableHttpResponse,
};

use crate::{authentication_middleware, EngineState};

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

pub(crate) fn create_json_api_router(state: Arc<EngineState>) -> axum::Router {
    let router = Router::new()
        // TODO: update method GET; for now we are only supporting queries. And
        // in JSON:API spec, all queries have the GET method. Not even HEAD is
        // supported. So this should be fine.
        .route("/*path", get(handle_request))
        .layer(axum::middleware::from_fn(
            hasura_authn_core::resolve_session,
        ))
        .layer(axum::middleware::from_fn_with_state(
            state.clone(),
            authentication_middleware,
        ))
        .layer(axum::middleware::from_fn(rest_request_tracing_middleware))
        // *PLEASE DO NOT ADD ANY MIDDLEWARE
        // BEFORE THE `explain_request_tracing_middleware`*
        // Refer to it for more details.
        .layer(TraceLayer::new_for_http())
        .with_state(state);
    Router::new().nest("/v1/jsonapi", router)
}

async fn handle_request(
    headers: HeaderMap,
    method: Method,
    uri: Uri,
    Query(query_string): Query<HashMap<String, String>>,
    axum::extract::State(state): axum::extract::State<Arc<EngineState>>,
    Extension(session): Extension<Session>,
) -> impl IntoResponse {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "handle_request",
            "Handle request",
            SpanVisibility::User,
            || {
                {
                    Box::pin(handler_internal(
                        state.expose_internal_errors,
                        &state.http_context,
                        &state.jsonapi_state,
                        &session,
                        &headers,
                        method,
                        uri,
                        query_string,
                        None,
                    ))
                }
            },
        )
        .await;

    set_status_on_current_span(&response);
    match response {
        Ok(r) => (axum::http::StatusCode::OK, Json(r)).into_response(),
        Err(e) => match e {
            RequestError::BadRequest(err) => (
                axum::http::StatusCode::BAD_REQUEST,
                Json(serde_json::json!({"error": err})),
            )
                .into_response(),
            RequestError::NotFound => (
                axum::http::StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "invalid route or path"})),
            )
                .into_response(),
        },
    }
}

#[allow(clippy::unused_async)]
async fn handler_internal(
    _expose_internal_errors: ExposeInternalErrors,
    _http_context: &HttpContext,
    state: &State,
    _session: &Session,
    _request_headers: &HeaderMap,
    http_method: Method,
    uri: Uri,
    query_string: HashMap<String, String>,
    _project_id: Option<&ProjectId>,
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
struct JsonApiDocument {
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
    _query_string: &HashMap<String, String>,
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
async fn rest_request_tracing_middleware<B: Send>(
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
