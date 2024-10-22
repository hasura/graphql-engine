use axum::{
    http::{HeaderMap, Method, Uri},
    response::IntoResponse,
    routing::get,
    Extension, Json, Router,
};
use hasura_authn_core::Session;
use std::convert::Infallible;
use std::sync::Arc;
use tower_http::trace::TraceLayer;
use tracing_util::{set_status_on_current_span, SpanVisibility, Traceable};

use crate::{authentication_middleware, EngineState};

pub(crate) fn create_json_api_router(state: EngineState) -> axum::Router {
    let router = Router::new()
        .route("/__schema", get(handle_schema))
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
        .layer(axum::middleware::from_fn(
            jsonapi::rest_request_tracing_middleware,
        ))
        // *PLEASE DO NOT ADD ANY MIDDLEWARE
        // BEFORE THE `explain_request_tracing_middleware`*
        // Refer to it for more details.
        .layer(TraceLayer::new_for_http())
        .with_state(state);
    Router::new().nest("/v1/rest", router)
}

struct JsonApiSchemaResponse {
    spec: oas3::spec::Spec,
}

impl IntoResponse for JsonApiSchemaResponse {
    fn into_response(self) -> axum::response::Response {
        (axum::http::StatusCode::OK, axum::Json(self.spec)).into_response()
    }
}

/// Implement traceable for GraphQL Response
impl Traceable for JsonApiSchemaResponse {
    type ErrorType<'a> = Infallible where Self: 'a;

    fn get_error(&self) -> Option<Self::ErrorType<'_>> {
        None
    }
}

async fn handle_schema(
    axum::extract::State(state): axum::extract::State<EngineState>,
    Extension(session): Extension<Session>,
) -> impl IntoResponse {
    let tracer = tracing_util::global_tracer();

    tracer.in_span(
        "handle_schema",
        "Handle schema",
        SpanVisibility::User,
        || match state.jsonapi_catalog.state_per_role.get(&session.role) {
            Some(jsonapi_state) => {
                let spec = jsonapi::openapi_schema(jsonapi_state);
                JsonApiSchemaResponse { spec }
            }
            None => JsonApiSchemaResponse {
                spec: jsonapi::empty_schema(),
            },
        },
    )
}

async fn handle_request(
    request_headers: HeaderMap,
    method: Method,
    uri: Uri,
    axum::extract::RawQuery(raw_query): axum::extract::RawQuery,
    axum::extract::State(state): axum::extract::State<EngineState>,
    Extension(session): Extension<Session>,
) -> impl IntoResponse {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "handle_request",
            "Handle request",
            SpanVisibility::User,
            || {
                Box::pin(jsonapi::handler_internal(
                    Arc::new(request_headers),
                    Arc::new(state.http_context.clone()),
                    Arc::new(session),
                    &state.jsonapi_catalog,
                    &state.resolved_metadata,
                    method,
                    uri,
                    jsonapi_library::query::Query::from_params(&raw_query.unwrap_or_default()),
                ))
            },
        )
        .await;

    set_status_on_current_span(&response);
    match response {
        Ok(r) => (axum::http::StatusCode::OK, Json(r)).into_response(),
        Err(e) => (match e {
            jsonapi::RequestError::BadRequest(err) => (
                axum::http::StatusCode::BAD_REQUEST,
                Json(serde_json::json!({"error": err})),
            ),
            jsonapi::RequestError::NotFound => (
                axum::http::StatusCode::NOT_FOUND,
                Json(serde_json::json!({"error": "invalid route or path"})),
            ),
            jsonapi::RequestError::InternalError(jsonapi::InternalError::EmptyQuerySet) => (
                axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": "Internal error"})),
            ),
            jsonapi::RequestError::PlanError(plan::PlanError::Internal(msg)) => (
                axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": msg })),
            ),
            jsonapi::RequestError::PlanError(plan::PlanError::External(_err)) => (
                axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": "Internal error" })),
            ),
            jsonapi::RequestError::PlanError(plan::PlanError::Permission(_msg)) => (
                axum::http::StatusCode::FORBIDDEN,
                Json(serde_json::json!({"error": "Access forbidden" })), // need to decide how much
                                                                         // we tell the user, for
                                                                         // now default to nothing
            ),
            jsonapi::RequestError::ExecuteError(field_error) => (
                axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({"error": field_error.to_string() })),
            ),
        })
        .into_response(),
    }
}
