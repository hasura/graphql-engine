use axum::{
    Extension, Json, Router,
    http::{HeaderMap, Method, Uri},
    response::IntoResponse,
    routing::get,
};
use hasura_authn_core::Session;
use std::convert::Infallible;
use std::sync::Arc;
use tower_http::trace::TraceLayer;
use tracing_util::{SpanVisibility, Traceable, set_status_on_current_span};

use crate::{EngineState, authentication_middleware};

pub fn create_json_api_router(state: EngineState) -> axum::Router {
    // Create the base router and nest both paths to the same handler
    Router::new()
        .nest(
            jsonapi::EndPoint::V1Rest.as_str(),
            build_router(state.clone(), jsonapi::EndPoint::V1Rest),
        )
        .nest(
            jsonapi::EndPoint::V1Jsonapi.as_str(),
            build_router(state, jsonapi::EndPoint::V1Jsonapi),
        )
}

fn build_router(state: EngineState, endpoint: jsonapi::EndPoint) -> axum::Router {
    Router::new()
        .route("/__schema", get(handle_jsonapi_schema))
        // TODO: update method GET; for now we are only supporting queries. And
        // in JSON:API spec, all queries have the GET method. Not even HEAD is
        // supported. So this should be fine.
        .route("/*path", get(handle_jsonapi_request))
        .layer(axum::middleware::from_fn_with_state(
            jsonapi::build_state_with_middleware_error_converter(()),
            hasura_authn_core::resolve_session,
        ))
        .layer(axum::middleware::from_fn_with_state(
            jsonapi::build_state_with_middleware_error_converter(state.clone()),
            authentication_middleware,
        ))
        .layer(axum::middleware::from_fn_with_state(
            endpoint,
            jsonapi::jsonapi_request_tracing_middleware,
        ))
        // *PLEASE DO NOT ADD ANY MIDDLEWARE
        // BEFORE THE `jsonapi_request_tracing_middleware`*
        // Refer to it for more details.
        .layer(TraceLayer::new_for_http())
        .with_state(state)
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
    type ErrorType<'a>
        = Infallible
    where
        Self: 'a;

    fn get_error(&self) -> Option<Self::ErrorType<'_>> {
        None
    }
}

async fn handle_jsonapi_schema(
    axum::extract::State(state): axum::extract::State<EngineState>,
    Extension(session): Extension<Session>,
) -> impl IntoResponse {
    let tracer = tracing_util::global_tracer();

    tracer.in_span(
        "handle_schema",
        "Handle schema",
        SpanVisibility::User,
        || match state.jsonapi_catalog.state_per_role.get(&session.role) {
            Some(jsonapi_state) => match jsonapi::openapi_schema(jsonapi_state) {
                Ok(spec) => JsonApiSchemaResponse { spec },
                Err(_) => JsonApiSchemaResponse {
                    spec: jsonapi::empty_schema(),
                },
            },
            None => JsonApiSchemaResponse {
                spec: jsonapi::empty_schema(),
            },
        },
    )
}

async fn handle_jsonapi_request(
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
            "handle_jsonapi_request",
            "Handle jsonapi request",
            SpanVisibility::User,
            || {
                Box::pin(jsonapi::handler_internal(
                    Arc::new(request_headers),
                    Arc::new(state.http_context.clone()),
                    Arc::new(state.resolved_metadata.plugin_configs.clone()),
                    Arc::new(session),
                    &state.jsonapi_catalog,
                    state.resolved_metadata,
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
        Err(e) => e
            .into_http_error(state.expose_internal_errors)
            .into_response(),
    }
}
