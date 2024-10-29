use axum::{extract::State, response::IntoResponse, Extension, Json};
use futures_util::FutureExt;

use crate::EngineState;
use hasura_authn_core::Session;
use lang_graphql as gql;
use tracing_util::{set_status_on_current_span, SpanVisibility};

pub async fn handle_request(
    headers: axum::http::header::HeaderMap,
    State(state): State<EngineState>,
    Extension(session): Extension<Session>,
    Json(request): Json<gql::http::RawRequest>,
) -> gql::http::Response {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "handle_request",
            "Handle request",
            SpanVisibility::User,
            || {
                {
                    Box::pin(
                        graphql_frontend::execute_query(
                            state.expose_internal_errors,
                            &state.http_context,
                            &state.graphql_state,
                            &session,
                            &headers,
                            request,
                            None,
                        )
                        .map(|(_operation_type, graphql_response)| graphql_response),
                    )
                }
            },
        )
        .await;

    // Set the span as error if the response contains an error
    // NOTE: Ideally, we should mark the root span as error in `graphql_request_tracing_middleware` function,
    // the tracing middleware, where the span is initialized. It is possible by completing the implementation
    // of `Traceable` trait for `AxumResponse` struct. The said struct just wraps the `axum::response::Response`.
    // The only way to determine the error is to inspect the status code from the `Response` struct.
    // In `/graphql` API, all responses are sent with `200` OK including errors, which leaves no way to deduce errors in the tracing middleware.
    set_status_on_current_span(&response);
    response.inner()
}

pub async fn handle_explain_request(
    headers: axum::http::header::HeaderMap,
    State(state): State<EngineState>,
    Extension(session): Extension<Session>,
    Json(request): Json<gql::http::RawRequest>,
) -> graphql_frontend::ExplainResponse {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "handle_explain_request",
            "Handle explain request",
            SpanVisibility::User,
            || {
                Box::pin(
                    graphql_frontend::execute_explain(
                        state.expose_internal_errors,
                        &state.http_context,
                        &state.graphql_state,
                        &session,
                        &headers,
                        request,
                    )
                    .map(|(_operation_type, graphql_response)| graphql_response),
                )
            },
        )
        .await;

    // Set the span as error if the response contains an error
    set_status_on_current_span(&response);
    response
}

pub async fn handle_websocket_request(
    headers: axum::http::header::HeaderMap,
    State(engine_state): State<EngineState>,
    ws: axum::extract::ws::WebSocketUpgrade,
) -> impl IntoResponse {
    // Create the context for the websocket server
    let context = graphql_ws::Context {
        connection_expiry: graphql_ws::ConnectionExpiry::Never,
        http_context: engine_state.http_context,
        project_id: None, // project_id is not needed for OSS v3-engine.
        expose_internal_errors: engine_state.expose_internal_errors,
        schema: engine_state.graphql_state,
        auth_config: engine_state.auth_config,
        plugin_configs: engine_state.plugin_configs,
        metrics: graphql_ws::NoOpWebSocketMetrics, // No metrics implementation
    };

    engine_state
        .graphql_websocket_server
        .upgrade_and_handle_websocket(ws, &headers, context)
}
