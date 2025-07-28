use std::borrow::Cow;

use crate::EngineState;
use crate::VERSION;
use axum::{
    Extension,
    extract::{ConnectInfo, State},
    http::{HeaderMap, Request},
    middleware::Next,
    response::IntoResponse,
};
use axum_core::body::Body;
use engine_types::WithMiddlewareErrorConverter;
use hasura_authn::authenticate;
use http_body_util::BodyExt;
use pre_parse_plugin::execute::pre_parse_plugins_handler;
use pre_response_plugin::execute::pre_response_plugins_handler;

use hasura_authn_core::Session;
use pre_response_plugin::execute::ProcessedPreResponsePluginResponse;
use tracing_util::{SpanVisibility, TraceableHttpResponse};

use super::types::RequestType;

/// Middleware to start tracing of the `/graphql` request.
/// This middleware must be active for the entire duration
/// of the request i.e. this middleware should be the
/// entry point and the exit point of the GraphQL request.
pub async fn graphql_request_tracing_middleware(
    request_type: RequestType,
    request: Request<Body>,
    next: Next,
) -> axum::response::Response {
    use tracing_util::*;
    let tracer = global_tracer();
    let path = "/graphql";

    tracer
        .in_span_async_with_parent_context(
            path,
            path,
            SpanVisibility::User,
            &request.headers().clone(),
            || {
                set_attribute_on_active_span(AttributeVisibility::Internal, "version", VERSION);
                set_attribute_on_active_span(
                    AttributeVisibility::Default,
                    "request.type",
                    request_type.to_str(),
                );
                Box::pin(async move {
                    let mut response = next.run(request).await;
                    get_text_map_propagator(|propagator| {
                        propagator.inject_context(
                            &Context::current(),
                            &mut HeaderInjector(response.headers_mut()),
                        );
                    });
                    TraceableHttpResponse::new(response, path)
                })
            },
        )
        .await
        .response
}

/// Middleware to start tracing of the `/v1/explain` request.
/// This middleware must be active for the entire duration
/// of the request i.e. this middleware should be the
/// entry point and the exit point of the GraphQL request.
pub async fn explain_request_tracing_middleware(
    request: Request<Body>,
    next: Next,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let path = "/v1/explain";
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

/// This middleware authenticates the incoming GraphQL request according to the
/// authentication configuration present in the `auth_config` of `EngineState`. The
/// result of the authentication is `hasura-authn-core::Identity`, which is then
/// made available to the GraphQL request handler.
pub async fn authentication_middleware(
    State(state): State<engine_types::WithMiddlewareErrorConverter<EngineState>>,
    headers_map: HeaderMap,
    mut request: Request<Body>,
    next: Next,
) -> axum::response::Result<axum::response::Response> {
    let tracer = tracing_util::global_tracer();

    let engine_state = &state.state;
    let resolved_identity = tracer
        .in_span_async(
            "authentication_middleware",
            "Authentication middleware",
            SpanVisibility::Internal,
            || {
                Box::pin(authenticate(
                    &headers_map,
                    &engine_state.http_context.client,
                    &engine_state.auth_config,
                    &engine_state.auth_mode_header,
                ))
            },
        )
        .await
        .map_err(|err| state.handle_error(err.into_middleware_error()))?;

    request.extensions_mut().insert(resolved_identity);
    Ok(next.run(request).await)
}

pub async fn plugins_middleware(
    ConnectInfo(client_address): ConnectInfo<std::net::SocketAddr>,
    State(state): State<WithMiddlewareErrorConverter<EngineState>>,
    Extension(session): Extension<Session>,
    headers_map: HeaderMap,
    request: Request<axum::body::Body>,
    next: Next,
) -> axum::response::Result<axum::response::Response<Body>> {
    let engine_state = &state.state;
    let (parts, body) = request.into_parts();
    let bytes = body
        .collect()
        .await
        .map_err(|err| {
            (reqwest::StatusCode::INTERNAL_SERVER_ERROR, err.to_string()).into_response()
        })?
        .to_bytes();
    let raw_request = bytes.clone();

    // Check if the pre_parse_plugins_config is empty
    let response = match nonempty::NonEmpty::from_slice(
        &engine_state
            .resolved_metadata
            .plugin_configs
            .pre_parse_plugins,
    ) {
        None => {
            // If empty, do nothing and pass the request to the next middleware
            let recreated_request = Request::from_parts(parts, axum::body::Body::from(bytes));
            Ok::<_, axum::response::ErrorResponse>(next.run(recreated_request).await)
        }
        Some(pre_parse_plugins) => {
            let response = pre_parse_plugins_handler(
                client_address,
                &pre_parse_plugins,
                &engine_state.http_context.client,
                session.clone(),
                &bytes,
                headers_map.clone(),
            )
            .await
            .map_err(|err| state.handle_error(err.into_middleware_error()))?;

            match response {
                pre_parse_plugin::execute::ProcessedPreParsePluginResponse::Return(response) => {
                    Ok(response)
                }
                pre_parse_plugin::execute::ProcessedPreParsePluginResponse::Continue(
                    new_raw_request,
                ) => {
                    let recreated_request = if let Some(new_raw_request) = new_raw_request {
                        let bytes = serde_json::to_vec(&new_raw_request).map_err(|err| {
                            (reqwest::StatusCode::INTERNAL_SERVER_ERROR, err.to_string())
                                .into_response()
                        })?;
                        Request::from_parts(parts, axum::body::Body::from(bytes))
                    } else {
                        Request::from_parts(parts, axum::body::Body::from(bytes))
                    };
                    Ok(next.run(recreated_request).await)
                }
            }
        }
    }?;

    let (parts, body) = response.into_parts();
    let response_bytes = body
        .collect()
        .await
        .map_err(|err| {
            (reqwest::StatusCode::INTERNAL_SERVER_ERROR, err.to_string()).into_response()
        })?
        .to_bytes();

    // Execute pre-response plugins
    let pre_response_plugins = &engine_state
        .resolved_metadata
        .plugin_configs
        .pre_response_plugins;

    if !pre_response_plugins.is_empty() {
        let plugin_response = pre_response_plugins_handler(
            client_address,
            pre_response_plugins,
            &engine_state.http_context.client,
            session,
            &raw_request,
            &response_bytes,
            headers_map,
        )
        .await?;
        match plugin_response {
            ProcessedPreResponsePluginResponse::Continue => {}
            ProcessedPreResponsePluginResponse::Response(new_raw_response) => {
                return Ok(new_raw_response);
            }
        }
    }
    let recreated_response =
        axum::response::Response::from_parts(parts, axum::body::Body::from(response_bytes));
    Ok(recreated_response)
}

/// Middleware to start tracing of the `/*path` request.
/// This middleware must be active for the entire duration
/// of the request i.e. this middleware should be the
/// entry point and the exit point of the pre-route request.
pub async fn pre_route_request_tracing_middleware(
    request: Request<Body>,
    next: Next,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let path = request.uri().to_string();
    tracer
        .in_span_async_with_parent_context(
            Cow::from(path.clone()),
            path.clone(),
            SpanVisibility::User,
            &request.headers().clone(),
            || {
                Box::pin(async move {
                    let response = next.run(request).await;
                    TraceableHttpResponse::new(response, Cow::from(path))
                })
            },
        )
        .await
        .response
}
