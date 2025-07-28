mod asynchronous;
mod common;
mod synchronous;

use axum::{http::HeaderMap, response::IntoResponse};

use hasura_authn_core::Session;
use lang_graphql::http::RawRequest;
use metadata_resolve::ResolvedLifecyclePreResponsePluginHooks;

pub use crate::execute::{
    asynchronous::execute_async_pre_response_plugins_in_task,
    common::{Error, ExecutePluginsTracing},
    synchronous::{
        PreResponsePluginResponse, ProcessedPreResponsePluginResponse,
        execute_sync_pre_response_plugins,
    },
};

pub async fn pre_response_plugins_handler(
    client_address: std::net::SocketAddr,
    pre_response_plugins_config: &ResolvedLifecyclePreResponsePluginHooks,
    http_client: &reqwest::Client,
    session: Session,
    raw_request_bytes: &axum::body::Bytes,
    raw_response_bytes: &axum::body::Bytes,
    headers_map: HeaderMap,
) -> Result<ProcessedPreResponsePluginResponse, Error> {
    let raw_request = serde_json::from_slice::<RawRequest>(raw_request_bytes)
        .map_err(Error::PluginRequestParseError)?;
    let raw_response = serde_json::from_slice::<serde_json::Value>(raw_response_bytes)
        .map_err(Error::EngineResponseParseError)?;
    // Execute the pre-response plugins in a separate task
    if let Some(async_plugins) =
        nonempty::NonEmpty::from_vec(pre_response_plugins_config.async_hooks.clone())
    {
        execute_async_pre_response_plugins_in_task(
            client_address,
            async_plugins,
            http_client.clone(),
            session.clone(),
            raw_request.clone(),
            raw_response.clone(),
            headers_map.clone(),
            ExecutePluginsTracing::ParentContext, // Use parent context for pre-response plugin tracing
        );
    }

    if let Some(sync_plugins) =
        nonempty::NonEmpty::from_vec(pre_response_plugins_config.sync_hooks.clone())
    {
        let response = execute_sync_pre_response_plugins(
            client_address,
            &sync_plugins,
            http_client,
            &session,
            raw_request,
            raw_response,
            &headers_map,
        )
        .await?;
        match response {
            PreResponsePluginResponse::Continue => {}
            PreResponsePluginResponse::ReturnResponse(new_response_bytes) => {
                let plugin_response = axum::response::Response::builder()
                    .status(reqwest::StatusCode::OK)
                    .header("Content-Type", "application/json")
                    .body(axum::body::Body::from(new_response_bytes))
                    .map_err(Error::ResponsePreparationError)?;
                return Ok(ProcessedPreResponsePluginResponse::Response(
                    plugin_response,
                ));
            }
            PreResponsePluginResponse::ReturnError { plugin_name, error } => {
                let status_code = error.to_status_code();
                let graphql_error = error.into_graphql_error(&plugin_name);
                let error_response =
                    lang_graphql::http::Response::error_with_status(status_code, graphql_error)
                        .into_response();
                return Ok(ProcessedPreResponsePluginResponse::Response(error_response));
            }
        }
    }
    Ok(ProcessedPreResponsePluginResponse::Continue)
}
