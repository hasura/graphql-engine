use axum::http::HeaderMap;

use hasura_authn_core::Session;
use lang_graphql::http::RawRequest;
use metadata_resolve::ResolvedLifecyclePreResponseAsyncPluginHook;
use tracing_util::{SpanVisibility, set_attribute_on_active_span};

use crate::execute::common::{Error, ExecutePluginsTracing, build_request};

/// Execute the pre-response plugins in a separate task
pub fn execute_async_pre_response_plugins_in_task(
    client_address: std::net::SocketAddr,
    pre_response_plugins_config: nonempty::NonEmpty<ResolvedLifecyclePreResponseAsyncPluginHook>,
    http_client: reqwest::Client,
    session: Session,
    raw_request: RawRequest,
    raw_response: serde_json::Value,
    headers_map: HeaderMap,
    tracing_strategy: ExecutePluginsTracing,
) {
    let tracer = tracing_util::global_tracer();
    // Spawn a new task to execute the pre-response plugins
    tokio::spawn(async move {
        // Execute all pre-response plugins
        let span_name = "execute_all_pre_response_plugins";
        let span_display_name = "Execute all Pre-response Plugins".to_string();
        let span_visibility = SpanVisibility::User;
        // See comments on `ExecutePluginsTracing` for more details on the tracing strategy.
        match tracing_strategy {
            ExecutePluginsTracing::ParentContext => {
                tracer
                    .in_span_async_with_parent_context(
                        span_name,
                        span_display_name,
                        span_visibility,
                        &headers_map,
                        || {
                            Box::pin(async {
                                execute_all_async_plugins(
                                    client_address,
                                    &pre_response_plugins_config,
                                    &http_client,
                                    &session,
                                    &raw_request,
                                    &raw_response,
                                    &headers_map,
                                )
                                .await;
                                tracing_util::Successful::new(())
                            })
                        },
                    )
                    .await;
            }
            ExecutePluginsTracing::NewTraceWithLink { span_link } => {
                tracer
                    .new_trace_async_with_link(
                        span_name,
                        span_display_name,
                        span_visibility,
                        span_link,
                        || {
                            Box::pin(async {
                                execute_all_async_plugins(
                                    client_address,
                                    &pre_response_plugins_config,
                                    &http_client,
                                    &session,
                                    &raw_request,
                                    &raw_response,
                                    &headers_map,
                                )
                                .await;
                                tracing_util::Successful::new(())
                            })
                        },
                    )
                    .await;
            }
        }
    });
}

/// Execute all pre-response plugins
async fn execute_all_async_plugins(
    client_address: std::net::SocketAddr,
    pre_response_plugins_config: &nonempty::NonEmpty<ResolvedLifecyclePreResponseAsyncPluginHook>,
    http_client: &reqwest::Client,
    session: &Session,
    raw_request: &RawRequest,
    raw_response: &serde_json::Value,
    headers_map: &HeaderMap,
) {
    let tracer = tracing_util::global_tracer();
    let mut async_executions = Vec::with_capacity(pre_response_plugins_config.capacity());
    // Execute each pre-response plugin asynchronously without await.
    for pre_plugin_config in pre_response_plugins_config {
        let async_execution = async {
            (tracer
                .in_span_async(
                    "execute_async_pre_response_plugin",
                    "Execute a Asynchronous Pre-response Plugin",
                    SpanVisibility::User,
                    || {
                        Box::pin(async {
                            set_attribute_on_active_span(
                                tracing_util::AttributeVisibility::Default,
                                "plugin.name",
                                pre_plugin_config.name.clone(),
                            );
                            execute_async_plugin(
                                client_address,
                                http_client,
                                pre_plugin_config,
                                headers_map,
                                session,
                                raw_request,
                                raw_response,
                            )
                            .await
                        })
                    },
                )
                .await,)
        };
        // Collect the async execution future
        async_executions.push(async_execution);
    }
    // Wait for all the async executions to complete
    let _plugin_responses = futures_util::future::join_all(async_executions).await;
}

pub async fn execute_async_plugin(
    client_address: std::net::SocketAddr,
    http_client: &reqwest::Client,
    config: &ResolvedLifecyclePreResponseAsyncPluginHook,
    client_headers: &HeaderMap,
    session: &Session,
    raw_request: &RawRequest,
    response: &serde_json::Value,
) -> Result<String, Error> {
    let tracer = tracing_util::global_tracer();
    let _response = tracer
        .in_span_async(
            "request_to_webhook",
            "Send request to webhook",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    let http_request_builder = build_request(
                        client_address,
                        http_client,
                        &config.request,
                        &config.url,
                        client_headers,
                        session,
                        raw_request,
                        response,
                    )
                    .map_err(|err| Error::BuildRequestError(config.name.clone(), err))?;
                    let req = http_request_builder.build().map_err(Error::ReqwestError)?;
                    http_client.execute(req).await.map_err(|e| {
                        Error::ErrorWhileMakingHTTPRequestToTheHook(config.name.clone(), e)
                    })
                })
            },
        )
        .await?;
    Ok("Done".to_string())
}
