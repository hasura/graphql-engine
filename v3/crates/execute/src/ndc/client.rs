use std::convert::identity;

use reqwest::header::{HeaderMap, HeaderValue};
use serde::{de::DeserializeOwned, Deserialize};
use thiserror::Error;

use tracing_util::{SpanVisibility, Successful};

use crate::ndc::response::handle_response_with_size_limit;

/// Error type for the NDC API client interactions
#[derive(Debug, Error)]
pub enum Error {
    #[error(
        "request to connector failed with status code {}: {0}",
        .0.status().map_or_else(|| "N/A".to_string(), |s| s.to_string())
    )]
    Reqwest(#[from] reqwest::Error),

    #[error("unable to decode JSON response from connector: {0}")]
    Serde(#[from] serde_json::Error),

    #[error("internal IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("invalid connector base URL")]
    InvalidBaseURL,

    #[error("invalid header value characters in project_id: {0}")]
    ProjectIdHeaderValueConversion(#[from] reqwest::header::InvalidHeaderValue),

    #[error("response received from connector is too large: {0}")]
    ResponseTooLarge(String),

    #[error("connector error: {0}")]
    Connector(ConnectorError),

    #[error("invalid connector error: {0}")]
    InvalidConnector(InvalidConnectorError),
}

impl tracing_util::TraceableError for Error {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::Internal
    }
}

#[derive(Debug, Clone, Error)]
#[error("connector returned status code {status} with message: {}", error_response.message)]
pub struct ConnectorError {
    pub status: reqwest::StatusCode,
    pub error_response: ndc_models::ErrorResponse,
}

#[derive(Debug, Clone, Error)]
#[error("invalid connector error with status {status} and {content}")]
pub struct InvalidConnectorError {
    pub status: reqwest::StatusCode,
    pub content: serde_json::Value,
}

/// Configuration for the API client
/// Contains all the information necessary to perform requests.
#[derive(Debug, Clone)]
pub struct Configuration {
    pub base_path: reqwest::Url,
    pub user_agent: Option<String>,
    pub client: reqwest::Client,
    pub headers: HeaderMap<HeaderValue>,
    pub response_size_limit: Option<usize>,
}

/// GET on /capabilities endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/capabilities.html>
pub async fn capabilities_get(
    configuration: &Configuration,
) -> Result<ndc_models::CapabilitiesResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "capabilities_get",
            "Get capabilities",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(&configuration.base_path, &["capabilities"])?;
                    let request =
                        construct_request(configuration, reqwest::Method::GET, url, identity);
                    execute_request(configuration, request).await
                })
            },
        )
        .await
}

/// POST on /query/explain endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/explain.html?highlight=%2Fexplain#request>
pub async fn explain_query_post(
    configuration: &Configuration,
    query_request: &ndc_models::QueryRequest,
) -> Result<ndc_models::ExplainResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "explain_query_post",
            "Post explain query",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(&configuration.base_path, &["query", "explain"])?;
                    let request =
                        construct_request(configuration, reqwest::Method::POST, url, |r| {
                            r.json(query_request)
                        });
                    execute_request(configuration, request).await
                })
            },
        )
        .await
}

/// POST on /mutation/explain endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/explain.html?highlight=%2Fexplain#request-1>
pub async fn explain_mutation_post(
    configuration: &Configuration,
    mutation_request: &ndc_models::MutationRequest,
) -> Result<ndc_models::ExplainResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "explain_mutation_post",
            "Post explain mutation",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(&configuration.base_path, &["mutation", "explain"])?;
                    let request =
                        construct_request(configuration, reqwest::Method::POST, url, |r| {
                            r.json(mutation_request)
                        });
                    execute_request(configuration, request).await
                })
            },
        )
        .await
}

/// POST on /mutation endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/mutations/index.html>
pub async fn mutation_post(
    configuration: &Configuration,
    mutation_request: &ndc_models::MutationRequest,
) -> Result<ndc_models::MutationResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "mutation_post",
            "Post request for mutation",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(&configuration.base_path, &["mutation"])?;
                    let request =
                        construct_request(configuration, reqwest::Method::POST, url, |r| {
                            r.json(mutation_request)
                        });
                    execute_request(configuration, request).await
                })
            },
        )
        .await
}

/// POST on /query endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/queries/index.html>
pub async fn query_post(
    configuration: &Configuration,
    query_request: &ndc_models::QueryRequest,
) -> Result<ndc_models::QueryResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "query_post",
            "Post request for query",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(&configuration.base_path, &["query"])?;
                    let request =
                        construct_request(configuration, reqwest::Method::POST, url, |r| {
                            r.json(query_request)
                        });
                    execute_request(configuration, request).await
                })
            },
        )
        .await
}

// Private utility functions

/// Append a path to a URL
fn append_path(url: &reqwest::Url, path: &[&str]) -> Result<reqwest::Url, Error> {
    let mut url = url.clone();
    url.path_segments_mut()
        .map_err(|_| Error::InvalidBaseURL)?
        .pop_if_empty()
        .extend(path);
    Ok(url)
}

fn construct_request(
    configuration: &Configuration,
    method: reqwest::Method,
    url: reqwest::Url,
    modify: impl FnOnce(reqwest::RequestBuilder) -> reqwest::RequestBuilder,
) -> reqwest::RequestBuilder {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span(
            "construct_request",
            "Construct request",
            SpanVisibility::Internal,
            || {
                let mut request_builder = configuration.client.request(method, url);
                // Apply customizations
                request_builder = modify(request_builder);
                // Set user agent if provided
                if let Some(ref user_agent) = configuration.user_agent {
                    request_builder =
                        request_builder.header(reqwest::header::USER_AGENT, user_agent);
                }
                // Set headers from configuration
                request_builder = request_builder.headers(configuration.headers.clone());
                // Return the prepared request
                Successful::new(request_builder)
            },
        )
        .into_inner()
}

/// Execute a request and deserialize the JSON response
async fn execute_request<T: DeserializeOwned>(
    configuration: &Configuration,
    request: reqwest::RequestBuilder,
) -> Result<T, Error> {
    let tracer = tracing_util::global_tracer();

    let response = tracer
        .in_span_async(
            "send_request",
            "Send request",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    // We inject the trace headers here so they are a child of this span.
                    request
                        .headers(tracing_util::get_trace_headers())
                        .send()
                        .await
                        .map_err(Error::from)
                })
            },
        )
        .await?;

    tracer
        .in_span_async(
            "deserialize_response",
            "Deserialize response",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let response_status = response.status();
                    if !response_status.is_client_error() && !response_status.is_server_error() {
                        let result = match configuration.response_size_limit {
                            None => response.json().await?,
                            Some(size_limit) => {
                                handle_response_with_size_limit(response, size_limit).await?
                            }
                        };
                        Ok(result)
                    } else {
                        Err(construct_error(response).await)
                    }
                })
            },
        )
        .await
}

/// Build an error from the response status and content
async fn construct_error(response: reqwest::Response) -> Error {
    let status = response.status();
    match response.json().await {
        Ok(body) => match ndc_models::ErrorResponse::deserialize(&body) {
            Ok(error_response) => {
                let connector_error = ConnectorError {
                    status,
                    error_response,
                };
                Error::Connector(connector_error)
            }
            // If we can't read the error response, respond as-is.
            Err(_) => Error::InvalidConnector(InvalidConnectorError {
                status,
                content: body,
            }),
        },
        // If we can't even parse the JSON response, drop it.
        Err(_) => Error::InvalidConnector(InvalidConnectorError {
            status,
            content: serde_json::Value::Null,
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::append_path;

    #[test]
    fn test_append_path() {
        let url = reqwest::Url::parse("http://hasura.io").unwrap();
        let path = "capabilities";
        let result = append_path(&url, &[path]).unwrap();
        assert_eq!(result.as_str(), "http://hasura.io/capabilities");
    }

    #[test]
    fn test_append_path_with_trailing_slash() {
        let url = reqwest::Url::parse("http://hasura.io/").unwrap();
        let path = "capabilities";
        let result = append_path(&url, &[path]).unwrap();
        assert_eq!(result.as_str(), "http://hasura.io/capabilities");
    }

    #[test]
    fn test_append_path_with_non_empty_path() {
        let url = reqwest::Url::parse("http://hasura.io/ndc").unwrap();
        let path = "capabilities";
        let result = append_path(&url, &[path]).unwrap();
        assert_eq!(result.as_str(), "http://hasura.io/ndc/capabilities");
    }

    #[test]
    fn test_append_path_with_non_empty_path_and_trailing_slash() {
        let url = reqwest::Url::parse("http://hasura.io/ndc/").unwrap();
        let path = "capabilities";
        let result = append_path(&url, &[path]).unwrap();
        assert_eq!(result.as_str(), "http://hasura.io/ndc/capabilities");
    }

    #[test]
    fn test_append_paths() {
        let url = reqwest::Url::parse("http://hasura.io/ndc/").unwrap();
        let paths = ["query", "explain"];
        let result = append_path(&url, &paths).unwrap();
        assert_eq!(result.as_str(), "http://hasura.io/ndc/query/explain");
    }
}
