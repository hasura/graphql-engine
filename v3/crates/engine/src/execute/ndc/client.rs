use super::response::handle_response_with_size_limit;
use ndc_models;
use reqwest::header::{HeaderMap, HeaderValue};
use serde::{de::DeserializeOwned, Deserialize};
use std::fmt;
use thiserror::Error;
use tracing_util::SpanVisibility;

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

/// Error type for the NDC API client interactions
#[derive(Debug, Error)]
pub enum Error {
    Reqwest(#[from] reqwest::Error),
    Serde(#[from] serde_json::Error),
    Io(#[from] std::io::Error),
    ConnectorError(ConnectorError),
    InvalidConnectorError(InvalidConnectorError),
    InvalidBaseURL,
    ResponseTooLarge(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (module, e) = match self {
            Error::Reqwest(e) => ("reqwest", e.to_string()),
            Error::Serde(e) => ("serde", e.to_string()),
            Error::Io(e) => ("IO", e.to_string()),
            Error::ConnectorError(e) => ("response", format!("status code {}", e.status)),
            Error::InvalidConnectorError(e) => ("response", format!("status code {}", e.status)),
            Error::InvalidBaseURL => ("url", "invalid base URL".into()),
            Error::ResponseTooLarge(message) => ("response", format!("too large: {}", message)),
        };
        write!(f, "error in {}: {}", module, e)
    }
}

impl tracing_util::TraceableError for Error {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        tracing_util::ErrorVisibility::Internal
    }
}

#[derive(Debug, Clone, Error)]
#[error("ConnectorError {{ status: {status}, error_response.message: {} }}", error_response.message)]
pub struct ConnectorError {
    pub status: reqwest::StatusCode,
    pub error_response: ndc_models::ErrorResponse,
}

#[derive(Debug, Clone, Error)]
#[error("InvalidConnectorError {{ status: {status}, content: {content} }}")]
pub struct InvalidConnectorError {
    pub status: reqwest::StatusCode,
    pub content: serde_json::Value,
}

/// GET on /capabilities endpoint
///
/// https://hasura.github.io/ndc-spec/specification/capabilities.html
pub async fn capabilities_get(
    configuration: &Configuration,
) -> Result<ndc_models::CapabilitiesResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "capabilities_get",
            "Get capabilities".to_string(),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let client = &configuration.client;

                    let uri = append_path(&configuration.base_path, &["capabilities"])
                        .map_err(|_| Error::InvalidBaseURL)?;
                    let req_builder = client.request(reqwest::Method::GET, uri);

                    execute_request(configuration, req_builder).await
                })
            },
        )
        .await
}

/// POST on /query/explain endpoint
///
/// https://hasura.github.io/ndc-spec/specification/explain.html?highlight=%2Fexplain#request
pub async fn explain_query_post(
    configuration: &Configuration,
    query_request: &ndc_models::QueryRequest,
) -> Result<ndc_models::ExplainResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "explain_query_post",
            "Post explain query".to_string(),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let client = &configuration.client;

                    let uri = append_path(&configuration.base_path, &["query", "explain"])
                        .map_err(|_| Error::InvalidBaseURL)?;
                    let req_builder = client.request(reqwest::Method::POST, uri);

                    execute_request(configuration, req_builder.json(query_request)).await
                })
            },
        )
        .await
}

/// POST on /mutation/explain endpoint
///
/// https://hasura.github.io/ndc-spec/specification/explain.html?highlight=%2Fexplain#request-1
pub async fn explain_mutation_post(
    configuration: &Configuration,
    mutation_request: &ndc_models::MutationRequest,
) -> Result<ndc_models::ExplainResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "explain_mutation_post",
            "Post explain mutation".to_string(),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let client = &configuration.client;

                    let uri = append_path(&configuration.base_path, &["mutation", "explain"])
                        .map_err(|_| Error::InvalidBaseURL)?;
                    let req_builder = client.request(reqwest::Method::POST, uri);

                    execute_request(configuration, req_builder.json(mutation_request)).await
                })
            },
        )
        .await
}

/// POST on /mutation endpoint
///
/// https://hasura.github.io/ndc-spec/specification/mutations/index.html
pub async fn mutation_post(
    configuration: &Configuration,
    mutation_request: &ndc_models::MutationRequest,
) -> Result<ndc_models::MutationResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "mutation_post",
            "Post request for mutation".to_string(),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let client = &configuration.client;

                    let uri = append_path(&configuration.base_path, &["mutation"])
                        .map_err(|_| Error::InvalidBaseURL)?;
                    let req_builder = client.request(reqwest::Method::POST, uri);

                    execute_request(configuration, req_builder.json(mutation_request)).await
                })
            },
        )
        .await
}

/// POST on /query endpoint
///
/// https://hasura.github.io/ndc-spec/specification/queries/index.html
pub async fn query_post(
    configuration: &Configuration,
    query_request: &ndc_models::QueryRequest,
) -> Result<ndc_models::QueryResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "query_post",
            "Post request for query".to_string(),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let client = &configuration.client;

                    let uri = append_path(&configuration.base_path, &["query"])
                        .map_err(|_| Error::InvalidBaseURL)?;
                    let req_builder = client.request(reqwest::Method::POST, uri);

                    execute_request(configuration, req_builder.json(query_request)).await
                })
            },
        )
        .await
}

/// GET on /schema endpoint
///
/// https://hasura.github.io/ndc-spec/specification/schema/index.html
pub async fn schema_get(
    configuration: &Configuration,
) -> Result<ndc_models::SchemaResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "schema_get",
            "Get schema".to_string(),
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let client = &configuration.client;

                    let uri = append_path(&configuration.base_path, &["schema"])
                        .map_err(|_| Error::InvalidBaseURL)?;
                    let req_builder = client.request(reqwest::Method::GET, uri);

                    execute_request(configuration, req_builder).await
                })
            },
        )
        .await
}

// Private utility functions

/// Inject trace context into the request via headers
fn inject_trace_context(mut request_builder: reqwest::RequestBuilder) -> reqwest::RequestBuilder {
    let trace_headers = tracing_util::get_trace_context();
    for (key, value) in trace_headers {
        request_builder = request_builder.header(key, value);
    }
    request_builder
}

/// Append a path to a URL
fn append_path(url: &reqwest::Url, path: &[&str]) -> Result<reqwest::Url, ()> {
    let mut url = url.clone();
    url.path_segments_mut()?.pop_if_empty().extend(path);
    Ok(url)
}

/// Execute a request and deserialize the JSON response
async fn execute_request<T: DeserializeOwned>(
    configuration: &Configuration,
    mut request_builder: reqwest::RequestBuilder,
) -> Result<T, Error> {
    // Inject trace context into the request
    request_builder = inject_trace_context(request_builder);
    // Set user agent if provided
    if let Some(ref user_agent) = configuration.user_agent {
        request_builder = request_builder.header(reqwest::header::USER_AGENT, user_agent);
    }
    // Set headers from configuration
    // Note: The headers will be merged in to any already set.
    request_builder = request_builder.headers(configuration.headers.clone());

    // Build and execute the request
    let request = request_builder.build()?;
    let response = configuration.client.execute(request).await?;

    let response_status = response.status();
    if !response_status.is_client_error() && !response_status.is_server_error() {
        let result = match configuration.response_size_limit {
            None => response.json().await?,
            Some(size_limit) => handle_response_with_size_limit(response, size_limit).await?,
        };
        Ok(result)
    } else {
        Err(construct_error(response).await)
    }
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
                Error::ConnectorError(connector_error)
            }
            // If we can't read the error response, respond as-is.
            Err(_) => Error::InvalidConnectorError(InvalidConnectorError {
                status,
                content: body,
            }),
        },
        // If we can't even parse the JSON response, drop it.
        Err(_) => Error::InvalidConnectorError(InvalidConnectorError {
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
