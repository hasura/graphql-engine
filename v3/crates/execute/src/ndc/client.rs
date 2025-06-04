use std::borrow::Cow;

use metadata_resolve::data_connectors::NdcVersion;
use reqwest::header::{HeaderMap, HeaderValue};
use serde::de::DeserializeOwned;
use thiserror::Error;

use tracing_util::{SpanVisibility, Successful};

use super::{
    NdcErrorResponse, NdcExplainResponse, NdcMutationRequest, NdcMutationResponse, NdcQueryRequest,
    NdcQueryResponse,
};

/// Error type for the NDC API client interactions
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(
        "request to connector failed with status code {status_code}: {0}",
        status_code = .0.status().map_or_else(|| "N/A".to_string(), |s| s.to_string()) 
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
        match self {
            // Invalid connector errors with 5xx status codes are considered user errors
            // (connector implementation issues, not engine issues)
            Self::InvalidConnector(InvalidConnectorError { status, .. })
                if status.is_server_error() =>
            {
                tracing_util::ErrorVisibility::User
            }

            // TODO some of these other cases seem like User errors also...

            // All other errors are internal
            _ => tracing_util::ErrorVisibility::Internal,
        }
    }
}

#[derive(Debug, Clone, Error)]
#[error("connector returned status code {status} with message: {}, details: {}", error_response.message(), error_response.details())]
pub struct ConnectorError {
    pub status: reqwest::StatusCode,
    pub error_response: NdcErrorResponse,
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
pub struct Configuration<'s> {
    pub base_path: &'s reqwest::Url,
    pub client: reqwest::Client,
    pub headers: Cow<'s, HeaderMap<HeaderValue>>,
    pub response_size_limit: Option<usize>,
}

/// POST on /query/explain endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/explain.html?highlight=%2Fexplain#request>
pub async fn explain_query_post(
    configuration: Configuration<'_>,
    query_request: &NdcQueryRequest,
) -> Result<NdcExplainResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "explain_query_post",
            "Post explain query",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(configuration.base_path, &["query", "explain"])?;
                    let response_size_limit = configuration.response_size_limit;

                    match query_request {
                        NdcQueryRequest::V01(req) => {
                            let request = construct_request(
                                configuration,
                                NdcVersion::V01,
                                reqwest::Method::POST,
                                url,
                                |r| r.json(req),
                            );
                            let response = execute_request(
                                request,
                                response_size_limit,
                                NdcErrorResponse::V01,
                            )
                            .await?;
                            Ok(NdcExplainResponse::V01(response))
                        }
                        NdcQueryRequest::V02(req) => {
                            let request = construct_request(
                                configuration,
                                NdcVersion::V02,
                                reqwest::Method::POST,
                                url,
                                |r| r.json(req),
                            );
                            let response = execute_request(
                                request,
                                response_size_limit,
                                NdcErrorResponse::V02,
                            )
                            .await?;
                            Ok(NdcExplainResponse::V02(response))
                        }
                    }
                })
            },
        )
        .await
}

/// POST on /mutation/explain endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/explain.html?highlight=%2Fexplain#request-1>
pub async fn explain_mutation_post(
    configuration: Configuration<'_>,
    mutation_request: &NdcMutationRequest,
) -> Result<NdcExplainResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "explain_mutation_post",
            "Post explain mutation",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(configuration.base_path, &["mutation", "explain"])?;
                    let response_size_limit = configuration.response_size_limit;

                    match mutation_request {
                        NdcMutationRequest::V01(req) => {
                            let request = construct_request(
                                configuration,
                                NdcVersion::V01,
                                reqwest::Method::POST,
                                url,
                                |r| r.json(req),
                            );
                            let response = execute_request(
                                request,
                                response_size_limit,
                                NdcErrorResponse::V01,
                            )
                            .await?;
                            Ok(NdcExplainResponse::V01(response))
                        }
                        NdcMutationRequest::V02(req) => {
                            let request = construct_request(
                                configuration,
                                NdcVersion::V02,
                                reqwest::Method::POST,
                                url,
                                |r| r.json(req),
                            );
                            let response = execute_request(
                                request,
                                response_size_limit,
                                NdcErrorResponse::V02,
                            )
                            .await?;
                            Ok(NdcExplainResponse::V02(response))
                        }
                    }
                })
            },
        )
        .await
}

/// POST on /mutation endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/mutations/index.html>
pub async fn mutation_post(
    configuration: Configuration<'_>,
    mutation_request: &NdcMutationRequest,
) -> Result<NdcMutationResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "mutation_post",
            "Post request for mutation",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(configuration.base_path, &["mutation"])?;
                    let response_size_limit = configuration.response_size_limit;

                    match mutation_request {
                        NdcMutationRequest::V01(req) => {
                            let request = construct_request(
                                configuration,
                                NdcVersion::V01,
                                reqwest::Method::POST,
                                url,
                                |r| r.json(req),
                            );
                            let response = execute_request(
                                request,
                                response_size_limit,
                                NdcErrorResponse::V01,
                            )
                            .await?;
                            Ok(NdcMutationResponse::V01(response))
                        }
                        NdcMutationRequest::V02(req) => {
                            let request = construct_request(
                                configuration,
                                NdcVersion::V02,
                                reqwest::Method::POST,
                                url,
                                |r| r.json(req),
                            );
                            let response = execute_request(
                                request,
                                response_size_limit,
                                NdcErrorResponse::V02,
                            )
                            .await?;
                            Ok(NdcMutationResponse::V02(response))
                        }
                    }
                })
            },
        )
        .await
}

/// POST on /query endpoint
///
/// <https://hasura.github.io/ndc-spec/specification/queries/index.html>
pub async fn query_post(
    configuration: Configuration<'_>,
    query_request: &NdcQueryRequest,
) -> Result<NdcQueryResponse, Error> {
    let tracer = tracing_util::global_tracer();
    tracer
        .in_span_async(
            "query_post",
            "Post request for query",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(configuration.base_path, &["query"])?;
                    let response_size_limit = configuration.response_size_limit;

                    match query_request {
                        NdcQueryRequest::V01(req) => {
                            let request = construct_request(
                                configuration,
                                NdcVersion::V01,
                                reqwest::Method::POST,
                                url,
                                |r| r.json(req),
                            );
                            let response = execute_request(
                                request,
                                response_size_limit,
                                NdcErrorResponse::V01,
                            )
                            .await?;
                            Ok(NdcQueryResponse::V01(response))
                        }
                        NdcQueryRequest::V02(req) => {
                            let request = construct_request(
                                configuration,
                                NdcVersion::V02,
                                reqwest::Method::POST,
                                url,
                                |r| r.json(req),
                            );
                            let response = execute_request(
                                request,
                                response_size_limit,
                                NdcErrorResponse::V02,
                            )
                            .await?;
                            Ok(NdcQueryResponse::V02(response))
                        }
                    }
                })
            },
        )
        .await
}

/// POST on undocumented /query/rel endpoint
pub async fn query_relational_post(
    configuration: Configuration<'_>,
    request: &ndc_models::RelationalQuery,
) -> Result<ndc_models::RelationalQueryResponse, Error> {
    let tracer = tracing_util::global_tracer();

    tracer
        .in_span_async(
            "query_rel_post",
            "Post relation for query",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    let url = append_path(configuration.base_path, &["query", "relational"])?;
                    let response_size_limit = configuration.response_size_limit;

                    let request = construct_request(
                        configuration,
                        NdcVersion::V02,
                        reqwest::Method::POST,
                        url,
                        |r| r.json(request),
                    );
                    let response =
                        execute_request(request, response_size_limit, NdcErrorResponse::V02)
                            .await?;
                    Ok(response)
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
        .map_err(|()| Error::InvalidBaseURL)?
        .pop_if_empty()
        .extend(path);
    Ok(url)
}

fn construct_request(
    configuration: Configuration,
    ndc_version: NdcVersion,
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
                // Set headers from configuration
                request_builder = request_builder.headers(configuration.headers.into_owned());
                // Set X-Hasura-NDC-Version
                let version = match ndc_version {
                    NdcVersion::V01 => "0.1.0",
                    NdcVersion::V02 => "0.2.0",
                };
                request_builder = request_builder.header(ndc_models::VERSION_HEADER_NAME, version);
                // Return the prepared request
                Successful::new(request_builder)
            },
        )
        .into_inner()
}

/// Execute a request and deserialize the JSON response
async fn execute_request<TResponse, TResponseError, F>(
    request: reqwest::RequestBuilder,
    response_size_limit: Option<usize>,
    to_error: F,
) -> Result<TResponse, Error>
where
    TResponse: DeserializeOwned,
    TResponseError: DeserializeOwned,
    F: FnOnce(TResponseError) -> NdcErrorResponse + Send,
{
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
                        let result = match response_size_limit {
                            None => response.json().await?,
                            Some(size_limit) => {
                                handle_response_with_size_limit(response, size_limit).await?
                            }
                        };
                        Ok(result)
                    } else {
                        Err(construct_error(response, to_error).await)
                    }
                })
            },
        )
        .await
}

/// Build an error from the response status and content
async fn construct_error<TResponseError, F>(response: reqwest::Response, to_error: F) -> Error
where
    TResponseError: DeserializeOwned,
    F: FnOnce(TResponseError) -> NdcErrorResponse,
{
    let status = response.status();
    match response.json().await {
        Ok(body) => match TResponseError::deserialize(&body) {
            Ok(error_response) => {
                let connector_error = ConnectorError {
                    status,
                    error_response: to_error(error_response),
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

/// Handle response return from an NDC request by applying the size limit and
/// deserializing into a JSON value
async fn handle_response_with_size_limit<T: for<'de> serde::Deserialize<'de>>(
    response: reqwest::Response,
    size_limit: usize,
) -> Result<T, Error> {
    if let Some(content_length) = &response.content_length() {
        // Check with content length
        if *content_length > size_limit as u64 {
            Err(Error::ResponseTooLarge(format!(
                "Received content length {content_length} exceeds the limit {size_limit}"
            )))
        } else {
            Ok(response.json().await?)
        }
    } else {
        // If no content length found, then check chunk-by-chunk
        handle_response_by_chunks_with_size_limit(response, size_limit).await
    }
}

/// Handle response by chunks. For each chunk consumed, check if the total size exceeds the limit.
///
/// This logic is separated in a function to allow testing.
async fn handle_response_by_chunks_with_size_limit<T: for<'de> serde::Deserialize<'de>>(
    response: reqwest::Response,
    size_limit: usize,
) -> Result<T, Error> {
    let mut size = 0;
    let mut buf = bytes::BytesMut::new();
    let mut response = response;
    while let Some(chunk) = response.chunk().await? {
        size += chunk.len();
        if size > size_limit {
            return Err(Error::ResponseTooLarge(format!(
                "Size exceeds the limit {size_limit}"
            )));
        }
        buf.extend_from_slice(&chunk);
    }
    Ok(serde_json::from_slice(&buf)?)
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

    use pretty_assertions::assert_eq;

    #[tokio::test]
    async fn test_content_length() {
        let mut server = mockito::Server::new_async().await;
        let test_api = server
            .mock("GET", "/test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"message": "hello"}"#)
            .create();
        let response = reqwest::get(server.url() + "/test").await.unwrap();
        test_api.assert();
        let err = super::handle_response_with_size_limit::<serde_json::Value>(response, 10)
            .await
            .unwrap_err();
        assert_eq!(
            err.to_string(),
            "response received from connector is too large: Received content length 20 exceeds the limit 10"
        );
    }

    #[tokio::test]
    async fn test_chunk_by_chunk() {
        let mut server = mockito::Server::new_async().await;
        let test_api = server
            .mock("GET", "/test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"message": "hello"}"#)
            .create();
        let response = reqwest::get(server.url() + "/test").await.unwrap();
        test_api.assert();
        let err =
            super::handle_response_by_chunks_with_size_limit::<serde_json::Value>(response, 5)
                .await
                .unwrap_err();
        assert_eq!(
            err.to_string(),
            "response received from connector is too large: Size exceeds the limit 5"
        );
    }

    #[tokio::test]
    async fn test_success() {
        let json = serde_json::json!(
            [
                {"name": "Alice"},
                {"name": "Bob"},
                {"name": "Charlie"}
            ]
        );
        let mut server = mockito::Server::new_async().await;
        let test_api = server
            .mock("GET", "/test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(serde_json::to_vec(&json).unwrap())
            .create();
        let response = reqwest::get(server.url() + "/test").await.unwrap();
        test_api.assert();
        let res = super::handle_response_with_size_limit::<serde_json::Value>(response, 100)
            .await
            .unwrap();
        assert_eq!(json, res);
    }

    #[tokio::test]
    async fn test_success_by_chunks() {
        let json = serde_json::json!(
            [
                {"name": "Alice"},
                {"name": "Bob"},
                {"name": "Charlie"}
            ]
        );
        let mut server = mockito::Server::new_async().await;
        let test_api = server
            .mock("GET", "/test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(serde_json::to_vec(&json).unwrap())
            .create();
        let response = reqwest::get(server.url() + "/test").await.unwrap();
        test_api.assert();
        let res =
            super::handle_response_by_chunks_with_size_limit::<serde_json::Value>(response, 100)
                .await
                .unwrap();
        assert_eq!(json, res);
    }
}
