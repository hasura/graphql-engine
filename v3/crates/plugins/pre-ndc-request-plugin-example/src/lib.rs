use axum::{
    body::Body,
    extract::Request,
    middleware,
    routing::{get, post},
    Router,
};
use open_dds::session_variables::SessionVariableName;
use pre_ndc_request_plugin::execute::PreNdcRequestPluginRequestBody;
use serde_json::{json, Value};
use std::str::FromStr;
use tracing::info;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCQuery {
    V1(Box<ndc_models_v01::QueryRequest>),
    V2(Box<ndc_models::QueryRequest>),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCMutation {
    V1(ndc_models_v01::MutationRequest),
    V2(ndc_models::MutationRequest),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCRequest {
    Query(NDCQuery),
    Mutation(NDCMutation),
}

// handle a query request and do nothing with it
// later we'll do something more interesting
pub async fn handle(
    headers: axum::http::header::HeaderMap,
    body: axum::Json<PreNdcRequestPluginRequestBody<NDCRequest>>,
) -> Result<(axum::http::StatusCode, axum::Json<Value>), axum::http::StatusCode> {
    let body = body.0;
    info!(
        operation = format!("{:?}", body.operation_type),
        ndc_version = %body.ndc_version,
        connector = %body.data_connector_name,
        "pre-ndc-request plugin invoked"
    );

    // if there is a limit defined in session variables or headers, use them
    let overwrite_limit =
        get_limit_from_session_variables(&body).or(get_limit_from_headers(headers));

    if let Some(ndc_request) = body.ndc_request {
        let response = match overwrite_limit {
            Some(limit) => {
                // set the limit in the query
                let ndc_request = set_limit_in_query(ndc_request, limit);
                json!({ "ndcRequest": ndc_request })
            }
            None => {
                // if there is a request, return it untouched
                json!({ "ndcRequest": ndc_request })
            }
        };
        Ok((axum::http::StatusCode::OK, axum::Json(response)))
    } else {
        Err(axum::http::StatusCode::NO_CONTENT)
    }
}

fn set_limit_in_query(body: NDCRequest, limit: u64) -> NDCRequest {
    match body {
        NDCRequest::Query(NDCQuery::V1(query)) => {
            NDCRequest::Query(NDCQuery::V1(Box::new(ndc_models_v01::QueryRequest {
                query: ndc_models_v01::Query {
                    limit: Some(limit as u32),
                    ..query.query
                },
                ..*query
            })))
        }
        NDCRequest::Query(NDCQuery::V2(query)) => {
            NDCRequest::Query(NDCQuery::V2(Box::new(ndc_models::QueryRequest {
                query: ndc_models::Query {
                    limit: Some(limit as u32),
                    ..query.query
                },
                ..*query
            })))
        }
        NDCRequest::Mutation(mutation) => NDCRequest::Mutation(mutation.clone()),
    }
}

pub fn router() -> Router {
    Router::new()
        .route("/health", get(|| async { "OK" }))
        .route("/", post(handle))
        .layer(middleware::from_fn(log_request))
}

// if the session variables contain a `x-hasura-limit` variable, use it as the limit
fn get_limit_from_session_variables(
    body: &PreNdcRequestPluginRequestBody<NDCRequest>,
) -> Option<u64> {
    if let Some(session) = &body.session {
        if let Some(value) = session
            .variables
            .get(&SessionVariableName::from_str("x-hasura-limit").unwrap())
        {
            // use the limit
            if let Some(limit) = value.as_u64() {
                return Some(limit);
            }
        }
    }
    None
}

// if the headers contain a `x-user-custom-limit` variable, use it as the limit
fn get_limit_from_headers(headers: axum::http::header::HeaderMap) -> Option<u64> {
    if let Some(value) = headers.get("x-user-custom-limit") {
        // use the limit
        if let Some(limit) = value.to_str().ok().and_then(|v| v.parse().ok()) {
            return Some(limit);
        }
    }

    None
}

// This function logs incoming requests
async fn log_request(req: Request<Body>, next: axum::middleware::Next) -> axum::response::Response {
    // Log the request details
    info!("Incoming request: {} {}", req.method(), req.uri());

    // Continue to the next middleware or handler
    next.run(req).await
}
