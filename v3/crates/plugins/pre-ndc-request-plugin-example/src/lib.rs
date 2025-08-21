use axum::{
    body::Body,
    extract::Request,
    middleware,
    routing::{get, post},
    Router,
};
use pre_ndc_request_plugin::execute::PreNdcRequestPluginRequestBody;
use serde_json::{json, Value};
use tracing::info;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCQuery {
    V1(ndc_models_v01::QueryRequest),
    V2(ndc_models::QueryRequest),
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
    body: axum::Json<PreNdcRequestPluginRequestBody<NDCRequest>>,
) -> Result<(axum::http::StatusCode, axum::Json<Value>), axum::http::StatusCode> {
    let body = body.0;
    info!(
        operation = format!("{:?}", body.operation_type),
        ndc_version = %body.ndc_version,
        connector = %body.data_connector_name,
        "pre-ndc-request plugin invoked"
    );

    // if there is a request, return it untouched
    if let Some(ndc_request) = body.ndc_request {
        let response = json!({ "ndcRequest": ndc_request });
        Ok((axum::http::StatusCode::OK, axum::Json(response)))
    } else {
        Err(axum::http::StatusCode::NO_CONTENT)
    }
}

pub fn router() -> Router {
    Router::new()
        .route("/health", get(|| async { "OK" }))
        .route("/", post(handle))
        .layer(middleware::from_fn(log_request))
}

// This function logs incoming requests
async fn log_request(req: Request<Body>, next: axum::middleware::Next) -> axum::response::Response {
    // Log the request details
    info!("Incoming request: {} {}", req.method(), req.uri());

    // Continue to the next middleware or handler
    next.run(req).await
}
