use axum::{
    body::Body,
    extract::Request,
    middleware,
    routing::{get, post},
    Router,
};
use pre_ndc_response_plugin::execute::PreNdcResponsePluginRequestBody;
use serde_json::{json, Value};
use tracing::info;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCQueryReq {
    V1(Box<ndc_models_v01::QueryRequest>),
    V2(Box<ndc_models::QueryRequest>),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCMutationReq {
    V1(ndc_models_v01::MutationRequest),
    V2(ndc_models::MutationRequest),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCRequest {
    Query(Box<NDCQueryReq>),
    Mutation(Box<NDCMutationReq>),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCQueryRes {
    V1(ndc_models_v01::QueryResponse),
    V2(ndc_models::QueryResponse),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCMutationRes {
    V1(ndc_models_v01::MutationResponse),
    V2(ndc_models::MutationResponse),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCExplainRes {
    V1(ndc_models_v01::ExplainResponse),
    V2(ndc_models::ExplainResponse),
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum NDCResponse {
    Query(NDCQueryRes),
    Mutation(NDCMutationRes),
    Explain(NDCExplainRes),
}

// handle a plugin request and echo back the ndcResponse unchanged if provided
pub async fn handle(
    body: axum::Json<PreNdcResponsePluginRequestBody<NDCRequest, NDCResponse>>,
) -> Result<(axum::http::StatusCode, axum::Json<Value>), axum::http::StatusCode> {
    let body = body.0;
    info!(
        operation = format!("{:?}", body.operation_type),
        ndc_version = %body.ndc_version,
        connector = %body.data_connector_name,
        "pre-ndc-response plugin invoked"
    );

    if let Some(ndc_response) = body.ndc_response {
        // Return the raw NDC response as the body
        Ok((axum::http::StatusCode::OK, axum::Json(json!(ndc_response))))
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
