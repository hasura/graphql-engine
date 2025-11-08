use axum::{
    body::Body,
    extract::Request,
    middleware,
    routing::{get, post},
    Router,
};
use pre_ndc_response_plugin::execute::PreNdcResponsePluginRequestBody;
use serde_json::Value;
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
    // Relational (v0.2) request
    Relational(Box<ndc_models::RelationalQuery>),
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
    // Relational (v0.2) response
    Relational(ndc_models::RelationalQueryResponse),
}

// handle a plugin request using the typed request body
pub async fn handle(
    body: axum::Json<PreNdcResponsePluginRequestBody<NDCRequest, NDCResponse>>,
) -> Result<(axum::http::StatusCode, axum::Json<Value>), axum::http::StatusCode> {
    let body = body.0;

    info!(
        operation = format!("{:?}", body.operation_type),
        connector = %body.data_connector_name,
        ndc_version = %body.ndc_version,
        "pre-ndc-response plugin invoked"
    );

    // Should we trim? Look for a test-only session flag in session.variables
    let should_trim = body
        .session
        .as_ref()
        .and_then(|s| serde_json::to_value(s).ok())
        .and_then(|s_json| s_json.get("variables").cloned())
        .and_then(|vars| vars.get("x-hasura-response-trim").cloned())
        .and_then(|val| {
            val.as_bool().or_else(|| {
                val.as_str()
                    .map(|s| s == "1" || s.eq_ignore_ascii_case("true"))
            })
        })
        .unwrap_or(false);

    // Echo or trim the ndc_response if provided
    if let Some(ndc_response_typed) = body.ndc_response {
        // Convert to raw JSON so we can handle both Query (array-of-rowsets) and Relational shapes uniformly
        let mut ndc_response = serde_json::to_value(ndc_response_typed)
            .map_err(|_| axum::http::StatusCode::NO_CONTENT)?;

        if should_trim {
            match &mut ndc_response {
                // Relational (v0.2) response: { rows: [ { ... }, ... ] }
                Value::Object(map) => {
                    if let Some(Value::Array(rows)) = map.get_mut("rows") {
                        // Detect Query (v0.1/v0.2) RowSet shape: rows = [ { rows: [...] } ]
                        let is_rowset = rows.first().and_then(|v| v.get("rows")).is_some();
                        if is_rowset {
                            if let Some(Value::Object(first_rowset)) = rows.first_mut() {
                                if let Some(Value::Array(inner_rows)) = first_rowset.get_mut("rows")
                                {
                                    if inner_rows.len() > 1 {
                                        inner_rows.truncate(1);
                                    }
                                }
                            }
                        } else if rows.len() > 1 {
                            rows.truncate(1);
                        }
                    }
                }
                // Standard NDC Query response (v0.1/v0.2): [ { rows: [...] }, ... ]
                Value::Array(rowsets) => {
                    if let Some(Value::Object(first_rowset)) = rowsets.first_mut() {
                        if let Some(Value::Array(inner_rows)) = first_rowset.get_mut("rows") {
                            if inner_rows.len() > 1 {
                                inner_rows.truncate(1);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok((axum::http::StatusCode::OK, axum::Json(ndc_response)))
    } else {
        // no response to operate on; skip
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
