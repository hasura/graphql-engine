use serde::Serialize;
use serde_json;

use ndc_models as ndc_models_v02;
use ndc_models_v01;

use super::migration;

#[derive(Serialize, Debug, Clone, PartialEq)]
#[serde(tag = "version")]
pub enum NdcQueryRequest {
    #[serde(rename = "v0.1.x")]
    V01(Box<ndc_models_v01::QueryRequest>),
    #[serde(rename = "v0.2.x")]
    V02(Box<ndc_models_v02::QueryRequest>),
}

#[derive(Serialize, Debug, Clone, PartialEq)]
#[serde(tag = "version")]
pub enum NdcQueryResponse {
    #[serde(rename = "v0.1.x")]
    V01(ndc_models_v01::QueryResponse),
    #[serde(rename = "v0.2.x")]
    V02(ndc_models_v02::QueryResponse),
}

impl NdcQueryResponse {
    pub fn as_latest_rowsets(self) -> Vec<ndc_models_v02::RowSet> {
        match self {
            NdcQueryResponse::V01(response) => response
                .0
                .into_iter()
                .map(migration::v01::upgrade_rowset_to_v02)
                .collect(),
            NdcQueryResponse::V02(response) => response.0,
        }
    }
}

#[derive(Serialize, Debug, Clone, PartialEq)]
#[serde(tag = "version")]
pub enum NdcExplainResponse {
    #[serde(rename = "v0.1.x")]
    V01(ndc_models_v01::ExplainResponse),
    #[serde(rename = "v0.2.x")]
    V02(ndc_models_v02::ExplainResponse),
}

#[derive(Serialize, Debug, Clone, PartialEq)]
#[serde(tag = "version")]
pub enum NdcMutationRequest {
    #[serde(rename = "v0.1.x")]
    V01(ndc_models_v01::MutationRequest),
    #[serde(rename = "v0.2.x")]
    V02(ndc_models_v02::MutationRequest),
}

#[derive(Serialize, Debug, Clone, PartialEq)]
#[serde(tag = "version")]
pub enum NdcMutationResponse {
    #[serde(rename = "v0.1.x")]
    V01(ndc_models_v01::MutationResponse),
    #[serde(rename = "v0.2.x")]
    V02(ndc_models_v02::MutationResponse),
}

impl NdcMutationResponse {
    pub fn as_latest(self) -> ndc_models_v02::MutationResponse {
        match self {
            NdcMutationResponse::V01(response) => {
                migration::v01::upgrade_mutation_response_to_v02(response)
            }
            NdcMutationResponse::V02(response) => response,
        }
    }
}

#[derive(Serialize, Debug, Clone, PartialEq)]
#[serde(tag = "version")]
pub enum NdcErrorResponse {
    #[serde(rename = "v0.1.x")]
    V01(ndc_models_v01::ErrorResponse),
    #[serde(rename = "v0.2.x")]
    V02(ndc_models_v02::ErrorResponse),
}

impl NdcErrorResponse {
    pub fn message(&self) -> &str {
        match self {
            NdcErrorResponse::V01(err) => err.message.as_str(),
            NdcErrorResponse::V02(err) => err.message.as_str(),
        }
    }

    pub fn details(&self) -> &serde_json::Value {
        match self {
            NdcErrorResponse::V01(err) => &err.details,
            NdcErrorResponse::V02(err) => &err.details,
        }
    }
}
