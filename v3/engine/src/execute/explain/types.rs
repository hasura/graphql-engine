use crate::execute::error;
use crate::execute::GraphQLErrors;
use lang_graphql::http::GraphQLError;
use nonempty::NonEmpty;
use serde::Serialize;
use serde_json::json;
use strum_macros::Display;
use tracing_util::Traceable;

#[derive(Debug, Display)]
pub enum RequestMode {
    Explain,
    Execute,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ExplainResponse {
    explain: Option<Step>,
    #[serde(skip_serializing_if = "Option::is_none")]
    errors: Option<nonempty::NonEmpty<GraphQLError>>,
}

impl Traceable for ExplainResponse {
    type ErrorType<'a> = GraphQLErrors<'a>;

    fn get_error(&self) -> Option<GraphQLErrors<'_>> {
        self.errors.as_ref().map(GraphQLErrors)
    }
}

impl ExplainResponse {
    pub fn error(error: GraphQLError) -> Self {
        Self {
            explain: None,
            errors: Some(nonempty::nonempty![error]),
        }
    }

    pub fn does_contain_error(&self) -> bool {
        self.errors.is_some()
    }
}

impl axum::response::IntoResponse for ExplainResponse {
    fn into_response(self) -> axum::response::Response {
        let response = match &self.errors {
            None => {
                json!({ "explain": self.explain })
            }
            Some(errors) => {
                json!({ "explain": self.explain, "errors": errors })
            }
        };
        axum::Json(response).into_response()
    }
}

#[derive(Serialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "value")]
pub(crate) enum Step {
    ModelSelect(ModelSelectIR),
    CommandSelect(CommandSelectIR),
    ForEach(ForEachStep),
    HashJoin,
    Sequence(NonEmpty<Box<Step>>),
    Parallel(NonEmpty<Box<Step>>),
}

impl Step {
    pub(crate) fn make_explain_response(self) -> ExplainResponse {
        ExplainResponse {
            explain: Some(self),
            errors: None,
        }
    }
}

#[derive(Serialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ModelSelectIR {
    pub(crate) model_name: String,
    pub(crate) query_request: ndc_client::models::QueryRequest,
    pub(crate) ndc_explain: NDCExplainResponse,
}

#[derive(Serialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CommandSelectIR {
    pub(crate) command_name: String,
    pub(crate) query_request: ndc_client::models::QueryRequest,
    pub(crate) ndc_explain: NDCExplainResponse,
}

#[derive(Serialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "value")]
pub(crate) enum ForEachStep {
    ModelSelect(ModelSelectIR),
    CommandSelect(CommandSelectIR),
}

#[derive(Serialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "value")]
pub(crate) enum NDCExplainResponse {
    NotSupported,
    Response(ndc_client::models::ExplainResponse),
    Error(GraphQLError),
}

impl NDCExplainResponse {
    pub(crate) fn error(error: error::Error) -> Self {
        Self::Error(error.to_graphql_error(None))
    }
    pub(crate) fn success(response: ndc_client::models::ExplainResponse) -> Self {
        Self::Response(response)
    }
    pub(crate) fn not_supported() -> Self {
        Self::NotSupported
    }
}
