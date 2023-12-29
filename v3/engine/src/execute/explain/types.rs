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
    pub explain: Option<Step>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub errors: Option<nonempty::NonEmpty<GraphQLError>>,
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

#[derive(Serialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "value")]
pub enum Step {
    ModelSelect(ModelSelectIR),
    CommandSelect(CommandSelectIR),
    ForEach(ForEachStep),
    HashJoin,
    Sequence(NonEmpty<Box<Step>>),
    Parallel(NonEmpty<Box<Step>>),
}

impl Step {
    pub fn to_explain_response(self) -> ExplainResponse {
        ExplainResponse {
            explain: Some(self),
            errors: None,
        }
    }
}

#[derive(Serialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ModelSelectIR {
    pub model_name: String,
    pub query_request: ndc_client::models::QueryRequest,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "camelCase")]
pub struct CommandSelectIR {
    pub command_name: String,
    pub query_request: ndc_client::models::QueryRequest,
}

#[derive(Serialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "value")]
pub enum ForEachStep {
    ModelSelect(ModelSelectIR),
    CommandSelect(CommandSelectIR),
}
