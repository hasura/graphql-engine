use std::collections::BTreeMap;

use nonempty::NonEmpty;
use serde::Serialize;

use engine_types::ExposeInternalErrors;
use lang_graphql::http::GraphQLError;
use ndc_models as ndc_models_v02;
use ndc_models_v01;
use tracing_util::Traceable;

use super::super::types::GraphQLErrors;
use execute::ndc;

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
    pub fn does_contain_internal_error(&self) -> bool {
        match &self.errors {
            Some(errors) => errors.iter().any(|e| e.is_internal),
            None => false,
        }
    }
}

impl axum::response::IntoResponse for ExplainResponse {
    fn into_response(self) -> axum::response::Response {
        axum::Json(self).into_response()
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
    pub(crate) ndc_request: NDCRequest,
    pub(crate) ndc_explain: NDCExplainResponse,
}

#[derive(Serialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CommandSelectIR {
    pub(crate) command_name: String,
    pub(crate) ndc_request: NDCRequest,
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
    Response(ndc::NdcExplainResponse),
    Error(GraphQLError),
}

#[derive(Serialize, Debug, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "type", content = "value")]
pub(crate) enum NDCRequest {
    Query(ndc::NdcQueryRequest),
    Mutation(ndc::NdcMutationRequest),
}

impl NDCExplainResponse {
    pub(crate) fn error(
        error: &execute::FieldError,
        expose_internal_errors: ExposeInternalErrors,
    ) -> Self {
        Self::Error(error.to_graphql_error(expose_internal_errors, None))
    }
    pub(crate) fn success(response: ndc::NdcExplainResponse) -> Self {
        Self::Response(response)
    }
    pub(crate) fn not_supported() -> Self {
        Self::NotSupported
    }
}

// Since the ndc_explain field might change between CI runs, we need to redact it
// before comparing the expected and actual results.
//
// This function is used in the tests, and we don't want to expose the fields of
// the structs, so we need to keep it here.
pub fn redact_ndc_explain(explain_response: ExplainResponse) -> ExplainResponse {
    ExplainResponse {
        explain: explain_response.explain.map(redact_step),
        errors: explain_response.errors,
    }
}

fn redact_step(step: Step) -> Step {
    match step {
        Step::ModelSelect(model_select) => Step::ModelSelect(redact_model_select(model_select)),
        Step::CommandSelect(command_select) => {
            Step::CommandSelect(redact_command_select(command_select))
        }
        Step::ForEach(ForEachStep::CommandSelect(command_select)) => Step::ForEach(
            ForEachStep::CommandSelect(redact_command_select(command_select)),
        ),
        Step::ForEach(ForEachStep::ModelSelect(model_select)) => {
            Step::ForEach(ForEachStep::ModelSelect(redact_model_select(model_select)))
        }
        Step::HashJoin => Step::HashJoin,
        Step::Sequence(steps) => {
            Step::Sequence(steps.map(|boxed_step| Box::new(redact_step(*boxed_step))))
        }
        Step::Parallel(steps) => {
            Step::Parallel(steps.map(|boxed_step| Box::new(redact_step(*boxed_step))))
        }
    }
}

fn redact_model_select(model_select: ModelSelectIR) -> ModelSelectIR {
    ModelSelectIR {
        model_name: model_select.model_name,
        ndc_request: model_select.ndc_request,
        ndc_explain: redact_ndc_explain_response(model_select.ndc_explain),
    }
}

fn redact_command_select(command_select: CommandSelectIR) -> CommandSelectIR {
    CommandSelectIR {
        command_name: command_select.command_name,
        ndc_request: command_select.ndc_request,
        ndc_explain: redact_ndc_explain_response(command_select.ndc_explain),
    }
}

fn redact_ndc_explain_response(ndc_explain: NDCExplainResponse) -> NDCExplainResponse {
    match ndc_explain {
        NDCExplainResponse::NotSupported => NDCExplainResponse::NotSupported,
        NDCExplainResponse::Error(error) => NDCExplainResponse::Error(error),
        NDCExplainResponse::Response(response) => {
            let mut redacted_details = BTreeMap::new();
            redacted_details.insert("explain".to_string(), "<redacted>".to_string());
            let ndc_response = match response {
                ndc::NdcExplainResponse::V01(_) => {
                    ndc::NdcExplainResponse::V01(ndc_models_v01::ExplainResponse {
                        details: redacted_details,
                    })
                }
                ndc::NdcExplainResponse::V02(_) => {
                    ndc::NdcExplainResponse::V02(ndc_models_v02::ExplainResponse {
                        details: redacted_details,
                    })
                }
            };
            NDCExplainResponse::Response(ndc_response)
        }
    }
}
