use metadata_resolve::{ModelWithArgumentPresets, Qualified};
use open_dds::{identifier::SubgraphName, models::ModelName, types::CustomTypeName};
use std::collections::HashMap;
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Debug)]
pub struct State {
    pub routes: HashMap<String, ModelWithArgumentPresets>,
}

impl State {
    pub fn new(metadata: &metadata_resolve::Metadata) -> Self {
        let routes = metadata
            .models
            .iter()
            // TODO: remove model.clone()
            .map(|(model_name, model)| {
                (
                    format!("/{}/{}", model_name.subgraph, model_name.name),
                    model.clone(),
                )
            })
            .collect::<HashMap<_, _>>();
        Self { routes }
    }
}

#[derive(Debug, derive_more::Display)]
pub enum RequestError {
    NotFound,
    BadRequest(String),
    InternalError(InternalError),
    PlanError(plan::PlanError),
    ExecuteError(execute::FieldError),
}

#[derive(Debug, derive_more::Display)]
pub enum InternalError {
    EmptyQuerySet,
}

impl TraceableError for RequestError {
    fn visibility(&self) -> ErrorVisibility {
        ErrorVisibility::User
    }
}

/// Model related info derived from URI path
#[allow(dead_code)]
pub struct ModelInfo {
    pub subgraph: SubgraphName,
    pub name: ModelName,
    /// value of the unique identifier of the model.
    // TODO: this won't be a string always
    pub unique_identifier: Option<String>,
    /// path to a relationship; like `["artist", "albums", "tracks"]`
    pub relationship: Vec<String>,
}

pub struct ParseError(String);

impl From<&str> for ParseError {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl From<ParseError> for RequestError {
    fn from(value: ParseError) -> Self {
        Self::BadRequest(format!("Parse Error: {}", value.0))
    }
}

// this is not the correct output type, we should be outputting a JSONAPI document instead
pub struct QueryResult {
    pub type_name: Qualified<CustomTypeName>,
    pub rowsets: Vec<ndc_models::RowSet>,
}
