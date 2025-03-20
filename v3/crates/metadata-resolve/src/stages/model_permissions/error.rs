use crate::types::error::ContextualError;
use crate::types::subgraph::Qualified;
use crate::{helpers::typecheck, types::error::TypePredicateError};
use error_context::Context;
use jsonpath::JSONPath;
use open_dds::{models::ModelName, permissions::Role, query::ArgumentName, spanned::Spanned};

#[derive(Debug, thiserror::Error)]
#[error("Error in model permission for model '{model_name}' for role '{role}': {error}")]
pub struct NamedModelPermissionError {
    pub model_name: Qualified<ModelName>,
    pub role: Spanned<Role>,
    pub error: ModelPermissionError,
}

#[derive(Debug, thiserror::Error)]
pub enum ModelPermissionError {
    #[error("preset argument '{argument_name}' is defined more than once")]
    DuplicateModelArgumentPreset {
        argument_name: Spanned<ArgumentName>,
    },

    #[error("model source is required for model '{model_name:}' to resolve predicate")]
    ModelSourceRequiredForPredicate {
        model_name: Spanned<Qualified<ModelName>>,
    },

    #[error("preset argument '{argument_name}' value has a type error: {type_error}")]
    ModelArgumentValuePresetTypeError {
        argument_name: Spanned<ArgumentName>,
        value_path: JSONPath,
        type_error: typecheck::TypecheckError,
    },

    #[error("a preset argument '{argument_name}' has been set for the model '{model_name}' but no such argument exists for this model")]
    ModelArgumentPresetArgumentNotFound {
        model_name: Spanned<Qualified<ModelName>>,
        argument_name: Spanned<ArgumentName>,
    },

    #[error("in select filter permissions: {error}")]
    SelectFilterPermissionTypePredicateError { error: TypePredicateError },
}

impl ContextualError for NamedModelPermissionError {
    fn create_error_context(&self) -> Option<Context> {
        match &self.error {
            ModelPermissionError::DuplicateModelArgumentPreset { argument_name } => {
                Some(Context(vec![
                    error_context::Step {
                        message: "This argument preset is a duplicate".to_owned(),
                        path: argument_name.path.clone(),
                        subgraph: Some(self.model_name.subgraph.clone()),
                    },
                    error_context::Step {
                        message: "The duplicate is defined in argument presets for this role"
                            .to_owned(),
                        path: self.role.path.clone(),
                        subgraph: Some(self.model_name.subgraph.clone()),
                    },
                ]))
            }

            ModelPermissionError::ModelSourceRequiredForPredicate { model_name } => {
                Some(Context(vec![error_context::Step {
                    message: "No source is defined for this model".to_owned(),
                    path: model_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                }]))
            }
            ModelPermissionError::ModelArgumentValuePresetTypeError {
                argument_name,
                value_path,
                type_error: _,
            } => Some(Context(vec![
                error_context::Step {
                    message: "This argument preset value has a type error".to_owned(),
                    path: value_path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                },
                error_context::Step {
                    message: "This argument preset's value has a type error".to_owned(),
                    path: argument_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                },
            ])),
            ModelPermissionError::ModelArgumentPresetArgumentNotFound {
                model_name,
                argument_name,
            } => Some(Context(vec![
                error_context::Step {
                    message: "This argument preset references an argument that does not exist"
                        .to_owned(),
                    path: argument_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                },
                error_context::Step {
                    message: "Arguments are defined on this model".to_owned(),
                    path: model_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                },
            ])),
            ModelPermissionError::SelectFilterPermissionTypePredicateError { error } => {
                error.create_error_context()
            }
        }
    }
}
