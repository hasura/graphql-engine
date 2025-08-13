use crate::stages::models;
use crate::types::error::ContextualError;
use crate::types::subgraph::Qualified;
use crate::{helpers::typecheck, types::error::TypePredicateError};
use error_context::{Context, Step};
use jsonpath::JSONPath;
use open_dds::data_connector::DataConnectorName;
use open_dds::{
    models::ModelName, permissions::Role, query::ArgumentName, spanned::Spanned,
    types::CustomTypeName,
};

#[derive(Debug, thiserror::Error)]
#[error("Error in model permission for model '{model_name}'{}: {error}",
        match role { Some(role) => format!(" for role '{role}'"), None => String::new()}
)]
pub struct NamedModelPermissionError {
    pub model_name: Qualified<ModelName>,
    pub role: Option<Spanned<Role>>,
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

    #[error(
        "a preset argument '{argument_name}' has been set for the model '{model_name}' but no such argument exists for this model"
    )]
    ModelArgumentPresetArgumentNotFound {
        model_name: Spanned<Qualified<ModelName>>,
        argument_name: Spanned<ArgumentName>,
    },

    #[error("in select filter permissions: {error}")]
    SelectFilterPermissionTypePredicateError { error: TypePredicateError },

    #[error("unknown type {custom_type_name}")]
    UnknownType {
        custom_type_name: Qualified<CustomTypeName>,
    },

    #[error("model source is required to resolve relational permissions")]
    ModelSourceRequiredForRelationalPermissions,
    #[error("unknown collection {collection} in data connector {data_connector}")]
    UnknownModelCollection {
        data_connector: Qualified<DataConnectorName>,
        collection: open_dds::data_connector::CollectionName,
    },
    #[error("relational insert is not supported for this model")]
    RelationalInsertNotSupported,
    #[error("relational update is not supported for this model")]
    RelationalUpdateNotSupported,
    #[error("relational delete is not supported for this model")]
    RelationalDeleteNotSupported,

    #[error("{0}")]
    ModelsError(#[from] models::ModelsError),
}

impl ContextualError for NamedModelPermissionError {
    fn create_error_context(&self) -> Option<Context> {
        match &self.error {
            ModelPermissionError::DuplicateModelArgumentPreset { argument_name } => {
                let root_error = Context::from_step(Step {
                    message: "This argument preset is a duplicate".to_owned(),
                    path: argument_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                });

                Some(match &self.role {
                    Some(role) => root_error.append(Step {
                        message: "The duplicate is defined in argument presets for this role"
                            .to_owned(),
                        path: role.path.clone(),
                        subgraph: Some(self.model_name.subgraph.clone()),
                    }),
                    None => root_error,
                })
            }

            ModelPermissionError::ModelSourceRequiredForPredicate { model_name } => {
                Some(Context::from_step(Step {
                    message: "No source is defined for this model".to_owned(),
                    path: model_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                }))
            }
            ModelPermissionError::ModelArgumentValuePresetTypeError {
                argument_name,
                value_path,
                type_error: _,
            } => Some(
                Context::from_step(Step {
                    message: "This argument preset value has a type error".to_owned(),
                    path: value_path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                })
                .append(Step {
                    message: "This argument preset's value has a type error".to_owned(),
                    path: argument_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                }),
            ),
            ModelPermissionError::ModelArgumentPresetArgumentNotFound {
                model_name,
                argument_name,
            } => Some(
                Context::from_step(Step {
                    message: "This argument preset references an argument that does not exist"
                        .to_owned(),
                    path: argument_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                })
                .append(Step {
                    message: "Arguments are defined on this model".to_owned(),
                    path: model_name.path.clone(),
                    subgraph: Some(self.model_name.subgraph.clone()),
                }),
            ),
            ModelPermissionError::SelectFilterPermissionTypePredicateError { error } => {
                let root_error = error.create_error_context()?;

                Some(match &self.role {
                    Some(role) => root_error.prepend(Step {
                        message: format!(
                            "Error in model permission for the role '{}' on the model '{}'",
                            role, self.model_name.name
                        ),
                        path: role.path.clone(),
                        subgraph: Some(self.model_name.subgraph.clone()),
                    }),
                    None => root_error,
                })
            }
            ModelPermissionError::UnknownType { .. }
            | ModelPermissionError::ModelSourceRequiredForRelationalPermissions
            | ModelPermissionError::UnknownModelCollection { .. }
            | ModelPermissionError::RelationalInsertNotSupported
            | ModelPermissionError::RelationalUpdateNotSupported
            | ModelPermissionError::RelationalDeleteNotSupported => None,
            ModelPermissionError::ModelsError(error) => error.create_error_context(),
        }
    }
}
