use crate::helpers::ndc_validation::NDCValidationError;
use crate::types::error::{ContextualError, ShouldBeAnError};
use crate::types::subgraph::Qualified;
use open_dds::data_connector::DataConnectorName;
use open_dds::flags;

impl ContextualError for NamedDataConnectorError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        None
    }
}

#[derive(Debug, thiserror::Error)]
#[error("The data connector {data_connector_name} has an error: {error}")]
pub struct NamedDataConnectorError {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub error: DataConnectorError,
}

#[derive(Debug, thiserror::Error)]
pub enum DataConnectorError {
    #[error("the data connector is defined more than once")]
    DuplicateDataConnectorDefinition,
    #[error("The url for the data connector is invalid: {error}")]
    InvalidDataConnectorUrl { error: url::ParseError },
    #[error("Invalid header name {header_name} specified")]
    InvalidHeaderName { header_name: String },
    #[error("Invalid value specified for header {header_name}")]
    InvalidHeaderValue { header_name: String },
    #[error("{0}")]
    NdcValidationError(NDCValidationError),
    #[error(
        "The version specified in the capabilities (\"{version}\") is an invalid version: {error}"
    )]
    InvalidNdcVersion {
        version: String,
        error: semver::Error,
    },
    #[error(
        "The version specified in the capabilities (\"{version}\") is not compatible with the schema version specified. The version requirement is {requirement}"
    )]
    IncompatibleNdcVersion {
        version: String,
        requirement: semver::VersionReq,
    },
}

#[derive(Debug, thiserror::Error)]
#[error("The data connector {data_connector_name} has an issue: {issue}")]
pub struct NamedDataConnectorIssue {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub issue: DataConnectorIssue,
}

impl ShouldBeAnError for NamedDataConnectorIssue {
    fn should_be_an_error(&self, flags: &flags::OpenDdFlags) -> bool {
        self.issue.should_be_an_error(flags)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum DataConnectorIssue {
    #[error(
        "The version specified in the capabilities (\"{version}\") is an invalid version: {error}. Consider upgrading the data connector to the latest version to fix this."
    )]
    InvalidNdcV01Version {
        version: String,
        error: semver::Error,
    },
}
impl ShouldBeAnError for DataConnectorIssue {
    fn should_be_an_error(&self, flags: &flags::OpenDdFlags) -> bool {
        match self {
            DataConnectorIssue::InvalidNdcV01Version { .. } => {
                flags.contains(flags::Flag::RequireValidNdcV01Version)
            }
        }
    }
}
