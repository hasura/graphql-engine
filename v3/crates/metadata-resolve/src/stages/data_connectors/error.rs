use crate::helpers::ndc_validation::NDCValidationError;
use crate::types::subgraph::Qualified;
use open_dds::data_connector::DataConnectorName;

#[derive(Debug, thiserror::Error)]
pub struct NamedDataConnectorError {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub error: DataConnectorError,
}

impl std::fmt::Display for NamedDataConnectorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "The data connector {} has an error: {}",
            self.data_connector_name, self.error
        )
    }
}

#[derive(Debug, thiserror::Error)]
pub enum DataConnectorError {
    #[error("The data connector uses ndc-spec v0.2.* and is not yet supported")]
    NdcV02DataConnectorNotSupported,
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
    #[error("The version specified in the capabilities (\"{version}\") is not compatible with the schema version specified. The version requirement is {requirement}")]
    IncompatibleNdcVersion {
        version: String,
        requirement: semver::VersionReq,
    },
}
