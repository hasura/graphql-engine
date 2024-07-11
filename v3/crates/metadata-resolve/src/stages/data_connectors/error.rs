use crate::helpers::ndc_validation::NDCValidationError;
use crate::types::subgraph::Qualified;
use open_dds::data_connector::DataConnectorName;

#[derive(Debug, thiserror::Error)]
pub enum DataConnectorError {
    #[error("The data connector {data_connector} uses ndc-spec v0.2.* and is not yet supported")]
    NdcV02DataConnectorNotSupported {
        data_connector: Qualified<DataConnectorName>,
    },
    #[error("the following data connector is defined more than once: {name:}")]
    DuplicateDataConnectorDefinition { name: Qualified<DataConnectorName> },
    #[error("The url for the data connector {data_connector_name:} is invalid: {error:}")]
    InvalidDataConnectorUrl {
        data_connector_name: Qualified<DataConnectorName>,
        error: url::ParseError,
    },
    #[error("Invalid header name {header_name} specified for data connector: {data_connector}.")]
    InvalidHeaderName {
        data_connector: Qualified<DataConnectorName>,
        header_name: String,
    },
    #[error(
        "Invalid value specified for header {header_name} for data connector: {data_connector}."
    )]
    InvalidHeaderValue {
        data_connector: Qualified<DataConnectorName>,
        header_name: String,
    },
    #[error("{0}")]
    NdcValidationError(NDCValidationError),
}
