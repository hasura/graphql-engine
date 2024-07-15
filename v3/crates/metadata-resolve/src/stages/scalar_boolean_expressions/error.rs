use crate::Qualified;
use open_dds::data_connector::{DataConnectorName, DataConnectorScalarType};

#[derive(Debug, thiserror::Error)]
pub enum ScalarBooleanExpressionTypeError {
    #[error("unknown data connector {data_connector:} referenced in scalar type representation of {scalar_type:}")]
    ScalarTypeFromUnknownDataConnector {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },
    #[error("cannot find scalar type {scalar_type:} in data connector {data_connector:}")]
    UnknownScalarTypeInDataConnector {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },
}
