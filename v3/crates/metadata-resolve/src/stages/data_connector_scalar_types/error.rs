use open_dds::types::CustomTypeName;

use open_dds::data_connector::{DataConnectorName, DataConnectorScalarType};

use crate::types::error::ContextualError;
use crate::types::subgraph::Qualified;

use crate::stages::{graphql_config, scalar_boolean_expressions};

impl ContextualError for DataConnectorScalarTypesError {
    fn create_error_context(&self) -> Option<error_context::Context> {
        None
    }
}

#[derive(Debug, thiserror::Error)]
pub enum DataConnectorScalarTypesError {
    #[error(
        "multiple type representations defined for scalar {scalar_type:} from data connector {data_connector:}"
    )]
    DuplicateDataConnectorScalarRepresentation {
        data_connector: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },
    #[error("unknown type represented for scalar type {scalar_type:}: {type_name:}")]
    ScalarTypeUnknownRepresentation {
        scalar_type: DataConnectorScalarType,
        type_name: Qualified<CustomTypeName>,
    },
    #[error("{0}")]
    GraphqlConfigError(#[from] graphql_config::GraphqlConfigError),
    #[error("{0}")]
    ScalarBooleanExpressionTypeError(
        #[from] scalar_boolean_expressions::ScalarBooleanExpressionTypeError,
    ),
}
