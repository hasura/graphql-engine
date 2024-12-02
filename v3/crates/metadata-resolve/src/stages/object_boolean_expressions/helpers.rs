use crate::stages::{data_connector_scalar_types, scalar_boolean_expressions};

use crate::types::subgraph::{
    mk_qualified_type_name, Qualified, QualifiedBaseType, QualifiedTypeReference,
};

use ndc_models;

use open_dds::data_connector::{DataConnectorName, DataConnectorScalarType};
use open_dds::identifier::SubgraphName;

// helper function to resolve ndc types to dds type based on scalar type representations
// this should only be used when we know the underlying type must be a scalar and not an object
pub fn resolve_ndc_type(
    data_connector: &Qualified<DataConnectorName>,
    source_type: &ndc_models::Type,
    scalars: &data_connector_scalar_types::DataConnectorScalars,
    subgraph: &SubgraphName,
) -> Result<QualifiedTypeReference, scalar_boolean_expressions::ScalarBooleanExpressionTypeError> {
    match source_type {
        ndc_models::Type::Named { name } => {
            let scalar_type = scalars.by_ndc_type.get(name.as_str()).ok_or(
                scalar_boolean_expressions::ScalarBooleanExpressionTypeError::UnknownScalarTypeInDataConnector {
                    data_connector: data_connector.clone(),
                    scalar_type: DataConnectorScalarType::from(name.as_str()),
                },
            )?;
            scalar_type
                .representation
                .clone()
                .ok_or(scalar_boolean_expressions::ScalarBooleanExpressionTypeError::DataConnectorScalarRepresentationRequired {
                    data_connector: data_connector.clone(),
                    scalar_type: DataConnectorScalarType::from(name.as_str()),
                })
                .map(|ty| QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::Named(mk_qualified_type_name(
                        &ty, subgraph,
                    )),
                    nullable: false,
                })
        }
        ndc_models::Type::Nullable { underlying_type } => {
            resolve_ndc_type(data_connector, underlying_type, scalars, subgraph).map(|ty| {
                QualifiedTypeReference {
                    underlying_type: ty.underlying_type,
                    nullable: true,
                }
            })
        }
        ndc_models::Type::Array { element_type } => {
            resolve_ndc_type(data_connector, element_type, scalars, subgraph).map(|ty| {
                QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::List(Box::new(ty)),
                    nullable: false,
                }
            })
        }
        ndc_models::Type::Predicate { .. } => Err(
            scalar_boolean_expressions::ScalarBooleanExpressionTypeError::PredicateTypesUnsupported,
        ),
    }
}
