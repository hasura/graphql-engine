use crate::stages::data_connector_scalar_types;
use crate::types::error::Error;
use ref_cast::RefCast;

use crate::types::subgraph::{
    mk_qualified_type_name, Qualified, QualifiedBaseType, QualifiedTypeReference,
};

use ndc_models;

use open_dds::data_connector::{DataConnectorName, DataConnectorScalarType};

// helper function to resolve ndc types to dds type based on scalar type representations
pub(crate) fn resolve_ndc_type(
    data_connector: &Qualified<DataConnectorName>,
    source_type: &ndc_models::Type,
    scalars: &data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    subgraph: &str,
) -> Result<QualifiedTypeReference, Error> {
    match source_type {
        ndc_models::Type::Named { name } => {
            let scalar_type_name = DataConnectorScalarType::ref_cast(name);
            let scalar_type =
                scalars
                    .0
                    .get(scalar_type_name)
                    .ok_or(Error::UnknownScalarTypeInDataConnector {
                        data_connector: data_connector.clone(),
                        scalar_type: scalar_type_name.clone(),
                    })?;
            scalar_type
                .representation
                .clone()
                .ok_or(Error::DataConnectorScalarRepresentationRequired {
                    data_connector: data_connector.clone(),
                    scalar_type: name.clone(),
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
        ndc_models::Type::Predicate { .. } => Err(Error::PredicateTypesUnsupported),
    }
}
