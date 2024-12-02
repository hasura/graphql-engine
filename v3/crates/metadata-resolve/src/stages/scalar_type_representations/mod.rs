use crate::stages::{data_connector_scalar_types, scalar_types};
use crate::Qualified;
use open_dds::{data_connector::DataConnectorName, types::CustomTypeName};
use std::collections::BTreeMap;
mod types;
pub use types::ScalarTypeRepresentation;

/// include data connector type representations for each scalar type
pub fn resolve(
    data_connector_scalar_types: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
) -> BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation> {
    let mut new_scalar_types = BTreeMap::new();
    for (scalar_type_name, scalar_type) in scalar_types {
        let mut representations = BTreeMap::new();
        // grab information about this scalar from each data connector
        for (data_connector_name, data_connector_scalars) in data_connector_scalar_types {
            if let Some(data_connector_representation) = data_connector_scalars
                .by_custom_type_name
                .get(scalar_type_name)
            {
                representations.insert(
                    data_connector_name.clone(),
                    data_connector_representation.clone(),
                );
            }
        }
        new_scalar_types.insert(
            scalar_type_name.clone(),
            ScalarTypeRepresentation {
                graphql_type_name: scalar_type.graphql_type_name.clone(),
                description: scalar_type.description.clone(),
                representations,
            },
        );
    }
    new_scalar_types
}
