pub mod types;
use std::collections::{BTreeMap, BTreeSet};

pub use types::{
    DataConnectorWithScalarsContext, DataConnectorsWithScalars, ScalarTypeWithRepresentationInfo,
};

use lang_graphql::ast::common as ast;

use open_dds::types::{CustomTypeName, TypeName};

use open_dds::data_connector::DataConnectorName;

use crate::helpers::types::mk_name;
use crate::types::error::Error;
use crate::types::subgraph::Qualified;

use crate::stages::{data_connectors, scalar_types};

pub struct DataConnectorWithScalarsOutput<'a> {
    pub data_connectors: DataConnectorsWithScalars<'a>,
    pub graphql_types: BTreeSet<ast::TypeName>,
}

/// resolve data connector scalar representations
pub fn resolve<'a>(
    metadata_accessor: &'a open_dds::accessor::MetadataAccessor,
    data_connectors: &'a data_connectors::DataConnectors,
    scalar_types: &'a BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    existing_graphql_types: &'a BTreeSet<ast::TypeName>,
) -> Result<DataConnectorWithScalarsOutput<'a>, Error> {
    let mut graphql_types = existing_graphql_types.clone();

    // we convert data from the old types to the new types and then start mutating everything
    // there is no doubt room for improvement here, but at least we are keeping the mutation
    // contained to this resolving stage
    let mut data_connectors_with_scalars = convert_data_connectors_contexts(data_connectors);

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: scalar_type_representation,
    } in &metadata_accessor.data_connector_scalar_representations
    {
        let scalar_type_name: &String = &scalar_type_representation.data_connector_scalar_type;

        let qualified_data_connector_name = Qualified::new(
            subgraph.to_string(),
            scalar_type_representation.data_connector_name.to_owned(),
        );

        let data_connector = data_connectors_with_scalars
            .get_mut(&qualified_data_connector_name)
            .ok_or_else(|| Error::ScalarTypeFromUnknownDataConnector {
                scalar_type: scalar_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
            })?;

        let scalar_type = data_connector
            .scalars
            .get_mut(
                scalar_type_representation
                    .data_connector_scalar_type
                    .as_str(),
            )
            .ok_or_else(|| Error::UnknownScalarTypeInDataConnector {
                scalar_type: scalar_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
            })?;

        if scalar_type.representation.is_none() {
            match &scalar_type_representation.representation {
                TypeName::Inbuilt(_) => {} // TODO: Validate Nullable and Array types in Inbuilt
                TypeName::Custom(type_name) => {
                    let qualified_type_name =
                        Qualified::new(subgraph.to_string(), type_name.to_owned());
                    let _representation =
                        scalar_types.get(&qualified_type_name).ok_or_else(|| {
                            Error::ScalarTypeUnknownRepresentation {
                                scalar_type: scalar_type_name.clone(),
                                type_name: qualified_type_name,
                            }
                        })?;
                }
            }
            scalar_type.representation = Some(scalar_type_representation.representation.clone());
        } else {
            return Err(Error::DuplicateDataConnectorScalarRepresentation {
                data_connector: qualified_data_connector_name.clone(),
                scalar_type: scalar_type_name.clone(),
            });
        }
        scalar_type.comparison_expression_name = match scalar_type_representation.graphql.as_ref() {
            None => Ok(None),
            Some(graphql) => match &graphql.comparison_expression_type_name {
                None => Ok(None),
                Some(type_name) => mk_name(type_name.0.as_ref()).map(ast::TypeName).map(Some),
            },
        }?;

        // We are allowing conflicting graphql types for scalar comparison expressions, but we still want the typename
        // to not conflict with other graphql type names
        //
        // TODO: This means that comparison expression names conflicting with already encountered graphql type names
        // will pass through. They'll eventually be caught during schema generation but only if the expression was
        // reachable in the graphql API. Ideally, we should just fail the build here.
        if let Some(new_graphql_type) = &scalar_type.comparison_expression_name {
            graphql_types.insert(new_graphql_type.clone());
        };
    }

    Ok(DataConnectorWithScalarsOutput {
        data_connectors: types::DataConnectorsWithScalars {
            data_connectors_with_scalars,
        },
        graphql_types,
    })
}

// convert from types in previous stage to this stage
fn convert_data_connectors_contexts<'a>(
    data_connectors: &data_connectors::DataConnectors<'a>,
) -> BTreeMap<Qualified<DataConnectorName>, DataConnectorWithScalarsContext<'a>> {
    let mut data_connectors_with_scalars = BTreeMap::new();

    for (data_connector_name, data_connectors::DataConnectorContext { scalars, inner }) in
        &data_connectors.data_connectors
    {
        let mut new_scalars = BTreeMap::new();
        for (scalar_name, scalar) in scalars {
            new_scalars.insert(
                *scalar_name,
                ScalarTypeWithRepresentationInfo {
                    scalar_type: scalar.scalar_type,
                    comparison_expression_name: None,
                    comparison_operators: scalar.comparison_operators.clone(),
                    representation: None,
                },
            );
        }
        data_connectors_with_scalars.insert(
            data_connector_name.clone(),
            DataConnectorWithScalarsContext {
                inner: inner.clone(),
                scalars: new_scalars,
            },
        );
    }
    data_connectors_with_scalars
}

// helper function to determine whether a ndc type is a simple scalar
pub fn get_simple_scalar<'a, 'b>(
    t: ndc_models::Type,
    scalars: &'a BTreeMap<&str, ScalarTypeWithRepresentationInfo<'b>>,
) -> Option<(String, &'a ScalarTypeWithRepresentationInfo<'b>)> {
    match t {
        ndc_models::Type::Named { name } => scalars.get(name.as_str()).map(|info| (name, info)),
        ndc_models::Type::Nullable { underlying_type } => {
            get_simple_scalar(*underlying_type, scalars)
        }
        ndc_models::Type::Array { element_type: _ } => None,
        ndc_models::Type::Predicate {
            object_type_name: _,
        } => None,
    }
}
