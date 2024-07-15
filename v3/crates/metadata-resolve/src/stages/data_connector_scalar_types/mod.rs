pub mod types;
use std::collections::{BTreeMap, BTreeSet};
pub use types::{
    ComparisonOperators, ScalarTypeWithRepresentationInfo, ScalarTypeWithRepresentationInfoMap,
};

use lang_graphql::ast::common as ast;

use open_dds::types::{CustomTypeName, TypeName};

use open_dds::data_connector::{DataConnectorName, DataConnectorScalarType};

use crate::helpers::types::mk_name;
use crate::types::error::Error;
use crate::types::subgraph::Qualified;

use crate::stages::{data_connectors, scalar_boolean_expressions, scalar_types};

pub struct DataConnectorWithScalarsOutput<'a> {
    pub data_connector_scalars:
        BTreeMap<Qualified<DataConnectorName>, ScalarTypeWithRepresentationInfoMap<'a>>,
    pub graphql_types: BTreeSet<ast::TypeName>,
}

/// resolve data connector scalar representations
/// also use scalar `BooleanExpressionType`s
pub fn resolve<'a>(
    metadata_accessor: &'a open_dds::accessor::MetadataAccessor,
    data_connectors: &'a data_connectors::DataConnectors,
    scalar_types: &'a BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    scalar_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    existing_graphql_types: &'a BTreeSet<ast::TypeName>,
) -> Result<DataConnectorWithScalarsOutput<'a>, Error> {
    let mut graphql_types = existing_graphql_types.clone();

    // we convert data from the old types to the new types and then start mutating everything
    // there is no doubt room for improvement here, but at least we are keeping the mutation
    // contained to this resolving stage
    let mut data_connector_scalars = convert_data_connectors_contexts(data_connectors);

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: scalar_type_representation,
    } in &metadata_accessor.data_connector_scalar_representations
    {
        let scalar_type_name = &scalar_type_representation.data_connector_scalar_type;

        let qualified_data_connector_name = Qualified::new(
            subgraph.to_string(),
            scalar_type_representation.data_connector_name.clone(),
        );

        let scalars = data_connector_scalars
            .get_mut(&qualified_data_connector_name)
            .ok_or_else(|| scalar_boolean_expressions::ScalarBooleanExpressionTypeError::ScalarTypeFromUnknownDataConnector {
                scalar_type: scalar_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
            })?;

        let scalar_type = scalars
            .0
            .get_mut(&scalar_type_representation.data_connector_scalar_type)
            .ok_or_else(|| scalar_boolean_expressions::ScalarBooleanExpressionTypeError::UnknownScalarTypeInDataConnector {
                scalar_type: scalar_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
            })?;

        if scalar_type.representation.is_none() {
            validate_type_name(
                &scalar_type_representation.representation,
                subgraph,
                scalar_types,
                scalar_type_name,
            )?;

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
                Some(type_name) => mk_name(type_name.as_ref()).map(ast::TypeName).map(Some),
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

    for scalar_boolean_expression in scalar_boolean_expression_types.values() {
        for (data_connector_name, operator_mapping) in
            &scalar_boolean_expression.data_connector_operator_mappings
        {
            let scalar_type_name = &operator_mapping.data_connector_scalar_type;

            let scalars = data_connector_scalars
                .get_mut(data_connector_name)
                .ok_or_else(|| scalar_boolean_expressions::ScalarBooleanExpressionTypeError::ScalarTypeFromUnknownDataConnector {
                    scalar_type: scalar_type_name.clone(),
                    data_connector: data_connector_name.clone(),
                })?;

            let scalar_type = scalars.0.get_mut(scalar_type_name).ok_or_else(|| {
                scalar_boolean_expressions::ScalarBooleanExpressionTypeError::UnknownScalarTypeInDataConnector {
                    scalar_type: scalar_type_name.clone(),
                    data_connector: data_connector_name.clone(),
                }
            })?;

            validate_type_name(
                &scalar_boolean_expression.representation,
                &scalar_boolean_expression.name.subgraph,
                scalar_types,
                scalar_type_name,
            )?;

            // we may have multiple `BooleanExpressionType` for the same type,
            // we allow it but check their OpenDD types don't conflict
            if let Some(existing_representation) = &scalar_type.representation {
                if *existing_representation != scalar_boolean_expression.representation {
                    return Err(Error::DataConnectorScalarRepresentationMismatch {
                        data_connector: data_connector_name.clone(),
                        old_representation: existing_representation.clone(),
                        new_representation: scalar_boolean_expression.representation.clone(),
                    });
                }
            }
            scalar_type.representation = Some(scalar_boolean_expression.representation.clone());
        }
    }

    Ok(DataConnectorWithScalarsOutput {
        data_connector_scalars,
        graphql_types,
    })
}

fn validate_type_name(
    type_name: &TypeName,
    subgraph: &str,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    scalar_type_name: &DataConnectorScalarType,
) -> Result<(), Error> {
    match type_name {
        TypeName::Inbuilt(_) => {} // TODO: Validate Nullable and Array types in Inbuilt
        TypeName::Custom(type_name) => {
            let qualified_type_name = Qualified::new(subgraph.to_string(), type_name.to_owned());
            let _representation = scalar_types.get(&qualified_type_name).ok_or_else(|| {
                Error::ScalarTypeUnknownRepresentation {
                    scalar_type: scalar_type_name.clone(),
                    type_name: qualified_type_name,
                }
            })?;
        }
    }
    Ok(())
}

// convert from types in previous stage to this stage
fn convert_data_connectors_contexts<'a>(
    old_data_connectors: &'a data_connectors::DataConnectors<'a>,
) -> BTreeMap<Qualified<DataConnectorName>, ScalarTypeWithRepresentationInfoMap<'a>> {
    let mut data_connector_scalars = BTreeMap::new();

    for (data_connector_name, context) in &old_data_connectors.0 {
        let mut new_scalars = BTreeMap::new();
        for (name, scalar) in &context.schema.scalar_types {
            let scalar_name = DataConnectorScalarType::from(name.as_str());
            new_scalars.insert(
                scalar_name.clone(),
                ScalarTypeWithRepresentationInfo {
                    scalar_type: scalar,
                    comparison_expression_name: None,
                    comparison_operators: get_comparison_operators(scalar),
                    representation: None,
                    aggregate_functions: &scalar.aggregate_functions,
                },
            );
        }

        data_connector_scalars.insert(
            data_connector_name.clone(),
            ScalarTypeWithRepresentationInfoMap(new_scalars),
        );
    }
    data_connector_scalars
}

fn get_comparison_operators(scalar_type: &ndc_models::ScalarType) -> ComparisonOperators {
    let mut comparison_operators = ComparisonOperators::default();
    for (operator_name, operator_definition) in &scalar_type.comparison_operators {
        match operator_definition {
            ndc_models::ComparisonOperatorDefinition::Equal => {
                comparison_operators
                    .equal_operators
                    .push(operator_name.clone());
            }
            ndc_models::ComparisonOperatorDefinition::In => {
                comparison_operators
                    .in_operators
                    .push(operator_name.clone());
            }
            ndc_models::ComparisonOperatorDefinition::Custom { argument_type: _ } => {}
        };
    }
    comparison_operators
}

// helper function to determine whether a ndc type is a simple scalar
pub fn get_simple_scalar<'a>(
    t: ndc_models::Type,
    scalars: &'a ScalarTypeWithRepresentationInfoMap<'a>,
) -> Option<&'a ScalarTypeWithRepresentationInfo<'a>> {
    match t {
        ndc_models::Type::Named { name } => scalars.0.get(name.as_str()),
        ndc_models::Type::Nullable { underlying_type } => {
            get_simple_scalar(*underlying_type, scalars)
        }
        ndc_models::Type::Array { element_type: _ }
        | ndc_models::Type::Predicate {
            object_type_name: _,
        } => None,
    }
}
