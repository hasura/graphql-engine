mod error;
mod types;
pub use error::DataConnectorScalarTypesError;
use open_dds::identifier::SubgraphName;
use std::collections::{BTreeMap, BTreeSet};
pub use types::{
    ComparisonOperators, DataConnectorScalars, DataConnectorWithScalarsOutput,
    ScalarTypeWithRepresentationInfo,
};

use lang_graphql::ast::common as ast;

use open_dds::types::{CustomTypeName, TypeName};

use open_dds::data_connector::{
    DataConnectorName, DataConnectorOperatorName, DataConnectorScalarType,
};

use crate::helpers::types::mk_name;
use crate::types::subgraph::Qualified;

use crate::stages::{data_connectors, scalar_boolean_expressions, scalar_types};

/// resolve data connector scalar representations
/// also use scalar `BooleanExpressionType`s
pub fn resolve<'a>(
    metadata_accessor: &'a open_dds::accessor::MetadataAccessor,
    data_connectors: &'a data_connectors::DataConnectors,
    scalar_types: &'a BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    mut graphql_types: BTreeSet<ast::TypeName>,
) -> Result<DataConnectorWithScalarsOutput<'a>, DataConnectorScalarTypesError> {
    // we convert data from the old types to the new types and then start mutating everything
    // there is no doubt room for improvement here, but at least we are keeping the mutation
    // contained to this resolving stage
    let mut data_connector_scalars = convert_data_connectors_contexts(data_connectors);

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: scalar_type_representation,
    } in &metadata_accessor.data_connector_scalar_representations
    {
        let scalar_type_name = &scalar_type_representation.data_connector_scalar_type;

        let qualified_data_connector_name = Qualified::new(
            subgraph.clone(),
            scalar_type_representation.data_connector_name.clone(),
        );

        let scalars = data_connector_scalars
            .get_mut(&qualified_data_connector_name)
            .ok_or_else(|| scalar_boolean_expressions::ScalarBooleanExpressionTypeError::ScalarTypeFromUnknownDataConnector {
                scalar_type: scalar_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
            })?;

        let scalar_type_by_ndc_type = scalars
            .by_ndc_type
            .get_mut(&scalar_type_representation.data_connector_scalar_type)
            .ok_or_else(|| scalar_boolean_expressions::ScalarBooleanExpressionTypeError::UnknownScalarTypeInDataConnector {
                scalar_type: scalar_type_name.clone(),
                data_connector: qualified_data_connector_name.clone(),
            })?;

        if scalar_type_by_ndc_type.representation.is_none() {
            validate_type_name(
                &scalar_type_representation.representation,
                subgraph,
                scalar_types,
                scalar_type_name,
            )?;

            scalar_type_by_ndc_type.representation =
                Some(scalar_type_representation.representation.clone());

            // if this is a custom scalar type,
            // record the TypeRepresentation for it (ie, `String`, `JSON`, etc)
            if let TypeName::Custom(custom_type_name) = &scalar_type_representation.representation {
                let data_connector_context = data_connectors
                    .0
                    .get(&qualified_data_connector_name)
                    .unwrap();

                let ndc_scalar_name = ndc_models::ScalarTypeName::new(ndc_models::TypeName::new(
                    scalar_type_representation
                        .data_connector_scalar_type
                        .clone()
                        .into(),
                ));

                let ndc_scalar_type = data_connector_context
                    .schema
                    .scalar_types
                    .get(&ndc_scalar_name)
                    .unwrap();

                scalars.by_custom_type_name.insert(
                    Qualified::new(subgraph.clone(), custom_type_name.clone()),
                    ndc_scalar_type.representation.clone(),
                );
            }
        } else {
            return Err(
                DataConnectorScalarTypesError::DuplicateDataConnectorScalarRepresentation {
                    data_connector: qualified_data_connector_name.clone(),
                    scalar_type: scalar_type_name.clone(),
                },
            );
        }
        scalar_type_by_ndc_type.comparison_expression_name =
            match scalar_type_representation.graphql.as_ref() {
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
        if let Some(new_graphql_type) = &scalar_type_by_ndc_type.comparison_expression_name {
            graphql_types.insert(new_graphql_type.clone());
        };
    }

    Ok(DataConnectorWithScalarsOutput {
        data_connector_scalars,
        graphql_types,
    })
}

fn validate_type_name(
    type_name: &TypeName,
    subgraph: &SubgraphName,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    scalar_type_name: &DataConnectorScalarType,
) -> Result<(), DataConnectorScalarTypesError> {
    match type_name {
        TypeName::Inbuilt(_) => {} // TODO: Validate Nullable and Array types in Inbuilt
        TypeName::Custom(type_name) => {
            let qualified_type_name = Qualified::new(subgraph.clone(), type_name.to_owned());
            let _representation = scalar_types.get(&qualified_type_name).ok_or_else(|| {
                DataConnectorScalarTypesError::ScalarTypeUnknownRepresentation {
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
) -> BTreeMap<Qualified<DataConnectorName>, DataConnectorScalars<'a>> {
    let mut data_connector_scalars = BTreeMap::new();

    for (data_connector_name, context) in &old_data_connectors.0 {
        let mut by_ndc_type = BTreeMap::new();
        for (name, scalar) in &context.schema.scalar_types {
            let ndc_scalar_type_name = DataConnectorScalarType::from(name.as_str());
            by_ndc_type.insert(
                ndc_scalar_type_name.clone(),
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
            DataConnectorScalars {
                by_ndc_type,
                by_custom_type_name: BTreeMap::new(),
            },
        );
    }
    data_connector_scalars
}

pub(crate) fn get_comparison_operators(
    scalar_type: &ndc_models::ScalarType,
) -> ComparisonOperators {
    let mut comparison_operators = ComparisonOperators::default();
    for (operator_name, operator_definition) in &scalar_type.comparison_operators {
        match operator_definition {
            ndc_models::ComparisonOperatorDefinition::Equal => {
                comparison_operators
                    .equal_operators
                    .push(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
            }
            ndc_models::ComparisonOperatorDefinition::In => {
                comparison_operators
                    .in_operators
                    .push(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
            }
            ndc_models::ComparisonOperatorDefinition::LessThan
            | ndc_models::ComparisonOperatorDefinition::LessThanOrEqual
            | ndc_models::ComparisonOperatorDefinition::GreaterThan
            | ndc_models::ComparisonOperatorDefinition::GreaterThanOrEqual
            | ndc_models::ComparisonOperatorDefinition::Custom { argument_type: _ } => {
                comparison_operators
                    .other_operators
                    .push(DataConnectorOperatorName::new(
                        operator_name.inner().clone(),
                    ));
            }
        };
    }
    comparison_operators
}

// helper function to determine whether a ndc type is a simple scalar
pub fn get_simple_scalar<'a>(
    t: ndc_models::Type,
    scalars: &'a DataConnectorScalars<'a>,
) -> Option<&'a ScalarTypeWithRepresentationInfo<'a>> {
    match t {
        ndc_models::Type::Named { name } => scalars.by_ndc_type.get(name.as_str()),
        ndc_models::Type::Nullable { underlying_type } => {
            get_simple_scalar(*underlying_type, scalars)
        }
        ndc_models::Type::Array { element_type: _ }
        | ndc_models::Type::Predicate {
            object_type_name: _,
        } => None,
    }
}
