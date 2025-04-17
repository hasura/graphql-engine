use std::collections::BTreeMap;

use open_dds::{
    data_connector::{DataConnectorName, DataConnectorScalarType},
    identifier::SubgraphName,
    types::{
        BaseType, DataConnectorScalarRepresentationV1, InbuiltType, OperatorName, TypeReference,
    },
};

pub use super::{
    IsNullOperator, IsNullOperatorGraphqlConfig, LogicalOperators,
    ResolvedScalarBooleanExpressionType, ScalarBooleanExpressionTypeError, resolve_ndc_type,
};
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, graphql_config,
};
use crate::types::subgraph::{mk_qualified_type_name, mk_qualified_type_reference};
use crate::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference};

// turn each `DataConnectorScalarRepresentation` into a scalar `BooleanExpressionType`
// so it can be used by legacy `ObjectBooleanExpressionType`.
pub fn resolve_data_connector_scalar_representation(
    data_connector_scalar_representation: &DataConnectorScalarRepresentationV1,
    subgraph: &SubgraphName,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<
    (
        boolean_expressions::BooleanExpressionTypeIdentifier,
        ResolvedScalarBooleanExpressionType,
    ),
    ScalarBooleanExpressionTypeError,
> {
    let qualified_type_name = mk_qualified_type_name(
        &data_connector_scalar_representation.representation,
        subgraph,
    );

    let data_connector_name = Qualified::new(
        subgraph.clone(),
        data_connector_scalar_representation
            .data_connector_name
            .clone(),
    );

    let data_connector_type = boolean_expressions::DataConnectorType {
        data_connector_name: data_connector_name.clone(),
        type_name: qualified_type_name.clone(),
    };

    let data_connector = data_connectors.0.get(&data_connector_name).ok_or_else(|| {
        ScalarBooleanExpressionTypeError::DataConnectorNotFound {
            data_connector: data_connector_name.clone(),
        }
    })?;

    let scalar_types = data_connector_scalars
        .get(&data_connector_name)
        .ok_or_else(|| {
            ScalarBooleanExpressionTypeError::DataConnectorScalarRepresentationsNotFound {
                data_connector: data_connector_name.clone(),
            }
        })?;

    let ndc_scalar_type_name = ndc_models::ScalarTypeName::new(
        data_connector_scalar_representation
            .data_connector_scalar_type
            .as_str()
            .into(),
    );

    let scalar_type = data_connector
        .schema
        .scalar_types
        .get(&ndc_scalar_type_name)
        .ok_or_else(|| {
            ScalarBooleanExpressionTypeError::DataConnectorScalarRepresentationRequired {
                data_connector: data_connector_name.clone(),
                scalar_type: DataConnectorScalarType::new(ndc_scalar_type_name.as_str().into()),
            }
        })?;

    let mut comparison_operators = BTreeMap::new();

    for (comparison_operator_name, comparison_operator_definition) in
        &scalar_type.comparison_operators
    {
        let operator_name = OperatorName::new(comparison_operator_name.as_str().into());

        // what is the OpenDD type of ourselves, we'll use this for equals and some of the
        // other built-ins
        let qualified_base_type = mk_qualified_type_reference(
            &TypeReference {
                underlying_type: BaseType::Named(
                    data_connector_scalar_representation.representation.clone(),
                ),
                nullable: false,
            },
            subgraph,
        );

        // this is the "interesting" part, we need to work back from NDC definitions
        // to OpenDD types again
        match comparison_operator_definition {
            ndc_models::ComparisonOperatorDefinition::Equal
            | ndc_models::ComparisonOperatorDefinition::LessThan
            | ndc_models::ComparisonOperatorDefinition::LessThanOrEqual
            | ndc_models::ComparisonOperatorDefinition::GreaterThan
            | ndc_models::ComparisonOperatorDefinition::GreaterThanOrEqual => {
                comparison_operators.insert(operator_name, qualified_base_type);
            }
            ndc_models::ComparisonOperatorDefinition::Contains
            | ndc_models::ComparisonOperatorDefinition::ContainsInsensitive
            | ndc_models::ComparisonOperatorDefinition::StartsWith
            | ndc_models::ComparisonOperatorDefinition::StartsWithInsensitive
            | ndc_models::ComparisonOperatorDefinition::EndsWith
            | ndc_models::ComparisonOperatorDefinition::EndsWithInsensitive => {
                comparison_operators.insert(
                    operator_name,
                    QualifiedTypeReference {
                        underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(
                            InbuiltType::String,
                        )),
                        nullable: false,
                    },
                );
            }

            ndc_models::ComparisonOperatorDefinition::In => {
                let array_type = QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::List(Box::new(qualified_base_type.clone())),
                    nullable: false,
                };
                comparison_operators.insert(operator_name, array_type);
            }

            ndc_models::ComparisonOperatorDefinition::Custom { argument_type } => {
                // only keep this around if we find it (it might be an object type, not a scalar)
                if let Ok(open_dd_type) =
                    resolve_ndc_type(&data_connector_name, argument_type, scalar_types)
                {
                    comparison_operators.insert(operator_name, open_dd_type);
                }
            }
        }
    }

    // although we have no operator mappings, we need to add an empty entry for our data
    // connector
    let mut data_connector_operator_mappings = BTreeMap::new();
    data_connector_operator_mappings.insert(
        data_connector_name,
        open_dds::boolean_expression::DataConnectorOperatorMapping {
            data_connector_name: data_connector_scalar_representation
                .data_connector_name
                .clone(),
            data_connector_scalar_type: data_connector_scalar_representation
                .data_connector_scalar_type
                .clone(),
            operator_mapping: BTreeMap::new(),
        },
    );

    let graphql_name = data_connector_scalar_representation
        .graphql
        .as_ref()
        .and_then(|graphql| graphql.comparison_expression_type_name.as_ref());

    let is_null_graphql_name =
        graphql_config
            .query
            .filter_input_config
            .as_ref()
            .map(|filter_input_config| IsNullOperatorGraphqlConfig {
                is_null_operator_name: filter_input_config.operator_names.is_null.clone(),
            });

    let is_null_operator = IsNullOperator::Include {
        graphql: is_null_graphql_name,
    };

    let scalar_boolean_expression_type = ResolvedScalarBooleanExpressionType {
        comparison_operators,
        graphql_name: graphql_name.cloned(),
        is_null_operator,
        logical_operators: LogicalOperators::Exclude,
        operand_type: qualified_type_name,
        data_connector_operator_mappings,
    };

    Ok((
        boolean_expressions::BooleanExpressionTypeIdentifier::FromDataConnectorScalarRepresentation(
            data_connector_type,
        ),
        scalar_boolean_expression_type,
    ))
}
