use super::error::ScalarBooleanExpressionTypeError;
use super::types::{IncludeIsNull, ResolvedScalarBooleanExpressionType};
use crate::stages::data_connectors;
use crate::Qualified;
use open_dds::{
    boolean_expression::{
        BooleanExpressionIsNull, BooleanExpressionScalarOperand,
        BooleanExpressionTypeGraphQlConfiguration,
    },
    types::CustomTypeName,
};
use std::collections::BTreeMap;

/// Resolves a given scalar boolean expression type
pub(crate) fn resolve_scalar_boolean_expression_type(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    scalar_boolean_expression_operand: &BooleanExpressionScalarOperand,
    is_null: &BooleanExpressionIsNull,
    subgraph: &str,
    data_connectors: &data_connectors::DataConnectors,
    graphql: &Option<BooleanExpressionTypeGraphQlConfiguration>,
) -> Result<ResolvedScalarBooleanExpressionType, ScalarBooleanExpressionTypeError> {
    let mut data_connector_operator_mappings = BTreeMap::new();

    // this scalar boolean expression type can be mapped to one or more data connectors
    for data_connector_operator_mapping in
        &scalar_boolean_expression_operand.data_connector_operator_mapping
    {
        let scalar_type_name = &data_connector_operator_mapping.data_connector_scalar_type;

        // scope the data connector to the current subgraph
        let qualified_data_connector_name = Qualified::new(
            subgraph.to_string(),
            data_connector_operator_mapping.data_connector_name.clone(),
        );

        // lookup the data connector we are referring to
        let data_connector_context = data_connectors
            .0
            .get(&qualified_data_connector_name)
            .ok_or_else(|| {
                ScalarBooleanExpressionTypeError::ScalarTypeFromUnknownDataConnector {
                    scalar_type: scalar_type_name.clone(),
                    data_connector: qualified_data_connector_name.clone(),
                }
            })?;

        // check that this scalar type actually exists for this data connector
        let _data_connector_scalar_type = data_connector_context
            .schema
            .scalar_types
            .get(
                data_connector_operator_mapping
                    .data_connector_scalar_type
                    .as_str(),
            )
            .ok_or_else(
                || ScalarBooleanExpressionTypeError::UnknownScalarTypeInDataConnector {
                    scalar_type: scalar_type_name.clone(),
                    data_connector: qualified_data_connector_name.clone(),
                },
            )?;

        data_connector_operator_mappings.insert(
            qualified_data_connector_name,
            data_connector_operator_mapping.clone(),
        );
    }

    let mut resolved_comparison_operators = BTreeMap::new();

    for comparison_operator in &scalar_boolean_expression_operand.comparison_operators {
        resolved_comparison_operators.insert(
            comparison_operator.name.clone(),
            comparison_operator.argument_type.clone(),
        );
    }

    let graphql_name = graphql.as_ref().map(|gql| gql.type_name.clone());

    Ok(ResolvedScalarBooleanExpressionType {
        name: boolean_expression_type_name.clone(),
        comparison_operators: resolved_comparison_operators,
        representation: scalar_boolean_expression_operand.r#type.clone(),
        data_connector_operator_mappings,
        include_is_null: resolve_is_null(is_null),
        graphql_name,
    })
}

pub fn resolve_is_null(is_null: &BooleanExpressionIsNull) -> IncludeIsNull {
    if is_null.enable {
        IncludeIsNull::Yes
    } else {
        IncludeIsNull::No
    }
}
