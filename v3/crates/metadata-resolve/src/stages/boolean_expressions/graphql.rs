use super::error::BooleanExpressionError;
use super::helpers;
pub use super::{
    BooleanExpressionComparableRelationship, BooleanExpressionGraphqlConfig,
    BooleanExpressionGraphqlFieldConfig, ComparisonExpressionInfo, ObjectComparisonExpressionInfo,
};
use crate::helpers::types::mk_name;
use crate::stages::{graphql_config, scalar_boolean_expressions};
use crate::types::subgraph::mk_qualified_type_reference;
use crate::Qualified;
use lang_graphql::ast::common::{self as ast};
use open_dds::{
    boolean_expression::{
        BooleanExpressionOperand, BooleanExpressionTypeGraphQlConfiguration,
        DataConnectorOperatorMapping,
    },
    data_connector::{DataConnectorName, DataConnectorOperatorName},
    types::{CustomTypeName, FieldName, OperatorName},
};
use std::collections::BTreeMap;

// validate graphql config
// we use the raw boolean expression types for lookup
pub(crate) fn resolve_object_boolean_graphql(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    boolean_expression_graphql_config: &BooleanExpressionTypeGraphQlConfiguration,
    comparable_fields: &BTreeMap<FieldName, Qualified<CustomTypeName>>,
    comparable_relationships: &BTreeMap<FieldName, BooleanExpressionComparableRelationship>,
    scalar_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &super::object::RawBooleanExpressionTypes,
    subgraph: &str,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<BooleanExpressionGraphqlConfig, BooleanExpressionError> {
    let boolean_expression_graphql_name =
        mk_name(boolean_expression_graphql_config.type_name.as_ref()).map(ast::TypeName)?;

    let mut scalar_fields = BTreeMap::new();

    let mut object_fields = BTreeMap::new();

    let filter_graphql_config = graphql_config
        .query
        .filter_input_config
        .as_ref()
        .ok_or_else(|| {
            graphql_config::GraphqlConfigError::MissingFilterInputFieldInGraphqlConfig
        })?;

    for (comparable_field_name, comparable_field_type_name) in comparable_fields {
        if let Some(scalar_boolean_expression_type) =
            scalar_boolean_expression_types.get(comparable_field_type_name)
        {
            // Generate comparison expression for fields mapped to simple scalar type
            if let Some(graphql_name) = &scalar_boolean_expression_type.graphql_name {
                let mut operators = BTreeMap::new();
                for (op_name, op_definition) in &scalar_boolean_expression_type.comparison_operators
                {
                    operators.insert(
                        op_name.clone(),
                        mk_qualified_type_reference(op_definition, subgraph),
                    );
                }
                let graphql_type_name = mk_name(graphql_name.as_str()).map(ast::TypeName)?;

                let operator_mapping = resolve_operator_mapping_for_scalar_type(
                    &scalar_boolean_expression_type.data_connector_operator_mappings,
                );

                // Register scalar comparison field only if it contains non-zero operators.
                if !operators.is_empty()
                    || scalar_boolean_expression_type.include_is_null
                        == scalar_boolean_expressions::IncludeIsNull::Yes
                {
                    scalar_fields.insert(
                        comparable_field_name.clone(),
                        ComparisonExpressionInfo {
                            object_type_name: Some(comparable_field_type_name.clone()),
                            type_name: graphql_type_name.clone(),
                            operators: operators.clone(),
                            operator_mapping,
                            is_null_operator_name: match scalar_boolean_expression_type
                                .include_is_null
                            {
                                scalar_boolean_expressions::IncludeIsNull::Yes => {
                                    Some(filter_graphql_config.operator_names.is_null.clone())
                                }
                                scalar_boolean_expressions::IncludeIsNull::No => None,
                            },
                        },
                    );
                };
            }
        } else {
            // if this field isn't a scalar, let's see if it's an object instead
            let (field_subgraph, raw_boolean_expression_type) =
                helpers::lookup_raw_boolean_expression(
                    boolean_expression_type_name,
                    comparable_field_type_name,
                    raw_boolean_expression_types,
                )?;

            if let (Some(graphql_name), BooleanExpressionOperand::Object(object_operand)) = (
                raw_boolean_expression_type
                    .graphql
                    .as_ref()
                    .map(|gql| gql.type_name.clone()),
                &raw_boolean_expression_type.operand,
            ) {
                let graphql_type_name = mk_name(graphql_name.as_str()).map(ast::TypeName)?;

                object_fields.insert(
                    comparable_field_name.clone(),
                    ObjectComparisonExpressionInfo {
                        object_type_name: comparable_field_type_name.clone(),
                        underlying_object_type_name: Qualified::new(
                            (*field_subgraph).to_string(),
                            object_operand.r#type.clone(),
                        ),
                        graphql_type_name: graphql_type_name.clone(),
                    },
                );
            }
        }
    }

    Ok(BooleanExpressionGraphqlConfig {
        type_name: boolean_expression_graphql_name,
        scalar_fields,
        object_fields,
        relationship_fields: comparable_relationships.clone(),
        graphql_config: (BooleanExpressionGraphqlFieldConfig {
            where_field_name: filter_graphql_config.where_field_name.clone(),
            and_operator_name: filter_graphql_config.operator_names.and.clone(),
            or_operator_name: filter_graphql_config.operator_names.or.clone(),
            not_operator_name: filter_graphql_config.operator_names.not.clone(),
        }),
    })
}

fn resolve_operator_mapping_for_scalar_type(
    data_connector_operator_mappings: &BTreeMap<
        Qualified<DataConnectorName>,
        DataConnectorOperatorMapping,
    >,
) -> BTreeMap<Qualified<DataConnectorName>, BTreeMap<OperatorName, DataConnectorOperatorName>> {
    let mut operator_mapping = BTreeMap::new();

    for (data_connector_name, data_connector_operator_mapping) in data_connector_operator_mappings {
        operator_mapping.insert(
            data_connector_name.clone(),
            data_connector_operator_mapping.operator_mapping.clone(),
        );
    }

    operator_mapping
}
