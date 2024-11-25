use super::error::{ScalarBooleanExpressionTypeError, ScalarBooleanExpressionTypeIssue};
use super::types::{
    IncludeIsNull, LogicalOperators, LogicalOperatorsGraphqlConfig,
    ResolvedScalarBooleanExpressionType,
};
use crate::helpers::types::{mk_name, store_new_graphql_type, unwrap_qualified_type_name};
use crate::stages::{data_connectors, graphql_config, object_types, scalar_types};
use crate::types::subgraph::{mk_qualified_type_name, mk_qualified_type_reference};
use crate::{Qualified, QualifiedTypeName};
use lang_graphql::ast::common as ast;
use open_dds::identifier::SubgraphName;
use open_dds::{
    boolean_expression::{
        BooleanExpressionIsNull, BooleanExpressionScalarOperand, BooleanExpressionTypeV1,
    },
    types::CustomTypeName,
};
use std::collections::{BTreeMap, BTreeSet};

/// Resolves a given scalar boolean expression type
pub(crate) fn resolve_scalar_boolean_expression_type(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    scalar_boolean_expression_operand: &BooleanExpressionScalarOperand,
    boolean_expression: &BooleanExpressionTypeV1,
    subgraph: &SubgraphName,
    data_connectors: &data_connectors::DataConnectors,
    object_types: &object_types::ObjectTypesWithTypeMappings,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    flags: &open_dds::flags::Flags,
    graphql_types: &mut BTreeSet<ast::TypeName>,
    issues: &mut Vec<ScalarBooleanExpressionTypeIssue>,
) -> Result<ResolvedScalarBooleanExpressionType, ScalarBooleanExpressionTypeError> {
    let mut data_connector_operator_mappings = BTreeMap::new();

    // this scalar boolean expression type can be mapped to one or more data connectors
    for data_connector_operator_mapping in
        &scalar_boolean_expression_operand.data_connector_operator_mapping
    {
        let scalar_type_name = &data_connector_operator_mapping.data_connector_scalar_type;

        // scope the data connector to the current subgraph
        let qualified_data_connector_name = Qualified::new(
            subgraph.clone(),
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
        let qualified_argument_type =
            mk_qualified_type_reference(&comparison_operator.argument_type, subgraph);
        // if our argument type is a Custom named type, check we know about it
        match unwrap_qualified_type_name(&qualified_argument_type) {
            QualifiedTypeName::Inbuilt(_) => Ok(()),
            QualifiedTypeName::Custom(custom_type_name) => {
                if object_types.contains_key(custom_type_name)
                    || scalar_types.contains_key(custom_type_name)
                {
                    Ok(())
                } else {
                    Err(ScalarBooleanExpressionTypeError
                        ::UnknownCustomTypeInComparisonOperatorArgument {
                        custom_type: custom_type_name.clone(),
                        operator_name: comparison_operator.name.clone(),
                        boolean_expression_type: boolean_expression_type_name.clone(),
                    })
                }
            }
        }?;
        resolved_comparison_operators
            .insert(comparison_operator.name.clone(), qualified_argument_type);
    }

    let graphql_name = boolean_expression
        .graphql
        .as_ref()
        .map(|gql| gql.type_name.clone());

    let graphql_type = graphql_name
        .as_ref()
        .map(|name| mk_name(name.as_str()).map(ast::TypeName))
        .transpose()?;

    store_new_graphql_type(graphql_types, graphql_type.as_ref())?;

    let logical_operators = if flags.logical_operators_in_scalar_boolean_expressions {
        resolve_logical_operators(boolean_expression, graphql_config, issues, || {
            ScalarBooleanExpressionTypeIssue::MissingLogicalOperatorNamesInGraphqlConfig {
                type_name: boolean_expression_type_name.clone(),
            }
        })
    } else {
        issues.push(
            ScalarBooleanExpressionTypeIssue::LogicalOperatorsUnavailable {
                type_name: boolean_expression_type_name.clone(),
            },
        );

        LogicalOperators::Exclude
    };

    Ok(ResolvedScalarBooleanExpressionType {
        name: boolean_expression_type_name.clone(),
        comparison_operators: resolved_comparison_operators,
        operand_type: mk_qualified_type_name(&scalar_boolean_expression_operand.r#type, subgraph),
        data_connector_operator_mappings,
        graphql_name,
        logical_operators,
        include_is_null: resolve_is_null(&boolean_expression.is_null),
    })
}

pub fn resolve_is_null(is_null: &BooleanExpressionIsNull) -> IncludeIsNull {
    if is_null.enable {
        IncludeIsNull::Yes
    } else {
        IncludeIsNull::No
    }
}

pub fn resolve_logical_operators<Issue, FGetIssue: FnOnce() -> Issue>(
    boolean_expression: &open_dds::boolean_expression::BooleanExpressionTypeV1,
    graphql_config: &graphql_config::GraphqlConfig,
    issues: &mut Vec<Issue>,
    mk_missing_logical_operators_issue: FGetIssue,
) -> LogicalOperators {
    if boolean_expression.logical_operators.enable {
        let graphql =
            graphql_config
                .query
                .filter_input_config
                .as_ref()
                .map(|filter_input_config| LogicalOperatorsGraphqlConfig {
                    and_operator_name: filter_input_config.operator_names.and.clone(),
                    or_operator_name: filter_input_config.operator_names.or.clone(),
                    not_operator_name: filter_input_config.operator_names.not.clone(),
                });

        // If they've enabled graphql for this boolean expression, but the logical operator names
        // have been omitted from the GraphqlConfig, raise an issue because this is probably a mistake
        if boolean_expression.graphql.is_some() && graphql.is_none() {
            issues.push(mk_missing_logical_operators_issue());
        }

        LogicalOperators::Include { graphql }
    } else {
        LogicalOperators::Exclude
    }
}
