use super::error::{
    FieldNameSource, ScalarBooleanExpressionTypeError, ScalarBooleanExpressionTypeIssue,
};
use super::types::{
    IsNullOperator, IsNullOperatorGraphqlConfig, LogicalOperators, LogicalOperatorsGraphqlConfig,
    ResolvedScalarBooleanExpressionType,
};
use crate::helpers::types::{mk_name, unwrap_qualified_type_name};
use crate::stages::{data_connectors, graphql_config, object_types, scalar_types};
use crate::types::subgraph::{mk_qualified_type_name, mk_qualified_type_reference};
use crate::{Qualified, QualifiedTypeName, QualifiedTypeReference};
use lang_graphql::ast::common as ast;
use open_dds::identifier::SubgraphName;
use open_dds::{
    boolean_expression::{BooleanExpressionScalarOperand, BooleanExpressionTypeV1},
    types::CustomTypeName,
};
use std::collections::BTreeMap;

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
    flags: &open_dds::flags::OpenDdFlags,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
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
        if let Some(_duplicate_operator) = resolved_comparison_operators
            .insert(comparison_operator.name.clone(), qualified_argument_type)
        {
            issues.push(
                ScalarBooleanExpressionTypeIssue::DuplicateComparableOperatorFound {
                    type_name: boolean_expression_type_name.clone(),
                    name: comparison_operator.name.clone(),
                },
            );
        };
    }

    let graphql_name = boolean_expression
        .graphql
        .as_ref()
        .map(|gql| gql.type_name.clone());

    let graphql_type = graphql_name
        .as_ref()
        .map(|name| mk_name(name.as_str()).map(ast::TypeName))
        .transpose()?;

    graphql_types.store(graphql_type.as_ref())?;

    let logical_operators =
        if flags.contains(open_dds::flags::Flag::LogicalOperatorsInScalarBooleanExpressions) {
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

    let is_null_operator = resolve_is_null_operator(
        &boolean_expression.is_null,
        boolean_expression.graphql.as_ref(),
        boolean_expression_type_name,
        graphql_config,
        issues,
    );

    check_graphql_field_name_conflicts(
        &resolved_comparison_operators,
        &logical_operators,
        &is_null_operator,
        boolean_expression_type_name,
        issues,
    );

    Ok(ResolvedScalarBooleanExpressionType {
        comparison_operators: resolved_comparison_operators,
        operand_type: mk_qualified_type_name(&scalar_boolean_expression_operand.r#type, subgraph),
        data_connector_operator_mappings,
        graphql_name,
        logical_operators,
        is_null_operator,
    })
}

fn resolve_is_null_operator(
    is_null: &open_dds::boolean_expression::BooleanExpressionIsNull,
    boolean_expression_graphql: Option<
        &open_dds::boolean_expression::BooleanExpressionTypeGraphQlConfiguration,
    >,
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    graphql_config: &graphql_config::GraphqlConfig,
    issues: &mut Vec<ScalarBooleanExpressionTypeIssue>,
) -> IsNullOperator {
    if is_null.enable {
        let graphql =
            graphql_config
                .query
                .filter_input_config
                .as_ref()
                .map(|filter_input_config| IsNullOperatorGraphqlConfig {
                    is_null_operator_name: filter_input_config.operator_names.is_null.clone(),
                });

        // If they've enabled graphql for this boolean expression, but the logical operator names
        // have been omitted from the GraphqlConfig, raise an issue because this is probably a mistake
        if boolean_expression_graphql.is_some() && graphql.is_none() {
            issues.push(
                ScalarBooleanExpressionTypeIssue::MissingIsNullOperatorNameInGraphqlConfig {
                    type_name: boolean_expression_type_name.clone(),
                },
            );
        }

        IsNullOperator::Include { graphql }
    } else {
        IsNullOperator::Exclude
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

fn check_graphql_field_name_conflicts(
    comparison_operators: &BTreeMap<open_dds::types::OperatorName, QualifiedTypeReference>,
    logical_operators: &LogicalOperators,
    is_null_operator: &IsNullOperator,
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    issues: &mut Vec<ScalarBooleanExpressionTypeIssue>,
) {
    // Comparison operator names
    let mut used_names = comparison_operators
        .keys()
        .map(|operator_name| (operator_name.as_str(), FieldNameSource::ComparisonOperator))
        .collect::<BTreeMap<_, _>>();

    // The logical operator names
    let logical_operator_names = match logical_operators {
        LogicalOperators::Include { graphql } => graphql
            .as_ref()
            .map(|g| {
                vec![
                    (
                        g.and_operator_name.as_str(),
                        FieldNameSource::LogicalOperator,
                    ),
                    (
                        g.or_operator_name.as_str(),
                        FieldNameSource::LogicalOperator,
                    ),
                    (
                        g.not_operator_name.as_str(),
                        FieldNameSource::LogicalOperator,
                    ),
                ]
            })
            .unwrap_or_default(),
        LogicalOperators::Exclude => vec![],
    };

    // The is null operator name
    let is_null_operator_name = match is_null_operator {
        IsNullOperator::Include { graphql } => graphql
            .as_ref()
            .map(|g| {
                vec![(
                    g.is_null_operator_name.as_str(),
                    FieldNameSource::IsNullOperator,
                )]
            })
            .unwrap_or_default(),
        IsNullOperator::Exclude => vec![],
    };

    // See whether any of the names conflict with the already-used names
    let names_to_add = logical_operator_names
        .into_iter()
        .chain(is_null_operator_name);
    for (name, name_source) in names_to_add {
        if let Some(conflicting_name_source) = used_names.insert(name, name_source) {
            issues.push(ScalarBooleanExpressionTypeIssue::GraphqlFieldNameConflict {
                type_name: boolean_expression_type_name.clone(),
                name: name.to_owned(),
                name_source_1: conflicting_name_source,
                name_source_2: name_source,
            });
        }
    }
}
