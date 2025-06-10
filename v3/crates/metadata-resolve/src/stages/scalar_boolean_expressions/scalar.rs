use super::error::{
    FieldNameSource, ScalarBooleanExpressionOperatorIssue, ScalarBooleanExpressionTypeError,
    ScalarBooleanExpressionTypeIssue, StringOperatorReason,
};
use super::types::{
    IsNullOperator, IsNullOperatorGraphqlConfig, LogicalOperators, LogicalOperatorsGraphqlConfig,
    ResolvedScalarBooleanExpressionType,
};
use crate::helpers::type_validation;
use crate::helpers::types::{mk_name, unwrap_qualified_type_name};
use crate::stages::{
    data_connector_scalar_types, data_connectors, graphql_config, object_types, scalar_types,
};
use crate::types::subgraph::{mk_qualified_type_name, mk_qualified_type_reference};
use crate::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference};
use lang_graphql::ast::common as ast;
use open_dds::data_connector::DataConnectorName;
use open_dds::identifier::SubgraphName;
use open_dds::{
    boolean_expression::{BooleanExpressionScalarOperand, BooleanExpressionTypeV1},
    data_connector::DataConnectorOperatorName,
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
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    object_types: &object_types::ObjectTypesWithTypeMappings,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    flags: &open_dds::flags::OpenDdFlags,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    issues: &mut Vec<ScalarBooleanExpressionTypeIssue>,
) -> Result<ResolvedScalarBooleanExpressionType, ScalarBooleanExpressionTypeError> {
    let qualified_scalar_type =
        mk_qualified_type_name(&scalar_boolean_expression_operand.r#type, subgraph);
    let mut comparison_operators = BTreeMap::new();

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
        if let Some(_duplicate_operator) =
            comparison_operators.insert(comparison_operator.name.clone(), qualified_argument_type)
        {
            issues.push(
                ScalarBooleanExpressionTypeIssue::DuplicateComparableOperatorFound {
                    type_name: boolean_expression_type_name.clone(),
                    name: comparison_operator.name.clone(),
                },
            );
        }
    }

    let mut data_connector_operator_mappings = BTreeMap::new();

    // this scalar boolean expression type can be mapped to one or more data connectors
    for data_connector_operator_mapping in
        &scalar_boolean_expression_operand.data_connector_operator_mapping
    {
        // make a copy now so that we can add in the inferred operator mappings (ie, any that
        // are not explicitly defined in mappings we assume are mapped to the same name)
        let mut data_connector_operator_mapping = data_connector_operator_mapping.clone();

        // add all implicitly defined mappings
        for comparison_operator in &scalar_boolean_expression_operand.comparison_operators {
            if !data_connector_operator_mapping
                .operator_mapping
                .contains_key(&comparison_operator.name)
            {
                data_connector_operator_mapping.operator_mapping.insert(
                    comparison_operator.name.clone(),
                    DataConnectorOperatorName::new(comparison_operator.name.as_str().into()),
                );
            }
        }

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

        let connector_scalars = data_connector_scalars
            .get(&qualified_data_connector_name)
            .ok_or_else(|| {
                ScalarBooleanExpressionTypeError::DataConnectorScalarRepresentationsNotFound {
                    data_connector: qualified_data_connector_name.clone(),
                }
            })?;

        // check that this scalar type actually exists for this data connector
        let data_connector_scalar_type = data_connector_context
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

        // validate the comparison operators against the data connector operator mapping
        issues.extend(validate_comparison_operators_mapping(
            boolean_expression_type_name,
            &qualified_scalar_type,
            &qualified_data_connector_name,
            connector_scalars,
            data_connector_scalar_type,
            &data_connector_operator_mapping,
            &comparison_operators,
        ));

        data_connector_operator_mappings.insert(
            qualified_data_connector_name,
            data_connector_operator_mapping,
        );
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
        &comparison_operators,
        &logical_operators,
        &is_null_operator,
        boolean_expression_type_name,
        issues,
    );

    Ok(ResolvedScalarBooleanExpressionType {
        comparison_operators,
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

fn validate_comparison_operators_mapping(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    qualified_scalar_type: &QualifiedTypeName,
    data_connector_name: &Qualified<DataConnectorName>,
    data_connector_scalars: &data_connector_scalar_types::DataConnectorScalars,
    data_connector_scalar_type: &ndc_models::ScalarType,
    data_connector_operator_mapping: &open_dds::boolean_expression::DataConnectorOperatorMapping,
    comparison_operators: &BTreeMap<open_dds::types::OperatorName, QualifiedTypeReference>,
) -> Vec<ScalarBooleanExpressionTypeIssue> {
    // Initialise the issues
    let mut issues = Vec::new();
    // Collect the issues as we traverse the comparison operators
    for (operator_name, argument_type) in comparison_operators {
        let mapped_ndc_operator_name_smol_str = data_connector_operator_mapping
            .operator_mapping
            .get(operator_name)
            .map_or_else(
                || operator_name.inner().clone(), // If no mapping is provided, use the OpenDD operator name as the mapped name
                |name| name.inner().clone(),
            );
        let mapped_ndc_operator_name =
            ndc_models::ComparisonOperatorName::new(mapped_ndc_operator_name_smol_str);
        let Some(comparison_operator_definition) = data_connector_scalar_type
            .comparison_operators
            .get(&mapped_ndc_operator_name)
        else {
            // push issue
            issues.push(ScalarBooleanExpressionTypeIssue::OperatorIssue(
                ScalarBooleanExpressionOperatorIssue::MappedOperatorNotFound {
                    type_name: boolean_expression_type_name.clone(),
                    operator_name: operator_name.clone(),
                    mapped_operator_name: mapped_ndc_operator_name,
                    data_connector_name: data_connector_name.clone(),
                },
            ));
            // Can't proceed further, so skip to the next operator
            continue;
        };
        match comparison_operator_definition {
            ndc_models::ComparisonOperatorDefinition::Custom {
                argument_type: ndc_argument_type,
            } => {
                if let Some(issue) = type_validation::validate_type_compatibility(
                    data_connector_scalars,
                    argument_type,
                    ndc_argument_type,
                ) {
                    // push issue
                    issues.push(ScalarBooleanExpressionTypeIssue::OperatorIssue(
                        ScalarBooleanExpressionOperatorIssue::ArgumentTypeMismatch {
                            type_name: boolean_expression_type_name.clone(),
                            operator_name: operator_name.clone(),
                            issue,
                        },
                    ));
                }
            }
            ndc_models::ComparisonOperatorDefinition::In => {
                // Check if argument type is an array wrapper on the scalar type
                let QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::List(inner_type),
                    nullable: _,
                } = argument_type
                else {
                    // push issue
                    issues.push(ScalarBooleanExpressionTypeIssue::OperatorIssue(
                        ScalarBooleanExpressionOperatorIssue::ArgumentTypeShouldBeList {
                            type_name: boolean_expression_type_name.clone(),
                            operator_name: operator_name.clone(),
                            argument_type: argument_type.clone(),
                        },
                    ));
                    // Can't proceed further, so skip to the next operator
                    continue;
                };
                // Check that the inner type is the same as the scalar type
                check_type_matching_scalar(
                    boolean_expression_type_name,
                    operator_name,
                    inner_type,
                    qualified_scalar_type,
                    &mut issues,
                );
            }
            ndc_models::ComparisonOperatorDefinition::Equal
            | ndc_models::ComparisonOperatorDefinition::LessThan
            | ndc_models::ComparisonOperatorDefinition::LessThanOrEqual
            | ndc_models::ComparisonOperatorDefinition::GreaterThan
            | ndc_models::ComparisonOperatorDefinition::GreaterThanOrEqual => {
                // Check that the argument type is the same as the scalar type
                check_type_matching_scalar(
                    boolean_expression_type_name,
                    operator_name,
                    argument_type,
                    qualified_scalar_type,
                    &mut issues,
                );
            }
            ndc_models::ComparisonOperatorDefinition::Contains
            | ndc_models::ComparisonOperatorDefinition::ContainsInsensitive
            | ndc_models::ComparisonOperatorDefinition::StartsWith
            | ndc_models::ComparisonOperatorDefinition::StartsWithInsensitive
            | ndc_models::ComparisonOperatorDefinition::EndsWith
            | ndc_models::ComparisonOperatorDefinition::EndsWithInsensitive => {
                // As per the ndc-spec, only ndc scalars with a string representation can use these operators
                // Check if scalar type has a string representation
                if !check_if_scalar_is_string(qualified_scalar_type, data_connector_scalars) {
                    let reason = StringOperatorReason::ScalarTypeNotString {
                        scalar_type: qualified_scalar_type.clone(),
                    };
                    issues.push(ScalarBooleanExpressionTypeIssue::OperatorIssue(
                        ScalarBooleanExpressionOperatorIssue::OnlyApplicableOnStringScalar {
                            type_name: boolean_expression_type_name.clone(),
                            operator_name: operator_name.clone(),
                            reason,
                        },
                    ));
                }
                // NOTE: Here, argument type need not be the same as the scalar type. It can be any scalar with a string representation.
                // Check if argument type is a string
                match &argument_type.underlying_type {
                    QualifiedBaseType::Named(qualified_argument_type) => {
                        if !check_if_scalar_is_string(
                            qualified_argument_type,
                            data_connector_scalars,
                        ) {
                            let reason = StringOperatorReason::ArgumentTypeNotString {
                                argument_type: qualified_argument_type.clone(),
                            };
                            // push issue with reason
                            issues.push(
                                ScalarBooleanExpressionTypeIssue::OperatorIssue(
                                    ScalarBooleanExpressionOperatorIssue::OnlyApplicableOnStringScalar {
                                        type_name: boolean_expression_type_name.clone(),
                                        operator_name: operator_name.clone(),
                                        reason,
                                    },
                                ),
                            );
                        }
                    }
                    QualifiedBaseType::List(_) => {
                        let reason = StringOperatorReason::ArgumentTypeShouldNotBeList {
                            argument_type: argument_type.clone(),
                        };
                        // push issue with reason
                        issues.push(ScalarBooleanExpressionTypeIssue::OperatorIssue(
                            ScalarBooleanExpressionOperatorIssue::OnlyApplicableOnStringScalar {
                                type_name: boolean_expression_type_name.clone(),
                                operator_name: operator_name.clone(),
                                reason,
                            },
                        ));
                    }
                }
            }
        }
    }
    // Return the issues
    issues
}

fn check_type_matching_scalar(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    operator_name: &open_dds::types::OperatorName,
    argument_type: &QualifiedTypeReference,
    qualified_scalar_type: &QualifiedTypeName,
    issues: &mut Vec<ScalarBooleanExpressionTypeIssue>,
) {
    match &argument_type.underlying_type {
        QualifiedBaseType::Named(inner_type_name) if inner_type_name == qualified_scalar_type => {
            // Expected condition
        }
        _ => {
            issues.push(ScalarBooleanExpressionTypeIssue::OperatorIssue(
                ScalarBooleanExpressionOperatorIssue::ArgumentTypeShouldMatchScalar {
                    type_name: boolean_expression_type_name.clone(),
                    operator_name: operator_name.clone(),
                    argument_type: argument_type.clone(),
                    scalar_type: qualified_scalar_type.clone(),
                },
            ));
        }
    }
}

fn check_if_scalar_is_string(
    scalar_type: &QualifiedTypeName,
    data_connector_scalars: &data_connector_scalar_types::DataConnectorScalars,
) -> bool {
    match scalar_type {
        QualifiedTypeName::Inbuilt(inbuilt_type) => {
            // Check if the inbuilt type is a string
            inbuilt_type == &open_dds::types::InbuiltType::String
        }
        QualifiedTypeName::Custom(custom_type_name) => {
            if let Some(scalar_type_representation) = data_connector_scalars
                .by_custom_type_name
                .get(custom_type_name)
            {
                // Check if the scalar type representation is a string
                scalar_type_representation == &ndc_models::TypeRepresentation::String
            } else {
                // If not a scalar type, then it can't be a string
                false
            }
        }
    }
}
