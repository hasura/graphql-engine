mod types;

use std::collections::BTreeMap;

use super::{
    aggregates::{AggregateCountDefinition, AggregateExpression, CountAggregateType},
    boolean_expressions, graphql_config, relationships,
    scalar_boolean_expressions::{self, LogicalOperators, resolve_logical_operators},
    scalar_types::ScalarTypeRepresentation,
    type_permissions::ObjectTypeWithPermissions,
};
use crate::{
    Qualified, QualifiedBaseType, QualifiedTypeName, ResolvedScalarBooleanExpressionType,
    configuration::UnstableFeatures, helpers::check_for_duplicates, mk_name,
    types::subgraph::mk_qualified_type_name,
};
use lang_graphql::ast::common as ast;
use open_dds::{
    accessor::QualifiedObject,
    aggregates::AggregateExpressionName,
    boolean_expression::{
        AggregateBooleanExpressionComparableCount, BooleanExpressionComparableAggregationFunction,
    },
    identifier::SubgraphName,
    types::{CustomTypeName, InbuiltType},
};
pub use types::*;

pub fn resolve(
    unstable_features: &UnstableFeatures,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    scalar_boolean_expression_types: &BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    aggregate_expressions: &BTreeMap<Qualified<AggregateExpressionName>, AggregateExpression>,
    unresolved_relationships: &relationships::Relationships,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<AggregateBooleanExpressionsOutput, Vec<NamedAggregateBooleanExpressionError>> {
    let mut output = AggregateBooleanExpressionsOutput {
        scalar_aggregates: BTreeMap::new(),
        object_aggregates: BTreeMap::new(),
        issues: vec![],
    };
    let mut results = vec![];

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: boolean_expression,
    } in &metadata_accessor.boolean_expression_types
    {
        results.push(resolve_aggregate_boolean_expression(
            &mut output,
            unstable_features,
            metadata_accessor,
            scalar_boolean_expression_types,
            aggregate_expressions,
            unresolved_relationships,
            object_types,
            scalar_types,
            graphql_config,
            graphql_types,
            subgraph,
            boolean_expression,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| output)
}

fn resolve_aggregate_boolean_expression(
    output: &mut AggregateBooleanExpressionsOutput,
    unstable_features: &UnstableFeatures,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    scalar_boolean_expression_types: &BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    aggregate_expressions: &BTreeMap<Qualified<AggregateExpressionName>, AggregateExpression>,
    unresolved_relationships: &relationships::Relationships,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithPermissions>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    subgraph: &SubgraphName,
    boolean_expression: &open_dds::boolean_expression::BooleanExpressionTypeV1,
) -> Result<(), NamedAggregateBooleanExpressionError> {
    match &boolean_expression.operand {
        open_dds::boolean_expression::BooleanExpressionOperand::ObjectAggregate(
            object_aggregate_operand,
        ) => {
            let mut issues = vec![];

            let qualified_name = resolve_common_aggregate_boolean_expression(
                unstable_features,
                subgraph,
                boolean_expression,
            )?;
            let object_aggregate = resolve_object_aggregate_boolean_expression(
                subgraph,
                boolean_expression,
                object_aggregate_operand,
                scalar_boolean_expression_types,
                &metadata_accessor.boolean_expression_types,
                unresolved_relationships,
                &metadata_accessor.models,
                aggregate_expressions,
                scalar_types,
                object_types,
                graphql_config,
                graphql_types,
                &mut issues,
            )
            .map_err(|error| NamedAggregateBooleanExpressionError {
                type_name: qualified_name.clone(),
                error,
            })?;

            output.issues.extend(issues.into_iter().map(|issue| {
                NamedAggregateBooleanExpressionIssue {
                    type_name: qualified_name.clone(),
                    issue,
                }
            }));

            output
                .object_aggregates
                .insert(qualified_name, object_aggregate);
        }
        open_dds::boolean_expression::BooleanExpressionOperand::ScalarAggregate(
            scalar_aggregate_operand,
        ) => {
            let mut issues = vec![];

            let qualified_name = resolve_common_aggregate_boolean_expression(
                unstable_features,
                subgraph,
                boolean_expression,
            )?;

            let scalar_aggregate = resolve_scalar_aggregate_boolean_expression(
                subgraph,
                boolean_expression,
                scalar_aggregate_operand,
                scalar_boolean_expression_types,
                aggregate_expressions,
                scalar_types,
                graphql_config,
                graphql_types,
                &mut issues,
            )
            .map_err(|error| NamedAggregateBooleanExpressionError {
                type_name: qualified_name.clone(),
                error,
            })?;

            output.issues.extend(issues.into_iter().map(|issue| {
                NamedAggregateBooleanExpressionIssue {
                    type_name: qualified_name.clone(),
                    issue,
                }
            }));

            output
                .scalar_aggregates
                .insert(qualified_name, scalar_aggregate);
        }
        _ => (),
    }
    Ok(())
}

fn resolve_common_aggregate_boolean_expression(
    unstable_features: &UnstableFeatures,
    subgraph: &SubgraphName,
    boolean_expression: &open_dds::boolean_expression::BooleanExpressionTypeV1,
) -> Result<Qualified<CustomTypeName>, NamedAggregateBooleanExpressionError> {
    let qualified_name = Qualified::new(subgraph.clone(), boolean_expression.name.clone());

    if !unstable_features.enable_aggregation_predicates {
        return Err(NamedAggregateBooleanExpressionError {
            type_name: qualified_name,
            error: AggregateBooleanExpressionError::AggregateBooleanExpressionsNotSupported,
        });
    }

    // Check if isNull is enabled; isNull is not supported for aggregates boolexps because
    // one applies isNull to the result of an aggregate (ie. the final scalar-operand comparison boolexp)
    // not the aggregate boolexp itself
    if boolean_expression.is_null.enable {
        return Err(NamedAggregateBooleanExpressionError {
            type_name: qualified_name,
            error: AggregateBooleanExpressionError::IsNullComparisonsNotSupported,
        });
    }

    Ok(qualified_name)
}

fn resolve_scalar_aggregate_boolean_expression(
    subgraph: &SubgraphName,
    boolean_expression: &open_dds::boolean_expression::BooleanExpressionTypeV1,
    scalar_aggregate_operand: &open_dds::boolean_expression::BooleanExpressionScalarAggregateOperand,
    scalar_boolean_expression_types: &BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    aggregate_expressions: &BTreeMap<Qualified<AggregateExpressionName>, AggregateExpression>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    issues: &mut Vec<AggregateBooleanExpressionIssue>,
) -> Result<ScalarAggregateBooleanExpression, AggregateBooleanExpressionError> {
    // Check if the specified operand type is actually a scalar type
    let scalar_operand_type = match &scalar_aggregate_operand.r#type {
        // All inbuilt types are scalar types
        open_dds::types::TypeName::Inbuilt(inbuilt) => Ok(QualifiedTypeName::Inbuilt(*inbuilt)),
        open_dds::types::TypeName::Custom(custom_type_name) => {
            let qualified_name = Qualified::new(subgraph.clone(), custom_type_name.clone());
            if scalar_types.keys().any(|t| *t == qualified_name) {
                Ok(QualifiedTypeName::Custom(qualified_name))
            } else {
                Err(
                    AggregateBooleanExpressionError::OperandTypeIsNotAScalarType {
                        operand_type: qualified_name,
                    },
                )
            }
        }
    }?;

    let aggregate_expression_name = Qualified::new(
        subgraph.clone(),
        scalar_aggregate_operand.aggregate_expression.clone(),
    );

    // Check if the referenced aggregate expression exists
    let aggregate_expression = aggregate_expressions
        .get(&aggregate_expression_name)
        .ok_or_else(
            || AggregateBooleanExpressionError::AggregateExpressionNotFound {
                aggregate_expression: aggregate_expression_name.clone(),
            },
        )?;

    // Check if the operand type matches the operand type in the aggregate expression
    if scalar_operand_type != aggregate_expression.operand.aggregated_type {
        return Err(
            AggregateBooleanExpressionError::AggregateOperandTypeMismatch {
                operand_type: scalar_operand_type,
                aggregate_expression: aggregate_expression_name,
                aggregate_operand: aggregate_expression.operand.aggregated_type.clone(),
            },
        );
    }

    // Check that no aggregation functions are duplicated in the list
    check_for_duplicates(
        &scalar_aggregate_operand.comparable_aggregation_functions,
        |function| &function.name,
    )
    .map_err(|duplicate_fn| {
        AggregateBooleanExpressionError::DuplicateAggregationFunctionFound {
            aggregation_function_name: duplicate_fn.name.clone(),
        }
    })?;

    // Resolve all the comparable aggregation functions
    let aggregation_functions = scalar_aggregate_operand
        .comparable_aggregation_functions
        .iter()
        .map(|comparable_agg_fn| {
            resolve_comparable_aggregation_function(
                comparable_agg_fn,
                subgraph,
                &aggregate_expression_name,
                aggregate_expression,
                scalar_boolean_expression_types,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Resolve the comparison against _count
    let count_aggregation = scalar_aggregate_operand
        .comparable_count
        .as_ref()
        .map(|comparable_count| {
            resolve_count_aggregation_function(
                comparable_count,
                CountAggregateType::Count,
                subgraph,
                &aggregate_expression_name,
                &aggregate_expression.count,
                boolean_expression,
                scalar_boolean_expression_types,
                graphql_config,
                issues,
            )
        })
        .transpose()?;

    // Resolve the comparison against _count_distinct
    let count_distinct_aggregation = scalar_aggregate_operand
        .comparable_count_distinct
        .as_ref()
        .map(|comparable_count| {
            resolve_count_aggregation_function(
                comparable_count,
                CountAggregateType::CountDistinct,
                subgraph,
                &aggregate_expression_name,
                &aggregate_expression.count_distinct,
                boolean_expression,
                scalar_boolean_expression_types,
                graphql_config,
                issues,
            )
        })
        .transpose()?;

    // Resolve logical operators
    let logical_operators =
        resolve_logical_operators(boolean_expression, graphql_config, issues, || {
            AggregateBooleanExpressionIssue::MissingLogicalOperatorNamesInGraphqlConfig
        });

    // Resolve the graphql configuration
    let graphql = resolve_aggregate_bool_exp_graphql_config(boolean_expression, graphql_types)?;

    // Check to make sure none of the field names conflict with one another
    // (eg. no aggregation function name that is the same as a logical operator)
    check_scalar_operand_graphql_name_conflicts(
        &aggregation_functions,
        count_aggregation.as_ref(),
        count_distinct_aggregation.as_ref(),
        &logical_operators,
    )?;

    Ok(ScalarAggregateBooleanExpression {
        operand_type: scalar_operand_type,
        aggregate_expression: aggregate_expression_name,
        aggregation_functions,
        count_aggregation,
        count_distinct_aggregation,
        logical_operators,
        graphql,
    })
}

fn resolve_aggregate_bool_exp_graphql_config(
    boolean_expression: &open_dds::boolean_expression::BooleanExpressionTypeV1,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<Option<AggregateBooleanExpressionGraphqlConfig>, AggregateBooleanExpressionError> {
    boolean_expression
        .graphql
        .as_ref()
        .map(|graphql| -> Result<_, AggregateBooleanExpressionError> {
            let boolean_expression_graphql_name =
                mk_name(graphql.type_name.as_ref()).map(ast::TypeName)?;

            graphql_types.store(Some(&boolean_expression_graphql_name))?;

            Ok(AggregateBooleanExpressionGraphqlConfig {
                type_name: boolean_expression_graphql_name,
            })
        })
        .transpose()
}

fn resolve_aggregate_predicate_bool_exp_graphql_config(
    object_aggregate_operand: &open_dds::boolean_expression::BooleanExpressionObjectAggregateOperand,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<Option<AggregatePredicateGraphqlConfig>, AggregateBooleanExpressionError> {
    object_aggregate_operand
        .graphql
        .as_ref()
        .map(|graphql| -> Result<_, AggregateBooleanExpressionError> {
            let predicate_graphql_name =
                mk_name(graphql.predicate_type_name.as_ref()).map(ast::TypeName)?;

            graphql_types.store(Some(&predicate_graphql_name))?;

            Ok(AggregatePredicateGraphqlConfig {
                type_name: predicate_graphql_name,
            })
        })
        .transpose()
}

fn resolve_comparable_aggregation_function(
    comparable_agg_fn: &BooleanExpressionComparableAggregationFunction,
    subgraph: &SubgraphName,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    aggregate_expression: &AggregateExpression,
    scalar_boolean_expression_types: &BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        ResolvedScalarBooleanExpressionType,
    >,
) -> Result<ComparableAggregationFunction, AggregateBooleanExpressionError> {
    // Check that the aggregation function is defined in the aggregation expression
    let agg_fn = aggregate_expression
        .operand
        .aggregation_functions
        .iter()
        .find(|agg_fn| agg_fn.name == comparable_agg_fn.name)
        .ok_or_else(
            || AggregateBooleanExpressionError::AggregationFunctionNotFound {
                aggregation_function_name: comparable_agg_fn.name.clone(),
                aggregate_expression: aggregate_expression_name.clone(),
            },
        )?;

    let agg_fn_bool_exp_name =
        boolean_expressions::BooleanExpressionTypeIdentifier::FromBooleanExpressionType(
            Qualified::new(
                subgraph.clone(),
                comparable_agg_fn.boolean_expression_type.clone(),
            ),
        );

    // Check that the referenced boolean expression for the aggregate function exists
    let agg_fn_bool_exp = scalar_boolean_expression_types
        .get(&agg_fn_bool_exp_name)
        .ok_or_else(
            || AggregateBooleanExpressionError::ScalarBooleanExpressionTypeNotFound {
                boolean_expression_type: agg_fn_bool_exp_name.clone(),
            },
        )?;

    // Check that the aggregate function isn't returning an array, since we don't support that
    let agg_fn_named_return_type = match &agg_fn.return_type.underlying_type {
        QualifiedBaseType::Named(named_type) => Ok(named_type),
        QualifiedBaseType::List(_) => Err(
            AggregateBooleanExpressionError::ComparisonAgainstArrayTypesNotSupported {
                aggregation_function_name: comparable_agg_fn.name.clone(),
                aggregation_function_return_type: agg_fn.return_type.clone(),
            },
        ),
    }?;

    // Check that the referenced boolean expression's type matches the aggregation function return type
    if agg_fn_bool_exp.operand_type != *agg_fn_named_return_type {
        return Err(
            AggregateBooleanExpressionError::AggregationFunctionTypeMismatch {
                aggregation_function_name: comparable_agg_fn.name.clone(),
                aggregation_function_return_type: agg_fn_named_return_type.clone(),
                boolean_expression_type: agg_fn_bool_exp_name.clone(),
                boolean_expression_operand_type: agg_fn_bool_exp.operand_type.clone(),
            },
        );
    }

    Ok(ComparableAggregationFunction {
        aggregate_function_name: comparable_agg_fn.name.clone(),
        description: agg_fn.description.clone(),
        boolean_expression_type: agg_fn_bool_exp_name,
    })
}

fn resolve_object_aggregate_boolean_expression(
    subgraph: &SubgraphName,
    boolean_expression: &open_dds::boolean_expression::BooleanExpressionTypeV1,
    object_aggregate_operand: &open_dds::boolean_expression::BooleanExpressionObjectAggregateOperand,
    scalar_boolean_expression_types: &BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    unresolved_bool_exps: &[QualifiedObject<
        open_dds::boolean_expression::BooleanExpressionTypeV1,
    >],
    unresolved_relationships: &relationships::Relationships,
    unresolved_models: &[QualifiedObject<open_dds::models::Model>],
    aggregate_expressions: &BTreeMap<Qualified<AggregateExpressionName>, AggregateExpression>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithPermissions>,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    issues: &mut Vec<AggregateBooleanExpressionIssue>,
) -> Result<ObjectAggregateBooleanExpression, AggregateBooleanExpressionError> {
    // Check if the specified operand type is actually an object type
    let qualified_operand_custom_type_name =
        Qualified::new(subgraph.clone(), object_aggregate_operand.r#type.clone());
    let (object_operand_type_name, operand_object_type) =
        if let Some(object_type) = object_types.get(&qualified_operand_custom_type_name) {
            Ok((
                QualifiedTypeName::Custom(qualified_operand_custom_type_name.clone()),
                object_type,
            ))
        } else {
            Err(
                AggregateBooleanExpressionError::OperandTypeIsNotAnObjectType {
                    operand_type: qualified_operand_custom_type_name.clone(),
                },
            )
        }?;

    let aggregate_expression_name = Qualified::new(
        subgraph.clone(),
        object_aggregate_operand.aggregate_expression.clone(),
    );

    // Check if the referenced aggregate expression exists
    let aggregate_expression = aggregate_expressions
        .get(&aggregate_expression_name)
        .ok_or_else(
            || AggregateBooleanExpressionError::AggregateExpressionNotFound {
                aggregate_expression: aggregate_expression_name.clone(),
            },
        )?;

    // Check if the operand type matches the operand type in the aggregate expression
    if object_operand_type_name != aggregate_expression.operand.aggregated_type {
        return Err(
            AggregateBooleanExpressionError::AggregateOperandTypeMismatch {
                operand_type: object_operand_type_name,
                aggregate_expression: aggregate_expression_name,
                aggregate_operand: aggregate_expression.operand.aggregated_type.clone(),
            },
        );
    }

    // Check that no comparable fields are duplicated in the list
    check_for_duplicates(&object_aggregate_operand.comparable_fields, |field| {
        &field.field_name
    })
    .map_err(|duplicate_field| {
        AggregateBooleanExpressionError::DuplicateComparableFieldsFound {
            field_name: duplicate_field.field_name.clone(),
        }
    })?;

    // Resolve comparable fields
    let comparable_fields = object_aggregate_operand
        .comparable_fields
        .iter()
        .map(|comparable_field| {
            resolve_comparable_field(
                comparable_field,
                subgraph,
                &qualified_operand_custom_type_name,
                operand_object_type,
                &aggregate_expression_name,
                aggregate_expression,
                scalar_types,
                unresolved_bool_exps,
            )
        })
        .collect::<Result<Vec<_>, AggregateBooleanExpressionError>>()?;

    // Check that no comparable relationships are duplicated in the list
    check_for_duplicates(
        &object_aggregate_operand.comparable_relationships,
        |relationship| &relationship.relationship_name,
    )
    .map_err(|duplicate_relationship| {
        AggregateBooleanExpressionError::DuplicateComparableRelationshipsFound {
            relationship_name: duplicate_relationship.relationship_name.clone(),
        }
    })?;

    // Resolve comparable relationships
    let comparable_relationships = object_aggregate_operand
        .comparable_relationships
        .iter()
        .map(|comparable_relationship| {
            resolve_comparable_relationship(
                comparable_relationship,
                subgraph,
                &qualified_operand_custom_type_name,
                unresolved_relationships,
                unresolved_models,
                unresolved_bool_exps,
            )
        })
        .collect::<Result<Vec<_>, AggregateBooleanExpressionError>>()?
        .into_iter()
        .flatten() // Drop all skipped relationships
        .collect::<Vec<_>>();

    // Resolve the comparison against _count
    let count_aggregation = object_aggregate_operand
        .comparable_count
        .as_ref()
        .map(|comparable_count| {
            resolve_count_aggregation_function(
                comparable_count,
                CountAggregateType::Count,
                subgraph,
                &aggregate_expression_name,
                &aggregate_expression.count,
                boolean_expression,
                scalar_boolean_expression_types,
                graphql_config,
                issues,
            )
        })
        .transpose()?;

    // Resolve the comparison against _count_distinct
    let count_distinct_aggregation = object_aggregate_operand
        .comparable_count_distinct
        .as_ref()
        .map(|comparable_count| {
            resolve_count_aggregation_function(
                comparable_count,
                CountAggregateType::CountDistinct,
                subgraph,
                &aggregate_expression_name,
                &aggregate_expression.count_distinct,
                boolean_expression,
                scalar_boolean_expression_types,
                graphql_config,
                issues,
            )
        })
        .transpose()?;

    let filter_input = object_aggregate_operand
        .filter_input
        .as_ref()
        .map(|filter_input| {
            resolve_object_aggregate_filter_input(
                subgraph,
                filter_input,
                &qualified_operand_custom_type_name,
                unresolved_models,
            )
        })
        .transpose()?;

    // Resolve logical operators
    let logical_operators =
        resolve_logical_operators(boolean_expression, graphql_config, issues, || {
            AggregateBooleanExpressionIssue::MissingLogicalOperatorNamesInGraphqlConfig
        });

    // Resolve graphql configuration
    let graphql = resolve_aggregate_bool_exp_graphql_config(boolean_expression, graphql_types)?;

    // Resolve aggregate predicate graphql configuration
    let predicate_graphql = resolve_aggregate_predicate_bool_exp_graphql_config(
        object_aggregate_operand,
        graphql_types,
    )?;

    // Check to make sure none of the field names conflict with one another
    // (eg. no comparable field name that is the same as a logical operator)
    check_object_operand_graphql_name_conflicts(
        &comparable_fields,
        &comparable_relationships,
        count_aggregation.as_ref(),
        count_distinct_aggregation.as_ref(),
        &logical_operators,
    )?;

    Ok(ObjectAggregateBooleanExpression {
        operand_type: qualified_operand_custom_type_name,
        aggregate_expression: aggregate_expression_name,
        comparable_fields,
        comparable_relationships,
        count_aggregation,
        count_distinct_aggregation,
        filter_input,
        logical_operators,
        graphql,
        predicate_graphql,
    })
}

fn resolve_comparable_field(
    comparable_field: &open_dds::boolean_expression::AggregateBooleanExpressionComparableField,
    subgraph: &SubgraphName,
    operand_type_name: &Qualified<CustomTypeName>,
    operand_object_type: &ObjectTypeWithPermissions,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    aggregate_expression: &AggregateExpression,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    unresolved_bool_exps: &[QualifiedObject<
        open_dds::boolean_expression::BooleanExpressionTypeV1,
    >],
) -> Result<ComparableAggregatableField, AggregateBooleanExpressionError> {
    // Check that the field is declared as a comparable field on the aggregate expression
    let aggregatable_field = aggregate_expression
        .operand
        .aggregatable_fields
        .iter()
        .find(|agg_field| agg_field.field_name == comparable_field.field_name)
        .ok_or_else(
            || AggregateBooleanExpressionError::ComparableFieldNotFound {
                field_name: comparable_field.field_name.clone(),
                aggregate_expression: aggregate_expression_name.clone(),
            },
        )?;

    // Get the field definition for the field from the object type
    let field_definition = operand_object_type
        .object_type
        .fields
        .get(&comparable_field.field_name)
        .ok_or_else(|| {
            // This won't happen in reality, because the aggregate expression
            // has already validated the field's existence
            AggregateBooleanExpressionError::ComparableFieldNotFoundOnObjectType {
                operand_type: operand_type_name.clone(),
                field_name: comparable_field.field_name.clone(),
            }
        })?;

    // Check that the field type is not an array type, we don't support nested arrays
    // (that'd be have to be aggregating an aggregate... ie nested aggregation)
    let field_type_name = match &field_definition.field_type.underlying_type {
        QualifiedBaseType::Named(type_name) => Ok(type_name),
        QualifiedBaseType::List(_) => Err(
            AggregateBooleanExpressionError::ComparableFieldNestedArrayTypeNotSupported {
                field_name: comparable_field.field_name.clone(),
                field_type: field_definition.field_type.clone(),
            },
        ),
    }?;

    let field_type_is_a_scalar_type = match field_type_name {
        QualifiedTypeName::Inbuilt(_) => true,
        QualifiedTypeName::Custom(custom_name) => scalar_types.contains_key(custom_name),
    };

    let comparable_field_bool_exp_name = Qualified::new(
        subgraph.clone(),
        comparable_field.aggregate_boolean_expression_type.clone(),
    );

    // Find the referenced boolean expression type for the comparable field
    // We have to do it with unresolved bool exps because not all boolean exp types have
    // been resolved (including aggregates, obviously) at this point in the process
    let comparable_field_bool_exp = unresolved_bool_exps
        .iter()
        .find(|q| {
            q.subgraph == *subgraph
                && q.object.name == comparable_field.aggregate_boolean_expression_type
        })
        .map(|q| &q.object)
        .ok_or_else(|| {
            AggregateBooleanExpressionError::ComparableFieldBooleanExpressionNotFound {
                field_name: comparable_field.field_name.clone(),
                boolean_expression_type: comparable_field_bool_exp_name.clone(),
            }
        })?;

    // Get the referenced boolean expression's operand type and make sure the operand is the right kind of
    // operand that matches the field type (ie objectAggregate or scalarAggregate)
    let comparable_field_bool_exp_operand_type = match (
        field_type_is_a_scalar_type,
        &comparable_field_bool_exp.operand,
    ) {
        (
            true,
            open_dds::boolean_expression::BooleanExpressionOperand::ScalarAggregate(operand),
        ) => mk_qualified_type_name(&operand.r#type, subgraph),
        (
            false,
            open_dds::boolean_expression::BooleanExpressionOperand::ObjectAggregate(operand),
        ) => QualifiedTypeName::Custom(Qualified::new(subgraph.clone(), operand.r#type.clone())),
        _ => return Err(
            AggregateBooleanExpressionError::ComparableFieldBooleanExpressionIncorrectOperandType {
                boolean_expression_type: comparable_field_bool_exp_name.clone(),
                aggregate_operand_type: if field_type_is_a_scalar_type {
                    AggregateOperandType::ScalarAggregate
                } else {
                    AggregateOperandType::ObjectAggregate
                },
                field_name: comparable_field.field_name.clone(),
                field_type: field_type_name.clone(),
            },
        ),
    };

    // Check that the type of the field matches the operand type of the referenced boolean expression
    if *field_type_name != comparable_field_bool_exp_operand_type {
        return Err(
            AggregateBooleanExpressionError::ComparableFieldBooleanExpressionTypeMismatch {
                field_name: comparable_field.field_name.clone(),
                field_type: field_type_name.clone(),
                boolean_expression_type: comparable_field_bool_exp_name.clone(),
                boolean_expression_operand_type: comparable_field_bool_exp_operand_type,
            },
        );
    }

    Ok(ComparableAggregatableField {
        field_name: comparable_field.field_name.clone(),
        description: aggregatable_field.description.clone(),
        aggregate_boolean_expression_type: comparable_field_bool_exp_name,
    })
}

fn resolve_comparable_relationship(
    comparable_relationship: &open_dds::boolean_expression::AggregateBooleanExpressionComparableRelationship,
    subgraph: &SubgraphName,
    operand_type_name: &Qualified<CustomTypeName>,
    unresolved_relationships: &relationships::Relationships,
    unresolved_models: &[QualifiedObject<open_dds::models::Model>],
    unresolved_bool_exps: &[QualifiedObject<
        open_dds::boolean_expression::BooleanExpressionTypeV1,
    >],
) -> Result<Option<ComparableAggregatableRelationship>, AggregateBooleanExpressionError> {
    // Check to make sure the comparable relationship exists
    // We have to use unresolved data here because relationships are resolved in a later step to this
    let relationship = unresolved_relationships
        .get(
            operand_type_name,
            &comparable_relationship.relationship_name,
        )
        .map_err(
            |_err| AggregateBooleanExpressionError::ComparableRelationshipNotFound {
                operand_type: operand_type_name.clone(),
                relationship_name: comparable_relationship.relationship_name.clone(),
            },
        )?;

    // If the relationship is to an unknown subgraph, skip it
    let relationship = match relationship {
        relationships::Relationship::RelationshipToUnknownSubgraph => return Ok(None),
        relationships::Relationship::Relationship(relationship) => *relationship,
    };

    // Check that the relationship is targeting a model, because we don't support targeting a command
    let model_target =
        match &relationship.target {
            open_dds::relationships::RelationshipTarget::Model(model_target) => model_target,
            open_dds::relationships::RelationshipTarget::Command(_) => return Err(
                AggregateBooleanExpressionError::ComparableRelationshipCommandTargetNotSupported {
                    operand_type: operand_type_name.clone(),
                    relationship_name: comparable_relationship.relationship_name.clone(),
                },
            ),
        };

    // Check that the relationship type is an object relationship only
    match &model_target.relationship_type {
        open_dds::relationships::RelationshipType::Object => (),
        open_dds::relationships::RelationshipType::Array => {
            return Err(
                AggregateBooleanExpressionError::ComparableArrayRelationshipNotSupported {
                    operand_type: operand_type_name.clone(),
                    relationship_name: comparable_relationship.relationship_name.clone(),
                },
            );
        }
    }

    // Get the relationship's targeted model's object type
    // We have to use unresolved models here because models are resolved after boolean expressions
    let model_target_subgraph = model_target.subgraph(); // Not inlined because lifetime
    let model_target_subgraph = model_target_subgraph.as_ref().unwrap_or(subgraph);
    let relationship_target_object_type = unresolved_models
        .iter()
        .find(|q| q.subgraph == *model_target_subgraph && *q.object.name() == model_target.name)
        .map(|q| {
            Qualified::new(
                model_target_subgraph.clone(),
                q.object.object_type().value.clone(),
            )
        })
        .ok_or_else(|| {
            AggregateBooleanExpressionError::ComparableRelationshipTargetModelNotFound {
                operand_type: operand_type_name.clone(),
                relationship_name: comparable_relationship.relationship_name.clone(),
                target_model_name: Qualified::new(
                    model_target_subgraph.clone(),
                    model_target.name.clone(),
                ),
            }
        })?;

    // Get the boolean expression for the comparable relationship
    let comparable_relationship_bool_exp_type = Qualified::new(
        model_target_subgraph.clone(),
        comparable_relationship
            .aggregate_boolean_expression_type
            .clone(),
    );
    let comparable_relationship_bool_exp = unresolved_bool_exps
        .iter()
        .find(|q| {
            q.subgraph == *model_target_subgraph
                && q.object.name == comparable_relationship.aggregate_boolean_expression_type
        })
        .map(|q| &q.object)
        .ok_or_else(|| {
            AggregateBooleanExpressionError::ComparableRelationshipBooleanExpressionNotFound {
                operand_type: operand_type_name.clone(),
                relationship_name: comparable_relationship.relationship_name.clone(),
                boolean_expression_type: comparable_relationship_bool_exp_type.clone(),
            }
        })?;

    // Ensure the boolean expression has an objectAggregate operand
    let comparable_relationship_bool_exp_operand_type = match &comparable_relationship_bool_exp
        .operand
    {
        open_dds::boolean_expression::BooleanExpressionOperand::ObjectAggregate(operand) => {
            Qualified::new(model_target_subgraph.clone(), operand.r#type.clone())
        }
        _ => {
            return Err(AggregateBooleanExpressionError::ComparableRelationshipBooleanExpressionIncorrectOperandType {
            relationship_name: comparable_relationship.relationship_name.clone(),
            boolean_expression_type: comparable_relationship_bool_exp_type.clone(),
        });
        }
    };

    // Check that the objectAggregate operand type matches the relationship target model object type
    if relationship_target_object_type != comparable_relationship_bool_exp_operand_type {
        return Err(
            AggregateBooleanExpressionError::ComparableRelationshipBooleanExpressionTypeMismatch {
                relationship_name: comparable_relationship.relationship_name.clone(),
                relationship_target_object_type,
                boolean_expression_type: comparable_relationship_bool_exp_type.clone(),
                boolean_expression_operand_type: comparable_relationship_bool_exp_operand_type,
            },
        );
    }

    Ok(Some(ComparableAggregatableRelationship {
        relationship_name: comparable_relationship.relationship_name.clone(),
        description: relationship.description.clone(),
        aggregate_boolean_expression_type: comparable_relationship_bool_exp_type.clone(),
    }))
}

fn resolve_count_aggregation_function(
    comparable_count: &AggregateBooleanExpressionComparableCount,
    count_type: CountAggregateType,
    subgraph: &SubgraphName,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    aggregate_count_definition: &AggregateCountDefinition,
    boolean_expression: &open_dds::boolean_expression::BooleanExpressionTypeV1,
    scalar_boolean_expression_types: &BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        ResolvedScalarBooleanExpressionType,
    >,
    graphql_config: &graphql_config::GraphqlConfig,
    issues: &mut Vec<AggregateBooleanExpressionIssue>,
) -> Result<ComparableCountAggregation, AggregateBooleanExpressionError> {
    // Check that the count aggregate is enabled on the aggregate expression
    // If not, we can't use it in a boolean expression
    if !aggregate_count_definition.enable {
        return Err(AggregateBooleanExpressionError::CountAggregateNotEnabled {
            count_type,
            aggregate_expression: aggregate_expression_name.clone(),
        });
    }

    let count_bool_exp_name =
        boolean_expressions::BooleanExpressionTypeIdentifier::FromBooleanExpressionType(
            Qualified::new(
                subgraph.clone(),
                comparable_count.boolean_expression_type.clone(),
            ),
        );

    // Check that the boolean expression used for comparison to the count result exists
    let count_bool_exp = scalar_boolean_expression_types
        .get(&count_bool_exp_name)
        .ok_or_else(
            || AggregateBooleanExpressionError::ScalarBooleanExpressionTypeNotFound {
                boolean_expression_type: count_bool_exp_name.clone(),
            },
        )?;

    // TODO(dchambers): Currently count aggregations are hardcoded to use Int.
    // This needs to be made customizable. See APIPG-873
    let count_aggregate_type = QualifiedTypeName::Inbuilt(InbuiltType::Int);

    // Check that the operand type of the boolean expression used for comparison to
    // the count result is the correct type
    if count_bool_exp.operand_type != count_aggregate_type {
        return Err(
            AggregateBooleanExpressionError::CountAggregateTypeMismatch {
                count_type,
                count_return_type: count_aggregate_type,
                boolean_expression_type: count_bool_exp_name,
                boolean_expression_operand_type: count_bool_exp.operand_type.clone(),
            },
        );
    }

    // Resolve the graphql field name
    let graphql = graphql_config
        .query
        .aggregate_config
        .as_ref()
        .map(|config| ComparableCountGraphqlConfig {
            field_name: match count_type {
                CountAggregateType::Count => config.count_field_name.clone(),
                CountAggregateType::CountDistinct => config.count_distinct_field_name.clone(),
            },
        });

    // If they've enabled graphql for this boolean expression, but the count field names
    // have been omitted from the GraphqlConfig, raise an issue because this is probably a mistake
    if boolean_expression.graphql.is_some() && graphql.is_none() {
        issues.push(
            AggregateBooleanExpressionIssue::MissingCountAggregationNamesInGraphqlConfig {
                count_type,
            },
        );
    }

    Ok(ComparableCountAggregation {
        boolean_expression_type: count_bool_exp_name,
        graphql,
    })
}

fn resolve_object_aggregate_filter_input(
    subgraph: &SubgraphName,
    filter_input: &open_dds::boolean_expression::ObjectAggregateFilterInput,
    operand_type: &Qualified<CustomTypeName>,
    unresolved_models: &[QualifiedObject<open_dds::models::Model>],
) -> Result<FilterInputDefinition, AggregateBooleanExpressionError> {
    match filter_input {
        open_dds::boolean_expression::ObjectAggregateFilterInput::Model(model_filter_input) => {
            let model_name =
                Qualified::new(subgraph.clone(), model_filter_input.model_name.clone());

            // Make sure the specified model actually exists
            let model = unresolved_models
                .iter()
                .find(|q| q.subgraph == model_name.subgraph && *q.object.name() == model_name.name)
                .map(|q| &q.object)
                .ok_or_else(
                    || AggregateBooleanExpressionError::FilterInputModelNotFound {
                        model_name: model_name.clone(),
                    },
                )?;

            // Check that the type of the model matches the operand type.
            // We can assume the subgraphs match because the model is from the same subgraph as this
            // object aggregate operand
            if operand_type.name != model.object_type().value {
                return Err(
                    AggregateBooleanExpressionError::FilterInputModelTypeMismatch {
                        operand_type: operand_type.clone(),
                        model_name,
                        model_type: Qualified::new(
                            subgraph.clone(),
                            model.object_type().value.clone(),
                        ),
                    },
                );
            }

            Ok(FilterInputDefinition::FromModel(
                FromModelFilterInputDefinition { model_name },
            ))
        }
    }
}

fn check_scalar_operand_graphql_name_conflicts(
    aggregation_functions: &[ComparableAggregationFunction],
    count_aggregation: Option<&ComparableCountAggregation>,
    count_distinct_aggregation: Option<&ComparableCountAggregation>,
    logical_operators: &LogicalOperators,
) -> Result<(), AggregateBooleanExpressionError> {
    // Uniqueness of these names is already guaranteed
    let used_names = aggregation_functions
        .iter()
        .map(|f| {
            (
                f.aggregate_function_name.as_str(),
                NameSource::ComparableAggregationFunction,
            )
        })
        .collect::<BTreeMap<&str, NameSource>>();

    check_graphql_name_conflicts(
        used_names,
        count_aggregation,
        count_distinct_aggregation,
        logical_operators,
    )
}

fn check_object_operand_graphql_name_conflicts(
    aggregatable_fields: &[ComparableAggregatableField],
    aggregatable_relationships: &[ComparableAggregatableRelationship],
    count_aggregation: Option<&ComparableCountAggregation>,
    count_distinct_aggregation: Option<&ComparableCountAggregation>,
    logical_operators: &LogicalOperators,
) -> Result<(), AggregateBooleanExpressionError> {
    // Uniqueness of these names is already guaranteed
    let used_names = aggregatable_fields
        .iter()
        .map(|f| {
            (
                f.field_name.as_str(),
                NameSource::ComparableAggregatableField,
            )
        })
        .chain(aggregatable_relationships.iter().map(|r| {
            (
                r.relationship_name.as_str(),
                NameSource::ComparableAggregatableRelationship,
            )
        }))
        .collect::<BTreeMap<&str, NameSource>>();

    check_graphql_name_conflicts(
        used_names,
        count_aggregation,
        count_distinct_aggregation,
        logical_operators,
    )
}

fn check_graphql_name_conflicts<'a>(
    mut used_names: BTreeMap<&'a str, NameSource>,
    count_aggregation: Option<&'a ComparableCountAggregation>,
    count_distinct_aggregation: Option<&'a ComparableCountAggregation>,
    logical_operators: &'a LogicalOperators,
) -> Result<(), AggregateBooleanExpressionError> {
    // The count aggregate names
    let count_aggregate_function_names = [
        count_aggregation
            .as_ref()
            .and_then(|c| c.graphql.as_ref())
            .map(|c| (c.field_name.as_str(), NameSource::CountAggregationFunction)),
        count_distinct_aggregation
            .as_ref()
            .and_then(|c| c.graphql.as_ref())
            .map(|c| {
                (
                    c.field_name.as_str(),
                    NameSource::CountDistinctAggregationFunction,
                )
            }),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>();

    // The logical operator names
    let logical_operator_names = match logical_operators {
        LogicalOperators::Include { graphql } => graphql
            .as_ref()
            .map(|g| {
                vec![
                    (g.and_operator_name.as_str(), NameSource::LogicalOperator),
                    (g.or_operator_name.as_str(), NameSource::LogicalOperator),
                    (g.not_operator_name.as_str(), NameSource::LogicalOperator),
                ]
            })
            .unwrap_or_default(),
        LogicalOperators::Exclude => vec![],
    };

    // See whether any of the names conflict with the already-used names
    let names_to_add = count_aggregate_function_names
        .into_iter()
        .chain(logical_operator_names);
    for (name, name_source) in names_to_add {
        if let Some(conflicting_name_source) = used_names.insert(name, name_source) {
            return Err(AggregateBooleanExpressionError::GraphqlNameConflict {
                name: name.to_owned(),
                name_source_1: conflicting_name_source,
                name_source_2: name_source,
            });
        }
    }

    Ok(())
}
