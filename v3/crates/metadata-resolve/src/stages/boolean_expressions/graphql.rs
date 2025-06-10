use super::error::BooleanExpressionError;
use super::types::FieldNameSource;
use super::{
    BooleanExpressionComparableRelationship, BooleanExpressionIssue, IncludeLogicalOperators,
    helpers,
};
pub use super::{
    BooleanExpressionGraphqlConfig, BooleanExpressionGraphqlFieldConfig,
    BooleanExpressionTypeIdentifier, ComparableFieldKind, ObjectBooleanExpressionGraphqlConfig,
    ScalarBooleanExpressionGraphqlConfig,
};
use crate::Qualified;
use crate::helpers::types::mk_name;
use crate::stages::{
    graphql_config,
    scalar_boolean_expressions::{self, LogicalOperatorsGraphqlConfig},
};
use lang_graphql::ast::common::{self as ast};
use open_dds::types::{CustomTypeName, FieldName, GraphQlTypeName};
use std::collections::BTreeMap;

// validate graphql config
// we use the raw boolean expression types for lookup
pub(crate) fn resolve_object_boolean_graphql(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    boolean_expression_graphql_name: &GraphQlTypeName,
    comparable_fields: &BTreeMap<FieldName, (ComparableFieldKind, BooleanExpressionTypeIdentifier)>,
    comparable_relationships: &BTreeMap<FieldName, BooleanExpressionComparableRelationship>,
    include_logical_operators: IncludeLogicalOperators,
    scalar_boolean_expression_types: &BTreeMap<
        BooleanExpressionTypeIdentifier,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &super::object::RawBooleanExpressionTypes,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    issues: &mut Vec<BooleanExpressionIssue>,
) -> Result<BooleanExpressionGraphqlConfig, BooleanExpressionError> {
    let boolean_expression_graphql_name =
        mk_name(boolean_expression_graphql_name.as_ref()).map(ast::TypeName)?;

    graphql_types.store(Some(&boolean_expression_graphql_name))?;

    let mut scalar_fields = BTreeMap::new();

    let mut object_fields = BTreeMap::new();

    let filter_graphql_config =
        graphql_config.query.filter_input_config.as_ref().ok_or({
            graphql_config::GraphqlConfigError::MissingFilterInputFieldInGraphqlConfig
        })?;

    for (comparable_field_name, (comparable_field_kind, comparable_field_type_name)) in
        comparable_fields
    {
        match comparable_field_kind {
            ComparableFieldKind::Scalar | ComparableFieldKind::ScalarArray => {
                if let Some(scalar_boolean_expression_type) =
                    scalar_boolean_expression_types.get(comparable_field_type_name)
                {
                    // Generate comparison expression for fields mapped to simple scalar type
                    if let Some(graphql_name) = &scalar_boolean_expression_type.graphql_name {
                        let graphql_type_name =
                            mk_name(graphql_name.as_str()).map(ast::TypeName)?;

                        // Register scalar comparison field only if it contains non-zero operators.
                        if !scalar_boolean_expression_type
                            .comparison_operators
                            .is_empty()
                            || matches!(
                                scalar_boolean_expression_type.is_null_operator,
                                scalar_boolean_expressions::IsNullOperator::Include { graphql: _ }
                            )
                        {
                            scalar_fields.insert(
                                comparable_field_name.clone(),
                                ScalarBooleanExpressionGraphqlConfig {
                                    type_name: graphql_type_name.clone(),
                                    is_null_operator_name: match scalar_boolean_expression_type
                                        .is_null_operator
                                    {
                                        scalar_boolean_expressions::IsNullOperator::Include {
                                            graphql: _,
                                        } => Some(
                                            // We draw the graphql name from filter_graphql_config rather than Include's graphql
                                            // because we've already hard-validated its presence above.
                                            filter_graphql_config.operator_names.is_null.clone(),
                                        ),
                                        scalar_boolean_expressions::IsNullOperator::Exclude => None,
                                    },
                                },
                            );
                        }
                    }
                }
            }
            ComparableFieldKind::Object | ComparableFieldKind::ObjectArray => {
                // if this field isn't a scalar, let's see if it's an object instead
                if let Some((_field_subgraph, raw_boolean_expression_type)) =
                    match &comparable_field_type_name {
                        BooleanExpressionTypeIdentifier::FromBooleanExpressionType(
                            comparable_field_type_name,
                        ) => Some(helpers::lookup_raw_boolean_expression(
                            boolean_expression_type_name,
                            comparable_field_type_name,
                            raw_boolean_expression_types,
                        )?),
                        BooleanExpressionTypeIdentifier::FromDataConnectorScalarRepresentation(
                            _,
                        ) => None,
                    }
                {
                    if let Some(graphql_name) = raw_boolean_expression_type
                        .graphql
                        .as_ref()
                        .map(|gql| gql.type_name.clone())
                    {
                        let graphql_type_name =
                            mk_name(graphql_name.as_str()).map(ast::TypeName)?;

                        object_fields.insert(
                            comparable_field_name.clone(),
                            ObjectBooleanExpressionGraphqlConfig {
                                graphql_type_name: graphql_type_name.clone(),
                            },
                        );
                    }
                }
            }
        }
    }

    let logical_operators = LogicalOperatorsGraphqlConfig {
        and_operator_name: filter_graphql_config.operator_names.and.clone(),
        or_operator_name: filter_graphql_config.operator_names.or.clone(),
        not_operator_name: filter_graphql_config.operator_names.not.clone(),
    };

    check_graphql_field_name_conflicts(
        comparable_fields,
        comparable_relationships,
        include_logical_operators,
        &logical_operators,
        boolean_expression_type_name,
        issues,
    );

    Ok(BooleanExpressionGraphqlConfig {
        type_name: boolean_expression_graphql_name,
        scalar_fields,
        object_fields,
        field_config: (BooleanExpressionGraphqlFieldConfig {
            where_field_name: filter_graphql_config.where_field_name.clone(),
            logical_operators,
        }),
    })
}

fn check_graphql_field_name_conflicts(
    comparable_fields: &BTreeMap<FieldName, (ComparableFieldKind, BooleanExpressionTypeIdentifier)>,
    comparable_relationships: &BTreeMap<FieldName, BooleanExpressionComparableRelationship>,
    include_logical_operators: IncludeLogicalOperators,
    logical_operators_graphql_config: &LogicalOperatorsGraphqlConfig,
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    issues: &mut Vec<BooleanExpressionIssue>,
) {
    // Comparable field names
    let mut used_names = comparable_fields
        .keys()
        .map(|operator_name| (operator_name.as_str(), FieldNameSource::ComparableField))
        .collect::<BTreeMap<_, _>>();

    // Comparable relationship names
    let relationship_names = comparable_relationships.keys().map(|relationship_name| {
        (
            relationship_name.as_str(),
            FieldNameSource::ComparableRelationship,
        )
    });

    // The logical operator names
    let logical_operator_names = match include_logical_operators {
        IncludeLogicalOperators::Yes => vec![
            (
                logical_operators_graphql_config.and_operator_name.as_str(),
                FieldNameSource::LogicalOperator,
            ),
            (
                logical_operators_graphql_config.or_operator_name.as_str(),
                FieldNameSource::LogicalOperator,
            ),
            (
                logical_operators_graphql_config.not_operator_name.as_str(),
                FieldNameSource::LogicalOperator,
            ),
        ],
        IncludeLogicalOperators::No => vec![],
    };

    // See whether any of the names conflict with the already-used names
    let names_to_add = logical_operator_names.into_iter().chain(relationship_names);
    for (name, name_source) in names_to_add {
        if let Some(conflicting_name_source) = used_names.insert(name, name_source) {
            issues.push(BooleanExpressionIssue::GraphqlFieldNameConflict {
                type_name: boolean_expression_type_name.clone(),
                name: name.to_owned(),
                name_source_1: conflicting_name_source,
                name_source_2: name_source,
            });
        }
    }
}
