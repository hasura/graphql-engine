use crate::types::error::TypePredicateError;

use crate::QualifiedTypeReference;
use crate::stages::{
    boolean_expressions, data_connectors, models, object_relationships, object_types,
    scalar_boolean_expressions,
};
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use open_dds::{
    models::ModelName,
    types::{CustomTypeName, FieldName},
};

use std::collections::{BTreeMap, HashSet};

// we want to ensure that our `BooleanExpressionType` (and all it's leaves)
// are compatible with our data connector
// we want to know
// a) does each scalar have mappings for our data connector?
// b) if we used nested objects, does the data connector have the correct capability?
// c) if we used nested arrays, does the data connector have the correct capability?
pub(crate) fn validate_data_connector_with_object_boolean_expression_type(
    data_connector: &data_connectors::DataConnectorLink,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    object_boolean_expression_type: &boolean_expressions::ResolvedObjectBooleanExpressionType,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    flags: &open_dds::flags::OpenDdFlags,
) -> Result<
    Vec<boolean_expressions::BooleanExpressionIssue>,
    boolean_expressions::BooleanExpressionError,
> {
    // Keep track of visited boolean expression types to avoid infinite recursion
    // of circular references of nested fields in the object boolean expressions
    let mut visited_object_boolean_expressions = HashSet::new();
    validate_data_connector_with_object_boolean_expression_type_internal(
        data_connector,
        source_type_mappings,
        object_boolean_expression_type,
        boolean_expression_types,
        object_types,
        models,
        flags,
        &mut visited_object_boolean_expressions,
    )
}

fn validate_data_connector_with_object_boolean_expression_type_internal(
    data_connector: &data_connectors::DataConnectorLink,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    object_boolean_expression_type: &boolean_expressions::ResolvedObjectBooleanExpressionType,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    flags: &open_dds::flags::OpenDdFlags,
    visited_object_boolean_expressions: &mut HashSet<Qualified<CustomTypeName>>,
) -> Result<
    Vec<boolean_expressions::BooleanExpressionIssue>,
    boolean_expressions::BooleanExpressionError,
> {
    // collect any issues found whilst resolving
    let mut issues = vec![];

    // Only validate once per object boolean expression
    if !visited_object_boolean_expressions.insert(object_boolean_expression_type.name.clone()) {
        return Ok(issues);
    }

    if let Some(fields) = object_boolean_expression_type.get_fields(flags) {
        for (field_name, object_comparison_expression_info) in &fields.object_fields {
            // look up the leaf boolean expression type
            let leaf_boolean_expression = boolean_expression_types
                .objects
                .get(&object_comparison_expression_info.boolean_expression_type_name)
                .ok_or_else(|| {
                    boolean_expressions::BooleanExpressionError::BooleanExpressionCouldNotBeFound {
                        parent_boolean_expression: object_boolean_expression_type.name.clone(),
                        child_boolean_expression: object_comparison_expression_info
                            .boolean_expression_type_name
                            .clone(),
                    }
                })?;

            match object_comparison_expression_info.field_kind {
                boolean_expressions::ObjectComparisonKind::Object => {
                    // throw an error if our data connector does not support filtering nested
                    // objects
                    if !data_connector.capabilities.supports_nested_object_filtering {
                        let field_type = get_field_type(
                            field_name,
                            object_types,
                            object_boolean_expression_type,
                        )?;
                        return Err(boolean_expressions::BooleanExpressionError::DataConnectorDoesNotSupportNestedObjectFiltering {
                            data_connector_name: data_connector.name.clone(),
                            boolean_expression_type_name: object_boolean_expression_type.name.clone(),
                            field_name: field_name.clone(),
                            field_type: field_type.clone(),
                            });
                    }
                }
                boolean_expressions::ObjectComparisonKind::ObjectArray => {
                    // raise a warning if our data connector does not support filtering nested arrays
                    if !data_connector
                        .capabilities
                        .supports_nested_object_array_filtering
                    {
                        let field_type = get_field_type(
                            field_name,
                            object_types,
                            object_boolean_expression_type,
                        )?;
                        issues.push(boolean_expressions::BooleanExpressionIssue::DataConnectorDoesNotSupportNestedObjectArrayFiltering {
                            data_connector_name: data_connector.name.clone(),
                            boolean_expression_type_name: object_boolean_expression_type.name.clone(),
                            field_name: field_name.clone(),
                            field_type: field_type.clone(),
                        });
                    }
                }
            }

            // If the connector does not support nested relationships in filtering then
            // this nested object field should not contain any relationship comparisons
            let supports_nested_relationships_in_filtering = data_connector
                .capabilities
                .supports_relationships
                .as_ref()
                .is_some_and(|r| {
                    r.supports_nested_relationships
                        .as_ref()
                        .is_some_and(|n| n.supports_nested_in_filtering)
                });
            if !supports_nested_relationships_in_filtering
                && !leaf_boolean_expression
                    .fields
                    .relationship_fields
                    .is_empty()
            {
                return Err(boolean_expressions::BooleanExpressionError::DataConnectorDoesNotSupportNestedRelationshipFiltering {
                    data_connector_name: data_connector.name.clone(),
                    parent_boolean_expression_type_name: object_boolean_expression_type.name.clone(),
                    field_name: field_name.clone(),
                    nested_boolean_expression_type_name: object_comparison_expression_info.boolean_expression_type_name.clone(),
                });
            }

            // continue checking the nested object...
            issues.extend(
                validate_data_connector_with_object_boolean_expression_type_internal(
                    data_connector,
                    source_type_mappings,
                    leaf_boolean_expression,
                    boolean_expression_types,
                    object_types,
                    models,
                    flags,
                    visited_object_boolean_expressions,
                )?,
            );
        }

        for (field_name, comparison_expression_info) in
            &object_boolean_expression_type.fields.scalar_fields
        {
            let boolean_expression_type_name =
                &comparison_expression_info.boolean_expression_type_name;

            let leaf_boolean_expression = boolean_expression_types
                    .scalars
                    .get(boolean_expression_type_name)
                    .ok_or_else(|| {
                        boolean_expressions::BooleanExpressionError::ScalarBooleanExpressionCouldNotBeFound {
                            parent_boolean_expression: object_boolean_expression_type.name.clone(),
                            child_boolean_expression: boolean_expression_type_name.clone(),
                        }
                    })?;

            match comparison_expression_info.field_kind {
                boolean_expressions::ScalarComparisonKind::ScalarArray => {
                    if !data_connector
                        .capabilities
                        .supports_nested_scalar_array_filtering
                    {
                        let field_type = get_field_type(
                            field_name,
                            object_types,
                            object_boolean_expression_type,
                        )?;
                        issues.push(boolean_expressions::BooleanExpressionIssue::DataConnectorDoesNotSupportNestedScalarArrayFiltering {
                                data_connector_name: data_connector.name.clone(),
                                boolean_expression_type_name: object_boolean_expression_type.name.clone(),
                                field_name: field_name.clone(),
                                field_type: field_type.clone(),
                            });
                    }
                }
                boolean_expressions::ScalarComparisonKind::Scalar => {}
            }

            // check scalar type
            validate_data_connector_with_scalar_boolean_expression_type(
                leaf_boolean_expression,
                &object_boolean_expression_type.name,
                data_connector,
                field_name,
            )?;
        }

        for comparable_relationship in object_boolean_expression_type
            .fields
            .relationship_fields
            .values()
        {
            validate_data_connector_with_comparable_relationship(
                data_connector,
                source_type_mappings,
                object_boolean_expression_type,
                comparable_relationship,
                object_types,
                models,
            )?;
        }
    }
    Ok(issues)
}

fn get_field_type<'a>(
    field_name: &FieldName,
    object_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    object_boolean_expression_type: &boolean_expressions::ResolvedObjectBooleanExpressionType,
) -> Result<&'a QualifiedTypeReference, boolean_expressions::BooleanExpressionError> {
    let operand_object_type = object_types
        .get(&object_boolean_expression_type.object_type)
        .ok_or_else(|| {
            boolean_expressions::BooleanExpressionError::UnknownTypeInObjectBooleanExpressionType {
                type_name: object_boolean_expression_type.name.clone(),
                boolean_expression_type_name: object_boolean_expression_type.name.clone(),
            }
        })?;

    operand_object_type
        .object_type
        .fields
        .get(field_name)
        .map(|field_definition| &field_definition.field_type)
        .ok_or_else(|| {
            boolean_expressions::BooleanExpressionError::UnknownFieldInObjectBooleanExpressionType {
                field_name: field_name.clone(),
                object_boolean_expression_type: object_boolean_expression_type.name.clone(),
            }
        })
}

// validate comparable relationship field against data connector
// for now, this means checking that a) the target has a source and b) that source is the same
// connector as the source of the boolean expression
fn validate_data_connector_with_comparable_relationship(
    data_connector: &data_connectors::DataConnectorLink,
    source_type_mappings: &BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    object_boolean_expression_type: &boolean_expressions::ResolvedObjectBooleanExpressionType,
    comparable_relationship: &boolean_expressions::BooleanExpressionComparableRelationship,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
) -> Result<(), boolean_expressions::BooleanExpressionError> {
    // if there isn't actually a boolean expression type here then
    // we don't need to validate anything
    if comparable_relationship.boolean_expression_type.is_none() {
        return Ok(());
    }

    let underlying_object = object_types
        .get(&object_boolean_expression_type.object_type)
        .ok_or_else(|| {
            boolean_expressions::BooleanExpressionError::UnknownTypeInObjectBooleanExpressionType {
                type_name: object_boolean_expression_type.object_type.clone(),
                boolean_expression_type_name: object_boolean_expression_type.name.clone(),
            }
        })?;

    let relationship = underlying_object
        .relationship_fields
        .get(&comparable_relationship.relationship_name)
        .ok_or_else(|| boolean_expressions::BooleanExpressionError::UnknownRelationshipInObjectBooleanExpressionType {
            relationship_name: comparable_relationship.relationship_name.clone(),
            object_boolean_expression_type: object_boolean_expression_type.name.clone(),
            type_name: object_boolean_expression_type.object_type.clone(),
        })?;

    if let object_relationships::RelationshipTarget::Model(relationship_target_model) =
        &relationship.target
    {
        let target_model = models
            .get(&relationship_target_model.model_name)
            .ok_or_else(
                || TypePredicateError::UnknownModelUsedInRelationshipTypePredicate {
                    type_name: object_boolean_expression_type.object_type.clone(),
                    target_model_name: relationship_target_model.model_name.clone(),
                    relationship_name: comparable_relationship.relationship_name.clone(),
                },
            )?;

        match &target_model.source {
            Some(target_model_source) => {
                let execution_strategy =
                    boolean_expressions::get_comparable_relationship_execution_strategy(
                        &data_connector.name,
                        &target_model_source.data_connector.name,
                        target_model_source
                            .data_connector
                            .capabilities
                            .supports_relationships
                            .as_ref(),
                    );
                match execution_strategy {
                    // If the comparable relationship would be evaluated using a remote predicate
                    boolean_expressions::ComparableRelationshipExecutionStrategy::InEngine => {
                        let source_type_mapping = source_type_mappings
                            .get(&object_boolean_expression_type.object_type)
                            .ok_or_else(|| TypePredicateError::UnknownTypeMapping {
                                type_name: object_boolean_expression_type.object_type.clone(),
                                data_connector_name: data_connector.name.clone(),
                            })?;
                        let source_field_mappings = match source_type_mapping {
                            object_types::TypeMapping::Object { field_mappings, .. } => {
                                field_mappings
                            }
                        };

                        // Validate each join mapping
                        for relationship_mapping in &relationship_target_model.mappings {
                            let source_field = &relationship_mapping.source_field.field_name;
                            let object_types::FieldMapping {
                                column: source_ndc_column,
                                comparison_operators,
                                ..
                            } = source_field_mappings.get(source_field).ok_or_else(|| {
                                TypePredicateError::UnknownFieldMapping {
                                    type_name: object_boolean_expression_type.object_type.clone(),
                                    field_name: source_field.clone(),
                                    data_connector_name: data_connector.name.clone(),
                                }
                            })?;

                            // Check that the source field in the mapping has an equality operator
                            let source_field_type_has_equal_operator = comparison_operators
                                .as_ref()
                                .is_some_and(|ops| ops.eq_operator.is_some());

                            if !source_field_type_has_equal_operator {
                                return Err(TypePredicateError::MissingEqualOperator {
                                    location: format!(
                                        "While resolving comparable relationship {0}",
                                        comparable_relationship.relationship_name
                                    ),
                                    type_name: object_boolean_expression_type.object_type.clone(),
                                    field_name: source_field.clone(),
                                    ndc_column: source_ndc_column.clone(),
                                    data_connector_name: data_connector.name.clone(),
                                }
                                .into());
                            }

                            // Check that the target of the mapping is not an argument.
                            // Arguments are not supported in remote predicates since we can't evaluate the
                            // model without a value for the argument, which we won't have when the remote
                            // predicate executes
                            match &relationship_mapping.target {
                                object_relationships::RelationshipModelMappingTarget::Argument(argument_target) => {
                                    return Err(boolean_expressions::BooleanExpressionError::RemoteComparableRelationshipWithArgumentMappingTargetNotSupported {
                                        boolean_expression_type_name: object_boolean_expression_type.name.clone(),
                                        relationship_name: comparable_relationship.relationship_name.clone(),
                                        source_type: object_boolean_expression_type.object_type.clone(),
                                        source_field: source_field.clone(),
                                        target_argument: argument_target.clone(),
                                    });
                                }
                                object_relationships::RelationshipModelMappingTarget::ModelField(_) => {}
                            }
                        }
                    }
                    boolean_expressions::ComparableRelationshipExecutionStrategy::NDCPushdown => {}
                }
            }
            None => {
                // no source for target model, explode!
                return Err(
                    boolean_expressions::BooleanExpressionError::CannotUseFilterExpressionsWithoutSource {
                        model: target_model.name.clone(),
                    },
                );
            }
        }
    }
    Ok(())
}

// check that a scalar BooleanExpressionType has info for whichever data connector we are using
fn validate_data_connector_with_scalar_boolean_expression_type(
    scalar_boolean_expression_type: &scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    parent_boolean_expression_type_name: &Qualified<CustomTypeName>,
    data_connector: &data_connectors::DataConnectorLink,
    field_name: &FieldName,
) -> Result<(), boolean_expressions::BooleanExpressionError> {
    if !scalar_boolean_expression_type
        .data_connector_operator_mappings
        .contains_key(&data_connector.name)
    {
        return Err(
            boolean_expressions::BooleanExpressionError::DataConnectorMappingMissingForField {
                field: field_name.clone(),
                boolean_expression_name: parent_boolean_expression_type_name.clone(),
                data_connector_name: data_connector.name.clone(),
            },
        );
    }
    Ok(())
}
