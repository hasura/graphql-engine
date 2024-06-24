use crate::types::error::{BooleanExpressionError, Error, TypePredicateError};

use super::types::ModelExpressionType;
use crate::stages::{
    boolean_expressions, data_connectors, models, object_boolean_expressions, relationships,
};
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use open_dds::{
    models::ModelName,
    types::{CustomTypeName, FieldName},
};

use std::collections::BTreeMap;

// given a valid source and a filter expression type, try and resolve a predicate type for this
// model
pub(crate) fn resolve_filter_expression_type(
    model_name: &Qualified<ModelName>,
    model_source: &models::ModelSource,
    model_data_type: &Qualified<CustomTypeName>,
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
) -> Result<ModelExpressionType, Error> {
    match object_boolean_expression_types.get(boolean_expression_type_name) {
        Some(object_boolean_expression_type) => {
            // we're using an old ObjectBooleanExpressionType kind

            // check that the model object type and boolean expression object type agree
            if object_boolean_expression_type.object_type != *model_data_type {
                return Err(Error::from(
                    BooleanExpressionError::BooleanExpressionTypeForInvalidObjectTypeInModel {
                        name: boolean_expression_type_name.clone(),
                        boolean_expression_object_type: object_boolean_expression_type
                            .object_type
                            .clone(),
                        model: model_name.clone(),
                        model_object_type: model_data_type.clone(),
                    },
                ));
            }

            // The `ObjectBooleanExpressionType` allows specifying Data Connector related information
            // there we still check it againt the type specified in the models source.
            // The newer `BooleanExpressionType` defers to the model's choice of object by default, so we
            // do not need this check there
            let data_connector = &object_boolean_expression_type.data_connector;

            if data_connector.name != model_source.data_connector.name {
                return Err(Error::DifferentDataConnectorInFilterExpression {
                    model: model_name.clone(),
                    model_data_connector: model_source.data_connector.name.clone(),
                    filter_expression_type: object_boolean_expression_type.name.clone(),
                    filter_expression_data_connector: data_connector.name.clone(),
                });
            }

            if data_connector.object_type != model_source.collection_type {
                return Err(Error::DifferentDataConnectorObjectTypeInFilterExpression {
                    model: model_name.clone(),
                    model_data_connector_object_type: model_source.collection_type.clone(),
                    filter_expression_type: boolean_expression_type_name.clone(),
                    filter_expression_data_connector_object_type: data_connector
                        .object_type
                        .clone(),
                });
            }

            Ok(ModelExpressionType::ObjectBooleanExpressionType(
                object_boolean_expression_type.clone(),
            ))
        }
        None => {
            // now we should also check in `BooleanExpressionTypes`, the new kind
            match boolean_expression_types
                .objects
                .get(boolean_expression_type_name)
            {
                Some(boolean_expression_object_type) => {
                    // we're using the new style of BooleanExpressionType
                    validate_data_connector_with_object_boolean_expression_type(
                        &model_source.data_connector,
                        boolean_expression_object_type,
                        boolean_expression_types,
                        object_types,
                        models,
                    )?;

                    Ok(ModelExpressionType::BooleanExpressionType(
                        boolean_expression_object_type.clone(),
                    ))
                }
                None => Err(Error::from(
                    BooleanExpressionError::UnknownBooleanExpressionTypeInModel {
                        name: boolean_expression_type_name.clone(),
                        model: model_name.clone(),
                    },
                )),
            }
        }
    }
}

// we want to ensure that our `BooleanExpressionType` (and all it's leaves)
// are compatible with our data connector
// we want to know
// a) does each scalar have mappings for our data connector?
// b) does each relationship live on the same data connector?
// c) if we used nested objects, does the data connector have the correct capability?
fn validate_data_connector_with_object_boolean_expression_type(
    data_connector: &data_connectors::DataConnectorLink,
    object_boolean_expression_type: &boolean_expressions::ResolvedObjectBooleanExpressionType,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
) -> Result<(), Error> {
    if let Some(graphql_config) = &object_boolean_expression_type.graphql {
        for object_comparison_expression_info in graphql_config.object_fields.values() {
            // look up the leaf boolean expression type
            let leaf_boolean_expression = boolean_expression_types
                .objects
                .get(&object_comparison_expression_info.object_type_name)
                .ok_or_else(|| {
                    Error::from(BooleanExpressionError::BooleanExpressionCouldNotBeFound {
                        parent_boolean_expression: object_boolean_expression_type.name.clone(),
                        child_boolean_expression: object_comparison_expression_info
                            .object_type_name
                            .clone(),
                    })
                })?;

            // this must be a nested object, so let's check our data connector is ready for that
            if !data_connector.capabilities.supports_nested_object_filtering {
                return Err(
                    BooleanExpressionError::NoNestedObjectFilteringCapabilitiesDefined {
                        parent_type_name: object_boolean_expression_type.name.clone(),
                        nested_type_name: object_comparison_expression_info
                            .object_type_name
                            .clone(),
                        data_connector_name: data_connector.name.clone(),
                    }
                    .into(),
                );
            }

            // continue checking the nested object...
            validate_data_connector_with_object_boolean_expression_type(
                data_connector,
                leaf_boolean_expression,
                boolean_expression_types,
                object_types,
                models,
            )?;
        }

        for (field_name, comparison_expression_info) in &graphql_config.scalar_fields {
            // this is always present with `BooleanExpressionType` but not
            // `ObjectBooleanExpressionType`, remove this partiality in
            // future
            if let Some(object_type_name) = &comparison_expression_info.object_type_name {
                let leaf_boolean_expression = boolean_expression_types
                    .scalars
                    .get(object_type_name)
                    .ok_or_else(|| {
                        Error::from(BooleanExpressionError::BooleanExpressionCouldNotBeFound {
                            parent_boolean_expression: object_boolean_expression_type.name.clone(),
                            child_boolean_expression: object_type_name.clone(),
                        })
                    })?;

                // check scalar type
                validate_data_connector_with_scalar_boolean_expression_type(
                    leaf_boolean_expression,
                    &object_boolean_expression_type.name,
                    data_connector,
                    field_name,
                )?;
            }
        }

        for comparable_relationship in graphql_config.relationship_fields.values() {
            validate_data_connector_with_comparable_relationship(
                data_connector,
                object_boolean_expression_type,
                comparable_relationship,
                object_types,
                models,
            )?;
        }
    }

    Ok(())
}

// validate comparable relationship field against data connector
// for now, this means checking that a) the target has a source and b) that source is the same
// connector as the source of the boolean expression
fn validate_data_connector_with_comparable_relationship(
    data_connector: &data_connectors::DataConnectorLink,
    object_boolean_expression_type: &boolean_expressions::ResolvedObjectBooleanExpressionType,
    comparable_relationship: &boolean_expressions::BooleanExpressionComparableRelationship,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
) -> Result<(), Error> {
    let underlying_object = object_types
        .get(&object_boolean_expression_type.object_type)
        .ok_or_else(|| Error::UnknownType {
            data_type: object_boolean_expression_type.object_type.clone(),
        })?;

    let relationship_field_name =
        relationships::make_relationship_field_name(&comparable_relationship.relationship_name)?;

    let relationship = underlying_object
        .relationship_fields
        .get(&relationship_field_name)
        .ok_or_else(|| Error::TypePredicateError {
            type_predicate_error: TypePredicateError::UnknownRelationshipInTypePredicate {
                relationship_name: comparable_relationship.relationship_name.clone(),
                type_name: object_boolean_expression_type.object_type.clone(),
            },
        })?;

    if let relationships::RelationshipTarget::Model(relationship_target_model) =
        &relationship.target
    {
        let target_model = models
            .get(&relationship_target_model.model_name)
            .ok_or_else(|| Error::TypePredicateError {
                type_predicate_error:
                    TypePredicateError::UnknownModelUsedInRelationshipTypePredicate {
                        type_name: object_boolean_expression_type.object_type.clone(),
                        target_model_name: relationship_target_model.model_name.clone(),
                        relationship_name: comparable_relationship.relationship_name.clone(),
                    },
            })?;

        match &target_model.source {
            Some(target_model_source) => {
                // we only support local relationships across boolean expression types
                // at the moment, so explode if the data connectors used do not match
                if data_connector.name != target_model_source.data_connector.name {
                    return Err(Error::DifferentDataConnectorInFilterExpression {
                        model: target_model.name.clone(),
                        model_data_connector: target_model_source.data_connector.name.clone(),
                        filter_expression_type: object_boolean_expression_type.name.clone(),
                        filter_expression_data_connector: data_connector.name.clone(),
                    });
                };
            }
            None => {
                // no source for target model, explode!
                return Err(Error::CannotUseFilterExpressionsWithoutSource {
                    model: target_model.name.clone(),
                });
            }
        }
    };
    Ok(())
}

// check that a scalar BooleanExpressionType has info for whichever data connector we are using
fn validate_data_connector_with_scalar_boolean_expression_type(
    scalar_boolean_expression_type: &boolean_expressions::ResolvedScalarBooleanExpressionType,
    parent_boolean_expression_type_name: &Qualified<CustomTypeName>,
    data_connector: &data_connectors::DataConnectorLink,
    field_name: &FieldName,
) -> Result<(), Error> {
    if !scalar_boolean_expression_type
        .data_connector_operator_mappings
        .contains_key(&data_connector.name)
    {
        return Err(Error::BooleanExpressionError {
            boolean_expression_error: BooleanExpressionError::DataConnectorMappingMissingForField {
                field: field_name.clone(),
                boolean_expression_name: parent_boolean_expression_type_name.clone(),
                data_connector_name: data_connector.name.clone(),
            },
        });
    };
    Ok(())
}
