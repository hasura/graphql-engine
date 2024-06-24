use crate::types::error::{BooleanExpressionError, Error};

use super::types::ModelExpressionType;
use crate::stages::{boolean_expressions, data_connectors, models, object_boolean_expressions};
use crate::types::subgraph::Qualified;
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
        // TODO: validate any relationship fields
    }

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
