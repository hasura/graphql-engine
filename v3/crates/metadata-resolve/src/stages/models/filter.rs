use super::types::{ModelExpressionType, ModelSource};

use crate::types::error::{BooleanExpressionError, Error};

use crate::stages::{boolean_expressions, object_boolean_expressions};
use crate::types::subgraph::Qualified;

use open_dds::{models::ModelName, types::CustomTypeName};

use std::collections::BTreeMap;

// given a valid source and a filter expression type, try and resolve a predicate type for this
// model
pub(crate) fn resolve_filter_expression_type(
    model_name: &Qualified<ModelName>,
    model_source: &ModelSource,
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

                    // TODO: what checks do we need here?

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
