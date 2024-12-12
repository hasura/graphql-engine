use open_dds::aggregates::AggregateExpressionName;
use open_dds::data_connector::{
    DataConnectorName, DataConnectorObjectType, DataConnectorScalarType,
};
use ref_cast::RefCast;
use std::sync::Arc;

use super::error::{ModelAggregateExpressionError, ModelsError};
use crate::stages::{aggregates, data_connectors, models, object_types, type_permissions};
use crate::types::subgraph::{Qualified, QualifiedTypeName};
use open_dds::{models::ModelName, types::CustomTypeName};

use std::collections::BTreeMap;

pub fn resolve_aggregate_expression(
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    model_name: &Qualified<ModelName>,
    model_object_type_name: &Qualified<CustomTypeName>,
    model_source: Option<&Arc<models::ModelSource>>,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
) -> Result<Qualified<AggregateExpressionName>, ModelsError> {
    let model_object_type = QualifiedTypeName::Custom(model_object_type_name.clone());

    // Check the model has a source
    let model_source = model_source.as_ref().ok_or_else(|| {
        ModelAggregateExpressionError::CannotUseAggregateExpressionsWithoutSource {
            model: model_name.clone(),
        }
    })?;

    // Check that the specified aggregate expression exists
    let aggregate_expression = aggregate_expressions
        .get(aggregate_expression_name)
        .ok_or_else(
            || ModelAggregateExpressionError::UnknownModelAggregateExpression {
                model_name: model_name.clone(),
                aggregate_expression: aggregate_expression_name.clone(),
            },
        )?;

    // Check that the specified aggregate expression actually aggregates the model's type
    if model_object_type != aggregate_expression.operand.aggregated_type {
        return Err(ModelsError::from(
            ModelAggregateExpressionError::ModelAggregateExpressionOperandTypeMismatch {
                model_name: model_name.clone(),
                aggregate_expression: aggregate_expression_name.clone(),
                model_type: model_object_type,
                aggregate_operand_type: aggregate_expression.operand.aggregated_type.clone(),
            },
        ));
    }

    // Check aggregate function mappings exist to the Model's source data connector
    resolve_aggregate_expression_data_connector_mapping(
        aggregate_expression,
        model_name,
        model_object_type_name,
        &model_source.data_connector.name,
        &model_source.collection_type,
        &model_source.data_connector.capabilities,
        aggregate_expressions,
        object_types,
    )?;

    // Check that the aggregate expression does not define count_distinct, as this is
    // not valid on a model (every object is already "distinct", so it is meaningless)
    if aggregate_expression.count_distinct.enable {
        return Err(ModelsError::from(
            ModelAggregateExpressionError::ModelAggregateExpressionCountDistinctNotAllowed {
                model_name: model_name.clone(),
                aggregate_expression: aggregate_expression_name.clone(),
            },
        ));
    }

    Ok(aggregate_expression_name.clone())
}

// ideally this would return `ModelAggregateExpressionError`
fn resolve_aggregate_expression_data_connector_mapping(
    aggregate_expression: &aggregates::AggregateExpression,
    model_name: &Qualified<ModelName>,
    object_type_name: &Qualified<CustomTypeName>,
    data_connector_name: &Qualified<DataConnectorName>,
    data_connector_object_type: &DataConnectorObjectType,
    data_connector_capabilities: &data_connectors::DataConnectorCapabilities,
    aggregate_expressions: &BTreeMap<
        Qualified<AggregateExpressionName>,
        aggregates::AggregateExpression,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
) -> Result<(), ModelsError> {
    // Find the object type being aggregated and its field mapping
    let object_type = object_types
        .get(object_type_name)
        .map_err(|object_type_error| {
            ModelAggregateExpressionError::ModelAggregateObjectTypeError {
                aggregate_expression: aggregate_expression.name.clone(),
                model_name: model_name.clone(),
                object_type_error,
            }
        })?;
    // if it was not for this error, we could return the smaller `ModelAggregateExpressionError`
    // from this function
    let object_type_mapping = object_type
        .type_mappings
        .get(data_connector_name, data_connector_object_type)
        .ok_or_else(|| ModelsError::TypeMappingRequired {
            model_name: model_name.clone(),
            type_name: object_type_name.clone(),
            data_connector: data_connector_name.clone(),
        })?;
    let object_type_field_mapping = match object_type_mapping {
        object_types::TypeMapping::Object { field_mappings, .. } => field_mappings,
    };

    // Resolve each aggregatable field
    for aggregatable_field in &aggregate_expression.operand.aggregatable_fields {
        // Ensure the aggregatable field actually exists in the object type
        let field_mapping = object_type_field_mapping
            .get(&aggregatable_field.field_name)
            .ok_or_else(|| {
                ModelsError::from(
                    aggregates::AggregateExpressionError::AggregateOperandObjectFieldNotFound {
                        name: aggregate_expression.name.clone(),
                        operand_type: object_type_name.clone(),
                        field_name: aggregatable_field.field_name.clone(),
                    },
                )
            })?;

        // Get the underlying data connector type name for the aggregatable field
        // We only accept named or nullable named types. Array/predicate types are not allowed
        let data_connector_field_type = match &field_mapping.column_type {
            ndc_models::Type::Named { name } => Ok(name),
            ndc_models::Type::Nullable { underlying_type } => match &**underlying_type {
                ndc_models::Type::Named { name } => Ok(name),
                _ => Err(ModelAggregateExpressionError::ModelAggregateExpressionUnexpectedDataConnectorType {
                    model_name: model_name.clone(),
                    aggregate_expression: aggregate_expression.name.clone(),
                    data_connector_name: data_connector_name.clone(),
                    field_name: aggregatable_field.field_name.clone(),
                }),
            },
            _ => Err(ModelAggregateExpressionError::ModelAggregateExpressionUnexpectedDataConnectorType {
                model_name: model_name.clone(),
                aggregate_expression: aggregate_expression.name.clone(),
                data_connector_name: data_connector_name.clone(),
                field_name: aggregatable_field.field_name.clone(),
            }),
        }?;

        // Get the aggregate expression used to aggregate the field's type
        let field_aggregate_expression = aggregate_expressions
            .get(&aggregatable_field.aggregate_expression)
            .ok_or_else(
                || ModelAggregateExpressionError::UnknownModelAggregateExpression {
                    model_name: model_name.clone(),
                    aggregate_expression: aggregatable_field.aggregate_expression.clone(),
                },
            )?;

        // Get the field's aggregate expression operand type, if it an object type
        let field_object_type_name = match &field_aggregate_expression.operand.aggregated_type {
            QualifiedTypeName::Inbuilt(_) => None,
            QualifiedTypeName::Custom(custom_type_name) => {
                if object_types.contains_key(custom_type_name) {
                    Some(custom_type_name)
                } else {
                    None // Must be a scalar (operands are already validated to be either object or scalar in aggregates resolution)
                }
            }
        };

        // If our field contains a nested object type
        if let Some(field_object_type_name) = field_object_type_name {
            // Check that the data connector supports aggregation over nested object fields
            if !data_connector_capabilities
                .supports_aggregates
                .as_ref()
                .is_some_and(|agg| agg.supports_nested_object_aggregations)
            {
                return Err(aggregates::AggregateExpressionError::NestedObjectAggregatesNotSupportedByDataConnector {
                    name: aggregate_expression.name.clone(),
                    data_connector_name: data_connector_name.clone(),
                    field_name: aggregatable_field.field_name.clone(),
                }.into());
            }

            // Resolve the aggregate expression for the nested object field type
            resolve_aggregate_expression_data_connector_mapping(
                field_aggregate_expression,
                model_name,
                field_object_type_name,
                data_connector_name,
                DataConnectorObjectType::ref_cast(data_connector_field_type.inner()),
                data_connector_capabilities,
                aggregate_expressions,
                object_types,
            )?;
        }
        // If our field contains a scalar type
        else {
            // Check that all aggregation functions over this scalar type
            // have a data connector mapping to the data connector used by the model
            let all_functions_have_a_data_connector_mapping = field_aggregate_expression
                .operand
                .aggregation_functions
                .iter()
                .all(|agg_fn| {
                    agg_fn.data_connector_functions.iter().any(|dc_fn| {
                        dc_fn.data_connector_name == *data_connector_name
                            && dc_fn.operand_scalar_type.as_str()
                                == data_connector_field_type.as_str()
                    })
                });
            if !all_functions_have_a_data_connector_mapping {
                return Err(ModelsError::from(ModelAggregateExpressionError::ModelAggregateExpressionDataConnectorMappingMissing {
                    model_name: model_name.clone(),
                    aggregate_expression: field_aggregate_expression.name.clone(),
                    data_connector_name: data_connector_name.clone(),
                    data_connector_operand_type: DataConnectorScalarType::from(
                        data_connector_field_type.as_str(),
                    ),
                }));
            }
        }
    }

    Ok(())
}
