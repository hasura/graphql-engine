use open_dds::aggregates::AggregateExpressionName;
use open_dds::data_connector::{
    DataConnectorName, DataConnectorObjectType, DataConnectorScalarType,
};
use open_dds::types::InbuiltType;
use ref_cast::RefCast;
use std::sync::Arc;

use super::error::{ModelAggregateExpressionError, ModelsError};
use crate::stages::{
    aggregates, data_connector_scalar_types, data_connectors, models, object_types,
    type_permissions,
};
use crate::types::subgraph::{Qualified, QualifiedTypeName, mk_qualified_type_name};
use open_dds::{models::ModelName, types::CustomTypeName};

use std::collections::{BTreeMap, BTreeSet};

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
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
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
    resolve_object_aggregate_expression_data_connector_mapping(
        aggregate_expression,
        model_name,
        model_object_type_name,
        &model_source.data_connector.name,
        &model_source.collection_type,
        &model_source.data_connector.capabilities,
        aggregate_expressions,
        object_types,
        data_connector_scalars,
        &mut BTreeSet::new(),
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
fn resolve_object_aggregate_expression_data_connector_mapping(
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
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    visited_aggregate_expressions: &mut BTreeSet<Qualified<AggregateExpressionName>>,
) -> Result<(), ModelsError> {
    // Check that we haven't already resolved this aggregate expression (via recursive types)
    if !visited_aggregate_expressions.insert(aggregate_expression.name.clone()) {
        return Ok(());
    }

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
            resolve_object_aggregate_expression_data_connector_mapping(
                field_aggregate_expression,
                model_name,
                field_object_type_name,
                data_connector_name,
                DataConnectorObjectType::ref_cast(data_connector_field_type.inner()),
                data_connector_capabilities,
                aggregate_expressions,
                object_types,
                data_connector_scalars,
                visited_aggregate_expressions,
            )?;
        }
        // If our field contains a scalar type
        else {
            resolve_scalar_aggregate_expression_data_connector_mapping(
                field_aggregate_expression,
                model_name,
                data_connector_name,
                data_connector_field_type,
                data_connector_capabilities,
                data_connector_scalars,
                visited_aggregate_expressions,
            )?;
        }
    }

    validate_count_aggregations(
        aggregate_expression,
        model_name,
        data_connector_name,
        data_connector_capabilities,
        data_connector_scalars,
    )?;

    Ok(())
}

fn resolve_scalar_aggregate_expression_data_connector_mapping(
    aggregate_expression: &aggregates::AggregateExpression,
    model_name: &Qualified<ModelName>,
    data_connector_name: &Qualified<DataConnectorName>,
    data_connector_field_type: &ndc_models::TypeName,
    data_connector_capabilities: &data_connectors::DataConnectorCapabilities,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    visited_aggregate_expressions: &mut BTreeSet<Qualified<AggregateExpressionName>>,
) -> Result<(), ModelsError> {
    // Check that we haven't already resolved this aggregate expression
    if !visited_aggregate_expressions.insert(aggregate_expression.name.clone()) {
        return Ok(());
    }

    let all_functions_have_a_data_connector_mapping = aggregate_expression
        .operand
        .aggregation_functions
        .iter()
        .all(|agg_fn| {
            agg_fn.data_connector_functions.iter().any(|dc_fn| {
                dc_fn.data_connector_name == *data_connector_name
                    && dc_fn.operand_scalar_type.as_str() == data_connector_field_type.as_str()
            })
        });
    // Check that all aggregation functions over this scalar type
    // have a data connector mapping to the data connector used by the model
    if !all_functions_have_a_data_connector_mapping {
        return Err(ModelsError::from(
            ModelAggregateExpressionError::ModelAggregateExpressionDataConnectorMappingMissing {
                model_name: model_name.clone(),
                aggregate_expression: aggregate_expression.name.clone(),
                data_connector_name: data_connector_name.clone(),
                data_connector_operand_type: DataConnectorScalarType::from(
                    data_connector_field_type.as_str(),
                ),
            },
        ));
    }

    validate_count_aggregations(
        aggregate_expression,
        model_name,
        data_connector_name,
        data_connector_capabilities,
        data_connector_scalars,
    )?;

    Ok(())
}

fn validate_count_aggregations(
    aggregate_expression: &aggregates::AggregateExpression,
    model_name: &Qualified<ModelName>,
    data_connector_name: &Qualified<DataConnectorName>,
    data_connector_capabilities: &data_connectors::DataConnectorCapabilities,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
) -> Result<(), ModelsError> {
    let data_connector_scalars =
        data_connector_scalars
            .get(data_connector_name)
            .ok_or_else(|| ModelsError::UnknownModelDataConnector {
                model_name: model_name.clone(),
                data_connector: data_connector_name.clone(),
                data_connector_path: None,
            })?;

    let connector_count_scalar_type = data_connector_capabilities
        .supports_aggregates
        .as_ref()
        .and_then(|t| t.aggregate_count_scalar_type.as_ref());

    validate_count_scalar_type(
        &aggregate_expression.count,
        aggregates::CountAggregateType::Count,
        connector_count_scalar_type,
        &aggregate_expression.name,
        model_name,
        data_connector_name,
        data_connector_scalars,
    )?;

    validate_count_scalar_type(
        &aggregate_expression.count_distinct,
        aggregates::CountAggregateType::CountDistinct,
        connector_count_scalar_type,
        &aggregate_expression.name,
        model_name,
        data_connector_name,
        data_connector_scalars,
    )?;

    Ok(())
}

fn validate_count_scalar_type(
    aggregate_expression_count: &aggregates::AggregateCountDefinition,
    count_type: aggregates::CountAggregateType,
    connector_count_scalar_type: Option<&DataConnectorScalarType>,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    model_name: &Qualified<ModelName>,
    data_connector_name: &Qualified<DataConnectorName>,
    data_connector_scalars: &data_connector_scalar_types::DataConnectorScalars<'_>,
) -> Result<(), ModelsError> {
    match connector_count_scalar_type {
        // If the data connector has declared a particular scalar type that will be returned
        // from count aggregates ...
        Some(connector_count_scalar_type) => {
            // We only validate if the count is enabled or
            // if the count type was explicitly specified (so we should check it anyway)
            if aggregate_expression_count.enable
                || !aggregate_expression_count.result_type_defaulted
            {
                // ... get the opendd scalar type that is mapped to that scalar type...
                let expected_connector_count_return_type = data_connector_scalars
                    .by_ndc_type
                    .get(connector_count_scalar_type)
                    .as_ref()
                    .and_then(|info| info.representation.as_ref())
                    .ok_or_else(|| {
                        ModelAggregateExpressionError::CountReturnTypeMappingMissing {
                            model_name: model_name.clone(),
                            aggregate_expression: aggregate_expression_name.clone(),
                            count_type,
                            count_return_type: aggregate_expression_count.result_type.clone(),
                            data_connector_name: data_connector_name.clone(),
                            data_connector_count_return_type: connector_count_scalar_type.clone(),
                        }
                    })?;

                let qualified_expected_count_return_type = mk_qualified_type_name(
                    expected_connector_count_return_type,
                    &data_connector_name.subgraph,
                );

                // ... and make sure it matches the type specified in the aggregate expression
                if aggregate_expression_count.result_type != qualified_expected_count_return_type {
                    return Err(
                        ModelAggregateExpressionError::CountReturnTypeMappingMismatch {
                            model_name: model_name.clone(),
                            aggregate_expression: aggregate_expression_name.clone(),
                            count_type,
                            count_return_type: aggregate_expression_count.result_type.clone(),
                            data_connector_name: data_connector_name.clone(),
                            expected_count_return_type: qualified_expected_count_return_type,
                        }
                        .into(),
                    );
                }
            }
        }
        None => {
            // If the connector does not specify a count scalar type, we only accept the built-in Int
            // because we can't validate any other type is acceptable, but we know an Int is an integer
            // and therefore is fine to use with a count aggregate
            if !matches!(
                aggregate_expression_count.result_type,
                QualifiedTypeName::Inbuilt(InbuiltType::Int)
            ) {
                return Err(ModelAggregateExpressionError::CountReturnTypeMustBeInt {
                    aggregate_expression: aggregate_expression_name.clone(),
                    model_name: model_name.clone(),
                    count_type,
                    data_connector_name: data_connector_name.clone(),
                }
                .into());
            }
        }
    }

    Ok(())
}
