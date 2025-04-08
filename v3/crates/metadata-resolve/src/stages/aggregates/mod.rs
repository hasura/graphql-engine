use std::collections::BTreeMap;

use lang_graphql::ast::common as ast;
use open_dds::aggregates::{AggregateExpressionName, AggregationFunctionName};
use open_dds::data_connector::{DataConnectorName, DataConnectorObjectType};
use open_dds::identifier::SubgraphName;
use open_dds::types::{CustomTypeName, InbuiltType, TypeName};

use crate::helpers::check_for_duplicates;
use crate::helpers::types::unwrap_qualified_type_name;
use crate::stages::{data_connector_scalar_types, graphql_config, scalar_types, type_permissions};
use crate::types::subgraph::{mk_qualified_type_name, mk_qualified_type_reference};
use crate::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference, mk_name};

mod types;
pub use types::*;

use super::data_connectors;

pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<AggregateExpressionsOutput, Vec<AggregateExpressionError>> {
    let mut resolved_aggregate_expressions =
        BTreeMap::<Qualified<AggregateExpressionName>, AggregateExpression>::new();
    let mut issues = vec![];
    let mut results = vec![];

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: aggregate_expression,
    } in &metadata_accessor.aggregate_expressions
    {
        results.push(process_aggregate_expression(
            metadata_accessor,
            data_connectors,
            data_connector_scalars,
            object_types,
            scalar_types,
            graphql_config,
            graphql_types,
            subgraph,
            aggregate_expression,
            &mut resolved_aggregate_expressions,
            &mut issues,
        ));
    }

    partition_eithers::collect_any_errors(results).map(|_| AggregateExpressionsOutput {
        aggregate_expressions: resolved_aggregate_expressions,
        issues,
    })
}

fn process_aggregate_expression(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    subgraph: &SubgraphName,
    aggregate_expression: &open_dds::aggregates::AggregateExpressionV1,
    resolved_aggregate_expressions: &mut BTreeMap<
        Qualified<AggregateExpressionName>,
        AggregateExpression,
    >,
    issues: &mut Vec<AggregateExpressionIssue>,
) -> Result<(), AggregateExpressionError> {
    let aggregate_expression_name =
        Qualified::new(subgraph.clone(), aggregate_expression.name.clone());

    // Have we seen this aggregate expression name before?
    // Check this before checking anything else, so we can fail fast
    if resolved_aggregate_expressions.contains_key(&aggregate_expression_name) {
        return Err(
            AggregateExpressionError::DuplicateAggregateExpressionDefinition {
                name: aggregate_expression_name,
            },
        );
    }

    let resolved_aggregate_expression = resolve_aggregate_expression(
        metadata_accessor,
        data_connectors,
        data_connector_scalars,
        object_types,
        scalar_types,
        graphql_config,
        &aggregate_expression_name,
        aggregate_expression,
        graphql_types,
        issues,
    )?;

    resolved_aggregate_expressions.insert(aggregate_expression_name, resolved_aggregate_expression);
    Ok(())
}

fn resolve_aggregate_expression(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_config: &graphql_config::GraphqlConfig,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    aggregate_expression: &open_dds::aggregates::AggregateExpressionV1,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
    issues: &mut Vec<AggregateExpressionIssue>,
) -> Result<AggregateExpression, AggregateExpressionError> {
    let operand = match &aggregate_expression.operand {
        open_dds::aggregates::AggregateOperand::Object(object_operand) => resolve_object_operand(
            metadata_accessor,
            object_types,
            aggregate_expression_name,
            object_operand,
        ),
        open_dds::aggregates::AggregateOperand::Scalar(scalar_operand) => resolve_scalar_operand(
            data_connectors,
            data_connector_scalars,
            object_types,
            scalar_types,
            aggregate_expression_name,
            scalar_operand,
        ),
    }?;

    let graphql = resolve_aggregate_expression_graphql_config(
        graphql_config,
        aggregate_expression_name,
        &operand,
        aggregate_expression.graphql.as_ref(),
        issues,
        graphql_types,
    )?;

    let count = resolve_aggregate_count(
        CountAggregateType::Count,
        aggregate_expression.count.as_ref(),
        aggregate_expression_name,
        scalar_types,
    )?;

    let count_distinct = resolve_aggregate_count(
        CountAggregateType::CountDistinct,
        aggregate_expression.count_distinct.as_ref(),
        aggregate_expression_name,
        scalar_types,
    )?;

    Ok(AggregateExpression {
        name: aggregate_expression_name.clone(),
        operand,
        graphql,
        count,
        count_distinct,
        description: aggregate_expression.description.clone(),
    })
}

fn resolve_object_operand(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    object_operand: &open_dds::aggregates::ObjectAggregateOperand,
) -> Result<AggregateOperand, AggregateExpressionError> {
    let operand_object_type_name = qualify(
        &object_operand.aggregated_type,
        &aggregate_expression_name.subgraph,
    );

    // Does the operand object type exist?
    let operand_object_type = object_types.get(&operand_object_type_name).map_err(|_| {
        AggregateExpressionError::AggregateOperandObjectTypeNotFound {
            name: aggregate_expression_name.clone(),
            type_name: operand_object_type_name.clone(),
        }
    })?;

    // Check that no fields have been duplicated
    check_for_duplicates(&object_operand.aggregatable_fields, |agg_field_def| {
        &agg_field_def.field_name
    })
    .map_err(|agg_field_def| {
        AggregateExpressionError::AggregateOperandObjectFieldDuplicated {
            name: aggregate_expression_name.clone(),
            field_name: agg_field_def.field_name.clone(),
        }
    })?;

    let aggregatable_fields = object_operand
        .aggregatable_fields
        .iter()
        .map(|agg_field_def| {
            resolve_aggregatable_field(
                agg_field_def,
                operand_object_type,
                aggregate_expression_name,
                &operand_object_type_name,
                metadata_accessor,
            )
        })
        .collect::<Result<Vec<_>, AggregateExpressionError>>()?;

    Ok(AggregateOperand {
        aggregated_type: QualifiedTypeName::Custom(operand_object_type_name),
        aggregatable_fields,
        aggregation_functions: vec![], // Object types don't support being aggregated themselves at this time
    })
}

fn resolve_aggregatable_field(
    aggregate_field_def: &open_dds::aggregates::AggregatableFieldDefinition,
    operand_object_type: &type_permissions::ObjectTypeWithPermissions,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    operand_object_type_name: &Qualified<CustomTypeName>,
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
) -> Result<AggregatableFieldInfo, AggregateExpressionError> {
    // Does the field exist in the operand object type?
    let field_def = operand_object_type
        .object_type
        .fields
        .get(&aggregate_field_def.field_name)
        .ok_or_else(
            || AggregateExpressionError::AggregateOperandObjectFieldNotFound {
                name: aggregate_expression_name.clone(),
                operand_type: operand_object_type_name.clone(),
                field_name: aggregate_field_def.field_name.clone(),
            },
        )?;

    // If the field has arguments, then we don't support aggregating over it
    if !field_def.field_arguments.is_empty() {
        return Err(
            AggregateExpressionError::AggregateOperandObjectFieldHasArguments {
                name: aggregate_expression_name.clone(),
                operand_type: operand_object_type_name.clone(),
                field_name: aggregate_field_def.field_name.clone(),
            },
        );
    }

    // Get the underlying type of the field (ie. ignore nullability and unwrap one level of array)
    let field_agg_type_name =
        get_underlying_aggregatable_type(&field_def.field_type).ok_or_else(|| {
            AggregateExpressionError::MultipleNestedArrayAggregationNotSupported {
                name: aggregate_expression_name.clone(),
                field_name: aggregate_field_def.field_name.clone(),
                field_type: field_def.field_type.clone(),
            }
        })?;

    let field_aggregate_expression_name = Qualified::new(
        aggregate_expression_name.subgraph.clone(),
        aggregate_field_def.aggregate_expression.clone(),
    );

    // Find the field's referenced aggregate expression
    let field_aggregate_expression = metadata_accessor
        .aggregate_expressions
        .iter()
        .find(|agg_exp| {
            agg_exp.subgraph == aggregate_expression_name.subgraph
                && agg_exp.object.name == aggregate_field_def.aggregate_expression
        })
        .map(|agg_exp| &agg_exp.object)
        .ok_or_else(|| {
            AggregateExpressionError::AggregateOperandObjectFieldAggregateExpressionNotFound {
                name: aggregate_expression_name.clone(),
                field_name: aggregate_field_def.field_name.clone(),
                field_aggregate_expression: field_aggregate_expression_name.clone(),
            }
        })?;

    // Get the operand type of the field's referenced aggregate expression
    let field_aggregate_expression_operand_type_name = match &field_aggregate_expression.operand {
        open_dds::aggregates::AggregateOperand::Object(object_operand) => {
            QualifiedTypeName::Custom(qualify(
                &object_operand.aggregated_type,
                &aggregate_expression_name.subgraph,
            ))
        }
        open_dds::aggregates::AggregateOperand::Scalar(scalar_operand) => mk_qualified_type_name(
            &scalar_operand.aggregated_type,
            &aggregate_expression_name.subgraph,
        ),
    };

    // Check that the field's aggregation expression actually operates over the field's type
    if *field_agg_type_name != field_aggregate_expression_operand_type_name {
        return Err(
            AggregateExpressionError::AggregateOperandObjectFieldTypeMismatch {
                name: aggregate_expression_name.clone(),
                operand_type: operand_object_type_name.clone(),
                field_name: aggregate_field_def.field_name.clone(),
                field_type: field_def.field_type.clone(),
                field_aggregate_exp_name: field_aggregate_expression_name,
                field_aggregate_exp_operand_type: field_aggregate_expression_operand_type_name,
            },
        );
    }

    Ok(AggregatableFieldInfo {
        field_name: aggregate_field_def.field_name.clone(),
        description: aggregate_field_def.description.clone(),
        aggregate_expression: field_aggregate_expression_name,
    })
}

fn resolve_scalar_operand(
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    scalar_operand: &open_dds::aggregates::ScalarAggregateOperand,
) -> Result<AggregateOperand, AggregateExpressionError> {
    // If a custom scalar type has been specified, check that it exists
    match &scalar_operand.aggregated_type {
        TypeName::Inbuilt(_) => (),
        TypeName::Custom(operand_type_name) => {
            let qualified_custom_scalar_name =
                qualify(operand_type_name, &aggregate_expression_name.subgraph);
            if !scalar_types.contains_key(&qualified_custom_scalar_name) {
                return Err(
                    AggregateExpressionError::AggregateOperandScalarTypeNotFound {
                        name: aggregate_expression_name.clone(),
                        type_name: qualified_custom_scalar_name,
                    },
                );
            }
        }
    };

    let operand_scalar_type = mk_qualified_type_name(
        &scalar_operand.aggregated_type,
        &aggregate_expression_name.subgraph,
    );

    // Check that no functions have been duplicated
    check_for_duplicates(&scalar_operand.aggregation_functions, |agg_fn_def| {
        &agg_fn_def.name
    })
    .map_err(
        |agg_fn_def| AggregateExpressionError::AggregateOperandFunctionDuplicated {
            name: aggregate_expression_name.clone(),
            function_name: agg_fn_def.name.clone(),
        },
    )?;

    // Resolve the aggregation functions
    let aggregation_functions = scalar_operand
        .aggregation_functions
        .iter()
        .map(|agg_fn_def| {
            resolve_aggregation_function(
                agg_fn_def,
                aggregate_expression_name,
                scalar_types,
                object_types,
                scalar_operand,
                data_connectors,
                data_connector_scalars,
            )
        })
        .collect::<Result<Vec<_>, AggregateExpressionError>>()?;

    // Check that none of the data connector mappings references an aggregation function that does not exist

    Ok(AggregateOperand {
        aggregated_type: operand_scalar_type,
        aggregatable_fields: vec![], // Scalar types don't have fields to aggregate
        aggregation_functions,
    })
}

fn resolve_aggregation_function(
    aggregation_function_def: &open_dds::aggregates::AggregationFunctionDefinition,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_operand: &open_dds::aggregates::ScalarAggregateOperand,
    data_connectors: &data_connectors::DataConnectors,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
) -> Result<AggregationFunctionInfo, AggregateExpressionError> {
    let return_type = mk_qualified_type_reference(
        &aggregation_function_def.return_type,
        &aggregate_expression_name.subgraph,
    );
    let return_type_name = unwrap_qualified_type_name(&return_type);

    // Check that the return type actually exists (only if it is a custom type, built-in ones obviously exist)
    if let QualifiedTypeName::Custom(custom_type_name) = return_type_name {
        if !scalar_types.contains_key(custom_type_name)
            && !object_types.contains_key(custom_type_name)
        {
            return Err(
                AggregateExpressionError::AggregateOperandFunctionUnknownReturnType {
                    name: aggregate_expression_name.clone(),
                    function_name: aggregation_function_def.name.clone(),
                    type_name: custom_type_name.name.clone(),
                },
            );
        }
    }

    // Resolve mappings to all specified data connector functions
    let data_connector_functions = scalar_operand
        .data_connector_aggregation_function_mapping
        .iter()
        .map(|data_connector_fn_mappings| {
            // Check if the data connector exists
            let data_connector_name = qualify(&data_connector_fn_mappings.data_connector_name, &aggregate_expression_name.subgraph);
            let scalars = data_connector_scalars.get(&data_connector_name)
                .ok_or_else(||
                    AggregateExpressionError::AggregateOperandDataConnectorMissing {
                        name: aggregate_expression_name.clone(),
                        data_connector_name: data_connector_name.clone(),
                })?;

            // Check if the data connector supports aggregations
            let data_connector = data_connectors.0.get(&data_connector_name)
                .ok_or_else(||
                    AggregateExpressionError::AggregateOperandDataConnectorMissing {
                        name: aggregate_expression_name.clone(),
                        data_connector_name: data_connector_name.clone(),
                })?;
            if data_connector.capabilities.supports_aggregates.is_none() {
                return Err(AggregateExpressionError::AggregateOperandDataConnectorNotSupported {
                    name: aggregate_expression_name.clone(),
                    data_connector_name: data_connector_name.clone(),
                })
            }

            // Make sure there is a mapping for this aggregation function
            let fn_mapping = data_connector_fn_mappings.function_mapping.get(&aggregation_function_def.name)
                .ok_or_else(||
                    AggregateExpressionError::AggregateOperandDataConnectorFunctionMappingMissing {
                        name: aggregate_expression_name.clone(),
                        function_name: aggregation_function_def.name.clone(),
                        data_connector_name: data_connector_name.clone(),
                })?;

            // Check that the data connector operand scalar type actually exists on the data connector
            let data_connector_scalar_type = scalars.by_ndc_type.get(&data_connector_fn_mappings.data_connector_scalar_type)
                .ok_or_else(||
                    AggregateExpressionError::AggregateOperandDataConnectorFunctionUnknownScalarType {
                        name: aggregate_expression_name.clone(),
                        function_name: aggregation_function_def.name.clone(),
                        data_connector_name: data_connector_name.clone(),
                        scalar_type: data_connector_fn_mappings.data_connector_scalar_type.clone(),
                })?;

            // Check that the mapped data connector aggregate function actually exists
            let data_connector_fn = data_connector_scalar_type.aggregate_functions.get(fn_mapping.name.as_str())
                .ok_or_else(||
                    AggregateExpressionError::AggregateOperandDataConnectorFunctionNotFound {
                        name: aggregate_expression_name.clone(),
                        function_name: aggregation_function_def.name.clone(),
                        data_connector_name: data_connector_name.clone(),
                        data_connector_aggregate_function_name: fn_mapping.name.clone(),
                })?;

            let data_connector_fn_result_type = match data_connector_fn {
                ndc_models::AggregateFunctionDefinition::Min |
                ndc_models::AggregateFunctionDefinition::Max => ndc_models::Type::Named { name: ndc_models::TypeName::from(data_connector_fn_mappings.data_connector_scalar_type.as_str()) },
                ndc_models::AggregateFunctionDefinition::Sum { result_type } |
                ndc_models::AggregateFunctionDefinition::Average { result_type } => ndc_models::Type::Named { name: result_type.inner().clone() },
                ndc_models::AggregateFunctionDefinition::Custom { result_type } => result_type.clone(),
            };

            check_aggregation_function_return_type(
                &return_type,
                &data_connector_fn_result_type,
                aggregate_expression_name,
                &aggregation_function_def.name,
                &data_connector_name,
                scalars,
                scalar_types,
                object_types,
            )?;

            let function_info = DataConnectorAggregationFunctionInfo {
                data_connector_name,
                function_name: fn_mapping.name.clone(),
                operand_scalar_type: data_connector_fn_mappings.data_connector_scalar_type.clone(),
            };
            Ok(function_info)
        })
        .collect::<Result<Vec<DataConnectorAggregationFunctionInfo>, AggregateExpressionError>>()?;

    Ok(AggregationFunctionInfo {
        name: aggregation_function_def.name.clone(),
        description: aggregation_function_def.description.clone(),
        return_type,
        data_connector_functions,
    })
}

fn check_aggregation_function_return_type(
    return_type: &QualifiedTypeReference,
    data_connector_return_type: &ndc_models::Type,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    aggregation_function_name: &AggregationFunctionName,
    data_connector_name: &Qualified<DataConnectorName>,
    data_connector_scalars: &data_connector_scalar_types::DataConnectorScalars,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_types: &type_permissions::ObjectTypesWithPermissions,
) -> Result<(), AggregateExpressionError> {
    let mk_error = |reason: &str| {
        AggregateExpressionError::AggregateOperandDataConnectorFunctionReturnTypeIncompatible {
            name: aggregate_expression_name.clone(),
            function_name: aggregation_function_name.clone(),
            return_type: return_type.clone(),
            data_connector_name: data_connector_name.clone(),
            reason: reason.to_owned(),
        }
    };

    // Unwrap and check all nullablity and array types
    let (named_return_type, ndc_named_return_type) = unwrap_aggregation_function_return_type(
        return_type,
        data_connector_return_type,
        aggregate_expression_name,
        aggregation_function_name,
        data_connector_name,
    )?;

    let validate_scalar_representation = || -> Result<(), AggregateExpressionError> {
        let type_name = data_connector_scalars.by_ndc_type.get(ndc_named_return_type.as_str())
            .ok_or_else(||
                mk_error(format!("The data connector's return type ({ndc_named_return_type}) isn't a scalar type").as_str())
            )?
            .representation
            .as_ref()
            .ok_or_else(||
                mk_error(format!("The data connector's return scalar type ({ndc_named_return_type}) doesn't have a type representation").as_str())
            )?;
        let ndc_qualified_type_name =
            mk_qualified_type_name(type_name, &aggregate_expression_name.subgraph);
        if ndc_qualified_type_name != *named_return_type {
            return Err(mk_error(format!("The data connector's return scalar type representation ({ndc_qualified_type_name}) does not match the Open DD return type").as_str()));
        }
        Ok(())
    };

    match named_return_type {
        // If the Open DD type is an inbuilt scalar, check that the NDC type maps to that scalar
        QualifiedTypeName::Inbuilt(..) => validate_scalar_representation(),
        QualifiedTypeName::Custom(custom_type_name) => {
            // If the Open DD type is a custom scalar, check that the NDC type maps to that custom scalar
            if scalar_types.contains_key(custom_type_name) {
                validate_scalar_representation()
            }
            // If the Open DD type is an object type, check that there is a mapping from that object type to the NDC object type
            else {
                let return_object_type = object_types.get(custom_type_name).map_err(|_| {
                    mk_error("The Open DD return type is not a scalar type or an object type")
                })?;
                let ndc_object_type_name =
                    DataConnectorObjectType::from(ndc_named_return_type.as_str());
                return_object_type.type_mappings.get(data_connector_name, &ndc_object_type_name)
                    .ok_or_else(||
                        mk_error(format!("There is no type mapping defined from the Open DD return object type to the data connector's object type '{ndc_named_return_type}'").as_str())
                    )?;

                Ok(())
            }
        }
    }
}

fn unwrap_aggregation_function_return_type<'a>(
    return_type: &'a QualifiedTypeReference,
    data_connector_return_type: &'a ndc_models::Type,
    aggregate_expression_name: &'a Qualified<AggregateExpressionName>,
    aggregation_function_name: &'a AggregationFunctionName,
    data_connector_name: &'a Qualified<DataConnectorName>,
) -> Result<(&'a QualifiedTypeName, &'a ndc_models::TypeName), AggregateExpressionError> {
    let mk_error = |reason: &str| {
        AggregateExpressionError::AggregateOperandDataConnectorFunctionReturnTypeIncompatible {
            name: aggregate_expression_name.clone(),
            function_name: aggregation_function_name.clone(),
            return_type: return_type.clone(),
            data_connector_name: data_connector_name.clone(),
            reason: reason.to_owned(),
        }
    };

    // Check that the Open DD type is nullable if the NDC return type is nullable
    let ndc_nullable_unwrapped = match data_connector_return_type {
        ndc_models::Type::Nullable { underlying_type } => {
            if return_type.nullable {
                Ok(underlying_type.as_ref())
            } else {
                Err(mk_error(
                    "The data connector's return type is nullable, but the Open DD return type is not",
                ))
            }
        }
        other => Ok(other),
    }?;

    match &return_type.underlying_type {
        // Ensure if the Open DD type is a named type, the NDC type is also a named type
        QualifiedBaseType::Named(named_type) => {
            match ndc_nullable_unwrapped {
                ndc_models::Type::Named { name } => Ok((named_type, name)),
                ndc_models::Type::Nullable { .. } => Err(mk_error(
                    "The data connector's return type is doubly-nullable",
                )), // This shouldn't happen, would be an invalid NDC type
                ndc_models::Type::Array { .. } => Err(mk_error(
                    "The data connector's return type is an array, but the Open DD return type is not",
                )),
                ndc_models::Type::Predicate { .. } => Err(mk_error(
                    "The data connector's return type is a predicate type, which is unsupported in aggregation return types",
                )),
            }
        }
        // Ensure if the Open DD type is a list, the NDC type is an array, and then recur to unwrap and check the element type
        QualifiedBaseType::List(type_reference) => {
            match ndc_nullable_unwrapped {
                ndc_models::Type::Named { name } => Err(mk_error(format!("The data connector's return type is the named type '{name}', but the Open DD return type is an array").as_str())),
                ndc_models::Type::Nullable { .. } => Err(mk_error("The data connector's return type was doubly-nullable")), // This shouldn't happen, would be an invalid NDC type
                ndc_models::Type::Array { element_type } => unwrap_aggregation_function_return_type(type_reference, element_type, aggregate_expression_name, aggregation_function_name, data_connector_name),
                ndc_models::Type::Predicate { .. } => Err(mk_error("The data connector's return type is a predicate type, which is unsupported in aggregation return types")),
            }
        }
    }
}

fn get_underlying_aggregatable_type(
    type_reference: &QualifiedTypeReference,
) -> Option<&QualifiedTypeName> {
    match &type_reference.underlying_type {
        QualifiedBaseType::Named(type_name) => Some(type_name),
        QualifiedBaseType::List(type_reference) => match &type_reference.underlying_type {
            QualifiedBaseType::Named(type_name) => Some(type_name),
            QualifiedBaseType::List(_) => None,
        },
    }
}

fn resolve_aggregate_expression_graphql_config(
    graphql_config: &graphql_config::GraphqlConfig,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    aggregate_operand: &AggregateOperand,
    aggregate_expression_graphql_definition: Option<
        &open_dds::aggregates::AggregateExpressionGraphQlDefinition,
    >,
    issues: &mut Vec<AggregateExpressionIssue>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<Option<AggregateExpressionGraphqlConfig>, AggregateExpressionError> {
    let select_type_name = aggregate_expression_graphql_definition
        .as_ref()
        .map(|def| mk_name(def.select_type_name.as_ref()).map(ast::TypeName))
        .transpose()
        .map_err(
            |graphql_config_error| AggregateExpressionError::GraphqlConfigError {
                aggregate_expression_name: aggregate_expression_name.clone(),
                graphql_config_error,
            },
        )?;

    graphql_types
        .store(select_type_name.as_ref())
        .map_err(
            |graphql_config_error| AggregateExpressionError::GraphqlConfigError {
                aggregate_expression_name: aggregate_expression_name.clone(),
                graphql_config_error,
            },
        )?;

    let graphql_config = match (select_type_name, &graphql_config.query.aggregate_config) {
        (None, _) => None,
        (Some(_select_type_name), None) => {
            // If the a select type name has been configured, but global aggregate config
            // is missing from GraphqlConfig, raise a warning since this is likely a user
            // misconfiguration issue.
            issues.push(AggregateExpressionIssue::ConfigMissingFromGraphQlConfig {
                name: aggregate_expression_name.clone(),
                config_name: "query.aggregate".to_string(),
            });
            None
        }
        (Some(select_type_name), Some(aggregate_config)) => {
            // Check that no aggregatable field conflicts with the _count field
            if let Some(field) = aggregate_operand.aggregatable_fields.iter().find(|field| {
                field.field_name.as_str() == aggregate_config.count_field_name.as_str()
            }) {
                return Err(AggregateExpressionError::AggregatableFieldNameConflict {
                    name: aggregate_expression_name.clone(),
                    config_name: "query.aggregate.countFieldName".to_string(),
                    aggregatable_field_name: field.field_name.clone(),
                });
            }

            // Check that no aggregation function conflicts with the _count field
            if let Some(function) =
                aggregate_operand
                    .aggregation_functions
                    .iter()
                    .find(|function| {
                        function.name.as_str() == aggregate_config.count_field_name.as_str()
                    })
            {
                return Err(AggregateExpressionError::AggregationFunctionNameConflict {
                    name: aggregate_expression_name.clone(),
                    config_name: "query.aggregate.countFieldName".to_string(),
                    function_name: function.name.clone(),
                });
            }

            // Check that no aggregatable field conflicts with the _count_distinct field
            if let Some(field) = aggregate_operand.aggregatable_fields.iter().find(|field| {
                field.field_name.as_str() == aggregate_config.count_distinct_field_name.as_str()
            }) {
                return Err(AggregateExpressionError::AggregatableFieldNameConflict {
                    name: aggregate_expression_name.clone(),
                    config_name: "query.aggregate.countDistinctFieldName".to_string(),
                    aggregatable_field_name: field.field_name.clone(),
                });
            }

            // Check that no aggregation function conflicts with the _count_distinct field
            if let Some(function) =
                aggregate_operand
                    .aggregation_functions
                    .iter()
                    .find(|function| {
                        function.name.as_str()
                            == aggregate_config.count_distinct_field_name.as_str()
                    })
            {
                return Err(AggregateExpressionError::AggregationFunctionNameConflict {
                    name: aggregate_expression_name.clone(),
                    config_name: "query.aggregate.countDistinctFieldName".to_string(),
                    function_name: function.name.clone(),
                });
            }

            Some(AggregateExpressionGraphqlConfig {
                select_output_type_name: select_type_name,
                count_field_name: aggregate_config.count_field_name.clone(),
                count_distinct_field_name: aggregate_config.count_distinct_field_name.clone(),
            })
        }
    };

    Ok(graphql_config)
}

fn resolve_aggregate_count(
    count_type: CountAggregateType,
    aggregate_count_definition: Option<&open_dds::aggregates::AggregateCountDefinition>,
    aggregate_expression_name: &Qualified<AggregateExpressionName>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
) -> Result<AggregateCountDefinition, AggregateExpressionError> {
    if let Some(aggregate_count_definition) = aggregate_count_definition {
        // Resolve the count aggregate result type
        let count_result_type = if let Some(count_return_type) =
            &aggregate_count_definition.return_type
        {
            match count_return_type {
                TypeName::Inbuilt(InbuiltType::Int) => QualifiedTypeName::Inbuilt(InbuiltType::Int),

                // Non-integer inbuilts are not valid
                TypeName::Inbuilt(
                    inbuilt @ (InbuiltType::Float
                    | InbuiltType::String
                    | InbuiltType::Boolean
                    | InbuiltType::ID),
                ) => {
                    return Err(AggregateExpressionError::InvalidCountReturnType {
                        aggregate_expression_name: aggregate_expression_name.clone(),
                        count_type,
                        return_type: QualifiedTypeName::Inbuilt(*inbuilt),
                    });
                }

                // We can't really validate custom types as they don't have a representation until they're
                // connected to a data connector, so we will need to validate that when we see how the
                // aggregate expression is used with a model
                TypeName::Custom(custom_type_name) => {
                    let qualified_name = Qualified::new(
                        aggregate_expression_name.subgraph.clone(),
                        custom_type_name.to_owned(),
                    );

                    // Check that the custom scalar actually exists
                    if !scalar_types.contains_key(&qualified_name) {
                        return Err(AggregateExpressionError::UnknownCountReturnType {
                            aggregate_expression_name: aggregate_expression_name.clone(),
                            count_type,
                            return_type: QualifiedTypeName::Custom(qualified_name),
                        });
                    }

                    QualifiedTypeName::Custom(qualified_name)
                }
            }
        } else {
            QualifiedTypeName::Inbuilt(InbuiltType::Int)
        };

        Ok(AggregateCountDefinition {
            enable: aggregate_count_definition.enable,
            description: aggregate_count_definition.description.clone(),
            result_type: count_result_type,
            result_type_defaulted: aggregate_count_definition.return_type.is_none(),
        })
    } else {
        Ok(AggregateCountDefinition {
            enable: false,
            description: None,
            result_type: QualifiedTypeName::Inbuilt(InbuiltType::Int),
            result_type_defaulted: true,
        })
    }
}

fn qualify<T: std::fmt::Display + std::clone::Clone>(
    item: &T,
    subgraph: &SubgraphName,
) -> Qualified<T> {
    Qualified::new(subgraph.clone(), item.clone())
}
