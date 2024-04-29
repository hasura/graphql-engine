use crate::metadata::resolved::error::{
    Error, TypeError, TypeMappingValidationError, TypePredicateError,
};
use crate::metadata::resolved::model::resolve_ndc_type;
use crate::metadata::resolved::ndc_validation;
use crate::metadata::resolved::permission::ValueExpression;
use crate::metadata::resolved::stages::{
    boolean_expressions, data_connector_scalar_types, data_connector_type_mappings, models,
    scalar_types, type_permissions,
};
use crate::metadata::resolved::subgraph::{ArgumentInfo, Qualified};
use crate::metadata::resolved::subgraph::{QualifiedBaseType, QualifiedTypeReference};
use crate::metadata::resolved::types::{
    get_object_type_for_boolean_expression, get_type_representation, unwrap_custom_type_name,
    TypeMappingToCollect, TypeRepresentation,
};
use indexmap::IndexMap;
use ndc_models;
use open_dds::arguments::ArgumentName;
use open_dds::data_connector::DataConnectorName;
use open_dds::permissions;
use open_dds::types::{CustomTypeName, FieldName, OperatorName};
use std::collections::{BTreeMap, HashMap};

use thiserror::Error;

use super::ndc_validation::NDCValidationError;

#[derive(Error, Debug)]
pub enum ArgumentMappingError {
    #[error(
        "the following arguments referenced in argument mappings are unknown: {}",
        argument_names.join(", ")
    )]
    UnknownArguments { argument_names: Vec<ArgumentName> },
    #[error("argument {argument_name:} is mapped to an unknown argument {ndc_argument_name:}")]
    UnknownNdcArgument {
        argument_name: ArgumentName,
        ndc_argument_name: String,
    },
    #[error("the mapping for argument {argument_name:} has been defined more than once")]
    DuplicateCommandArgumentMapping { argument_name: ArgumentName },
    #[error("{argument_name:} has the data type {data_type:} that has not been defined")]
    UnknownType {
        argument_name: ArgumentName,
        data_type: Qualified<CustomTypeName>,
    },
    #[error(
        "the type {unknown_ndc_type:} is not defined as an object type in the connector's schema. This type is being mapped to by the type {type_name:} used in argument {argument_name:} which is mapped to the data connector argument {ndc_argument_name:}"
    )]
    UnknownNdcType {
        argument_name: ArgumentName,
        ndc_argument_name: String,
        type_name: Qualified<CustomTypeName>,
        unknown_ndc_type: String,
    },
    #[error("ndc validation error: {0}")]
    NDCValidationError(NDCValidationError),
}

pub fn get_argument_mappings<'a>(
    arguments: &'a IndexMap<ArgumentName, ArgumentInfo>,
    argument_mapping: &HashMap<ArgumentName, String>,
    ndc_arguments: &'a BTreeMap<String, ndc_models::ArgumentInfo>,
    object_types: &'a HashMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    scalar_types: &'a HashMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &'a HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<(HashMap<ArgumentName, String>, Vec<TypeMappingToCollect<'a>>), ArgumentMappingError> {
    let mut unconsumed_argument_mappings: HashMap<&ArgumentName, &String> =
        HashMap::from_iter(argument_mapping.iter());
    let mut resolved_argument_mappings = HashMap::<ArgumentName, String>::new();
    let mut type_mappings_to_collect = Vec::<TypeMappingToCollect>::new();
    for (argument_name, argument_type) in arguments {
        let mapped_to_ndc_argument_name = if let Some(mapped_to_ndc_argument_name) =
            unconsumed_argument_mappings.remove(&argument_name)
        {
            mapped_to_ndc_argument_name.clone()
        } else {
            // If there's no mapping defined for an argument, assume that it
            // implicitly maps to the same name
            argument_name.to_string()
        };

        let ndc_argument_info =
            ndc_arguments
                .get(&mapped_to_ndc_argument_name)
                .ok_or_else(|| ArgumentMappingError::UnknownNdcArgument {
                    argument_name: argument_name.clone(),
                    ndc_argument_name: mapped_to_ndc_argument_name.clone(),
                })?;

        let existing_mapping = resolved_argument_mappings
            .insert(argument_name.clone(), mapped_to_ndc_argument_name.clone());

        if existing_mapping.is_some() {
            return Err(ArgumentMappingError::DuplicateCommandArgumentMapping {
                argument_name: argument_name.clone(),
            });
        }

        // only do further checks if this is not a built-in type
        if let Some(object_type_name) = unwrap_custom_type_name(&argument_type.argument_type) {
            match get_type_representation(
                object_type_name,
                object_types,
                scalar_types,
                boolean_expression_types,
            )
            .map_err(|_| ArgumentMappingError::UnknownType {
                argument_name: argument_name.clone(),
                data_type: object_type_name.clone(),
            })? {
                TypeRepresentation::Object(_) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(&ndc_argument_info.argument_type)
                            .map_err(ArgumentMappingError::NDCValidationError)?;

                    type_mappings_to_collect.push(TypeMappingToCollect {
                        type_name: object_type_name,
                        ndc_object_type_name: underlying_ndc_argument_named_type,
                    })
                }
                TypeRepresentation::Scalar(_) => (),
                TypeRepresentation::BooleanExpression(boolean_expression_type) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(&ndc_argument_info.argument_type)
                            .map_err(ArgumentMappingError::NDCValidationError)?;

                    // resolve the object type the boolean expression refers to
                    type_mappings_to_collect.push(TypeMappingToCollect {
                        type_name: &boolean_expression_type.object_type,
                        ndc_object_type_name: underlying_ndc_argument_named_type,
                    })
                }
            }
        }
    }

    // If any unconsumed argument mappings, these do not exist as actual arguments
    let unconsumed_argument_names = unconsumed_argument_mappings
        .into_keys()
        .cloned()
        .collect::<Vec<_>>();
    if !unconsumed_argument_names.is_empty() {
        return Err(ArgumentMappingError::UnknownArguments {
            argument_names: unconsumed_argument_names,
        });
    }

    Ok((resolved_argument_mappings, type_mappings_to_collect))
}

/// resolve a value expression
/// as it may contain a predicate, we also need to provide a
/// type to validate it against to ensure the fields it refers to
/// exist etc
pub(crate) fn resolve_value_expression_for_argument(
    argument_name: &open_dds::arguments::ArgumentName,
    value_expression: &open_dds::permissions::ValueExpression,
    argument_type: &QualifiedTypeReference,
    subgraph: &str,
    object_types: &HashMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    boolean_expression_types: &HashMap<
        Qualified<CustomTypeName>,
        boolean_expressions::ObjectBooleanExpressionType,
    >,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    data_connector_type_mappings: &data_connector_type_mappings::DataConnectorTypeMappings,
) -> Result<ValueExpression, Error> {
    match value_expression {
        open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
            Ok::<ValueExpression, Error>(ValueExpression::SessionVariable(session_variable.clone()))
        }
        open_dds::permissions::ValueExpression::Literal(json_value) => {
            Ok(ValueExpression::Literal(json_value.clone()))
        }
        open_dds::permissions::ValueExpression::BooleanExpression(bool_exp) => {
            // get underlying object type name from argument type (ie, unwrap
            // array, nullability etc)
            let base_type =
                unwrap_custom_type_name(argument_type).ok_or_else(|| Error::ArgumentTypeError {
                    argument_name: argument_name.clone(),
                    type_error: TypeError::NoNamedTypeFound {
                        qualified_type_reference: argument_type.clone(),
                    },
                })?;

            // lookup the relevant ObjectBooleanExpressionType
            let boolean_expression_type =
                boolean_expression_types
                    .get(base_type)
                    .ok_or_else(|| Error::UnknownType {
                        data_type: base_type.clone(),
                    })?;

            // get the type that the expression is based on
            let object_type_representation =
                get_object_type_for_boolean_expression(boolean_expression_type, object_types)?;

            // look up this type in the context of it's data connector
            // so that we use the correct column names for the data source
            let data_connector_field_mappings = data_connector_type_mappings
                .get(
                    &boolean_expression_type.object_type,
                    &boolean_expression_type.data_connector_name,
                    &boolean_expression_type.data_connector_object_type,
                )
                .map(|type_mapping| match type_mapping {
                    data_connector_type_mappings::TypeMapping::Object {
                        field_mappings, ..
                    } => field_mappings,
                })
                .ok_or(Error::DataConnectorTypeMappingValidationError {
                    type_name: base_type.clone(),
                    error: TypeMappingValidationError::DataConnectorTypeMappingNotFound {
                        object_type_name: base_type.clone(),
                        data_connector_name: boolean_expression_type.data_connector_name.clone(),
                        data_connector_object_type: boolean_expression_type
                            .data_connector_object_type
                            .clone(),
                    },
                })?;

            let resolved_model_predicate = resolve_model_predicate_with_type(
                bool_exp,
                base_type,
                data_connector_field_mappings,
                &boolean_expression_type.data_connector_name,
                subgraph,
                data_connectors,
                &object_type_representation.object_type.fields,
            )?;

            Ok(ValueExpression::BooleanExpression(Box::new(
                resolved_model_predicate,
            )))
        }
    }
}

/// a simplified version of resolve_model_predicate that only requires a type rather than an entire
/// Model for context. It skips relationships for simplicity, this should be simple enough to
/// re-add in future. Because this function takes the `data_connector_field_mappings` as an input,
/// many of the errors thrown in `resolve_model_predicate` are pushed out.
pub(crate) fn resolve_model_predicate_with_type(
    model_predicate: &permissions::ModelPredicate,
    type_name: &Qualified<CustomTypeName>,
    data_connector_field_mappings: &BTreeMap<FieldName, data_connector_type_mappings::FieldMapping>,
    data_connector_name: &Qualified<DataConnectorName>,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    fields: &IndexMap<FieldName, data_connector_type_mappings::FieldDefinition>,
) -> Result<models::ModelPredicate, Error> {
    match model_predicate {
        permissions::ModelPredicate::FieldComparison(permissions::FieldComparisonPredicate {
            field,
            operator,
            value,
        }) => {
            // TODO: (anon) typecheck the value expression with the field
            // TODO: resolve the "in" operator too (ndc_models::BinaryArrayComparisonOperator)

            // Determine field_mapping for the predicate field
            let field_mapping = data_connector_field_mappings.get(field).ok_or_else(|| {
                Error::TypePredicateError {
                    type_predicate_error: TypePredicateError::UnknownFieldInTypePredicate {
                        field_name: field.clone(),
                        type_name: type_name.clone(),
                    },
                }
            })?;
            // Determine ndc type of the field
            let field_ndc_type = &field_mapping.column_type;

            // Determine whether the ndc type is a simple scalar
            // Get available scalars defined in the data connector
            let scalars = &data_connectors
                .data_connectors_with_scalars
                .get(data_connector_name)
                .ok_or(Error::TypePredicateError {
                    type_predicate_error: TypePredicateError::UnknownTypeDataConnector {
                        type_name: type_name.clone(),
                        data_connector: data_connector_name.clone(),
                    },
                })?
                .scalars;

            // Get scalar type info from the data connector
            let (_, scalar_type_info) =
                data_connector_scalar_types::get_simple_scalar(field_ndc_type.clone(), scalars)
                    .ok_or_else(|| Error::TypePredicateError {
                        type_predicate_error: TypePredicateError::UnsupportedFieldInTypePredicate {
                            field_name: field.clone(),
                            type_name: type_name.clone(),
                        },
                    })?;

            let (resolved_operator, argument_type) = resolve_binary_operator_for_type(
                operator,
                type_name,
                data_connector_name,
                field,
                fields,
                scalars,
                scalar_type_info.scalar_type,
                subgraph,
            )?;

            let value_expression = match value {
                open_dds::permissions::ValueExpression::Literal(json_value) => {
                    Ok(ValueExpression::Literal(json_value.clone()))
                }
                open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
                    Ok(ValueExpression::SessionVariable(session_variable.clone()))
                }
                open_dds::permissions::ValueExpression::BooleanExpression(
                    _inner_model_predicate,
                ) => Err(Error::TypePredicateError {
                    type_predicate_error: TypePredicateError::NestedPredicateInTypePredicate {
                        type_name: type_name.clone(),
                    },
                }),
            }?;

            Ok(models::ModelPredicate::BinaryFieldComparison {
                field: field.clone(),
                ndc_column: field_mapping.column.clone(),
                operator: resolved_operator,
                argument_type,
                value: value_expression,
            })
        }
        permissions::ModelPredicate::FieldIsNull(permissions::FieldIsNullPredicate { field }) => {
            // Determine field_mapping for the predicate field
            let field_mapping = data_connector_field_mappings.get(field).ok_or_else(|| {
                Error::TypePredicateError {
                    type_predicate_error: TypePredicateError::UnknownFieldInTypePredicate {
                        field_name: field.clone(),
                        type_name: type_name.clone(),
                    },
                }
            })?;

            Ok(models::ModelPredicate::UnaryFieldComparison {
                field: field.clone(),
                ndc_column: field_mapping.column.clone(),
                operator: ndc_models::UnaryComparisonOperator::IsNull,
            })
        }
        permissions::ModelPredicate::Relationship(permissions::RelationshipPredicate {
            ..
        }) => Err(Error::UnsupportedFeature {
            message: "relationships not supported in type predicates".to_string(),
        }),
        permissions::ModelPredicate::Not(predicate) => {
            let resolved_predicate = resolve_model_predicate_with_type(
                predicate,
                type_name,
                data_connector_field_mappings,
                data_connector_name,
                subgraph,
                data_connectors,
                fields,
            )?;
            Ok(models::ModelPredicate::Not(Box::new(resolved_predicate)))
        }
        permissions::ModelPredicate::And(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate_with_type(
                    predicate,
                    type_name,
                    data_connector_field_mappings,
                    data_connector_name,
                    subgraph,
                    data_connectors,
                    fields,
                )?);
            }
            Ok(models::ModelPredicate::And(resolved_predicates))
        }
        permissions::ModelPredicate::Or(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate_with_type(
                    predicate,
                    type_name,
                    data_connector_field_mappings,
                    data_connector_name,
                    subgraph,
                    data_connectors,
                    fields,
                )?);
            }
            Ok(models::ModelPredicate::Or(resolved_predicates))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn resolve_binary_operator_for_type(
    operator: &OperatorName,
    type_name: &Qualified<CustomTypeName>,
    data_connector: &Qualified<DataConnectorName>,
    field_name: &FieldName,
    fields: &IndexMap<FieldName, data_connector_type_mappings::FieldDefinition>,
    scalars: &HashMap<&str, data_connector_scalar_types::ScalarTypeWithRepresentationInfo>,
    ndc_scalar_type: &ndc_models::ScalarType,
    subgraph: &str,
) -> Result<(String, QualifiedTypeReference), Error> {
    let field_definition = fields
        .get(field_name)
        .ok_or_else(|| Error::TypePredicateError {
            type_predicate_error: TypePredicateError::UnknownFieldInTypePredicate {
                field_name: field_name.clone(),
                type_name: type_name.clone(),
            },
        })?;
    let comparison_operator_definition = &ndc_scalar_type
        .comparison_operators
        .get(&operator.0)
        .ok_or_else(|| Error::TypePredicateError {
            type_predicate_error: TypePredicateError::InvalidOperatorInTypePredicate {
                type_name: type_name.clone(),
                operator_name: operator.clone(),
            },
        })?;
    match comparison_operator_definition {
        ndc_models::ComparisonOperatorDefinition::Equal => {
            Ok((operator.0.clone(), field_definition.field_type.clone()))
        }
        ndc_models::ComparisonOperatorDefinition::In => Ok((
            operator.0.clone(),
            QualifiedTypeReference {
                underlying_type: QualifiedBaseType::List(Box::new(
                    field_definition.field_type.clone(),
                )),
                nullable: true,
            },
        )),
        ndc_models::ComparisonOperatorDefinition::Custom { argument_type } => Ok((
            operator.0.clone(),
            resolve_ndc_type(data_connector, argument_type, scalars, subgraph)?,
        )),
    }
}
