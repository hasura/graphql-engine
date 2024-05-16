use crate::helpers::model::resolve_ndc_type;
use crate::helpers::ndc_validation;
use crate::helpers::type_mappings;
use crate::helpers::types::{
    get_object_type_for_boolean_expression, get_type_representation, mk_name,
    unwrap_custom_type_name, TypeRepresentation,
};
use crate::stages::{
    data_connector_scalar_types, data_connectors, model_permissions, models,
    object_boolean_expressions, object_types, relationships, scalar_types, type_permissions,
};
use crate::types::error::{
    Error, RelationshipError, TypeError, TypeMappingValidationError, TypePredicateError,
};
use crate::types::permission::ValueExpression;
use crate::types::subgraph::{ArgumentInfo, Qualified, QualifiedBaseType, QualifiedTypeReference};

use indexmap::IndexMap;
use ndc_models;
use open_dds::arguments::ArgumentName;
use open_dds::data_connector::DataConnectorName;
use open_dds::data_connector::DataConnectorObjectType;
use open_dds::models::ModelName;
use open_dds::permissions;
use open_dds::types::{CustomTypeName, FieldName, OperatorName};
use ref_cast::RefCast;
use std::collections::BTreeMap;

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
        ndc_argument_name: models::ConnectorArgumentName,
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
        ndc_argument_name: models::ConnectorArgumentName,
        type_name: Qualified<CustomTypeName>,
        unknown_ndc_type: String,
    },
    #[error("ndc validation error: {0}")]
    NDCValidationError(NDCValidationError),
}

pub fn get_argument_mappings<'a>(
    arguments: &'a IndexMap<ArgumentName, ArgumentInfo>,
    argument_mapping: &BTreeMap<ArgumentName, String>,
    ndc_arguments_types: &'a BTreeMap<models::ConnectorArgumentName, ndc_models::Type>,
    object_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    scalar_types: &'a BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<
    (
        BTreeMap<ArgumentName, models::ConnectorArgumentName>,
        Vec<type_mappings::TypeMappingToCollect<'a>>,
    ),
    ArgumentMappingError,
> {
    let mut unconsumed_argument_mappings: BTreeMap<&ArgumentName, &models::ConnectorArgumentName> =
        BTreeMap::from_iter(
            argument_mapping
                .iter()
                .map(|(k, v)| (k, models::ConnectorArgumentName::ref_cast(v))),
        );

    let mut resolved_argument_mappings =
        BTreeMap::<ArgumentName, models::ConnectorArgumentName>::new();

    let mut type_mappings_to_collect = Vec::<type_mappings::TypeMappingToCollect>::new();

    for (argument_name, argument_type) in arguments {
        let mapped_to_ndc_argument_name = if let Some(mapped_to_ndc_argument_name) =
            unconsumed_argument_mappings.remove(&argument_name)
        {
            mapped_to_ndc_argument_name.clone()
        } else {
            // If there's no mapping defined for an argument, assume that it
            // implicitly maps to the same name
            let ArgumentName(inner) = argument_name;
            models::ConnectorArgumentName(inner.to_string())
        };

        let ndc_argument_type = ndc_arguments_types
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
                object_boolean_expression_types,
            )
            .map_err(|_| ArgumentMappingError::UnknownType {
                argument_name: argument_name.clone(),
                data_type: object_type_name.clone(),
            })? {
                TypeRepresentation::Object(_) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(ndc_argument_type)
                            .map_err(ArgumentMappingError::NDCValidationError)?;

                    type_mappings_to_collect.push(type_mappings::TypeMappingToCollect {
                        type_name: object_type_name,
                        ndc_object_type_name: DataConnectorObjectType::ref_cast(
                            underlying_ndc_argument_named_type,
                        ),
                    })
                }
                TypeRepresentation::Scalar(_) => (),
                TypeRepresentation::BooleanExpression(object_boolean_expression_type) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(ndc_argument_type)
                            .map_err(ArgumentMappingError::NDCValidationError)?;

                    // resolve the object type the boolean expression refers to
                    type_mappings_to_collect.push(type_mappings::TypeMappingToCollect {
                        type_name: &object_boolean_expression_type.object_type,
                        ndc_object_type_name: DataConnectorObjectType::ref_cast(
                            underlying_ndc_argument_named_type,
                        ),
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
    source_argument_type: Option<&ndc_models::Type>,
    subgraph: &str,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
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
            let object_boolean_expression_type = object_boolean_expression_types
                .get(base_type)
                .ok_or_else(|| Error::UnknownType {
                    data_type: base_type.clone(),
                })?;

            // get the type that the expression is based on
            let object_type_representation = get_object_type_for_boolean_expression(
                object_boolean_expression_type,
                object_types,
            )?;

            // get the data_connector_object_type from the NDC command argument type
            // or explode
            let data_connector_object_type = match &source_argument_type {
                Some(ndc_models::Type::Predicate { object_type_name }) => {
                    Some(DataConnectorObjectType(object_type_name.clone()))
                }
                _ => None,
            }
            .ok_or_else(|| Error::DataConnectorTypeMappingValidationError {
                type_name: base_type.clone(),
                error: TypeMappingValidationError::PredicateTypeNotFound {
                    argument_name: argument_name.clone(),
                },
            })?;

            // look up this type in the context of it's data connector
            // so that we use the correct column names for the data source
            let data_connector_field_mappings = object_type_representation
                .type_mappings
                .get(
                    &object_boolean_expression_type.data_connector_name,
                    &data_connector_object_type,
                )
                .map(|type_mapping| match type_mapping {
                    object_types::TypeMapping::Object { field_mappings, .. } => field_mappings,
                })
                .ok_or(Error::DataConnectorTypeMappingValidationError {
                    type_name: base_type.clone(),
                    error: TypeMappingValidationError::DataConnectorTypeMappingNotFound {
                        object_type_name: base_type.clone(),
                        data_connector_name: object_boolean_expression_type
                            .data_connector_name
                            .clone(),
                        data_connector_object_type: data_connector_object_type.clone(),
                    },
                })?;

            let resolved_model_predicate = resolve_model_predicate_with_type(
                bool_exp,
                base_type,
                object_type_representation,
                data_connector_field_mappings,
                &object_boolean_expression_type.data_connector_name,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                models,
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
    object_type_representation: &relationships::ObjectTypeWithRelationships,
    data_connector_field_mappings: &BTreeMap<FieldName, object_types::FieldMapping>,
    data_connector_name: &Qualified<DataConnectorName>,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,

    models: &IndexMap<Qualified<ModelName>, models::Model>,
    fields: &IndexMap<FieldName, object_types::FieldDefinition>,
) -> Result<model_permissions::ModelPredicate, Error> {
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

            Ok(model_permissions::ModelPredicate::BinaryFieldComparison {
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

            Ok(model_permissions::ModelPredicate::UnaryFieldComparison {
                field: field.clone(),
                ndc_column: field_mapping.column.clone(),
                operator: ndc_models::UnaryComparisonOperator::IsNull,
            })
        }

        permissions::ModelPredicate::Relationship(permissions::RelationshipPredicate {
            name,
            predicate,
        }) => {
            if let Some(nested_predicate) = predicate {
                let relationship_field_name = mk_name(&name.0)?;
                let relationship = &object_type_representation
                    .relationships
                    .get(&relationship_field_name)
                    .ok_or_else(|| Error::TypePredicateError {
                        type_predicate_error:
                            TypePredicateError::UnknownRelationshipInTypePredicate {
                                relationship_name: name.clone(),
                                type_name: type_name.clone(),
                            },
                    })?;

                match &relationship.target {
                    relationships::RelationshipTarget::Command { .. } => {
                        Err(Error::UnsupportedFeature {
                            message: "Predicate cannot be built using command relationships"
                                .to_string(),
                        })
                    }
                    relationships::RelationshipTarget::Model {
                        model_name,
                        relationship_type,
                        target_typename,
                        mappings,
                    } => {
                        let target_model = models.get(model_name).ok_or_else(|| {
                            Error::TypePredicateError { type_predicate_error: TypePredicateError::UnknownModelUsedInRelationshipTypePredicate {
                                type_name: type_name.clone(),
                                target_model_name: model_name.clone(),
                                relationship_name: name.clone(),
                            }}
                        })?;

                        // predicates with relationships is currently only supported for local relationships
                        if let Some(target_model_source) = &target_model.source {
                            if target_model_source.data_connector.name == *data_connector_name {
                                let target_source =
                                    model_permissions::ModelTargetSource::from_model_source(
                                        target_model_source,
                                        relationship,
                                    )
                                    .map_err(|_| {
                                        Error::RelationshipError {
                                    relationship_error:
                                        RelationshipError::NoRelationshipCapabilitiesDefined {
                                            relationship_name: relationship.name.clone(),
                                            type_name: type_name.clone(),
                                            data_connector_name: target_model_source
                                                .data_connector
                                                .name
                                                .clone(),
                                        },
                                }
                                    })?;

                                // validate data connector name
                                let data_connector_context = data_connectors
                                    .data_connectors_with_scalars
                                    .get(data_connector_name)
                                    .ok_or_else(|| {
                                        Error::from(TypePredicateError::UnknownTypeDataConnector {
                                            data_connector: data_connector_name.clone(),
                                            type_name: type_name.clone(),
                                        })
                                    })?;

                                let data_connector_link = data_connectors::DataConnectorLink::new(
                                    data_connector_name.clone(),
                                    data_connector_context.inner.url.clone(),
                                    data_connector_context.inner.headers,
                                )?;

                                // look up this type in the context of it's data connector
                                // so that we use the correct column names for the data source
                                let data_connector_field_mappings = object_type_representation.type_mappings.get(
                                    &target_source.model.data_connector.name,
                                    DataConnectorObjectType::ref_cast(&target_source.model.collection)
                                )
                                .map(|type_mapping| match type_mapping {
                                    object_types::TypeMapping::Object {
                                        field_mappings, ..
                                    } => field_mappings,
                                })
                                .ok_or(Error::DataConnectorTypeMappingValidationError {
                                    type_name: target_typename.clone(),
                                    error: TypeMappingValidationError::DataConnectorTypeMappingNotFound {
                                        object_type_name: target_typename.clone(),
                                        data_connector_name: target_source.model.data_connector.name.clone(),
                                        data_connector_object_type: DataConnectorObjectType(target_source.model.collection
                                            .clone())
                                    },
                                })?;

                                // Collect type mappings.
                                let mut source_type_mappings = BTreeMap::new();

                                // get names of collections we want to lookup
                                let collection_types = object_type_representation
                                    .type_mappings
                                    .object_types_for_data_connector(data_connector_name);

                                let type_mappings_to_collect: Vec<
                                    type_mappings::TypeMappingToCollect,
                                > = collection_types
                                    .iter()
                                    .map(|collection_type| type_mappings::TypeMappingToCollect {
                                        type_name,
                                        ndc_object_type_name: collection_type,
                                    })
                                    .collect();

                                for type_mapping_to_collect in type_mappings_to_collect {
                                    type_mappings::collect_type_mapping_for_source(
                                        &type_mapping_to_collect,
                                        data_connector_name,
                                        &remove_object_relationships(object_types),
                                        scalar_types,
                                        &mut source_type_mappings,
                                    )
                                    .map_err(|error| {
                                        Error::from(
                                            TypePredicateError::TypeMappingCollectionError {
                                                type_name: type_name.clone(),
                                                error,
                                            },
                                        )
                                    })?;
                                }

                                let annotation = model_permissions::PredicateRelationshipInfo {
                                    source_type: relationship.source.clone(),
                                    relationship_name: relationship.name.clone(),
                                    target_model_name: model_name.clone(),
                                    target_source: target_source.clone(),
                                    target_type: target_typename.clone(),
                                    relationship_type: relationship_type.clone(),
                                    mappings: mappings.clone(),
                                    source_data_connector: data_connector_link,
                                    source_type_mappings,
                                };

                                let target_object_type =
                                    object_types.get(&target_model.data_type).ok_or_else(|| {
                                        Error::from(TypePredicateError::ObjectTypeNotFound {
                                            type_name: target_model.data_type.clone(),
                                        })
                                    })?;

                                let target_model_predicate = resolve_model_predicate_with_type(
                                    nested_predicate,
                                    &target_model.data_type,
                                    target_object_type,
                                    data_connector_field_mappings,
                                    data_connector_name,
                                    subgraph,
                                    data_connectors,
                                    object_types,
                                    scalar_types,
                                    models,
                                    &target_model.type_fields,
                                )?;

                                Ok(model_permissions::ModelPredicate::Relationship {
                                    relationship_info: annotation,
                                    predicate: Box::new(target_model_predicate),
                                })
                            } else {
                                Err(Error::UnsupportedFeature {
                                    message: "Predicate cannot be built using remote relationships"
                                        .to_string(),
                                })
                            }
                        } else {
                            Err(Error::from(
                                TypePredicateError::TargetSourceRequiredForRelationshipPredicate {
                                    source_type_name: type_name.clone(),
                                    target_model_name: target_model.name.clone(),
                                },
                            ))
                        }
                    }
                }
            } else {
                Err(Error::from(
                    TypePredicateError::NoPredicateDefinedForRelationshipPredicate {
                        type_name: type_name.clone(),
                        relationship_name: name.clone(),
                    },
                ))
            }
        }

        permissions::ModelPredicate::Not(predicate) => {
            let resolved_predicate = resolve_model_predicate_with_type(
                predicate,
                type_name,
                object_type_representation,
                data_connector_field_mappings,
                data_connector_name,
                subgraph,
                data_connectors,
                object_types,
                scalar_types,
                models,
                fields,
            )?;
            Ok(model_permissions::ModelPredicate::Not(Box::new(
                resolved_predicate,
            )))
        }
        permissions::ModelPredicate::And(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate_with_type(
                    predicate,
                    type_name,
                    object_type_representation,
                    data_connector_field_mappings,
                    data_connector_name,
                    subgraph,
                    data_connectors,
                    object_types,
                    scalar_types,
                    models,
                    fields,
                )?);
            }
            Ok(model_permissions::ModelPredicate::And(resolved_predicates))
        }
        permissions::ModelPredicate::Or(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate_with_type(
                    predicate,
                    type_name,
                    object_type_representation,
                    data_connector_field_mappings,
                    data_connector_name,
                    subgraph,
                    data_connectors,
                    object_types,
                    scalar_types,
                    models,
                    fields,
                )?);
            }
            Ok(model_permissions::ModelPredicate::Or(resolved_predicates))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn resolve_binary_operator_for_type(
    operator: &OperatorName,
    type_name: &Qualified<CustomTypeName>,
    data_connector: &Qualified<DataConnectorName>,
    field_name: &FieldName,
    fields: &IndexMap<FieldName, object_types::FieldDefinition>,
    scalars: &BTreeMap<&str, data_connector_scalar_types::ScalarTypeWithRepresentationInfo>,
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

fn remove_object_relationships(
    object_types_with_relationships: &BTreeMap<
        Qualified<CustomTypeName>,
        relationships::ObjectTypeWithRelationships,
    >,
) -> BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions> {
    object_types_with_relationships
        .iter()
        .map(
            |(
                object_name,
                relationships::ObjectTypeWithRelationships {
                    object_type,
                    type_mappings,
                    type_input_permissions,
                    type_output_permissions,
                    ..
                },
            )| {
                (
                    object_name.clone(),
                    type_permissions::ObjectTypeWithPermissions {
                        object_type: object_type.clone(),
                        type_mappings: type_mappings.clone(),
                        type_input_permissions: type_input_permissions.clone(),
                        type_output_permissions: type_output_permissions.clone(),
                    },
                )
            },
        )
        .collect()
}
