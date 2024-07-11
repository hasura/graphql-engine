use crate::helpers::model::resolve_ndc_type;
use crate::helpers::ndc_validation;
use crate::helpers::type_mappings;
use crate::helpers::types::{
    get_object_type_for_boolean_expression, get_object_type_for_object_boolean_expression,
    get_type_representation, mk_name, unwrap_custom_type_name, TypeRepresentation,
};
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, model_permissions,
    models_graphql, object_boolean_expressions, object_types, relationships, scalar_types,
    type_permissions,
};
use crate::types::error::{Error, RelationshipError, TypeError, TypePredicateError};
use crate::types::permission::{ValueExpression, ValueExpressionOrPredicate};
use crate::types::subgraph::{ArgumentInfo, Qualified, QualifiedBaseType, QualifiedTypeReference};

use indexmap::IndexMap;
use ndc_models;
use open_dds::arguments::ArgumentName;
use open_dds::data_connector::{
    DataConnectorName, DataConnectorObjectType, DataConnectorOperatorName,
};
use open_dds::models::ModelName;
use open_dds::permissions;
use open_dds::types::DataConnectorArgumentName;
use open_dds::types::{CustomTypeName, FieldName, OperatorName};
use ref_cast::RefCast;
use std::collections::BTreeMap;

use super::ndc_validation::NDCValidationError;

#[derive(Debug, thiserror::Error)]
pub enum ArgumentMappingError {
    #[error(
        "the following arguments referenced in argument mappings are unknown: {}",
        argument_names.join(", ")
    )]
    UnknownArguments { argument_names: Vec<ArgumentName> },
    #[error("argument {argument_name:} is mapped to an unknown argument {ndc_argument_name:}")]
    UnknownNdcArgument {
        argument_name: ArgumentName,
        ndc_argument_name: DataConnectorArgumentName,
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
        ndc_argument_name: DataConnectorArgumentName,
        type_name: Qualified<CustomTypeName>,
        unknown_ndc_type: String,
    },
    #[error("ndc validation error: {0}")]
    NDCValidationError(NDCValidationError),
}

pub fn get_argument_mappings<'a>(
    arguments: &'a IndexMap<ArgumentName, ArgumentInfo>,
    argument_mapping: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
    ndc_arguments_types: &'a BTreeMap<DataConnectorArgumentName, ndc_models::Type>,
    object_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        type_permissions::ObjectTypeWithPermissions,
    >,
    scalar_types: &'a BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &'a boolean_expressions::BooleanExpressionTypes,
) -> Result<
    (
        BTreeMap<ArgumentName, DataConnectorArgumentName>,
        Vec<type_mappings::TypeMappingToCollect<'a>>,
    ),
    ArgumentMappingError,
> {
    let mut unconsumed_argument_mappings: BTreeMap<&ArgumentName, &DataConnectorArgumentName> =
        argument_mapping.iter().collect();

    let mut resolved_argument_mappings = BTreeMap::<ArgumentName, DataConnectorArgumentName>::new();

    let mut type_mappings_to_collect = Vec::<type_mappings::TypeMappingToCollect>::new();

    for (argument_name, argument_type) in arguments {
        let mapped_to_ndc_argument_name = if let Some(mapped_to_ndc_argument_name) =
            unconsumed_argument_mappings.remove(&argument_name)
        {
            mapped_to_ndc_argument_name.clone()
        } else {
            // If there's no mapping defined for an argument, assume that it
            // implicitly maps to the same name
            DataConnectorArgumentName::from(argument_name.as_str())
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
                boolean_expression_types,
            )
            .map_err(|_| ArgumentMappingError::UnknownType {
                argument_name: argument_name.clone(),
                data_type: object_type_name.clone(),
            })? {
                TypeRepresentation::Object(_) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(ndc_argument_type);

                    type_mappings_to_collect.push(type_mappings::TypeMappingToCollect {
                        type_name: object_type_name,
                        ndc_object_type_name: underlying_ndc_argument_named_type,
                    });
                }
                TypeRepresentation::Scalar(_) | TypeRepresentation::BooleanExpressionScalar(_) => {}
                TypeRepresentation::BooleanExpressionObject(boolean_expression_type) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(ndc_argument_type);

                    // resolve the object type the boolean expression refers to
                    type_mappings_to_collect.push(type_mappings::TypeMappingToCollect {
                        type_name: &boolean_expression_type.object_type,
                        ndc_object_type_name: underlying_ndc_argument_named_type,
                    });
                }
                TypeRepresentation::BooleanExpression(object_boolean_expression_type) => {
                    let underlying_ndc_argument_named_type =
                        ndc_validation::get_underlying_named_type(ndc_argument_type);

                    // resolve the object type the boolean expression refers to
                    type_mappings_to_collect.push(type_mappings::TypeMappingToCollect {
                        type_name: &object_boolean_expression_type.object_type,
                        ndc_object_type_name: underlying_ndc_argument_named_type,
                    });
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
    value_expression: &open_dds::permissions::ValueExpressionOrPredicate,
    argument_type: &QualifiedTypeReference,
    source_argument_type: Option<&ndc_models::Type>,
    data_connector_link: &data_connectors::DataConnectorLink,
    subgraph: &str,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    >,
) -> Result<ValueExpressionOrPredicate, Error> {
    match value_expression {
        open_dds::permissions::ValueExpressionOrPredicate::SessionVariable(session_variable) => {
            Ok::<ValueExpressionOrPredicate, Error>(ValueExpressionOrPredicate::SessionVariable(
                session_variable.clone(),
            ))
        }
        open_dds::permissions::ValueExpressionOrPredicate::Literal(json_value) => {
            Ok(ValueExpressionOrPredicate::Literal(json_value.clone()))
        }
        open_dds::permissions::ValueExpressionOrPredicate::BooleanExpression(bool_exp) => {
            // get underlying object type name from argument type (ie, unwrap
            // array, nullability etc)
            let base_type =
                unwrap_custom_type_name(argument_type).ok_or_else(|| Error::ArgumentTypeError {
                    argument_name: argument_name.clone(),
                    type_error: TypeError::NoNamedTypeFound {
                        qualified_type_reference: argument_type.clone(),
                    },
                })?;

            // lookup the relevant boolean expression type and get the underlying object type
            let (boolean_expression_type, object_type_representation) =
                match object_boolean_expression_types.get(base_type) {
                    Some(object_boolean_expression_type) => Ok((
                        None,
                        get_object_type_for_object_boolean_expression(
                            object_boolean_expression_type,
                            object_types,
                        )?,
                    )),
                    None => match boolean_expression_types.objects.get(base_type) {
                        Some(boolean_expression_type) => Ok((
                            boolean_expression_type.graphql.as_ref(),
                            get_object_type_for_boolean_expression(
                                boolean_expression_type,
                                object_types,
                            )?,
                        )),
                        None => Err(Error::UnknownType {
                            data_type: base_type.clone(),
                        }),
                    },
                }?;

            // get the data_connector_object_type from the NDC command argument type
            // or explode
            let data_connector_object_type = match &source_argument_type {
                Some(ndc_models::Type::Predicate { object_type_name }) => {
                    Some(DataConnectorObjectType::from(object_type_name.as_str()))
                }
                _ => None,
            }
            .ok_or_else(|| {
                Error::from(
                    object_types::ObjectTypesError::DataConnectorTypeMappingValidationError {
                        type_name: base_type.clone(),
                        error: object_types::TypeMappingValidationError::PredicateTypeNotFound {
                            argument_name: argument_name.clone(),
                        },
                    },
                )
            })?;

            let data_connector_field_mappings = object_type_representation
                .type_mappings
                .get(&data_connector_link.name, &data_connector_object_type)
                .map(|type_mapping| match type_mapping {
                    object_types::TypeMapping::Object { field_mappings, .. } => field_mappings,
                })
                .ok_or_else(||Error::from(object_types::ObjectTypesError::DataConnectorTypeMappingValidationError {
                    type_name: base_type.clone(),
                    error:
                        object_types::TypeMappingValidationError::DataConnectorTypeMappingNotFound {
                            object_type_name: base_type.clone(),
                            data_connector_name: data_connector_link.name.clone(),
                            data_connector_object_type: data_connector_object_type.clone(),
                        },
                }))?;

            // Get available scalars defined for this data connector
            let specific_data_connector_scalars = data_connector_scalars
                .get(&data_connector_link.name)
                .ok_or(Error::TypePredicateError {
                    type_predicate_error: TypePredicateError::UnknownTypeDataConnector {
                        type_name: base_type.clone(),
                        data_connector: data_connector_link.name.clone(),
                    },
                })?;

            let resolved_model_predicate = resolve_model_predicate_with_type(
                bool_exp,
                base_type,
                object_type_representation,
                boolean_expression_type,
                data_connector_field_mappings,
                data_connector_link,
                subgraph,
                specific_data_connector_scalars,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
                &object_type_representation.object_type.fields,
            )?;

            Ok(ValueExpressionOrPredicate::BooleanExpression(Box::new(
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
    boolean_expression_graphql: Option<&boolean_expressions::BooleanExpressionGraphqlConfig>,
    data_connector_field_mappings: &BTreeMap<FieldName, object_types::FieldMapping>,
    data_connector_link: &data_connectors::DataConnectorLink,
    subgraph: &str,
    scalars: &data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
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
                Error::from(TypePredicateError::UnknownFieldInTypePredicate {
                    field_name: field.clone(),
                    type_name: type_name.clone(),
                })
            })?;

            // Determine ndc type of the field
            let field_ndc_type = &field_mapping.column_type;

            // Get scalar type info from the data connector
            let scalar_type_info =
                data_connector_scalar_types::get_simple_scalar(field_ndc_type.clone(), scalars)
                    .ok_or_else(|| {
                        Error::from(TypePredicateError::UnsupportedFieldInTypePredicate {
                            field_name: field.clone(),
                            type_name: type_name.clone(),
                        })
                    })?;

            // newer boolean expression types have operator_mappings that allow us
            // to rename operators, if we have those, we'll need to fetch them
            let operator_mapping =
                boolean_expression_graphql.map_or(Ok(BTreeMap::new()), |graphql| {
                    // lookup this field
                    let comparable_field = graphql.scalar_fields.get(field).ok_or_else(|| {
                        Error::from(TypePredicateError::UnknownFieldInTypePredicate {
                            field_name: field.clone(),
                            type_name: type_name.clone(),
                        })
                    })?;

                    // get any operator mappings
                    comparable_field
                        .operator_mapping
                        .get(&data_connector_link.name)
                        .cloned()
                        .ok_or_else(|| {
                            Error::from(TypePredicateError::OperatorMappingsNotFound {
                                data_connector_name: data_connector_link.name.clone(),
                            })
                        })
                })?;

            let (resolved_operator, argument_type) = resolve_binary_operator_for_type(
                operator,
                type_name,
                &data_connector_link.name,
                field,
                fields,
                scalars,
                scalar_type_info.scalar_type,
                subgraph,
                &operator_mapping,
            )?;

            let value_expression = match value {
                open_dds::permissions::ValueExpression::Literal(json_value) => {
                    ValueExpression::Literal(json_value.clone())
                }
                open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
                    ValueExpression::SessionVariable(session_variable.clone())
                }
            };

            Ok(model_permissions::ModelPredicate::BinaryFieldComparison {
                field: field.clone(),
                field_parent_type: type_name.to_owned(),
                ndc_column: field_mapping.column.clone(),
                operator: resolved_operator.clone(),
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
                field_parent_type: type_name.to_owned(),
                ndc_column: field_mapping.column.clone(),
                operator: model_permissions::UnaryComparisonOperator::IsNull,
            })
        }

        permissions::ModelPredicate::Relationship(permissions::RelationshipPredicate {
            name,
            predicate,
        }) => {
            if let Some(nested_predicate) = predicate {
                let relationship_field_name = mk_name(name.as_str())?;

                let relationship = &object_type_representation
                    .relationship_fields
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
                    relationships::RelationshipTarget::ModelAggregate { .. } => {
                        Err(Error::UnsupportedFeature {
                            message:
                                "Predicate cannot be built using model aggregate relationships"
                                    .to_string(),
                        })
                    }
                    relationships::RelationshipTarget::Model(
                        relationships::ModelRelationshipTarget {
                            model_name,
                            relationship_type,
                            target_typename,
                            mappings,
                        },
                    ) => {
                        let target_model = models.get(model_name).ok_or_else(|| {
                            Error::TypePredicateError { type_predicate_error: TypePredicateError::UnknownModelUsedInRelationshipTypePredicate {
                                type_name: type_name.clone(),
                                target_model_name: model_name.clone(),
                                relationship_name: name.clone(),
                            }}
                        })?;

                        // predicates with relationships is currently only supported for local relationships
                        if let Some(target_model_source) = &target_model.inner.source {
                            if target_model_source.data_connector.name == data_connector_link.name {
                                let target_source =
                                    model_permissions::ModelTargetSource::from_model_source(
                                        target_model_source,
                                        relationship,
                                    )
                                    .map_err(|_| {
                                        Error::RelationshipError {
                                    relationship_error:
                                        RelationshipError::NoRelationshipCapabilitiesDefined {
                                            relationship_name: relationship.relationship_name.clone(),
                                            type_name: type_name.clone(),
                                            data_connector_name: target_model_source
                                                .data_connector
                                                .name
                                                .clone(),
                                        },
                                }
                                    })?;

                                let target_object_type_representation = object_types
                                    .get(&target_model.inner.data_type)
                                    .ok_or(Error::UnknownType {
                                        data_type: target_model.inner.data_type.clone(),
                                    })?;

                                // look up this type in the context of it's data connector
                                // so that we use the correct column names for the data source
                                let data_connector_field_mappings = target_object_type_representation.type_mappings.get(
                                    &target_source.model.data_connector.name,
                                    DataConnectorObjectType::ref_cast(target_source.model.collection.inner())
                                )
                                .map(|type_mapping| match type_mapping {
                                    object_types::TypeMapping::Object {
                                        field_mappings, ..
                                    } => field_mappings,
                                })
                                .ok_or_else(||Error::from(object_types::ObjectTypesError::DataConnectorTypeMappingValidationError {
                                    type_name: target_typename.clone(),
                                    error: object_types::TypeMappingValidationError::DataConnectorTypeMappingNotFound {
                                        object_type_name: target_typename.clone(),
                                        data_connector_name: target_source.model.data_connector.name.clone(),
                                        data_connector_object_type: DataConnectorObjectType::from(target_source.model.collection.as_str())
                                    },
                                }))?;

                                // Collect type mappings.
                                let mut source_type_mappings = BTreeMap::new();

                                // get names of collections we want to lookup
                                let collection_types = object_type_representation
                                    .type_mappings
                                    .object_types_for_data_connector(&data_connector_link.name)
                                    .iter()
                                    .map(|collection_type| {
                                        ndc_models::TypeName::from(collection_type.as_str())
                                    })
                                    .collect::<Vec<_>>();

                                for collection_type in &collection_types {
                                    let type_mapping_to_collect =
                                        type_mappings::TypeMappingToCollect {
                                            type_name,
                                            ndc_object_type_name: collection_type,
                                        };

                                    type_mappings::collect_type_mapping_for_source(
                                        &type_mapping_to_collect,
                                        &data_connector_link.name,
                                        &remove_object_relationships(object_types),
                                        scalar_types,
                                        &mut source_type_mappings,
                                        &None,
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
                                    relationship_name: relationship.relationship_name.clone(),
                                    target_model_name: model_name.clone(),
                                    target_source: target_source.clone(),
                                    target_type: target_typename.clone(),
                                    relationship_type: relationship_type.clone(),
                                    mappings: mappings.clone(),
                                    source_data_connector: data_connector_link.clone(),
                                    source_type_mappings,
                                };

                                let target_object_type = object_types
                                    .get(&target_model.inner.data_type)
                                    .ok_or_else(|| {
                                        Error::from(TypePredicateError::ObjectTypeNotFound {
                                            type_name: target_model.inner.data_type.clone(),
                                        })
                                    })?;

                                // if a boolean expression type was provided, we can find the
                                // target boolean expression type by following the appropriate
                                // `comparable_relationship` field
                                let target_boolean_expression_graphql = boolean_expression_graphql
                                    .and_then(|graphql| {
                                        graphql
                                            .relationship_fields
                                            .get(relationship.relationship_name.as_str())
                                    })
                                    .and_then(|comparable_relationship| {
                                        match &comparable_relationship.boolean_expression_type {
                                            Some(target_bool_exp_name) => boolean_expression_types
                                                .objects
                                                .get(target_bool_exp_name),
                                            None => {target_model.filter_expression_type.as_ref().and_then(|met| match met {
                                                    models_graphql::ModelExpressionType::BooleanExpressionType(bool_exp) => Some(bool_exp),
                                                    models_graphql::ModelExpressionType::ObjectBooleanExpressionType(_) => None})
                                            }}
                                        }
                                    )
                                    .and_then(|bool_exp| bool_exp.graphql.as_ref());

                                let target_model_predicate = resolve_model_predicate_with_type(
                                    nested_predicate,
                                    &target_model.inner.data_type,
                                    target_object_type,
                                    target_boolean_expression_graphql,
                                    data_connector_field_mappings,
                                    data_connector_link,
                                    subgraph,
                                    scalars,
                                    object_types,
                                    scalar_types,
                                    boolean_expression_types,
                                    models,
                                    &target_model.inner.type_fields,
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
                                    target_model_name: target_model.inner.name.clone(),
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
                boolean_expression_graphql,
                data_connector_field_mappings,
                data_connector_link,
                subgraph,
                scalars,
                object_types,
                scalar_types,
                boolean_expression_types,
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
                    boolean_expression_graphql,
                    data_connector_field_mappings,
                    data_connector_link,
                    subgraph,
                    scalars,
                    object_types,
                    scalar_types,
                    boolean_expression_types,
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
                    boolean_expression_graphql,
                    data_connector_field_mappings,
                    data_connector_link,
                    subgraph,
                    scalars,
                    object_types,
                    scalar_types,
                    boolean_expression_types,
                    models,
                    fields,
                )?);
            }
            Ok(model_permissions::ModelPredicate::Or(resolved_predicates))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn resolve_binary_operator_for_type<'a>(
    operator: &'a OperatorName,
    type_name: &'a Qualified<CustomTypeName>,
    data_connector: &'a Qualified<DataConnectorName>,
    field_name: &'a FieldName,
    fields: &'a IndexMap<FieldName, object_types::FieldDefinition>,
    scalars: &'a data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    ndc_scalar_type: &'a ndc_models::ScalarType,
    subgraph: &'a str,
    operator_mappings: &'a BTreeMap<OperatorName, DataConnectorOperatorName>,
) -> Result<(&'a DataConnectorOperatorName, QualifiedTypeReference), Error> {
    let field_definition = fields
        .get(field_name)
        .ok_or_else(|| Error::TypePredicateError {
            type_predicate_error: TypePredicateError::UnknownFieldInTypePredicate {
                field_name: field_name.clone(),
                type_name: type_name.clone(),
            },
        })?;

    // lookup ndc operator name in mappings, falling back to using OperatorName
    let ndc_operator_name = operator_mappings
        .get(operator)
        .unwrap_or_else(|| DataConnectorOperatorName::ref_cast(operator.inner()));

    let comparison_operator_definition = &ndc_scalar_type
        .comparison_operators
        .get(ndc_operator_name.as_str())
        .ok_or_else(|| Error::TypePredicateError {
            type_predicate_error: TypePredicateError::InvalidOperatorInTypePredicate {
                type_name: type_name.clone(),
                operator_name: operator.clone(),
            },
        })?;

    match comparison_operator_definition {
        ndc_models::ComparisonOperatorDefinition::Equal => {
            Ok((ndc_operator_name, field_definition.field_type.clone()))
        }
        ndc_models::ComparisonOperatorDefinition::In => Ok((
            ndc_operator_name,
            QualifiedTypeReference {
                underlying_type: QualifiedBaseType::List(Box::new(
                    field_definition.field_type.clone(),
                )),
                nullable: true,
            },
        )),
        ndc_models::ComparisonOperatorDefinition::Custom { argument_type } => Ok((
            ndc_operator_name,
            resolve_ndc_type(data_connector, argument_type, scalars, subgraph)?,
        )),
    }
}

fn remove_object_relationships(
    object_types_with_relationships: &BTreeMap<
        Qualified<CustomTypeName>,
        relationships::ObjectTypeWithRelationships,
    >,
) -> type_permissions::ObjectTypesWithPermissions {
    type_permissions::ObjectTypesWithPermissions(
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
            .collect(),
    )
}
