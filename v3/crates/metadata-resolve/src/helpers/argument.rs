use crate::data_connectors::DataConnectorContext;
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
use crate::types::error::{Error, TypeError, TypePredicateError};
use crate::types::permission::{ValueExpression, ValueExpressionOrPredicate};
use crate::types::subgraph::{
    ArgumentInfo, ArgumentKind, Qualified, QualifiedBaseType, QualifiedTypeReference,
};
use crate::FieldMapping;
use indexmap::IndexMap;
use ndc_models;
use open_dds::arguments::ArgumentName;
use open_dds::data_connector::{
    DataConnectorName, DataConnectorObjectType, DataConnectorOperatorName,
};
use open_dds::identifier::SubgraphName;
use open_dds::models::ModelName;
use open_dds::permissions;
use open_dds::relationships::RelationshipName;
use open_dds::types::DataConnectorArgumentName;
use open_dds::types::{BaseType, CustomTypeName, FieldName, OperatorName, TypeName, TypeReference};
use ref_cast::RefCast;
use std::collections::BTreeMap;
use std::collections::HashSet;

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
    #[error("the data connector argument {ndc_argument_name} has been mapped to more than once")]
    DuplicateNdcArgumentMapping {
        ndc_argument_name: DataConnectorArgumentName,
    },
    #[error("the argument {argument_name} is mapped to the data connector argument {ndc_argument_name} which is already used as an argument preset in the DataConnectorLink")]
    ArgumentAlreadyPresetInDataConnectorLink {
        argument_name: ArgumentName,
        ndc_argument_name: DataConnectorArgumentName,
    },
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

#[derive(Debug, thiserror::Error)]
pub enum ArgumentMappingIssue {
    #[error(
        "the following data connector arguments are not mapped to an argument: {}",
        ndc_argument_names.join(", ")
    )]
    UnmappedNdcArguments {
        ndc_argument_names: Vec<DataConnectorArgumentName>,
    },
}

pub struct ArgumentMappingResults<'a> {
    pub argument_mappings: BTreeMap<ArgumentName, DataConnectorArgumentName>,
    pub data_connector_link_argument_presets:
        BTreeMap<DataConnectorArgumentName, data_connectors::ArgumentPresetValue>,
    pub argument_type_mappings_to_resolve: Vec<type_mappings::TypeMappingToCollect<'a>>,
    pub issues: Vec<ArgumentMappingIssue>,
}

pub fn get_argument_mappings<'a>(
    arguments: &'a IndexMap<ArgumentName, ArgumentInfo>,
    argument_mapping: &BTreeMap<ArgumentName, DataConnectorArgumentName>,
    ndc_arguments_types: &'a BTreeMap<DataConnectorArgumentName, ndc_models::Type>,
    data_connector_context: &DataConnectorContext,
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
) -> Result<ArgumentMappingResults<'a>, ArgumentMappingError> {
    let mut unconsumed_argument_mappings: BTreeMap<&ArgumentName, &DataConnectorArgumentName> =
        argument_mapping.iter().collect();
    let mut unmapped_ndc_arguments: HashSet<&DataConnectorArgumentName> =
        ndc_arguments_types.keys().collect();

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
        if !unmapped_ndc_arguments.remove(&mapped_to_ndc_argument_name) {
            return Err(ArgumentMappingError::DuplicateNdcArgumentMapping {
                ndc_argument_name: mapped_to_ndc_argument_name.clone(),
            });
        }

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

    // Mark off any ndc arguments that are preset at the data connector level via
    // DataConnectorLink argument presets
    let mut data_connector_link_argument_presets = BTreeMap::new();
    for argument_preset in &data_connector_context.argument_presets {
        if let Some((argument_name, ndc_argument_name)) = resolved_argument_mappings
            .iter()
            .find(|(_, ndc_argument_name)| *ndc_argument_name == &argument_preset.name)
        {
            return Err(
                ArgumentMappingError::ArgumentAlreadyPresetInDataConnectorLink {
                    argument_name: argument_name.clone(),
                    ndc_argument_name: ndc_argument_name.clone(),
                },
            );
        }

        // We don't care if the argument preset is for an argument that doesn't exist.
        // These presets are set for the whole connector and are skipped when the
        // function/procedure/collection doesn't take that argument.
        if unmapped_ndc_arguments.remove(&argument_preset.name) {
            data_connector_link_argument_presets
                .insert(argument_preset.name.clone(), argument_preset.value.clone());
        }
    }

    // If any unmapped ndc arguments, we have missing arguments or data connector link argument presets
    // We raise this as an issue because existing projects have this issue and we need to be backwards
    // compatible. Those existing projects will probably fail at query time, but they do build and start 😭
    let issues = if unmapped_ndc_arguments.is_empty() {
        vec![]
    } else {
        vec![
            (ArgumentMappingIssue::UnmappedNdcArguments {
                ndc_argument_names: unmapped_ndc_arguments.into_iter().cloned().collect(),
            }),
        ]
    };

    Ok(ArgumentMappingResults {
        argument_mappings: resolved_argument_mappings,
        data_connector_link_argument_presets,
        argument_type_mappings_to_resolve: type_mappings_to_collect,
        issues,
    })
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
    subgraph: &SubgraphName,
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
            let (boolean_expression_graphql, object_type_representation) =
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
                boolean_expression_graphql,
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
    subgraph: &SubgraphName,
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

            // for newer boolean expressions we already have the information we need
            let (resolved_operator, argument_type) = match boolean_expression_graphql {
                Some(graphql) => {
                    // lookup this field
                    let comparable_field = graphql.scalar_fields.get(field).ok_or_else(|| {
                        Error::from(TypePredicateError::UnknownFieldInTypePredicate {
                            field_name: field.clone(),
                            type_name: type_name.clone(),
                        })
                    })?;

                    // get any operator mappings
                    let operator_mappings = comparable_field
                        .operator_mapping
                        .get(&data_connector_link.name)
                        .ok_or_else(|| {
                            Error::from(TypePredicateError::OperatorMappingsNotFound {
                                data_connector_name: data_connector_link.name.clone(),
                            })
                        })?;

                    // lookup ndc operator name in mappings, falling back to using OperatorName
                    // when an override has not been specified
                    let ndc_operator_name = operator_mappings
                        .get(operator)
                        .unwrap_or_else(|| DataConnectorOperatorName::ref_cast(operator.inner()));

                    // lookup the argument type for this comparison operator
                    let argument_type =
                        comparable_field.operators.get(operator).ok_or_else(|| {
                            Error::from(TypePredicateError::OperatorNotFoundForField {
                                field_name: field.clone(),
                                operator_name: operator.clone(),
                            })
                        })?;

                    Ok((ndc_operator_name.clone(), argument_type.clone()))
                }
                None => {
                    // otherwise we need to infer a lot of it from data_connector_scalar_types info
                    resolve_binary_operator_for_type(
                        operator,
                        type_name,
                        &data_connector_link.name,
                        field,
                        fields,
                        scalars,
                        scalar_type_info.scalar_type,
                        subgraph,
                    )
                }
            }?;

            let value_expression = match value {
                open_dds::permissions::ValueExpression::Literal(json_value) => {
                    ValueExpression::Literal(json_value.clone())
                }
                open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
                    ValueExpression::SessionVariable(session_variable.clone())
                }
            };

            let field_definition = fields.get(field).ok_or_else(|| Error::TypePredicateError {
                type_predicate_error: TypePredicateError::UnknownFieldInTypePredicate {
                    field_name: field.clone(),
                    type_name: type_name.clone(),
                },
            })?;

            Ok(model_permissions::ModelPredicate::BinaryFieldComparison {
                field: field.clone(),
                field_parent_type: type_name.to_owned(),
                ndc_column: field_mapping.column.clone(),
                operator: resolved_operator,
                argument_type,
                value: value_expression,
                deprecated: field_definition.is_deprecated(),
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

            let field_definition = fields.get(field).ok_or_else(|| Error::TypePredicateError {
                type_predicate_error: TypePredicateError::UnknownFieldInTypePredicate {
                    field_name: field.clone(),
                    type_name: type_name.clone(),
                },
            })?;

            Ok(model_permissions::ModelPredicate::UnaryFieldComparison {
                field: field.clone(),
                field_parent_type: type_name.to_owned(),
                ndc_column: field_mapping.column.clone(),
                operator: model_permissions::UnaryComparisonOperator::IsNull,
                deprecated: field_definition.is_deprecated(),
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

                        if let Some(target_model_source) = &target_model.inner.source {
                            // Only relationships targetting the models within the same subgraph are allowed
                            if target_model_source.data_connector.name.subgraph
                                != data_connector_link.name.subgraph
                            {
                                return Err(Error::TypePredicateError {
                                    type_predicate_error:
                                        TypePredicateError::RelationshipAcrossSubgraphs {
                                            relationship_name: name.clone(),
                                            source_data_connector: data_connector_link.name.clone(),
                                            target_data_connector: target_model_source
                                                .data_connector
                                                .name
                                                .clone(),
                                        },
                                });
                            } else if
                            // If this is a remote relationship predicate
                            target_model_source.data_connector.name.name
                                != data_connector_link.name.name
                            {
                                // Check for the presence of a equality operator on source field mapped NDC columns
                                // Reject at build time if no equality operator is found for any source field
                                // in the remote relationship mapping
                                for relationship_mapping in mappings {
                                    let source_field =
                                        &relationship_mapping.source_field.field_name;
                                    let FieldMapping {
                                        column,
                                        equal_operators,
                                        ..
                                    } = data_connector_field_mappings
                                        .get(source_field)
                                        .ok_or_else(|| {
                                            Error::from(
                                                TypePredicateError::UnknownFieldInTypePredicate {
                                                    field_name: source_field.clone(),
                                                    type_name: type_name.clone(),
                                                },
                                            )
                                        })?;

                                    if equal_operators.is_empty() {
                                        return Err(Error::TypePredicateError {
                                            type_predicate_error:
                                                TypePredicateError::MissingEqualOperator {
                                                    location: format!("While building a remote relationship predicate {name}"),
                                                    type_name: type_name.clone(),
                                                    field_name: source_field.clone(),
                                                    ndc_column: column.clone(),
                                                    data_connector_name: data_connector_link
                                                        .name
                                                        .clone(),
                                                },
                                        });
                                    }
                                }
                            }

                            let target_source =
                                model_permissions::ModelTargetSource::from_model_source(
                                    target_model_source,
                                    relationship,
                                )?;

                            let target_object_type_representation = object_types
                                .get(&target_model.inner.data_type)
                                .ok_or(Error::UnknownType {
                                    data_type: target_model.inner.data_type.clone(),
                                })?;

                            // look up this type in the context of it's data connector
                            // so that we use the correct column names for the data source
                            let target_data_connector_field_mappings = target_object_type_representation.type_mappings.get(
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
                                let type_mapping_to_collect = type_mappings::TypeMappingToCollect {
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
                                    Error::from(TypePredicateError::TypeMappingCollectionError {
                                        type_name: type_name.clone(),
                                        error,
                                    })
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

                            // // if a boolean expression type was provided, we can find the
                            // // target boolean expression type by following the appropriate
                            // // `comparable_relationship` field
                            let target_boolean_expression_graphql = match boolean_expression_graphql
                            {
                                Some(graphql) => lookup_relationship_in_boolean_expression(
                                    graphql,
                                    type_name,
                                    &relationship.relationship_name,
                                    target_model,
                                    boolean_expression_types,
                                ),
                                None => Ok(None),
                            }?;

                            let target_model_predicate = resolve_model_predicate_with_type(
                                nested_predicate,
                                &target_model.inner.data_type,
                                target_object_type,
                                target_boolean_expression_graphql.as_ref(),
                                target_data_connector_field_mappings,
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

// if we use a relationship in a predicate, we should be able to find it in our
// `BooleanExpressionType` and use it
fn lookup_relationship_in_boolean_expression(
    graphql: &boolean_expressions::BooleanExpressionGraphqlConfig,
    type_name: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    target_model: &models_graphql::ModelWithGraphql,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> Result<Option<boolean_expressions::BooleanExpressionGraphqlConfig>, Error> {
    // lookup relationship in boolean expression type's
    // comparable relationships
    let comparable_relationship = graphql
        .relationship_fields
        .get(relationship_name.as_str())
        .ok_or_else(|| {
            Error::from(TypePredicateError::UnknownRelationshipInTypePredicate {
                type_name: type_name.clone(),
                relationship_name: relationship_name.clone(),
            })
        })?;

    // lookup the boolean expression type for this comparable
    // relationship
    // if it is defined, we fetch it from metadata
    match &comparable_relationship.boolean_expression_type {
        Some(target_bool_exp_name) => boolean_expression_types
            .objects
            .get(target_bool_exp_name)
            .map(|bool_exp| bool_exp.graphql.clone())
            .ok_or_else(|| {
                Error::from(TypePredicateError::BooleanExpressionNotFound {
                    boolean_expression_name: target_bool_exp_name.clone(),
                })
            }),
        None => {
            // if it is not defined we fall back to the one defined on the model
            // we ignore `ObjectBooleanExpressionType` as we should not be using a mixture
            match &target_model.filter_expression_type {
                Some(models_graphql::ModelExpressionType::BooleanExpressionType(bool_exp)) => {
                    Ok(bool_exp.graphql.clone())
                }
                _ => Ok(None),
            }
        }
    }
}

// this is only used for the older `ObjectBooleanExpressionType` where we
// don't have this information explicitly provided in metadata
fn resolve_binary_operator_for_type<'a>(
    operator: &'a OperatorName,
    type_name: &'a Qualified<CustomTypeName>,
    data_connector: &'a Qualified<DataConnectorName>,
    field_name: &'a FieldName,
    fields: &'a IndexMap<FieldName, object_types::FieldDefinition>,
    scalars: &'a data_connector_scalar_types::ScalarTypeWithRepresentationInfoMap,
    ndc_scalar_type: &'a ndc_models::ScalarType,
    subgraph: &'a SubgraphName,
) -> Result<(DataConnectorOperatorName, QualifiedTypeReference), Error> {
    let field_definition = fields
        .get(field_name)
        .ok_or_else(|| Error::TypePredicateError {
            type_predicate_error: TypePredicateError::UnknownFieldInTypePredicate {
                field_name: field_name.clone(),
                type_name: type_name.clone(),
            },
        })?;

    // this function is only used for `ObjectBooleanExpressionType` where we do not have a concept
    // of mapping OpenDD operator names to NDC operator names, so we just cast it and yolo
    let ndc_operator_name = DataConnectorOperatorName::new(operator.inner().clone());

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
            object_boolean_expressions::resolve_ndc_type(
                data_connector,
                argument_type,
                scalars,
                subgraph,
            )?,
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

// in short, should we convert this to an NDC expression before sending it
pub fn get_argument_kind(
    type_obj: &TypeReference,
    subgraph: &SubgraphName,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
) -> ArgumentKind {
    match &type_obj.underlying_type {
        BaseType::List(type_obj) => get_argument_kind(
            type_obj,
            subgraph,
            object_boolean_expression_types,
            boolean_expression_types,
        ),
        BaseType::Named(type_name) => match type_name {
            TypeName::Inbuilt(_) => ArgumentKind::Other,
            TypeName::Custom(type_name) => {
                let qualified_type_name = Qualified::new(subgraph.clone(), type_name.to_owned());

                match get_type_representation::<type_permissions::ObjectTypesWithPermissions>(
                    &qualified_type_name,
                    &BTreeMap::new(),
                    &BTreeMap::new(),
                    object_boolean_expression_types,
                    boolean_expression_types,
                ) {
                    Ok(
                        TypeRepresentation::BooleanExpressionScalar(_)
                        | TypeRepresentation::BooleanExpressionObject(_)
                        | TypeRepresentation::BooleanExpression(_),
                    ) => ArgumentKind::NDCExpression,
                    _ => ArgumentKind::Other,
                }
            }
        },
    }
}
