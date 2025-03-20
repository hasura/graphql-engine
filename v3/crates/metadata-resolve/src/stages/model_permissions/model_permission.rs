use super::types::ModelPermissionIssue;
use super::types::{
    FilterPermission, ModelPredicate, ModelTargetSource, PredicateRelationshipInfo,
    SelectPermission,
};
use super::{ModelPermissionError, NamedModelPermissionError};

use crate::helpers::argument::resolve_value_expression_for_argument;
use crate::helpers::type_mappings;
use crate::helpers::typecheck::typecheck_value_expression;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, data_connectors, models, models_graphql,
    object_relationships, object_types, scalar_boolean_expressions, scalar_types, type_permissions,
};
use crate::types::error::{Error, TypePredicateError};
use crate::types::permission::ValueExpression;
use crate::types::subgraph::{Qualified, QualifiedBaseType, QualifiedTypeReference};
use crate::FieldMapping;
use crate::UnaryComparisonOperator;

use indexmap::IndexMap;
use ndc_models;
use open_dds::data_connector::DataConnectorOperatorName;
use open_dds::identifier::SubgraphName;
use open_dds::permissions::NullableModelPredicate;
use open_dds::spanned::Spanned;
use open_dds::{
    arguments::ArgumentName,
    permissions::{ModelPermissionsV1, Role},
    types::FieldName,
};
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};
use std::collections::{BTreeMap, BTreeSet};

fn resolve_model_predicate_with_model(
    flags: &open_dds::flags::OpenDdFlags,
    model_predicate: &open_dds::permissions::ModelPredicate,
    role: &Spanned<open_dds::permissions::Role>,
    model: &models::Model,
    subgraph: &SubgraphName,
    boolean_expression: Option<&boolean_expressions::ResolvedObjectBooleanExpressionType>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    fields: &IndexMap<FieldName, object_types::FieldDefinition>,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
) -> Result<ModelPredicate, Error> {
    let model_source = model
        .source
        .as_ref()
        .ok_or_else(|| NamedModelPermissionError {
            model_name: model.name.clone(),
            role: role.clone(),
            error: ModelPermissionError::ModelSourceRequiredForPredicate {
                model_name: Spanned {
                    path: model.path.clone(),
                    value: model.name.clone(),
                },
            },
        })?;

    // get available scalars defined for this data connector
    let scalars = data_connector_scalars
        .get(&model_source.data_connector.name)
        .ok_or_else(|| NamedModelPermissionError {
            model_name: model.name.clone(),
            role: role.clone(),
            error: ModelPermissionError::SelectFilterPermissionTypePredicateError {
                error: TypePredicateError::UnknownTypeDataConnector {
                    type_name: model.data_type.clone(),
                    data_connector: model_source.data_connector.name.clone(),
                },
            },
        })?;

    // get the type that the expression is based on
    let object_type_representation =
        object_types
            .get(&model.data_type)
            .ok_or_else(|| Error::UnknownType {
                data_type: model.data_type.clone(),
            })?;

    // Get field mappings of model data type
    let object_types::TypeMapping::Object { field_mappings, .. } = model_source
        .type_mappings
        .get(&model.data_type)
        .ok_or_else(|| models::ModelsError::TypeMappingRequired {
            model_name: model.name.clone(),
            type_name: model.data_type.clone(),
            data_connector: model_source.data_connector.name.clone(),
        })?;

    resolve_model_predicate_with_type(
        flags,
        model_predicate,
        &model.data_type,
        object_type_representation,
        boolean_expression,
        field_mappings,
        &model_source.data_connector,
        subgraph,
        scalars,
        object_types,
        scalar_types,
        boolean_expression_types,
        models,
        fields,
    )
    .map_err(|error| {
        Error::from(NamedModelPermissionError {
            model_name: model.name.clone(),
            role: role.clone(),
            error: ModelPermissionError::SelectFilterPermissionTypePredicateError { error },
        })
    })
}

// get the ndc_models::Type for an argument if it is available
pub fn get_model_source_argument<'a>(
    argument_name: &'a ArgumentName,
    model: &'a models::Model,
) -> Option<&'a ndc_models::Type> {
    model
        .source
        .as_ref()
        .and_then(|source| {
            source
                .argument_mappings
                .get(argument_name)
                .map(|connector_argument_name| source.source_arguments.get(connector_argument_name))
        })
        .flatten()
}

pub fn resolve_all_model_select_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    model: &models::Model,
    subgraph: &SubgraphName,
    model_permissions: &ModelPermissionsV1,
    boolean_expression: Option<&boolean_expressions::ResolvedObjectBooleanExpressionType>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars,
    >,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<BTreeMap<Role, SelectPermission>, Error> {
    let mut validated_permissions = BTreeMap::new();
    let mut resolved_roles = BTreeSet::new();

    for model_permission in &model_permissions.permissions {
        if !resolved_roles.insert(model_permission.role.value.clone()) {
            issues.push(ModelPermissionIssue::DuplicateRole {
                role: model_permission.role.clone(),
                model_name: model.name.clone(),
            });
        }

        if let Some(select_perms) = &model_permission.select {
            let resolved_permission = resolve_model_select_permissions(
                select_perms,
                &model_permission.role,
                flags,
                model,
                subgraph,
                boolean_expression,
                data_connector_scalars,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
                issues,
            )?;

            validated_permissions.insert(model_permission.role.value.clone(), resolved_permission);
        }
    }
    Ok(validated_permissions)
}

fn resolve_model_select_permissions(
    select_perms: &open_dds::permissions::SelectPermission,
    role: &Spanned<open_dds::permissions::Role>,
    flags: &open_dds::flags::OpenDdFlags,
    model: &crate::Model,
    subgraph: &SubgraphName,
    boolean_expression: Option<&boolean_expressions::ResolvedObjectBooleanExpressionType>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, crate::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<SelectPermission, Error> {
    let resolved_predicate = match &select_perms.filter {
        NullableModelPredicate::NotNull(model_predicate) => resolve_model_predicate_with_model(
            flags,
            model_predicate,
            role,
            model,
            subgraph,
            boolean_expression,
            data_connector_scalars,
            &model.type_fields,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
        )
        .map(FilterPermission::Filter)?,
        NullableModelPredicate::Null(()) => FilterPermission::AllowAll,
    };

    let mut argument_presets = BTreeMap::new();
    for argument_preset in &select_perms.argument_presets {
        if argument_presets.contains_key(&argument_preset.argument.value) {
            return Err(NamedModelPermissionError {
                model_name: model.name.clone(),
                role: role.clone(),
                error: ModelPermissionError::DuplicateModelArgumentPreset {
                    argument_name: argument_preset.argument.clone(),
                },
            }
            .into());
        }

        let source_argument_type = get_model_source_argument(&argument_preset.argument, model);

        let model_source = model
            .source
            .as_ref()
            .ok_or_else(|| NamedModelPermissionError {
                model_name: model.name.clone(),
                role: role.clone(),
                error: ModelPermissionError::ModelSourceRequiredForPredicate {
                    model_name: Spanned {
                        path: model.path.clone(),
                        value: model.name.clone(),
                    },
                },
            })?;

        let argument = model
            .arguments
            .get(&argument_preset.argument.value)
            .ok_or_else(|| NamedModelPermissionError {
                model_name: model.name.clone(),
                role: role.clone(),
                error: ModelPermissionError::ModelArgumentPresetArgumentNotFound {
                    model_name: Spanned {
                        path: model.path.clone(),
                        value: model.name.clone(),
                    },
                    argument_name: argument_preset.argument.clone(),
                },
            })?;

        let error_mapper = |type_error| {
            Error::ModelPermissionsError(NamedModelPermissionError {
                model_name: model.name.clone(),
                role: role.clone(),
                error: ModelPermissionError::ModelArgumentValuePresetTypeError {
                    argument_name: argument_preset.argument.clone(),
                    value_path: argument_preset.value.path.clone(),
                    type_error,
                },
            })
        };
        let (value_expression, new_issues) = resolve_value_expression_for_argument(
            role,
            flags,
            &argument_preset.argument,
            &argument_preset.value,
            &argument.argument_type,
            source_argument_type,
            &model_source.data_connector,
            subgraph,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
            data_connector_scalars,
            error_mapper,
        )?;

        // Convert typecheck issues into model permission issues and collect them
        for issue in new_issues {
            issues.push(ModelPermissionIssue::ModelArgumentPresetTypecheckIssue {
                role: role.value.clone(),
                model_name: model.name.clone(),
                argument_name: argument_preset.argument.value.clone(),
                typecheck_issue: issue,
            });
        }

        argument_presets.insert(
            argument_preset.argument.value.clone(),
            (argument.argument_type.clone(), value_expression),
        );
    }
    let resolved_permission = SelectPermission {
        filter: resolved_predicate,
        argument_presets,
        allow_subscriptions: select_perms.allow_subscriptions,
    };

    Ok(resolved_permission)
}

pub(crate) fn resolve_model_predicate_with_type(
    flags: &open_dds::flags::OpenDdFlags,
    model_predicate: &open_dds::permissions::ModelPredicate,
    type_name: &Qualified<CustomTypeName>,
    object_type_representation: &object_relationships::ObjectTypeWithRelationships,
    boolean_expression: Option<&boolean_expressions::ResolvedObjectBooleanExpressionType>,
    data_connector_field_mappings: &BTreeMap<FieldName, object_types::FieldMapping>,
    data_connector_link: &data_connectors::DataConnectorLink,
    subgraph: &SubgraphName,
    scalars: &data_connector_scalar_types::DataConnectorScalars,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    fields: &IndexMap<FieldName, object_types::FieldDefinition>,
) -> Result<ModelPredicate, TypePredicateError> {
    match model_predicate {
        open_dds::permissions::ModelPredicate::FieldComparison(
            open_dds::permissions::FieldComparisonPredicate {
                field,
                operator,
                value,
            },
        ) => {
            // TODO: resolve the "in" operator too (ndc_models::BinaryArrayComparisonOperator)

            let field_definition = object_type_representation
                .object_type
                .fields
                .get(&field.value)
                .ok_or_else(|| TypePredicateError::UnknownFieldInTypePredicate {
                    field_name: field.clone(),
                    type_name: type_name.clone(),
                })?;

            // Determine field_mapping for the predicate field
            let field_mapping =
                data_connector_field_mappings
                    .get(&field.value)
                    .ok_or_else(|| TypePredicateError::UnknownFieldInTypePredicate {
                        field_name: field.clone(),
                        type_name: type_name.clone(),
                    })?;

            // Determine ndc type of the field
            let field_ndc_type = &field_mapping.column_type;

            // Get scalar type info from the data connector
            let scalar_type_info =
                data_connector_scalar_types::get_simple_scalar(field_ndc_type.clone(), scalars)
                    .ok_or_else(
                        || TypePredicateError::UnsupportedFieldComparisonToArrayType {
                            field_name: field.clone(),
                            field_type: field_definition.field_type.clone(),
                            type_name: type_name.clone(),
                        },
                    )?;

            // for newer boolean expressions we already have the information we need
            let bool_exp_fields = boolean_expression.and_then(|b| b.get_fields(flags));
            let (resolved_operator, argument_type) = match bool_exp_fields {
                Some(fields) => {
                    // lookup this field
                    let comparable_field =
                        fields.scalar_fields.get(&field.value).ok_or_else(|| {
                            TypePredicateError::UnknownFieldInTypePredicate {
                                field_name: field.clone(),
                                type_name: type_name.clone(),
                            }
                        })?;

                    // get any operator mappings
                    let operator_mappings = comparable_field
                        .operator_mapping
                        .get(&data_connector_link.name)
                        .ok_or_else(|| TypePredicateError::OperatorMappingsNotFound {
                            data_connector_name: data_connector_link.name.clone(),
                        })?;

                    // lookup ndc operator name in mappings, falling back to using OperatorName
                    // when an override has not been specified
                    let ndc_operator_name = operator_mappings.get(&operator.value);

                    // lookup the argument type for this comparison operator
                    let argument_type = comparable_field
                        .operators
                        .get(&operator.value)
                        .ok_or_else(|| TypePredicateError::OperatorNotFoundForField {
                            field_name: field.clone(),
                            field_type: field_definition.field_type.clone(),
                            operator_name: operator.clone(),
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

            let field_definition = fields.get(&field.value).ok_or_else(|| {
                TypePredicateError::UnknownFieldInTypePredicate {
                    field_name: field.clone(),
                    type_name: type_name.clone(),
                }
            })?;

            // typecheck the `open_dds::permissions::ValueExpression` with the field
            typecheck_value_expression(
                &object_types
                    .iter()
                    .map(|(field_name, object_type)| (field_name, &object_type.object_type))
                    .collect(), // Convert &BTreeMap<field_name, object_type> to BTreeMap<&field_name, &object_type>
                &field_definition.field_type,
                value,
            )?;

            let value_expression = match value {
                open_dds::permissions::ValueExpression::Literal(json_value) => {
                    ValueExpression::Literal(json_value.clone())
                }
                open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
                    ValueExpression::SessionVariable(hasura_authn_core::SessionVariableReference {
                        name: session_variable.clone(),
                        passed_as_json: flags.contains(open_dds::flags::Flag::JsonSessionVariables),
                        disallow_unknown_fields: flags
                            .contains(open_dds::flags::Flag::DisallowUnknownValuesInArguments),
                    })
                }
            };

            Ok(ModelPredicate::BinaryFieldComparison {
                field: field.value.clone(),
                field_parent_type: type_name.to_owned(),
                ndc_column: field_mapping.column.clone(),
                operator: resolved_operator,
                argument_type,
                value: value_expression,
                deprecated: field_definition.deprecated.clone(),
            })
        }
        open_dds::permissions::ModelPredicate::FieldIsNull(
            open_dds::permissions::FieldIsNullPredicate { field },
        ) => {
            // Determine field_mapping for the predicate field
            let field_mapping =
                data_connector_field_mappings
                    .get(&field.value)
                    .ok_or_else(|| TypePredicateError::UnknownFieldInTypePredicate {
                        field_name: field.clone(),
                        type_name: type_name.clone(),
                    })?;

            let field_definition = fields.get(&field.value).ok_or_else(|| {
                TypePredicateError::UnknownFieldInTypePredicate {
                    field_name: field.clone(),
                    type_name: type_name.clone(),
                }
            })?;

            Ok(ModelPredicate::UnaryFieldComparison {
                field: field.value.clone(),
                field_parent_type: type_name.to_owned(),
                ndc_column: field_mapping.column.clone(),
                operator: UnaryComparisonOperator::IsNull,
                deprecated: field_definition.deprecated.clone(),
            })
        }

        open_dds::permissions::ModelPredicate::Relationship(
            open_dds::permissions::RelationshipPredicate {
                name: relationship_name,
                predicate,
            },
        ) => {
            if let Some(nested_predicate) = predicate {
                let relationship = &object_type_representation
                    .relationship_fields
                    .get(&relationship_name.value)
                    .ok_or_else(|| TypePredicateError::UnknownRelationshipInTypePredicate {
                        relationship_name: relationship_name.clone(),
                        type_name: type_name.clone(),
                    })?;

                match &relationship.target {
                    object_relationships::RelationshipTarget::Command { .. } => {
                        Err(TypePredicateError::UnsupportedFeature {
                            message: "Predicate cannot be built using command relationships"
                                .to_string(),
                        })
                    }
                    object_relationships::RelationshipTarget::Model(model_relationship_target) => {
                        let object_relationships::ModelRelationshipTarget {
                            model_name,
                            relationship_type,
                            target_typename,
                            mappings,
                            relationship_aggregate: _,
                        } = model_relationship_target.as_ref();
                        let target_model = models.get(model_name).ok_or_else(|| {
                            TypePredicateError::UnknownModelUsedInRelationshipTypePredicate {
                                type_name: type_name.clone(),
                                target_model_name: model_name.clone(),
                                relationship_name: relationship_name.value.clone(),
                            }
                        })?;

                        if let Some(target_model_source) = &target_model.inner.source {
                            // Only relationships targetting the models within the same subgraph are allowed
                            if target_model_source.data_connector.name.subgraph
                                != data_connector_link.name.subgraph
                            {
                                return Err(TypePredicateError::RelationshipAcrossSubgraphs {
                                    relationship_name: relationship_name.clone(),
                                    source_data_connector: data_connector_link.name.clone(),
                                    target_data_connector: target_model_source
                                        .data_connector
                                        .name
                                        .clone(),
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
                                        comparison_operators,
                                        ..
                                    } = data_connector_field_mappings
                                        .get(source_field)
                                        .ok_or_else(|| TypePredicateError::UnknownFieldInModelRelationshipTargetMapping {
                                            field_name: source_field.clone(),
                                            type_name: type_name.clone(),
                                            relationship_name: relationship_name.value.clone(),
                                        })?;

                                    let equal_operators = comparison_operators
                                        .clone()
                                        .map(|ops| ops.eq_operator)
                                        .unwrap_or_default();

                                    if equal_operators.is_none() {
                                        return Err(TypePredicateError::MissingEqualOperator {
                                                    location: format!("While building a remote relationship predicate {relationship_name}"),
                                                    type_name: type_name.clone(),
                                                    field_name: source_field.clone(),
                                                    ndc_column: column.clone(),
                                                    data_connector_name: data_connector_link
                                                        .name
                                                        .clone(),
                                                });
                                    }
                                }
                            }

                            let target_source = ModelTargetSource::from_model_source(
                                &target_model_source.clone(),
                                relationship,
                            )
                            .map_err(|error| TypePredicateError::OtherError(Box::new(error)))?;

                            let target_object_type_representation = object_types
                                .get(&target_model.inner.data_type)
                                .ok_or_else(|| {
                                    TypePredicateError::OtherError(Box::new(Error::UnknownType {
                                        data_type: target_model.inner.data_type.clone(),
                                    }))
                                })?;

                            // look up this type in the context of it's data connector
                            // so that we use the correct column names for the data source
                            let target_data_connector_field_mappings = target_object_type_representation.type_mappings.get(
                                    &target_source.model.data_connector.name,
                                    &target_source.model.collection_type
                                )
                                .map(|type_mapping| match type_mapping {
                                    object_types::TypeMapping::Object {
                                        field_mappings, ..
                                    } => field_mappings,
                                })
                                .ok_or_else(||TypePredicateError::OtherError(Box::new(Error::from(object_types::ObjectTypesError::DataConnectorTypeMappingValidationError {
                                    type_name: target_typename.clone(),
                                    error: object_types::TypeMappingValidationError::DataConnectorTypeMappingNotFound {
                                        object_type_name: target_typename.clone(),
                                        data_connector_name: target_source.model.data_connector.name.clone(),
                                        data_connector_object_type: target_source.model.collection_type.clone()
                                    },
                                }))))?;

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
                                    None,
                                )
                                .map_err(|error| {
                                    TypePredicateError::TypeMappingCollectionError {
                                        type_name: type_name.clone(),
                                        error,
                                    }
                                })?;
                            }

                            let annotation = PredicateRelationshipInfo {
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
                                .ok_or_else(|| TypePredicateError::ObjectTypeNotFound {
                                    type_name: target_model.inner.data_type.clone(),
                                })?;

                            // // if a boolean expression type was provided, we can find the
                            // // target boolean expression type by following the appropriate
                            // // `comparable_relationship` field
                            let target_boolean_expression =
                                lookup_relationship_in_boolean_expression(
                                    boolean_expression,
                                    relationship_name,
                                    boolean_expression_types,
                                    flags,
                                )?;

                            let target_model_predicate = resolve_model_predicate_with_type(
                                flags,
                                nested_predicate,
                                &target_model.inner.data_type,
                                target_object_type,
                                target_boolean_expression,
                                target_data_connector_field_mappings,
                                &target_source.model.data_connector,
                                subgraph,
                                scalars,
                                object_types,
                                scalar_types,
                                boolean_expression_types,
                                models,
                                &target_model.inner.type_fields,
                            )?;

                            Ok(ModelPredicate::Relationship {
                                relationship_info: annotation,
                                predicate: Box::new(target_model_predicate),
                            })
                        } else {
                            Err(
                                TypePredicateError::TargetSourceRequiredForRelationshipPredicate {
                                    source_type_name: type_name.clone(),
                                    target_model_name: target_model.inner.name.clone(),
                                },
                            )
                        }
                    }
                }
            } else {
                Err(
                    TypePredicateError::NoPredicateDefinedForRelationshipPredicate {
                        type_name: type_name.clone(),
                        relationship_name: relationship_name.clone(),
                    },
                )
            }
        }

        open_dds::permissions::ModelPredicate::Not(predicate) => {
            let resolved_predicate = resolve_model_predicate_with_type(
                flags,
                predicate,
                type_name,
                object_type_representation,
                boolean_expression,
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
            Ok(ModelPredicate::Not(Box::new(resolved_predicate)))
        }
        open_dds::permissions::ModelPredicate::And(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate_with_type(
                    flags,
                    predicate,
                    type_name,
                    object_type_representation,
                    boolean_expression,
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
            Ok(ModelPredicate::And(resolved_predicates))
        }
        open_dds::permissions::ModelPredicate::Or(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate_with_type(
                    flags,
                    predicate,
                    type_name,
                    object_type_representation,
                    boolean_expression,
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
            Ok(ModelPredicate::Or(resolved_predicates))
        }
    }
}

// if we use a relationship in a predicate, we should be able to find it in our
// `BooleanExpressionType` and use it
fn lookup_relationship_in_boolean_expression<'a>(
    boolean_expression: Option<&boolean_expressions::ResolvedObjectBooleanExpressionType>,
    relationship_name: &Spanned<open_dds::relationships::RelationshipName>,
    boolean_expression_types: &'a boolean_expressions::BooleanExpressionTypes,
    flags: &open_dds::flags::OpenDdFlags,
) -> Result<Option<&'a boolean_expressions::ResolvedObjectBooleanExpressionType>, TypePredicateError>
{
    let Some((bool_exp_name, fields)) =
        boolean_expression.and_then(|b| b.get_fields(flags).map(|f| (&b.name, f)))
    else {
        return Ok(None);
    };

    // lookup relationship in boolean expression type's
    // comparable relationships
    let comparable_relationship = fields
        .relationship_fields
        .get(relationship_name.as_str())
        .ok_or_else(
            || TypePredicateError::RelationshipNotComparableInTypePredicate {
                boolean_expression_type_name: bool_exp_name.clone(),
                relationship_name: relationship_name.clone(),
            },
        )?;

    // lookup the boolean expression type for this comparable
    // relationship and fetch it from metadata
    let relationship_boolean_expression = boolean_expression_types
        .objects
        .get(&comparable_relationship.boolean_expression_type)
        .ok_or_else(|| TypePredicateError::BooleanExpressionNotFound {
            boolean_expression_name: comparable_relationship.boolean_expression_type.clone(),
        })?;

    Ok(Some(relationship_boolean_expression))
}

// this is only used for the older `ObjectBooleanExpressionType` where we
// don't have this information explicitly provided in metadata
fn resolve_binary_operator_for_type<'a>(
    operator: &'a open_dds::types::OperatorName,
    type_name: &'a Qualified<CustomTypeName>,
    data_connector: &'a Qualified<DataConnectorName>,
    field_name: &'a Spanned<FieldName>,
    fields: &'a IndexMap<FieldName, object_types::FieldDefinition>,
    scalars: &'a data_connector_scalar_types::DataConnectorScalars,
    ndc_scalar_type: &'a ndc_models::ScalarType,
    subgraph: &'a SubgraphName,
) -> Result<(DataConnectorOperatorName, QualifiedTypeReference), TypePredicateError> {
    let field_definition = fields.get(&field_name.value).ok_or_else(|| {
        TypePredicateError::UnknownFieldInTypePredicate {
            field_name: field_name.clone(),
            type_name: type_name.clone(),
        }
    })?;

    // this function is only used for `ObjectBooleanExpressionType` where we do not have a concept
    // of mapping OpenDD operator names to NDC operator names, so we just cast it and yolo
    let ndc_operator_name = DataConnectorOperatorName::new(operator.inner().clone());

    let comparison_operator_definition = &ndc_scalar_type
        .comparison_operators
        .get(ndc_operator_name.as_str())
        .ok_or_else(|| TypePredicateError::InvalidOperatorInTypePredicate {
            type_name: type_name.clone(),
            operator_name: operator.clone(),
        })?;

    match comparison_operator_definition {
        ndc_models::ComparisonOperatorDefinition::Equal
        | ndc_models::ComparisonOperatorDefinition::LessThan
        | ndc_models::ComparisonOperatorDefinition::LessThanOrEqual
        | ndc_models::ComparisonOperatorDefinition::GreaterThan
        | ndc_models::ComparisonOperatorDefinition::GreaterThanOrEqual
        | ndc_models::ComparisonOperatorDefinition::Contains
        | ndc_models::ComparisonOperatorDefinition::ContainsInsensitive
        | ndc_models::ComparisonOperatorDefinition::StartsWith
        | ndc_models::ComparisonOperatorDefinition::StartsWithInsensitive
        | ndc_models::ComparisonOperatorDefinition::EndsWith
        | ndc_models::ComparisonOperatorDefinition::EndsWithInsensitive => {
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
            scalar_boolean_expressions::resolve_ndc_type(
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
        object_relationships::ObjectTypeWithRelationships,
    >,
) -> type_permissions::ObjectTypesWithPermissions {
    type_permissions::ObjectTypesWithPermissions(
        object_types_with_relationships
            .iter()
            .map(
                |(
                    object_name,
                    object_relationships::ObjectTypeWithRelationships {
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
