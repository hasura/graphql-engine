mod types;
use crate::helpers::typecheck;
use crate::stages::{
    data_connector_scalar_types, models, object_boolean_expressions, object_types, relationships,
    scalar_types,
};
use crate::types::permission::ValueExpression;
use indexmap::IndexMap;
use open_dds::{models::ModelName, types::CustomTypeName};
use std::collections::BTreeMap;
pub use types::{
    FilterPermission, ModelPredicate, ModelTargetSource, ModelWithPermissions,
    PredicateRelationshipInfo, SelectPermission,
};

use crate::helpers::argument::resolve_value_expression_for_argument;
use crate::types::error::{Error, RelationshipError};

use crate::helpers::types::mk_name;
use crate::types::subgraph::{
    mk_qualified_type_name, Qualified, QualifiedBaseType, QualifiedTypeReference,
};

use ndc_models;
use open_dds::permissions::{FieldIsNullPredicate, NullableModelPredicate, RelationshipPredicate};
use open_dds::{
    arguments::ArgumentName,
    data_connector::DataConnectorName,
    permissions::{ModelPermissionsV1, Role},
    types::{FieldName, OperatorName},
};

/// resolve model permissions
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<IndexMap<Qualified<ModelName>, ModelWithPermissions>, Error> {
    let mut models_with_permissions: IndexMap<Qualified<ModelName>, ModelWithPermissions> = models
        .iter()
        .map(|(model_name, model)| {
            (
                model_name.clone(),
                ModelWithPermissions {
                    model: model.clone(),
                    select_permissions: BTreeMap::new(),
                },
            )
        })
        .collect();

    // Note: Model permissions's predicate can include the relationship field,
    // hence Model permissions should be resolved after the relationships of a
    // model is resolved.
    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: permissions,
    } in &metadata_accessor.model_permissions
    {
        let model_name = Qualified::new(subgraph.to_string(), permissions.model_name.clone());
        let model = models_with_permissions
            .get_mut(&model_name)
            .ok_or_else(|| Error::UnknownModelInModelSelectPermissions {
                model_name: model_name.clone(),
            })?;

        if model.select_permissions.is_empty() {
            let select_permissions = resolve_model_select_permissions(
                &model.model,
                subgraph,
                permissions,
                data_connectors,
                object_types,
                scalar_types,
                models, // This is required to get the model for the relationship target
                object_boolean_expression_types,
            )?;

            model.select_permissions = select_permissions;
        } else {
            return Err(Error::DuplicateModelSelectPermission {
                model_name: model_name.clone(),
            });
        }
    }
    Ok(models_with_permissions)
}

// helper function to resolve ndc types to dds type based on scalar type representations
pub(crate) fn resolve_ndc_type(
    data_connector: &Qualified<DataConnectorName>,
    source_type: &ndc_models::Type,
    scalars: &BTreeMap<&str, data_connector_scalar_types::ScalarTypeWithRepresentationInfo>,
    subgraph: &str,
) -> Result<QualifiedTypeReference, Error> {
    match source_type {
        ndc_models::Type::Named { name } => {
            let scalar_type =
                scalars
                    .get(name.as_str())
                    .ok_or(Error::UnknownScalarTypeInDataConnector {
                        data_connector: data_connector.clone(),
                        scalar_type: name.clone(),
                    })?;
            scalar_type
                .representation
                .clone()
                .ok_or(Error::DataConnectorScalarRepresentationRequired {
                    data_connector: data_connector.clone(),
                    scalar_type: name.clone(),
                })
                .map(|ty| QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::Named(mk_qualified_type_name(
                        &ty, subgraph,
                    )),
                    nullable: false,
                })
        }
        ndc_models::Type::Nullable { underlying_type } => {
            resolve_ndc_type(data_connector, underlying_type, scalars, subgraph).map(|ty| {
                QualifiedTypeReference {
                    underlying_type: ty.underlying_type,
                    nullable: true,
                }
            })
        }
        ndc_models::Type::Array { element_type } => {
            resolve_ndc_type(data_connector, element_type, scalars, subgraph).map(|ty| {
                QualifiedTypeReference {
                    underlying_type: QualifiedBaseType::List(Box::new(ty)),
                    nullable: false,
                }
            })
        }
        ndc_models::Type::Predicate { .. } => Err(Error::PredicateTypesUnsupported),
    }
}

fn resolve_model_predicate(
    model_predicate: &open_dds::permissions::ModelPredicate,
    model: &models::Model,
    subgraph: &str,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    fields: &IndexMap<FieldName, object_types::FieldDefinition>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
) -> Result<ModelPredicate, Error> {
    match model_predicate {
        open_dds::permissions::ModelPredicate::FieldComparison(
            open_dds::permissions::FieldComparisonPredicate {
                field,
                operator,
                value,
            },
        ) => {
            // TODO: (anon) typecheck the value expression with the field
            // TODO: resolve the "in" operator too (ndc_models::BinaryArrayComparisonOperator)
            if let Some(model_source) = &model.source {
                // Get field mappings of model data type
                let object_types::TypeMapping::Object { field_mappings, .. } = model_source
                    .type_mappings
                    .get(&model.data_type)
                    .ok_or(Error::TypeMappingRequired {
                        model_name: model.name.clone(),
                        type_name: model.data_type.clone(),
                        data_connector: model_source.data_connector.name.clone(),
                    })?;

                // Determine field_mapping for the predicate field
                let field_mapping = field_mappings.get(field).ok_or_else(|| {
                    Error::UnknownFieldInSelectPermissionsDefinition {
                        field_name: field.clone(),
                        model_name: model.name.clone(),
                    }
                })?;
                // Determine ndc type of the field
                let field_ndc_type = &field_mapping.column_type;

                // Determine whether the ndc type is a simple scalar
                // Get available scalars defined in the data connector
                let scalars = &data_connectors
                    .data_connectors_with_scalars
                    .get(&model_source.data_connector.name)
                    .ok_or(Error::UnknownModelDataConnector {
                        model_name: model.name.clone(),
                        data_connector: model_source.data_connector.name.clone(),
                    })?
                    .scalars;

                // Get scalar type info from the data connector
                let (_, scalar_type_info) =
                    data_connector_scalar_types::get_simple_scalar(field_ndc_type.clone(), scalars)
                        .ok_or_else(|| Error::UnsupportedFieldInSelectPermissionsPredicate {
                            field_name: field.clone(),
                            model_name: model.name.clone(),
                        })?;

                let (resolved_operator, argument_type) = resolve_binary_operator_for_model(
                    operator,
                    &model.name,
                    &model_source.data_connector.name,
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
                    open_dds::permissions::ValueExpression::BooleanExpression(_model_predicate) => {
                        Err(Error::NestedPredicateInSelectPermissionPredicate {
                            model_name: model.name.clone(),
                        })
                    }
                }?;

                Ok(ModelPredicate::BinaryFieldComparison {
                    field: field.clone(),
                    ndc_column: field_mapping.column.clone(),
                    operator: resolved_operator,
                    argument_type,
                    value: value_expression,
                })
            } else {
                Err(Error::ModelSourceRequiredForPredicate {
                    model_name: model.name.clone(),
                })
            }
        }
        open_dds::permissions::ModelPredicate::FieldIsNull(FieldIsNullPredicate { field }) => {
            if let Some(model_source) = &model.source {
                // Get field mappings of model data type
                let object_types::TypeMapping::Object { field_mappings, .. } = model_source
                    .type_mappings
                    .get(&model.data_type)
                    .ok_or(Error::TypeMappingRequired {
                        model_name: model.name.clone(),
                        type_name: model.data_type.clone(),
                        data_connector: model_source.data_connector.name.clone(),
                    })?;
                // Determine field_mapping for the predicate field
                let field_mapping = field_mappings.get(field).ok_or_else(|| {
                    Error::UnknownFieldInSelectPermissionsDefinition {
                        field_name: field.clone(),
                        model_name: model.name.clone(),
                    }
                })?;

                Ok(ModelPredicate::UnaryFieldComparison {
                    field: field.clone(),
                    ndc_column: field_mapping.column.clone(),
                    operator: ndc_models::UnaryComparisonOperator::IsNull,
                })
            } else {
                Err(Error::ModelSourceRequiredForPredicate {
                    model_name: model.name.clone(),
                })
            }
        }
        open_dds::permissions::ModelPredicate::Relationship(RelationshipPredicate {
            name,
            predicate,
        }) => {
            if let Some(nested_predicate) = predicate {
                let object_type_representation = get_model_object_type_representation(
                    object_types,
                    &model.data_type,
                    &model.name,
                )?;
                let relationship_field_name = mk_name(&name.0)?;
                let relationship = &object_type_representation
                    .relationships
                    .get(&relationship_field_name)
                    .ok_or_else(|| Error::UnknownRelationshipInSelectPermissionsPredicate {
                        relationship_name: name.clone(),
                        model_name: model.name.clone(),
                        type_name: model.data_type.clone(),
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
                            Error::UnknownModelUsedInRelationshipSelectPermissionsPredicate {
                                model_name: model.name.clone(),
                                target_model_name: model_name.clone(),
                                relationship_name: name.clone(),
                            }
                        })?;

                        // predicates with relationships is currently only supported for local relationships
                        if let (Some(target_model_source), Some(model_source)) =
                            (&target_model.source, &model.source)
                        {
                            if target_model_source.data_connector.name
                                == model_source.data_connector.name
                            {
                                let target_source = ModelTargetSource::from_model_source(
                                    target_model_source,
                                    relationship,
                                )
                                .map_err(|_| Error::RelationshipError {
                                    relationship_error:
                                        RelationshipError::NoRelationshipCapabilitiesDefined {
                                            relationship_name: relationship.name.clone(),
                                            type_name: model.data_type.clone(),
                                            data_connector_name: target_model_source
                                                .data_connector
                                                .name
                                                .clone(),
                                        },
                                })?;

                                let annotation = PredicateRelationshipInfo {
                                    source_type: relationship.source.clone(),
                                    relationship_name: relationship.name.clone(),
                                    target_model_name: model_name.clone(),
                                    target_source: target_source.clone(),
                                    target_type: target_typename.clone(),
                                    relationship_type: relationship_type.clone(),
                                    mappings: mappings.clone(),
                                    source_data_connector: model_source.data_connector.clone(),
                                    source_type_mappings: model_source.type_mappings.clone(),
                                };

                                let target_model_predicate = resolve_model_predicate(
                                    nested_predicate,
                                    target_model,
                                    // local relationships exists in the same subgraph as the source model
                                    subgraph,
                                    data_connectors,
                                    &target_model.type_fields,
                                    object_types,
                                    models,
                                )?;

                                Ok(ModelPredicate::Relationship {
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
                            Err(
                                Error::ModelAndTargetSourceRequiredForRelationshipPredicate {
                                    source_model_name: model.name.clone(),
                                    target_model_name: target_model.name.clone(),
                                },
                            )
                        }
                    }
                }
            } else {
                Err(Error::NoPredicateDefinedForRelationshipPredicate {
                    model_name: model.name.clone(),
                    relationship_name: name.clone(),
                })
            }
        }
        open_dds::permissions::ModelPredicate::Not(predicate) => {
            let resolved_predicate = resolve_model_predicate(
                predicate,
                model,
                subgraph,
                data_connectors,
                fields,
                object_types,
                models,
            )?;
            Ok(ModelPredicate::Not(Box::new(resolved_predicate)))
        }
        open_dds::permissions::ModelPredicate::And(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate(
                    predicate,
                    model,
                    subgraph,
                    data_connectors,
                    fields,
                    object_types,
                    models,
                )?);
            }
            Ok(ModelPredicate::And(resolved_predicates))
        }
        open_dds::permissions::ModelPredicate::Or(predicates) => {
            let mut resolved_predicates = Vec::new();
            for predicate in predicates {
                resolved_predicates.push(resolve_model_predicate(
                    predicate,
                    model,
                    subgraph,
                    data_connectors,
                    fields,
                    object_types,
                    models,
                )?);
            }
            Ok(ModelPredicate::Or(resolved_predicates))
        }
    }
}

#[allow(clippy::too_many_arguments)]
/// this is mostly the same code as `resolve_binary_operator_for_type`, they could probably be
/// recombined if we nest our error types better, so we don't need to specify the model name this deep
/// into the code
fn resolve_binary_operator_for_model(
    operator: &OperatorName,
    model_name: &Qualified<ModelName>,
    data_connector: &Qualified<DataConnectorName>,
    field_name: &FieldName,
    fields: &IndexMap<FieldName, object_types::FieldDefinition>,
    scalars: &BTreeMap<&str, data_connector_scalar_types::ScalarTypeWithRepresentationInfo>,
    ndc_scalar_type: &ndc_models::ScalarType,
    subgraph: &str,
) -> Result<(String, QualifiedTypeReference), Error> {
    let field_definition =
        fields
            .get(field_name)
            .ok_or_else(|| Error::UnknownFieldInSelectPermissionsDefinition {
                field_name: field_name.clone(),
                model_name: model_name.clone(),
            })?;
    let comparison_operator_definition = &ndc_scalar_type
        .comparison_operators
        .get(&operator.0)
        .ok_or_else(|| Error::InvalidOperatorInModelSelectPermission {
            model_name: model_name.clone(),
            operator_name: operator.clone(),
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

/// Gets the `type_permissions::ObjectTypeWithPermissions` of the type identified with the
/// `data_type`, it will throw an error if the type is not found to be an object
/// or if the model has an unknown data type.
fn get_model_object_type_representation<'s>(
    object_types: &'s BTreeMap<
        Qualified<CustomTypeName>,
        relationships::ObjectTypeWithRelationships,
    >,
    data_type: &Qualified<CustomTypeName>,
    model_name: &Qualified<ModelName>,
) -> Result<&'s relationships::ObjectTypeWithRelationships, crate::Error> {
    match object_types.get(data_type) {
        Some(object_type_representation) => Ok(object_type_representation),
        None => Err(Error::UnknownModelDataType {
            model_name: model_name.clone(),
            data_type: data_type.clone(),
        }),
    }
}

// get the ndc_models::Type for an argument if it is available
fn get_model_source_argument<'a>(
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

pub fn resolve_model_select_permissions(
    model: &models::Model,
    subgraph: &str,
    model_permissions: &ModelPermissionsV1,
    data_connectors: &data_connector_scalar_types::DataConnectorsWithScalars,
    object_types: &BTreeMap<Qualified<CustomTypeName>, relationships::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    models: &IndexMap<Qualified<ModelName>, models::Model>,
    object_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_boolean_expressions::ObjectBooleanExpressionType,
    >,
) -> Result<BTreeMap<Role, SelectPermission>, Error> {
    let mut validated_permissions = BTreeMap::new();
    for model_permission in &model_permissions.permissions {
        if let Some(select) = &model_permission.select {
            let resolved_predicate = match &select.filter {
                NullableModelPredicate::NotNull(model_predicate) => resolve_model_predicate(
                    model_predicate,
                    model,
                    subgraph,
                    data_connectors,
                    &model.type_fields,
                    object_types,
                    models,
                )
                .map(FilterPermission::Filter)?,
                NullableModelPredicate::Null(()) => FilterPermission::AllowAll,
            };

            let mut argument_presets = BTreeMap::new();

            for argument_preset in &select.argument_presets {
                if argument_presets.contains_key(&argument_preset.argument) {
                    return Err(Error::DuplicateModelArgumentPreset {
                        model_name: model.name.clone(),
                        argument_name: argument_preset.argument.clone(),
                    });
                }

                let source_argument_type =
                    get_model_source_argument(&argument_preset.argument, model);

                match model.arguments.get(&argument_preset.argument) {
                    Some(argument) => {
                        let value_expression = resolve_value_expression_for_argument(
                            &argument_preset.argument,
                            &argument_preset.value,
                            &argument.argument_type,
                            source_argument_type,
                            subgraph,
                            object_types,
                            scalar_types,
                            object_boolean_expression_types,
                            models,
                            data_connectors,
                        )?;

                        // additionally typecheck literals
                        // we do this outside the argument resolve so that we can emit a model-specific error
                        // on typechecking failure
                        typecheck::typecheck_value_expression(
                            &argument.argument_type,
                            &argument_preset.value,
                        )
                        .map_err(|type_error| {
                            Error::ModelArgumentPresetTypeError {
                                model_name: model.name.clone(),
                                argument_name: argument_preset.argument.clone(),
                                type_error,
                            }
                        })?;

                        argument_presets.insert(
                            argument_preset.argument.clone(),
                            (argument.argument_type.clone(), value_expression),
                        );
                    }
                    None => {
                        return Err(Error::ModelArgumentPresetMismatch {
                            model_name: model.name.clone(),
                            argument_name: argument_preset.argument.clone(),
                        });
                    }
                }
            }

            let resolved_permission = SelectPermission {
                filter: resolved_predicate.clone(),
                argument_presets,
            };
            validated_permissions.insert(model_permission.role.clone(), resolved_permission);
        }
    }
    Ok(validated_permissions)
}
