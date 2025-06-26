use super::types::{FilterPermission, ModelPermissionIssue, ResolvedPermissions, SelectPermission};
use super::{
    ModelPermissionError, NamedModelPermissionError, RelationalDeletePermission,
    RelationalInsertPermission, RelationalUpdatePermission, predicate,
};
use crate::helpers::argument::resolve_value_expression_for_argument;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, models_graphql, object_relationships,
    scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;
use crate::{
    ArgumentInfo, ModelsError, QualifiedTypeReference, ValueExpressionOrPredicate, data_connectors,
};

use indexmap::IndexMap;
use open_dds::permissions::{
    ModelPermissionOperand, ModelPermissionsV2, NullableModelPredicate, Role,
};
use open_dds::query::ArgumentName;
use open_dds::spanned::Spanned;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};
use std::collections::{BTreeMap, BTreeSet};

pub fn resolve_all_model_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::Model,
    arguments: &IndexMap<ArgumentName, ArgumentInfo>,
    model_permissions: &ModelPermissionsV2,
    boolean_expression: Option<&boolean_expressions::ResolvedObjectBooleanExpressionType>,
    data_connectors: &data_connectors::DataConnectors,
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
) -> Result<BTreeMap<Role, ResolvedPermissions>, Error> {
    let mut validated_permissions = BTreeMap::new();
    let mut resolved_roles = BTreeSet::new();

    match &model_permissions.permissions {
        ModelPermissionOperand::RoleBased(role_based_model_permissions) => {
            for model_permission in role_based_model_permissions {
                if !resolved_roles.insert(model_permission.role.value.clone()) {
                    issues.push(ModelPermissionIssue::DuplicateRole {
                        role: model_permission.role.clone(),
                        model_name: model.name.clone(),
                    });
                    // Continue processing this role's permissions, but we've already
                    // recorded the duplicate role issue
                }

                let mut resolved_permission = ResolvedPermissions {
                    select: None,
                    relational_insert: None,
                    relational_update: None,
                    relational_delete: None,
                };

                // Resolve select permissions
                if let Some(select_perms) = &model_permission.select {
                    let filter = resolve_model_select_permissions(
                        select_perms,
                        &model_permission.role,
                        flags,
                        model,
                        boolean_expression,
                        data_connector_scalars,
                        object_types,
                        scalar_types,
                        boolean_expression_types,
                        models,
                    )?;

                    let argument_presets = resolve_model_argument_presets(
                        select_perms,
                        &model_permission.role,
                        flags,
                        model,
                        arguments,
                        data_connector_scalars,
                        object_types,
                        scalar_types,
                        boolean_expression_types,
                        models,
                        issues,
                    )?;

                    let select_permission = SelectPermission {
                        filter,
                        argument_presets,
                        allow_subscriptions: select_perms.allow_subscriptions,
                    };

                    resolved_permission.select = Some(select_permission);
                }

                // Resolve relational insert permissions
                if let Some(_relational_insert) = &model_permission.relational_insert {
                    let collection_info =
                        lookup_collection_info(model, model_permission, data_connectors)?;
                    if !collection_info
                        .relational_mutations
                        .as_ref()
                        .is_some_and(|caps| caps.insertable)
                    {
                        return Err(Error::ModelPermissionsError(NamedModelPermissionError {
                            model_name: model.name.clone(),
                            role: model_permission.role.clone(),
                            error: ModelPermissionError::RelationalInsertNotSupported,
                        }));
                    }

                    resolved_permission.relational_insert = Some(RelationalInsertPermission {});
                }

                // Resolve relational update permissions
                if let Some(_relational_update) = &model_permission.relational_update {
                    let collection_info =
                        lookup_collection_info(model, model_permission, data_connectors)?;
                    if !collection_info
                        .relational_mutations
                        .as_ref()
                        .is_some_and(|caps| caps.updatable)
                    {
                        return Err(Error::ModelPermissionsError(NamedModelPermissionError {
                            model_name: model.name.clone(),
                            role: model_permission.role.clone(),
                            error: ModelPermissionError::RelationalUpdateNotSupported,
                        }));
                    }

                    resolved_permission.relational_update = Some(RelationalUpdatePermission {});
                }

                // Resolve relational delete permissions
                if let Some(_relational_delete) = &model_permission.relational_delete {
                    let collection_info =
                        lookup_collection_info(model, model_permission, data_connectors)?;
                    if !collection_info
                        .relational_mutations
                        .as_ref()
                        .is_some_and(|caps| caps.deletable)
                    {
                        return Err(Error::ModelPermissionsError(NamedModelPermissionError {
                            model_name: model.name.clone(),
                            role: model_permission.role.clone(),
                            error: ModelPermissionError::RelationalDeleteNotSupported,
                        }));
                    }

                    resolved_permission.relational_delete = Some(RelationalDeletePermission {});
                }

                // Insert the resolved permissions for this role
                validated_permissions
                    .insert(model_permission.role.value.clone(), resolved_permission);
            }
            Ok(validated_permissions)
        }
    }
}

fn lookup_collection_info<'a>(
    model: &'a crate::Model,
    model_permission: &'a open_dds::permissions::ModelPermission,
    data_connectors: &'a data_connectors::DataConnectors<'_>,
) -> Result<&'a ndc_models::CollectionInfo, Error> {
    let model_source = model.source.as_ref().ok_or_else(|| {
        Error::ModelPermissionsError(NamedModelPermissionError {
            model_name: model.name.clone(),
            role: model_permission.role.clone(),
            error: ModelPermissionError::ModelSourceRequiredForRelationalPermissions,
        })
    })?;

    let collection_info = data_connectors
        .0
        .get(&model_source.data_connector.name)
        .ok_or_else(|| {
            Error::ModelsError(ModelsError::UnknownModelDataConnector {
                model_name: model.name.clone(),
                data_connector: model_source.data_connector.name.clone(),
                data_connector_path: None,
            })
        })?
        .schema
        .collections
        .get(model_source.collection.as_str())
        .ok_or_else(|| {
            Error::ModelPermissionsError(NamedModelPermissionError {
                model_name: model.name.clone(),
                role: model_permission.role.clone(),
                error: ModelPermissionError::UnknownModelCollection {
                    data_connector: model_source.data_connector.name.clone(),
                    collection: model_source.collection.clone(),
                },
            })
        })?;

    Ok(collection_info)
}

fn resolve_model_select_permissions(
    select_perms: &open_dds::permissions::SelectPermission,
    role: &Spanned<open_dds::permissions::Role>,
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::Model,
    boolean_expression: Option<&boolean_expressions::ResolvedObjectBooleanExpressionType>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, crate::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
) -> Result<FilterPermission, Error> {
    match &select_perms.filter {
        NullableModelPredicate::NotNull(model_predicate) => {
            predicate::resolve_model_predicate_with_model(
                flags,
                model_predicate,
                model,
                boolean_expression,
                data_connector_scalars,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
            )
            .map_err(|error| {
                Error::ModelPermissionsError(NamedModelPermissionError {
                    model_name: model.name.clone(),
                    role: role.clone(),
                    error,
                })
            })
            .map(FilterPermission::Filter)
        }
        NullableModelPredicate::Null(()) => Ok(FilterPermission::AllowAll),
    }
}

fn resolve_model_argument_presets(
    select_perms: &open_dds::permissions::SelectPermission,
    role: &Spanned<open_dds::permissions::Role>,
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::Model,
    arguments: &IndexMap<ArgumentName, ArgumentInfo>,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, crate::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpressionOrPredicate)>, Error> {
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

        let argument = arguments
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
            &model_source.data_connector,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
            &model_source.type_mappings,
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

    Ok(argument_presets)
}
