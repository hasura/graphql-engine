use super::predicate;
use super::types::ModelPermissionIssue;
use super::types::{FilterPermission, SelectPermission};
use super::{ModelPermissionError, NamedModelPermissionError};
use crate::ArgumentInfo;
use crate::helpers::argument::resolve_value_expression_for_argument;
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, models_graphql, object_relationships,
    scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;

use indexmap::IndexMap;
use open_dds::permissions::NullableModelPredicate;
use open_dds::permissions::{ModelPermissionsV1, Role};
use open_dds::query::ArgumentName;
use open_dds::spanned::Spanned;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};
use std::collections::{BTreeMap, BTreeSet};

pub fn resolve_all_model_select_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::Model,
    arguments: &IndexMap<ArgumentName, ArgumentInfo>,
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
                arguments,
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
    model: &models_graphql::Model,
    arguments: &IndexMap<ArgumentName, ArgumentInfo>,
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
            .map(FilterPermission::Filter)?
        }
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
    let resolved_permission = SelectPermission {
        filter: resolved_predicate,
        argument_presets,
        allow_subscriptions: select_perms.allow_subscriptions,
    };

    Ok(resolved_permission)
}
