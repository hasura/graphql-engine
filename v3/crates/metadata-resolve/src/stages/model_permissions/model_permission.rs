use super::types::{
    FilterPermission, ModelAuthorizationRule, ModelInputPermission, ModelPermission,
    ModelPermissionIssue, ModelPermissions, SelectPermission,
};
use super::{ModelPermissionError, NamedModelPermissionError, RelationalOperation, predicate};
use crate::helpers::argument::resolve_value_expression_for_argument;
use crate::stages::type_permissions::{
    ObjectTypeToCheck, resolve_condition, types_that_use_fancy_auth,
};
use crate::stages::{
    boolean_expressions, data_connector_scalar_types, models_graphql, object_relationships,
    scalar_types,
};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;
use crate::{
    AllowOrDeny, BinaryOperation, Condition, Conditions, ModelsError, QualifiedTypeReference,
    ValueExpression, ValueExpressionOrPredicate, data_connectors, unwrap_custom_type_name,
};

use hasura_authn_core::{Role, SESSION_VARIABLE_ROLE, SessionVariableReference};
use indexmap::IndexMap;
use open_dds::permissions::{ModelPermissionOperand, ModelPermissionsV2, NullableModelPredicate};
use open_dds::query::ArgumentName;
use open_dds::spanned::Spanned;
use open_dds::{data_connector::DataConnectorName, models::ModelName, types::CustomTypeName};
use std::collections::{BTreeMap, BTreeSet};

pub fn resolve_all_model_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::ModelWithGraphql,
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
    conditions: &mut Conditions,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<ModelPermissions, Error> {
    warn_on_rules_based_permissions(model, model_permissions, object_types, issues);

    match &model_permissions.permissions {
        ModelPermissionOperand::RoleBased(role_based_model_permissions) => {
            resolve_role_based_model_permissions(
                flags,
                model,
                role_based_model_permissions,
                boolean_expression,
                data_connectors,
                data_connector_scalars,
                object_types,
                scalar_types,
                models,
                boolean_expression_types,
                conditions,
                issues,
            )
        }
        ModelPermissionOperand::RulesBased(model_authorization_rules) => {
            resolve_rules_based_model_permissions(
                flags,
                model,
                model_authorization_rules,
                boolean_expression,
                data_connector_scalars,
                object_types,
                scalar_types,
                models,
                boolean_expression_types,
                conditions,
                issues,
            )
        }
    }
}

pub fn resolve_rules_based_model_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::ModelWithGraphql,
    model_authorization_rules: &[open_dds::authorization::ModelAuthorizationRule],
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
    conditions: &mut Conditions,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<ModelPermissions, Error> {
    let mut authorization_rules = vec![];

    for model_authorization_rule in model_authorization_rules {
        let new_authorization_rules = match model_authorization_rule {
            open_dds::authorization::ModelAuthorizationRule::Allow(
                open_dds::authorization::Allow { condition },
            ) => {
                let condition = condition
                    .as_ref()
                    .map(|condition| conditions.add(resolve_condition(condition, flags)));

                vec![ModelAuthorizationRule::Access {
                    allow_or_deny: AllowOrDeny::Allow,
                    condition,
                }]
            }
            open_dds::authorization::ModelAuthorizationRule::Deny(
                open_dds::authorization::Deny { condition },
            ) => {
                let condition = conditions.add(resolve_condition(condition, flags));

                vec![ModelAuthorizationRule::Access {
                    allow_or_deny: AllowOrDeny::Deny,
                    condition: Some(condition),
                }]
            }
            open_dds::authorization::ModelAuthorizationRule::PresetArgument(
                open_dds::authorization::PresetArgument {
                    argument_name,
                    condition,
                    value,
                },
            ) => {
                let condition = condition
                    .as_ref()
                    .map(|condition| conditions.add(resolve_condition(condition, flags)));

                let (value_expression_or_predicate, argument_type) = resolve_model_argument_preset(
                    argument_name,
                    value,
                    None,
                    flags,
                    model,
                    data_connector_scalars,
                    object_types,
                    scalar_types,
                    boolean_expression_types,
                    models,
                    issues,
                )?;

                vec![match value_expression_or_predicate.split_predicate() {
                    Ok(value_expression) => ModelAuthorizationRule::ArgumentPresetValue {
                        condition,
                        argument_type,
                        argument_name: argument_name.value.clone(),
                        value: value_expression,
                    },
                    Err(boolean_expression) => ModelAuthorizationRule::ArgumentAuthPredicate {
                        condition,
                        argument_name: argument_name.value.clone(),
                        predicate: boolean_expression,
                    },
                }]
            }
            open_dds::authorization::ModelAuthorizationRule::Filter(
                open_dds::authorization::Filter {
                    condition,
                    predicate,
                },
            ) => {
                let condition = condition
                    .as_ref()
                    .map(|condition| conditions.add(resolve_condition(condition, flags)));

                let predicate = predicate::resolve_model_predicate_with_model(
                    flags,
                    predicate,
                    &model.inner,
                    boolean_expression,
                    data_connector_scalars,
                    object_types,
                    scalar_types,
                    boolean_expression_types,
                    models,
                )
                .map_err(|error| {
                    Error::ModelPermissionsError(NamedModelPermissionError {
                        model_name: model.inner.name.clone(),
                        role: None,
                        error,
                    })
                })?;

                vec![ModelAuthorizationRule::Filter {
                    condition,
                    predicate,
                }]
            }
            open_dds::authorization::ModelAuthorizationRule::AllowRelationalOperations(
                open_dds::authorization::AllowRelationalOperations {
                    condition,
                    operations,
                },
            ) => {
                let condition = condition
                    .as_ref()
                    .map(|condition| conditions.add(resolve_condition(condition, flags)));

                operations
                    .iter()
                    .map(|operation| {
                        let relational_operation = match operation {
                            open_dds::authorization::RelationalOperation::Insert => {
                                RelationalOperation::Insert
                            }
                            open_dds::authorization::RelationalOperation::Update => {
                                RelationalOperation::Update
                            }
                            open_dds::authorization::RelationalOperation::Delete => {
                                RelationalOperation::Delete
                            }
                        };
                        ModelAuthorizationRule::RelationalPermission {
                            condition,
                            allow_or_deny: AllowOrDeny::Allow,
                            relational_operation,
                        }
                    })
                    .collect()
            }
            open_dds::authorization::ModelAuthorizationRule::DenyRelationalOperations(
                open_dds::authorization::DenyRelationalOperations {
                    condition,
                    operations,
                },
            ) => {
                let condition = conditions.add(resolve_condition(condition, flags));

                operations
                    .iter()
                    .map(|operation| {
                        let relational_operation = match operation {
                            open_dds::authorization::RelationalOperation::Insert => {
                                RelationalOperation::Insert
                            }
                            open_dds::authorization::RelationalOperation::Update => {
                                RelationalOperation::Update
                            }
                            open_dds::authorization::RelationalOperation::Delete => {
                                RelationalOperation::Delete
                            }
                        };
                        ModelAuthorizationRule::RelationalPermission {
                            condition: Some(condition),
                            allow_or_deny: AllowOrDeny::Deny,
                            relational_operation,
                        }
                    })
                    .collect()
            }
        };
        authorization_rules.extend(new_authorization_rules);
    }

    Ok(ModelPermissions {
        authorization_rules,
        by_role: BTreeMap::new(),
    })
}

pub fn resolve_role_based_model_permissions(
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::ModelWithGraphql,
    role_based_model_permissions: &[open_dds::permissions::ModelPermission],
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
    conditions: &mut Conditions,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<ModelPermissions, Error> {
    let mut resolved_roles = BTreeSet::new();
    let mut authorization_rules = vec![];
    let mut by_role = BTreeMap::new();

    for model_permission in role_based_model_permissions {
        if !resolved_roles.insert(model_permission.role.value.clone()) {
            issues.push(ModelPermissionIssue::DuplicateRole {
                role: model_permission.role.clone(),
                model_name: model.inner.name.clone(),
            });
            // Continue processing this role's permissions, but we've already
            // recorded the duplicate role issue
        }
        let mut model_permission_for_role = ModelPermission {
            select: None,
            input: None,
        };

        // Resolve select permissions
        if let Some(select_perms) = &model_permission.select {
            let filter = resolve_model_select_permissions(
                select_perms,
                &model_permission.role,
                flags,
                &model.inner,
                boolean_expression,
                data_connector_scalars,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
            )?;

            // create authorization rules
            authorization_rules.extend(authorization_rules_for_role(
                &model_permission.role,
                &filter,
                select_perms.allow_subscriptions,
                flags,
                conditions,
            ));

            model_permission_for_role.select = Some(SelectPermission {
                filter,
                allow_subscriptions: select_perms.allow_subscriptions,
            });

            let ArgumentPresets {
                argument_presets,
                authorization_rules: argument_preset_authorization_rules,
            } = resolve_model_argument_presets(
                select_perms,
                &model_permission.role,
                flags,
                model,
                data_connector_scalars,
                object_types,
                scalar_types,
                boolean_expression_types,
                models,
                conditions,
                issues,
            )?;

            authorization_rules.extend(argument_preset_authorization_rules);

            model_permission_for_role.input = Some(ModelInputPermission { argument_presets });
        }

        // Resolve relational insert permissions
        if let Some(_relational_insert) = &model_permission.relational_insert {
            let collection_info =
                lookup_collection_info(&model.inner, model_permission, data_connectors)?;
            if !collection_info
                .relational_mutations
                .as_ref()
                .is_some_and(|caps| caps.insertable)
            {
                return Err(Error::ModelPermissionsError(NamedModelPermissionError {
                    model_name: model.inner.name.clone(),
                    role: Some(model_permission.role.clone()),
                    error: ModelPermissionError::RelationalInsertNotSupported,
                }));
            }

            authorization_rules.push(authorization_rule_for_relational_operation(
                &model_permission.role,
                RelationalOperation::Insert,
                flags,
                conditions,
            ));
        }

        // Resolve relational update permissions
        if let Some(_relational_update) = &model_permission.relational_update {
            let collection_info =
                lookup_collection_info(&model.inner, model_permission, data_connectors)?;
            if !collection_info
                .relational_mutations
                .as_ref()
                .is_some_and(|caps| caps.updatable)
            {
                return Err(Error::ModelPermissionsError(NamedModelPermissionError {
                    model_name: model.inner.name.clone(),
                    role: Some(model_permission.role.clone()),
                    error: ModelPermissionError::RelationalUpdateNotSupported,
                }));
            }

            authorization_rules.push(authorization_rule_for_relational_operation(
                &model_permission.role,
                RelationalOperation::Update,
                flags,
                conditions,
            ));
        }

        // Resolve relational delete permissions
        if let Some(_relational_delete) = &model_permission.relational_delete {
            let collection_info =
                lookup_collection_info(&model.inner, model_permission, data_connectors)?;
            if !collection_info
                .relational_mutations
                .as_ref()
                .is_some_and(|caps| caps.deletable)
            {
                return Err(Error::ModelPermissionsError(NamedModelPermissionError {
                    model_name: model.inner.name.clone(),
                    role: Some(model_permission.role.clone()),
                    error: ModelPermissionError::RelationalDeleteNotSupported,
                }));
            }

            authorization_rules.push(authorization_rule_for_relational_operation(
                &model_permission.role,
                RelationalOperation::Delete,
                flags,
                conditions,
            ));
        }

        by_role.insert(
            model_permission.role.value.clone(),
            model_permission_for_role,
        );
    }

    Ok(ModelPermissions {
        authorization_rules,
        by_role,
    })
}

// Expresses "the user's role matches `role`" as a `Condition`
fn user_matches_role(role: &Role, flags: &open_dds::flags::OpenDdFlags) -> Condition {
    Condition::BinaryOperation {
        op: BinaryOperation::Equals,
        left: ValueExpression::SessionVariable(SessionVariableReference {
            name: SESSION_VARIABLE_ROLE,
            passed_as_json: flags.contains(open_dds::flags::Flag::JsonSessionVariables),
            disallow_unknown_fields: flags
                .contains(open_dds::flags::Flag::DisallowUnknownValuesInArguments),
        }),
        right: ValueExpression::Literal(serde_json::Value::String(role.0.clone())),
    }
}

// given a role and some fields, return a FieldAuthorizationRule
// that allows those exact fields given `x-hasura-role` session variable matches the role
fn authorization_rules_for_role(
    role: &Role,
    filter_permission: &FilterPermission,
    allow_subscriptions: bool,
    flags: &open_dds::flags::OpenDdFlags,
    conditions: &mut Conditions,
) -> Vec<ModelAuthorizationRule> {
    let condition_hash = conditions.add(user_matches_role(role, flags));

    // always add an `allow` rule
    let allow_rule = ModelAuthorizationRule::Access {
        condition: Some(condition_hash),
        allow_or_deny: AllowOrDeny::Allow,
    };

    let allow_subscription_rule = ModelAuthorizationRule::Subscription {
        condition: Some(condition_hash),
        allow_or_deny: if allow_subscriptions {
            AllowOrDeny::Allow
        } else {
            AllowOrDeny::Deny
        },
    };

    match filter_permission {
        FilterPermission::AllowAll => vec![allow_rule, allow_subscription_rule],
        FilterPermission::Filter(model_predicate) => {
            vec![
                allow_rule,
                allow_subscription_rule,
                ModelAuthorizationRule::Filter {
                    predicate: model_predicate.clone(),
                    condition: Some(condition_hash),
                },
            ]
        }
    }
}

// given a role and a preset value, return a ModelAuthorizationRule
// that includes these presets when `x-hasura-role` matches our role
fn authorization_rule_for_argument_preset(
    role: &Role,
    argument_name: &ArgumentName,
    argument_type: &QualifiedTypeReference,
    value_expression_or_predicate: &ValueExpressionOrPredicate,
    flags: &open_dds::flags::OpenDdFlags,
    conditions: &mut Conditions,
) -> ModelAuthorizationRule {
    let condition_hash = conditions.add(user_matches_role(role, flags));

    match value_expression_or_predicate.clone().split_predicate() {
        Err(model_predicate) => ModelAuthorizationRule::ArgumentAuthPredicate {
            argument_name: argument_name.clone(),
            predicate: model_predicate,
            condition: Some(condition_hash),
        },
        Ok(value_expression) => ModelAuthorizationRule::ArgumentPresetValue {
            argument_name: argument_name.clone(),
            argument_type: argument_type.clone(),
            value: value_expression,
            condition: Some(condition_hash),
        },
    }
}

// given a role and a preset value, return a ModelAuthorizationRule
// that includes these presets when `x-hasura-role` matches our role
fn authorization_rule_for_relational_operation(
    role: &Role,
    relational_operation: RelationalOperation,
    flags: &open_dds::flags::OpenDdFlags,
    conditions: &mut Conditions,
) -> ModelAuthorizationRule {
    let condition_hash = conditions.add(user_matches_role(role, flags));

    ModelAuthorizationRule::RelationalPermission {
        condition: Some(condition_hash),
        allow_or_deny: AllowOrDeny::Allow,
        relational_operation,
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
            role: Some(model_permission.role.clone()),
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
                role: Some(model_permission.role.clone()),
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
                    role: Some(role.clone()),
                    error,
                })
            })
            .map(FilterPermission::Filter)
        }
        NullableModelPredicate::Null(()) => Ok(FilterPermission::AllowAll),
    }
}

struct ArgumentPresets {
    argument_presets: BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpressionOrPredicate)>,
    authorization_rules: Vec<ModelAuthorizationRule>,
}

fn resolve_model_argument_presets(
    select_perms: &open_dds::permissions::SelectPermission,
    role: &Spanned<open_dds::permissions::Role>,
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::ModelWithGraphql,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, crate::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    conditions: &mut Conditions,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<ArgumentPresets, Error> {
    let mut argument_presets = BTreeMap::new();
    let mut authorization_rules = vec![];
    for argument_preset in &select_perms.argument_presets {
        if argument_presets.contains_key(&argument_preset.argument.value) {
            return Err(NamedModelPermissionError {
                model_name: model.inner.name.clone(),
                role: Some(role.clone()),
                error: ModelPermissionError::DuplicateModelArgumentPreset {
                    argument_name: argument_preset.argument.clone(),
                },
            }
            .into());
        }

        let (value_expression_or_predicate, argument_type) = resolve_model_argument_preset(
            &argument_preset.argument,
            &argument_preset.value,
            Some(role),
            flags,
            model,
            data_connector_scalars,
            object_types,
            scalar_types,
            boolean_expression_types,
            models,
            issues,
        )?;

        // store authorization rule for argument preset
        authorization_rules.push(authorization_rule_for_argument_preset(
            role,
            &argument_preset.argument.value,
            &argument_type,
            &value_expression_or_predicate,
            flags,
            conditions,
        ));

        argument_presets.insert(
            argument_preset.argument.value.clone(),
            (argument_type.clone(), value_expression_or_predicate),
        );
    }

    Ok(ArgumentPresets {
        argument_presets,
        authorization_rules,
    })
}

fn resolve_model_argument_preset(
    argument_name: &Spanned<ArgumentName>,
    value_expression_or_predicate: &Spanned<open_dds::permissions::ValueExpressionOrPredicate>,
    role: Option<&Spanned<open_dds::permissions::Role>>,
    flags: &open_dds::flags::OpenDdFlags,
    model: &models_graphql::ModelWithGraphql,
    data_connector_scalars: &BTreeMap<
        Qualified<DataConnectorName>,
        data_connector_scalar_types::DataConnectorScalars<'_>,
    >,
    object_types: &BTreeMap<Qualified<CustomTypeName>, crate::ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    boolean_expression_types: &boolean_expressions::BooleanExpressionTypes,
    models: &IndexMap<Qualified<ModelName>, models_graphql::ModelWithGraphql>,
    issues: &mut Vec<ModelPermissionIssue>,
) -> Result<(ValueExpressionOrPredicate, QualifiedTypeReference), Error> {
    let model_source = model
        .inner
        .source
        .as_ref()
        .ok_or_else(|| NamedModelPermissionError {
            model_name: model.inner.name.clone(),
            role: role.cloned(),
            error: ModelPermissionError::ModelSourceRequiredForPredicate {
                model_name: Spanned {
                    path: model.inner.path.clone(),
                    value: model.inner.name.clone(),
                },
            },
        })?;

    let argument =
        model
            .arguments
            .get(&argument_name.value)
            .ok_or_else(|| NamedModelPermissionError {
                model_name: model.inner.name.clone(),
                role: role.cloned(),
                error: ModelPermissionError::ModelArgumentPresetArgumentNotFound {
                    model_name: Spanned {
                        path: model.inner.path.clone(),
                        value: model.inner.name.clone(),
                    },
                    argument_name: argument_name.clone(),
                },
            })?;

    let error_mapper = |type_error| {
        Error::ModelPermissionsError(NamedModelPermissionError {
            model_name: model.inner.name.clone(),
            role: role.cloned(),
            error: ModelPermissionError::ModelArgumentValuePresetTypeError {
                argument_name: argument_name.clone(),
                value_path: value_expression_or_predicate.path.clone(),
                type_error,
            },
        })
    };

    let (value_expression_or_predicate, new_issues) = resolve_value_expression_for_argument(
        role.map(|v| &**v),
        flags,
        argument_name,
        value_expression_or_predicate,
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
            role: role.as_ref().map(|role| role.value.clone()),
            model_name: model.inner.name.clone(),
            argument_name: argument_name.value.clone(),
            typecheck_issue: issue,
        });
    }

    Ok((
        value_expression_or_predicate,
        argument.argument_type.clone(),
    ))
}

// raise a warning if this model uses fancy auth and has a graphql has_graphql_api
// (as it won't appear in the schema)
fn warn_on_rules_based_permissions(
    model: &models_graphql::ModelWithGraphql,
    model_permissions: &ModelPermissionsV2,
    object_types: &BTreeMap<
        Qualified<CustomTypeName>,
        object_relationships::ObjectTypeWithRelationships,
    >,
    issues: &mut Vec<ModelPermissionIssue>,
) {
    // have we exposed this model?
    let has_graphql_api = model.graphql_api.select_many.is_some()
        || model.graphql_api.select_aggregate.is_some()
        || !model.graphql_api.select_uniques.is_empty();

    if !has_graphql_api {
        return;
    }

    // raise issue if model itself uses rules-based auth
    if matches!(
        model_permissions.permissions,
        ModelPermissionOperand::RulesBased(_)
    ) {
        issues.push(ModelPermissionIssue::ModelUsesRulesBasedAuthorization {
            model_name: model.inner.name.clone(),
        });
        // don't need the more granular warnings if model is not exposed in the schema
        return;
    }

    if has_graphql_api {
        let mut argument_types_with_fancy_auth = BTreeSet::new();
        for argument in model.arguments.values() {
            if let Some(custom_type_name) = unwrap_custom_type_name(&argument.argument_type) {
                argument_types_with_fancy_auth.extend(types_that_use_fancy_auth(
                    object_types,
                    custom_type_name,
                    ObjectTypeToCheck::Input,
                ));
            }
        }

        // raise issue for every input type that uses rules-based auth
        for argument_type in argument_types_with_fancy_auth {
            issues.push(
                ModelPermissionIssue::ModelArgumentTypeUsesRulesBasedAuthorization {
                    model_name: model.inner.name.clone(),
                    argument_type,
                },
            );
        }

        // raise issue for every output type that uses rules-based auth
        for custom_type_name in types_that_use_fancy_auth(
            object_types,
            &model.inner.data_type,
            ObjectTypeToCheck::Output,
        ) {
            issues.push(
                ModelPermissionIssue::ModelDataTypeUsesRulesBasedAuthorization {
                    model_name: model.inner.name.clone(),
                    data_type: custom_type_name,
                },
            );
        }
    }
}
