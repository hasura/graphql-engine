use crate::{PermissionError, types::PlanState};
use authorization_rules::{
    ArgumentPolicy, ConditionCache, ObjectInputPolicy, evaluate_command_authorization_rules,
    evaluate_field_authorization_rules, evaluate_type_input_authorization_rules,
};
use hasura_authn_core::{Role, SessionVariables};
use indexmap::IndexMap;
use metadata_resolve::{
    Conditions, FieldDefinition, Metadata, ObjectTypeWithRelationships, Qualified,
    QualifiedTypeReference, RelationshipTarget, ValueExpression,
};
use open_dds::{
    commands::CommandName,
    models::ModelName,
    query::ArgumentName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use std::collections::BTreeMap;

// instead of passing around the full `metadata-resolve` ObjectType,
// how small a subset of it can we get away with providing?
#[derive(Debug, Clone)]
pub struct OutputObjectTypeView<'metadata> {
    pub object_type_name: &'metadata Qualified<CustomTypeName>,
    pub fields: BTreeMap<&'metadata FieldName, FieldView<'metadata>>,
    pub relationship_fields:
        BTreeMap<&'metadata RelationshipName, &'metadata metadata_resolve::RelationshipField>,
}

impl OutputObjectTypeView<'_> {
    pub fn get_field(
        &self,
        field_name: &FieldName,
        role: &Role,
    ) -> Result<&FieldView, PermissionError> {
        self.fields
            .get(field_name)
            .ok_or_else(|| PermissionError::ObjectFieldNotFound {
                object_type_name: self.object_type_name.clone(),
                field_name: field_name.clone(),
                role: role.clone(),
            })
    }
}

#[derive(Debug, Clone)]
pub struct FieldView<'metadata> {
    pub field_type: &'metadata QualifiedTypeReference,
}

// we have a problem that permissions aren't "baked in" to planning
// so a starter, let's replace direct lookups to the metadata with functions
// that care about permissions
pub fn get_output_object_type<'metadata>(
    metadata: &'metadata Metadata,
    object_type_name: &'metadata Qualified<CustomTypeName>,
    role: &'_ Role,
    session_variables: &'_ SessionVariables,
    plan_state: &mut PlanState,
) -> Result<OutputObjectTypeView<'metadata>, PermissionError> {
    let object_type = metadata.object_types.get(object_type_name).ok_or_else(|| {
        PermissionError::ObjectTypeNotFound {
            object_type_name: object_type_name.clone(),
        }
    })?;

    let accessible_fields = get_accessible_fields_for_object(
        object_type,
        session_variables,
        &metadata.conditions,
        &mut plan_state.condition_cache,
    )?;

    if accessible_fields.is_empty() {
        return Err(PermissionError::ObjectTypeNotAccessible {
            object_type_name: object_type_name.clone(),
            role: role.clone(),
        });
    }

    let relationship_fields = object_type
        .relationship_fields
        .iter()
        .filter(|(_relationship_name, relationship)| {
            // we only include a relationship if we're allowed to access it
            match &relationship.target {
                RelationshipTarget::Model(model) => get_model(
                    metadata,
                    &model.model_name,
                    role,
                    session_variables,
                    plan_state,
                )
                .is_ok(),
                RelationshipTarget::Command(command) => get_command(
                    metadata,
                    &command.command_name,
                    role,
                    session_variables,
                    plan_state,
                )
                .is_ok(),
            }
        })
        .collect();

    let fields = accessible_fields
        .into_iter()
        .map(|(field_name, field)| {
            (
                field_name,
                FieldView {
                    field_type: &field.field_type,
                },
            )
        })
        .collect();

    Ok(OutputObjectTypeView {
        object_type_name,
        fields,
        relationship_fields,
    })
}

#[derive(Debug, Clone)]
pub struct InputObjectTypeView<'metadata> {
    pub field_presets: BTreeMap<&'metadata FieldName, &'metadata ValueExpression>,
}

pub fn get_input_object_type<'metadata>(
    metadata: &'metadata Metadata,
    object_type_name: &'metadata Qualified<CustomTypeName>,
    session_variables: &'_ SessionVariables,
    plan_state: &mut PlanState,
) -> Result<InputObjectTypeView<'metadata>, PermissionError> {
    let object_type = metadata.object_types.get(object_type_name).ok_or_else(|| {
        PermissionError::ObjectTypeNotFound {
            object_type_name: object_type_name.clone(),
        }
    })?;

    let ObjectInputPolicy { field_presets } = evaluate_type_input_authorization_rules(
        &object_type.type_input_permissions.authorization_rules,
        session_variables,
        &metadata.conditions,
        &mut plan_state.condition_cache,
    )?;

    Ok(InputObjectTypeView { field_presets })
}

pub struct ModelView<'metadata> {
    pub data_type: &'metadata Qualified<CustomTypeName>,
    pub source: &'metadata metadata_resolve::ModelSource,
    pub select_permission: &'metadata metadata_resolve::SelectPermission,
}

// fetch a model from metadata, ensuring we have ModelPermissions
// and permissions to access the underlying type, and that it has a source
pub fn get_model<'metadata>(
    metadata: &'metadata Metadata,
    model_name: &'_ Qualified<ModelName>,
    role: &'_ Role,
    session_variables: &'_ SessionVariables,
    plan_state: &mut PlanState,
) -> Result<ModelView<'metadata>, PermissionError> {
    let model = metadata
        .models
        .get(model_name)
        .ok_or_else(|| PermissionError::ModelNotFound {
            model_name: model_name.clone(),
        })?;

    if let Some(permission) = model.permissions.get(role) {
        if let Some(select_permission) = &permission.select {
            if is_allowed_access_to_object_type(
                metadata,
                &model.model.data_type,
                session_variables,
                &mut plan_state.condition_cache,
            )? {
                if let Some(model_source) = &model.model.source {
                    return Ok(ModelView {
                        data_type: &model.model.data_type,
                        source: model_source,
                        select_permission,
                    });
                }
                return Err(PermissionError::ModelHasNoSource {
                    model_name: model_name.clone(),
                });
            }
        }
    }

    Err(PermissionError::ModelNotAccessible {
        model_name: model_name.clone(),
        role: role.clone(),
    })
}

pub struct CommandView<'a> {
    pub argument_presets: BTreeMap<&'a ArgumentName, ArgumentPolicy<'a>>,
}

// fetch a command from metadata, ensuring we have CommandPermissions
// and permissions to access the return type
pub fn get_command<'a>(
    metadata: &'a Metadata,
    command_name: &'_ Qualified<CommandName>,
    role: &'_ Role,
    session_variables: &'_ SessionVariables,
    plan_state: &mut PlanState,
) -> Result<CommandView<'a>, PermissionError> {
    let command =
        metadata
            .commands
            .get(command_name)
            .ok_or_else(|| PermissionError::CommandNotFound {
                command_name: command_name.clone(),
            })?;

    // if the command return type is a custom type (as opposed to a built-in like Int),
    // check that we have access to it
    let is_allowed_access_to_return_type = if let Some(custom_type_name) =
        metadata_resolve::unwrap_custom_type_name(&command.command.output_type)
    {
        is_valid_scalar_type(metadata, custom_type_name)
            || is_allowed_access_to_object_type(
                metadata,
                custom_type_name,
                session_variables,
                &mut plan_state.condition_cache,
            )?
    } else {
        // if the return type is a built-in, we're good
        true
    };

    // evaluate authorization rules for the command, to decide a) are we allowed to access it?
    // and b) which argument presets are applicable?
    if let Some(command_permission) = evaluate_command_authorization_rules(
        &command.permissions.authorization_rules,
        session_variables,
        &metadata.conditions,
        &mut plan_state.condition_cache,
    )? {
        if is_allowed_access_to_return_type {
            return Ok(CommandView {
                argument_presets: command_permission.argument_presets,
            });
        }
    }

    Err(PermissionError::CommandNotAccessible {
        command_name: command_name.clone(),
        role: role.clone(),
    })
}

// scalar types don't have their own permissions, but we do need to check that they exist
fn is_valid_scalar_type(
    metadata: &Metadata,
    scalar_type_name: &'_ Qualified<CustomTypeName>,
) -> bool {
    metadata.scalar_types.contains_key(scalar_type_name)
}

// are we allowed access to at least one field of this object type?
fn is_allowed_access_to_object_type(
    metadata: &Metadata,
    object_type_name: &'_ Qualified<CustomTypeName>,
    session_variables: &'_ SessionVariables,
    condition_cache: &mut ConditionCache,
) -> Result<bool, PermissionError> {
    match metadata.object_types.get(object_type_name) {
        Some(object_type) => {
            let fields = get_accessible_fields_for_object(
                object_type,
                session_variables,
                &metadata.conditions,
                condition_cache,
            )?;
            Ok(!fields.is_empty())
        }
        None => Err(PermissionError::ObjectTypeNotFound {
            object_type_name: object_type_name.clone(),
        }),
    }
}

// which fields of this object type are we allowed to access?
fn get_accessible_fields_for_object<'a>(
    object_type: &'a ObjectTypeWithRelationships,
    session_variables: &'_ SessionVariables,
    conditions: &Conditions,
    condition_cache: &mut ConditionCache,
) -> Result<IndexMap<&'a FieldName, &'a FieldDefinition>, PermissionError> {
    Ok(evaluate_field_authorization_rules(
        &object_type.type_output_permissions.authorization_rules,
        &object_type.object_type.fields,
        session_variables,
        conditions,
        condition_cache,
    )?)
}
