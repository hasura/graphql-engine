use crate::PermissionError;
use hasura_authn_core::Role;
use metadata_resolve::{Metadata, Qualified, QualifiedTypeReference, RelationshipTarget};
use open_dds::{
    commands::CommandName,
    models::ModelName,
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
) -> Result<OutputObjectTypeView<'metadata>, PermissionError> {
    let object_type = metadata.object_types.get(object_type_name).ok_or_else(|| {
        PermissionError::ObjectTypeNotFound {
            object_type_name: object_type_name.clone(),
        }
    })?;

    let type_output_permission =
        object_type
            .type_output_permissions
            .get(role)
            .ok_or_else(|| PermissionError::ObjectTypeNotAccessible {
                object_type_name: object_type_name.clone(),
                role: role.clone(),
            })?;

    let relationship_fields = object_type
        .relationship_fields
        .iter()
        .filter(|(_relationship_name, relationship)| {
            // we only include a relationship if we're allowed to access it
            match &relationship.target {
                RelationshipTarget::Model(model) => {
                    get_model(metadata, &model.model_name, role).is_ok()
                }
                RelationshipTarget::Command(command) => {
                    get_command(metadata, &command.command_name, role).is_ok()
                }
            }
        })
        .collect();

    let fields = object_type
        .object_type
        .fields
        .iter()
        .filter(|(field_name, _)| type_output_permission.allowed_fields.contains(*field_name))
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
) -> Result<ModelView<'metadata>, PermissionError> {
    let model = metadata
        .models
        .get(model_name)
        .ok_or_else(|| PermissionError::ModelNotFound {
            model_name: model_name.clone(),
        })?;

    if let Some(select_permission) = model.select_permissions.get(role) {
        if role_can_access_object_type(metadata, &model.model.data_type, role) {
            {
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

pub struct CommandView {}

// fetch a command from metadata, ensuring we have CommandPermissions
// and permissions to access the return type
fn get_command(
    metadata: &Metadata,
    command_name: &'_ Qualified<CommandName>,
    role: &'_ Role,
) -> Result<CommandView, PermissionError> {
    let command =
        metadata
            .commands
            .get(command_name)
            .ok_or_else(|| PermissionError::CommandNotFound {
                command_name: command_name.clone(),
            })?;

    // if we have a custom type, we need to check permissions on that too
    let can_access_type = if let Some(custom_type_name) =
        metadata_resolve::unwrap_custom_type_name(&command.command.output_type)
    {
        role_can_access_object_type(metadata, custom_type_name, role)
            || is_valid_scalar_type(metadata, custom_type_name)
    } else {
        true
    };
    if can_access_type && command.permissions.contains_key(role) {
        Ok(CommandView {})
    } else {
        Err(PermissionError::CommandNotAccessible {
            command_name: command_name.clone(),
            role: role.clone(),
        })
    }
}

// we use this at leaves to stop recursing forever
fn is_valid_scalar_type(
    metadata: &Metadata,
    scalar_type_name: &'_ Qualified<CustomTypeName>,
) -> bool {
    metadata.scalar_types.contains_key(scalar_type_name)
}

// we use this at leaves to stop recursing forever
fn role_can_access_object_type(
    metadata: &Metadata,
    object_type_name: &'_ Qualified<CustomTypeName>,
    role: &'_ Role,
) -> bool {
    metadata
        .object_types
        .get(object_type_name)
        .is_some_and(|object_type| object_type.type_output_permissions.contains_key(role))
}
