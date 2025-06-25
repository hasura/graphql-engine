use super::types::{ObjectType, RelationshipTarget, ScalarTypeForDataConnector, Type};
use crate::types::ObjectTypeWarning;
use hasura_authn_core::Role;
use indexmap::IndexMap;
use metadata_resolve::{
    ObjectTypeWithRelationships, Qualified, QualifiedBaseType, QualifiedTypeName,
    QualifiedTypeReference, ScalarTypeRepresentation, unwrap_custom_type_name,
};
use open_dds::types::{CustomTypeName, InbuiltType};
use std::collections::BTreeMap;

// look at permissions and work out which fields we're allowed to see
// this is quite limited and leans to be overcautious
pub fn build_object_type(
    object_type: &ObjectTypeWithRelationships,
    role: &Role,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
) -> Result<ObjectType, ObjectTypeWarning> {
    // if we have no output permissions for the underlying object type, ignore it
    let output_permissions_for_role = object_type
        .type_output_permissions
        .by_role
        .get(role)
        .ok_or(ObjectTypeWarning::NoObjectTypePermission {})?;

    let mut type_fields = IndexMap::new();

    // otherwise return all fields
    for (field_name, field_info) in
        object_type
            .object_type
            .fields
            .iter()
            .filter(|(field_name, _field_info)| {
                output_permissions_for_role
                    .allowed_fields
                    .contains(*field_name)
            })
    {
        let field_type =
            type_from_type_representation(&field_info.field_type, scalar_types, object_types)?;

        type_fields.insert(field_name.clone(), field_type);
    }

    // Relationships
    let mut type_relationships = IndexMap::new();
    for (_, relationship_field) in &object_type.relationship_fields {
        // Only track the relationship if its output type is accessible to the role.
        let mut target = None;
        match &relationship_field.target {
            metadata_resolve::RelationshipTarget::Model(model) => {
                if object_type_permission_access(role, &model.target_typename, object_types) {
                    target = Some(RelationshipTarget::Model {
                        object_type: model.target_typename.clone(),
                        relationship_type: model.relationship_type.clone(),
                    });
                }
            }
            metadata_resolve::RelationshipTarget::Command(command) => {
                let track_this_relationship = if let Some(target_object_type) =
                    unwrap_custom_type_name(&command.target_type)
                {
                    // For command relationship of a custom type, check if type exists.
                    object_type_permission_access(role, target_object_type, object_types)
                } else {
                    // The output type of this command relationship is not a custom type; it is a built-in type (scalar).
                    // Track it.
                    true
                };
                if track_this_relationship {
                    target = Some(RelationshipTarget::Command {
                        type_reference: command.target_type.clone(),
                    });
                }
            }
        }
        if let Some(target) = target {
            type_relationships.insert(relationship_field.relationship_name.clone(), target);
        }
    }

    Ok(ObjectType {
        type_fields,
        type_relationships,
    })
}

// Check if object_type is accessible to given role
fn object_type_permission_access(
    role: &Role,
    type_name: &Qualified<CustomTypeName>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
) -> bool {
    let mut accessible = false;
    if let Some(object_type) = object_types.get(type_name) {
        accessible = object_type
            .type_output_permissions
            .by_role
            .contains_key(role);
    }
    accessible
}

// turn an OpenDD type into a type representation
fn type_from_type_representation(
    qualified_type_reference: &QualifiedTypeReference,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, ScalarTypeRepresentation>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
) -> Result<Type, ObjectTypeWarning> {
    // NOTE: currently we assume everything is nullable because a user might
    // not include a field in sparse fields
    match &qualified_type_reference.underlying_type {
        QualifiedBaseType::Named(name) => match name {
            QualifiedTypeName::Inbuilt(inbuilt) => Ok(Type::Scalar(match inbuilt {
                InbuiltType::String | InbuiltType::ID => ndc_models::TypeRepresentation::String,
                InbuiltType::Int => ndc_models::TypeRepresentation::Int64,
                InbuiltType::Float => ndc_models::TypeRepresentation::Float64,
                InbuiltType::Boolean => ndc_models::TypeRepresentation::Boolean,
            })),
            QualifiedTypeName::Custom(custom_type_name) => {
                match scalar_types.get(custom_type_name) {
                    Some(scalar_type) => {
                        Ok(Type::ScalarForDataConnector(ScalarTypeForDataConnector {
                            type_representations: scalar_type
                                .representations
                                .values()
                                .cloned()
                                .collect(),
                        }))
                    }
                    None => {
                        if object_types.contains_key(custom_type_name) {
                            // return reference to said object
                            Ok(Type::Object(custom_type_name.clone()))
                        } else {
                            Err(ObjectTypeWarning::NestedObjectNotFound {
                                object_type_name: custom_type_name.clone(),
                            })
                        }
                    }
                }
            }
        },
        QualifiedBaseType::List(ty) => Ok(Type::List(Box::new(type_from_type_representation(
            ty,
            scalar_types,
            object_types,
        )?))),
    }
}
