use lang_graphql::ast::common as ast;
use lang_graphql::schema::{self as gql_schema};
use open_dds::commands::DataConnectorCommand;
use open_dds::{
    relationships,
    types::{CustomTypeName, InbuiltType},
};
use std::collections::{HashMap, HashSet};

use self::relationship::{
    CommandRelationshipAnnotation, CommandTargetSource, ModelRelationshipAnnotation,
    ModelTargetSource,
};
use super::inbuilt_type::base_type_container_for_inbuilt_type;
use super::Annotation;
use crate::metadata::resolved::subgraph::{
    Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference,
};
use crate::metadata::resolved::types::ObjectTypeRepresentation;
use crate::metadata::resolved::{
    self,
    types::{mk_name, TypeRepresentation},
};
use crate::schema::commands::generate_command_argument;
use crate::schema::permissions;
use crate::schema::query_root::select_many::generate_select_many_arguments;
use crate::schema::{Role, GDS};

type Error = crate::schema::Error;

pub mod relationship;

pub(crate) const ID_TYPE_REFERENCE: QualifiedTypeReference = QualifiedTypeReference {
    underlying_type: QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(InbuiltType::ID)),
    nullable: false,
};

pub fn get_base_type_container(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &QualifiedTypeName,
) -> Result<ast::BaseTypeContainer<gql_schema::RegisteredTypeName>, Error> {
    match type_name {
        QualifiedTypeName::Inbuilt(inbuilt_type) => {
            Ok(base_type_container_for_inbuilt_type(inbuilt_type))
        }
        QualifiedTypeName::Custom(type_name) => Ok(ast::BaseTypeContainer::Named(
            get_custom_output_type(gds, builder, type_name)?,
        )),
    }
}

pub fn get_output_type(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    gds_type: &QualifiedTypeReference,
) -> Result<ast::TypeContainer<gql_schema::RegisteredTypeName>, Error> {
    if gds_type.nullable {
        match &gds_type.underlying_type {
            QualifiedBaseType::Named(type_name) => {
                let base = get_base_type_container(gds, builder, type_name)?;
                Ok(ast::TypeContainer {
                    base,
                    nullable: true,
                })
            }
            QualifiedBaseType::List(list_type) => {
                let output_type = get_output_type(gds, builder, list_type)?;
                Ok(ast::TypeContainer::list_null(output_type))
            }
        }
    } else {
        match &gds_type.underlying_type {
            QualifiedBaseType::Named(type_name) => {
                let base = get_base_type_container(gds, builder, type_name)?;
                Ok(ast::TypeContainer {
                    base,
                    nullable: false,
                })
            }
            QualifiedBaseType::List(list_type) => {
                let output_type = get_output_type(gds, builder, list_type)?;
                Ok(ast::TypeContainer::list_non_null(output_type))
            }
        }
    }
}

pub fn node_interface_type(
    builder: &mut gql_schema::Builder<GDS>,
) -> gql_schema::RegisteredTypeName {
    builder.register_type(super::TypeId::NodeRoot)
}

pub fn get_custom_output_type(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    gds_type: &Qualified<CustomTypeName>,
) -> Result<gql_schema::RegisteredTypeName, Error> {
    let type_representation = gds.metadata.types.get(gds_type).ok_or_else(|| {
        crate::schema::Error::InternalTypeNotFound {
            type_name: gds_type.clone(),
        }
    })?;
    match type_representation {
        TypeRepresentation::Object(object_type_representation) => {
            Ok(builder.register_type(super::TypeId::OutputType {
                gds_type_name: gds_type.clone(),
                graphql_type_name: object_type_representation
                    .graphql_output_type_name
                    .as_ref()
                    .ok_or_else(|| Error::NoGraphQlOutputTypeNameForObject {
                        type_name: gds_type.clone(),
                    })?
                    .clone(),
            }))
        }
        TypeRepresentation::ScalarType(scalar_type_representation) => {
            Ok(builder.register_type(super::TypeId::ScalarType {
                gds_type_name: gds_type.clone(),
                graphql_type_name: scalar_type_representation
                    .graphql_type_name
                    .as_ref()
                    .ok_or_else(|| Error::NoGraphQlTypeNameForScalar {
                        type_name: gds_type.clone(),
                    })?
                    .clone(),
            }))
        }
    }
}

pub(crate) fn get_type_kind(
    gds: &GDS,
    field_type: &QualifiedTypeReference,
) -> Result<super::TypeKind, Error> {
    match &field_type.underlying_type {
        QualifiedBaseType::Named(qualified_type_name) => match qualified_type_name {
            QualifiedTypeName::Inbuilt(_) => Ok(super::TypeKind::Scalar), // Inbuilt types are all scalars
            QualifiedTypeName::Custom(type_name) => {
                let type_rep =
                    gds.metadata
                        .types
                        .get(type_name)
                        .ok_or(Error::InternalTypeNotFound {
                            type_name: type_name.to_owned(),
                        })?;
                match type_rep {
                    TypeRepresentation::Object(_) => Ok(super::TypeKind::Object),
                    TypeRepresentation::ScalarType(_) => Ok(super::TypeKind::Scalar),
                }
            }
        },
        QualifiedBaseType::List(element_type) => get_type_kind(gds, element_type),
    }
}

/// generate graphql schema for object type fields
fn object_type_fields(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    object_type_representation: &ObjectTypeRepresentation,
) -> Result<HashMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>>, Error> {
    let mut graphql_fields = object_type_representation
        .fields
        .iter()
        .map(|(field_name, field_definition)| -> Result<_, Error> {
            let graphql_field_name = mk_name(field_name.0.as_str())?;
            let field = gql_schema::Field::<GDS>::new(
                graphql_field_name.clone(),
                field_definition.description.clone(),
                Annotation::Output(super::OutputAnnotation::Field {
                    name: field_name.clone(),
                    field_type: field_definition.field_type.clone(),
                    field_base_type_kind: get_type_kind(gds, &field_definition.field_type)?,
                }),
                get_output_type(gds, builder, &field_definition.field_type)?,
                HashMap::new(),
                gql_schema::DeprecationStatus::NotDeprecated,
            );
            // if output permissions are defined for this type, we conditionally
            // include fields
            let namespaced_field = {
                let mut role_map = HashMap::new();
                for (role, perms) in &object_type_representation.type_permissions {
                    if perms.allowed_fields.contains(field_name) {
                        role_map.insert(Role(role.0.clone()), None);
                    }
                }
                builder.conditional_namespaced(field, role_map)
            };
            Ok((graphql_field_name, namespaced_field))
        })
        .collect::<Result<HashMap<_, _>, _>>()?;
    let graphql_relationship_fields = object_type_representation
        .relationships
        .iter()
        .map(
            |(relationship_field_name, relationship)| -> Result<_, Error> {
                let graphql_field_name = relationship_field_name.clone();

                let relationship_field = match &relationship.target {
                    resolved::relationship::RelationshipTarget::Command {
                        command_name,
                        target_type,
                        mappings,
                    } => {
                        let relationship_output_type = get_output_type(gds, builder, target_type)?;

                        let command = gds.metadata.commands.get(command_name).ok_or_else(|| {
                            Error::InternalCommandNotFound {
                                command_name: command_name.clone(),
                            }
                        })?;

                        let mut arguments_with_mapping = HashSet::new();
                        for argument_mapping in mappings {
                            arguments_with_mapping.insert(&argument_mapping.argument_name);
                        }

                        // generate argument fields for the command arguments which are not mapped to
                        // any type fields, so that they can be exposed in the relationship field schema
                        let mut arguments = HashMap::new();
                        for (argument_name, argument_type) in &command.arguments {
                            if !arguments_with_mapping.contains(argument_name) {
                                let (field_name, input_field) = generate_command_argument(
                                    gds,
                                    builder,
                                    command,
                                    argument_name,
                                    argument_type,
                                )?;
                                arguments.insert(field_name, input_field);
                            }
                        }

                        builder.conditional_namespaced(
                            gql_schema::Field::<GDS>::new(
                                graphql_field_name.clone(),
                                relationship.description.clone(),
                                Annotation::Output(super::OutputAnnotation::RelationshipToCommand(
                                    CommandRelationshipAnnotation {
                                        source_type: relationship.source.clone(),
                                        relationship_name: relationship.name.clone(),
                                        command_name: command_name.clone(),
                                        target_source: CommandTargetSource::new(
                                            command,
                                            relationship,
                                        )?,
                                        target_type: target_type.clone(),
                                        target_base_type_kind: get_type_kind(gds, target_type)?,
                                        mappings: mappings.clone(),
                                    },
                                )),
                                relationship_output_type,
                                arguments,
                                gql_schema::DeprecationStatus::NotDeprecated,
                            ),
                            permissions::get_command_relationship_namespace_annotations(
                                command,
                                object_type_representation,
                                mappings,
                            )?,
                        )
                    }
                    resolved::relationship::RelationshipTarget::Model {
                        model_name,
                        relationship_type,
                        target_typename,
                        mappings,
                    } => {
                        let relationship_base_output_type =
                            get_custom_output_type(gds, builder, target_typename)?;

                        let relationship_output_type = match relationship_type {
                            relationships::RelationshipType::Array => {
                                let non_nullable_relationship_base_type =
                                    ast::TypeContainer::named_non_null(
                                        relationship_base_output_type,
                                    );
                                ast::TypeContainer::list_null(non_nullable_relationship_base_type)
                            }
                            relationships::RelationshipType::Object => {
                                ast::TypeContainer::named_null(relationship_base_output_type)
                            }
                        };

                        let model = gds.metadata.models.get(model_name).ok_or_else(|| {
                            Error::InternalModelNotFound {
                                model_name: model_name.clone(),
                            }
                        })?;

                        if !model.arguments.is_empty() {
                            return Err(Error::InternalUnsupported {
                                summary: "Relationships to models with arguments aren't supported"
                                    .into(),
                            });
                        }

                        let arguments = match relationship_type {
                            relationships::RelationshipType::Array => {
                                generate_select_many_arguments(builder, model)?
                            }
                            relationships::RelationshipType::Object => HashMap::new(),
                        };

                        let target_object_type_representation =
                            get_object_type_representation(gds, &model.data_type)?;

                        builder.conditional_namespaced(
                            gql_schema::Field::<GDS>::new(
                                graphql_field_name.clone(),
                                relationship.description.clone(),
                                Annotation::Output(super::OutputAnnotation::RelationshipToModel(
                                    ModelRelationshipAnnotation {
                                        source_type: relationship.source.clone(),
                                        relationship_name: relationship.name.clone(),
                                        model_name: model_name.clone(),
                                        target_source: ModelTargetSource::new(model, relationship)?,
                                        target_type: target_typename.clone(),
                                        relationship_type: relationship_type.clone(),
                                        mappings: mappings.clone(),
                                    },
                                )),
                                relationship_output_type,
                                arguments,
                                gql_schema::DeprecationStatus::NotDeprecated,
                            ),
                            permissions::get_model_relationship_namespace_annotations(
                                model,
                                object_type_representation,
                                target_object_type_representation,
                                mappings,
                            ),
                        )
                    }
                };
                Ok((graphql_field_name, relationship_field))
            },
        )
        .collect::<Result<HashMap<_, _>, _>>()?;
    graphql_fields.extend(graphql_relationship_fields);
    Ok(graphql_fields)
}

pub fn output_type_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &Qualified<CustomTypeName>,
    graphql_type_name: &ast::TypeName,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let type_representation =
        gds.metadata
            .types
            .get(type_name)
            .ok_or_else(|| Error::InternalTypeNotFound {
                type_name: type_name.clone(),
            })?;

    let graphql_type_name = graphql_type_name.clone();

    match &type_representation {
        resolved::types::TypeRepresentation::Object(object_type_representation) => {
            let mut object_type_fields =
                object_type_fields(gds, builder, object_type_representation)?;
            if object_type_representation.global_id_fields.is_empty() {
                Ok(gql_schema::TypeInfo::Object(gql_schema::Object::new(
                    builder,
                    graphql_type_name,
                    object_type_representation.description.clone(),
                    object_type_fields,
                    HashMap::new(),
                )))
            } else {
                // Generate the Global object `id` field and insert it
                // into the `object_type_fields`.
                let mut interfaces = HashMap::new();
                let global_id_field_name = lang_graphql::mk_name!("id");
                let global_id_field = gql_schema::Field::<GDS>::new(
                    global_id_field_name.clone(),
                    object_type_representation.description.clone(),
                    Annotation::Output(super::OutputAnnotation::GlobalIDField {
                        global_id_fields: object_type_representation.global_id_fields.to_vec(),
                    }),
                    get_output_type(gds, builder, &ID_TYPE_REFERENCE)?,
                    HashMap::new(),
                    gql_schema::DeprecationStatus::NotDeprecated,
                );
                if object_type_fields
                    .insert(
                        global_id_field_name.clone(),
                        builder.conditional_namespaced(
                            global_id_field,
                            permissions::get_node_interface_annotations(object_type_representation),
                        ),
                    )
                    .is_some()
                {
                    return Err(Error::DuplicateFieldNameGeneratedInObjectType {
                        field_name: global_id_field_name,
                        type_name: type_name.clone(),
                    });
                }
                let node_interface_annotations =
                    permissions::get_node_interface_annotations(object_type_representation);
                if !node_interface_annotations.is_empty() {
                    interfaces.insert(
                        node_interface_type(builder),
                        builder.conditional_namespaced((), node_interface_annotations),
                    );
                }
                Ok(gql_schema::TypeInfo::Object(gql_schema::Object::new(
                    builder,
                    graphql_type_name,
                    object_type_representation.description.clone(),
                    object_type_fields,
                    interfaces,
                )))
            }
        }
        resolved::types::TypeRepresentation::ScalarType { .. } => Err(Error::InternalUnsupported {
            summary: format!(
                "a scalar type {} mapping to non-scalar GraphQL types",
                type_name.clone()
            ),
        }),
    }
}

/// Gets the `ObjectTypeRepresentation` of the type
/// identified with the `gds_type`, it will throw
/// an error if the type is not found to be an object.
pub(crate) fn get_object_type_representation<'s>(
    gds: &'s GDS,
    gds_type: &Qualified<CustomTypeName>,
) -> Result<&'s ObjectTypeRepresentation, crate::schema::Error> {
    let type_representation = gds.metadata.types.get(gds_type).ok_or_else(|| {
        crate::schema::Error::InternalTypeNotFound {
            type_name: gds_type.clone(),
        }
    })?;
    match type_representation {
        TypeRepresentation::Object(object_type_representation) => Ok(object_type_representation),
        TypeRepresentation::ScalarType { .. } => {
            Err(crate::schema::Error::ExpectedTypeToBeObject {
                type_name: gds_type.clone(),
            })
        }
    }
}
