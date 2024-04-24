use crate::{
    metadata::resolved::{
        stages::{data_connector_type_mappings, type_permissions},
        subgraph::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference},
        types::{get_type_representation, mk_name, TypeRepresentation},
    },
    schema::{types, Role, GDS},
};
use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::types::CustomTypeName;
use std::collections::{BTreeMap, HashMap};

use super::inbuilt_type::base_type_container_for_inbuilt_type;

type Error = crate::schema::Error;

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
            get_custom_input_type(gds, builder, type_name)?,
        )),
    }
}

pub fn get_input_type(
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
                let input_type = get_input_type(gds, builder, list_type)?;
                Ok(ast::TypeContainer::list_null(input_type))
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
                let input_type = get_input_type(gds, builder, list_type)?;
                Ok(ast::TypeContainer::list_non_null(input_type))
            }
        }
    }
}

fn get_custom_input_type(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    gds_type_name: &Qualified<CustomTypeName>,
) -> Result<gql_schema::RegisteredTypeName, Error> {
    match get_type_representation(
        gds_type_name,
        &gds.metadata.object_types,
        &gds.metadata.scalar_types,
    )
    .map_err(|_| crate::schema::Error::InternalTypeNotFound {
        type_name: gds_type_name.clone(),
    })? {
        TypeRepresentation::Object(type_permissions::ObjectTypeWithPermissions {
            object_type:
                data_connector_type_mappings::ObjectTypeRepresentation {
                    graphql_input_type_name,
                    ..
                },
            ..
        }) => Ok(builder.register_type(super::TypeId::InputObjectType {
            gds_type_name: gds_type_name.clone(),
            graphql_type_name: graphql_input_type_name
                .as_ref()
                .ok_or_else(|| Error::NoGraphQlInputTypeNameForObject {
                    type_name: gds_type_name.clone(),
                })?
                .clone(),
        })),
        TypeRepresentation::Scalar(graphql_type_name) => {
            Ok(builder.register_type(super::TypeId::ScalarType {
                gds_type_name: gds_type_name.clone(),
                graphql_type_name: graphql_type_name
                    .graphql_type_name
                    .as_ref()
                    .ok_or_else(|| Error::NoGraphQlTypeNameForScalar {
                        type_name: gds_type_name.clone(),
                    })?
                    .clone(),
            }))
        }
    }
}

fn input_object_type_input_fields(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    object_type_representation: &type_permissions::ObjectTypeWithPermissions,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, Error> {
    object_type_representation
        .object_type
        .fields
        .iter()
        .map(|(field_name, field_definition)| {
            let graphql_field_name = mk_name(field_name.0.as_str())?;

            let input_field = gql_schema::InputField::new(
                graphql_field_name.clone(),
                field_definition.description.clone(),
                types::Annotation::Input(types::InputAnnotation::InputObjectField {
                    field_name: field_name.clone(),
                    field_type: field_definition.field_type.clone(),
                }),
                get_input_type(gds, builder, &field_definition.field_type)?,
                None, // Default value
                gql_schema::DeprecationStatus::NotDeprecated,
            );

            // construct the input field based on input permissions
            let namespaced_input_field = {
                // if no input permissions are defined, include the field for all roles
                if object_type_representation.type_input_permissions.is_empty() {
                    builder.allow_all_namespaced(input_field, None)
                // if input permissions are defined, include the field conditionally
                } else {
                    let mut role_map = HashMap::new();
                    for (role, permission) in &object_type_representation.type_input_permissions {
                        // add the field only if there is no field preset defined
                        // for this role
                        if !permission.field_presets.contains_key(field_name) {
                            role_map.insert(Role(role.0.clone()), None);
                        }
                    }
                    // for roles present in the metadata, but does not have any
                    // input permission defined for this types, we still allow
                    // all fields
                    let roles_in_this_permission: Vec<_> = object_type_representation
                        .type_input_permissions
                        .keys()
                        .collect();
                    let roles_not_in_this_permission: Vec<_> = gds
                        .metadata
                        .roles
                        .iter()
                        .filter(|role| !roles_in_this_permission.contains(role))
                        .collect();
                    for role in roles_not_in_this_permission {
                        role_map.insert(Role(role.0.clone()), None);
                    }

                    builder.conditional_namespaced(input_field, role_map)
                }
            };
            Ok((graphql_field_name, namespaced_input_field))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()
}

pub fn input_object_type_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &Qualified<CustomTypeName>,
    graphql_type_name: &ast::TypeName,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let object_type_representation =
        gds.metadata
            .object_types
            .get(type_name)
            .ok_or_else(|| Error::InternalTypeNotFound {
                type_name: type_name.clone(),
            })?;

    let graphql_type_name = graphql_type_name.clone();

    let input_fields = input_object_type_input_fields(gds, builder, object_type_representation)?;

    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(
            graphql_type_name,
            object_type_representation.object_type.description.clone(),
            input_fields,
            Vec::new(),
        ),
    ))
}
