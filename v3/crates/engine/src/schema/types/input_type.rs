use crate::{
    metadata::resolved::{
        subgraph::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference},
        types::{mk_name, FieldDefinition, ObjectTypeRepresentation, TypeRepresentation},
    },
    schema::{types, GDS},
};
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::types::{CustomTypeName, FieldName};
use std::collections::BTreeMap;

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
    let type_representation = gds.metadata.types.get(gds_type_name).ok_or_else(|| {
        crate::schema::Error::InternalTypeNotFound {
            type_name: gds_type_name.clone(),
        }
    })?;
    match type_representation {
        TypeRepresentation::Object(ObjectTypeRepresentation {
            graphql_input_type_name,
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
        TypeRepresentation::ScalarType(graphql_type_name) => {
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
    fields: &IndexMap<FieldName, FieldDefinition>,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, Error> {
    fields
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

            let namespaced_input_field = builder.allow_all_namespaced(input_field, None);
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
    let type_representation =
        gds.metadata
            .types
            .get(type_name)
            .ok_or_else(|| Error::InternalTypeNotFound {
                type_name: type_name.clone(),
            })?;

    let graphql_type_name = graphql_type_name.clone();

    let object_type_representation = match &type_representation {
        TypeRepresentation::ScalarType { .. } => Err(Error::InternalUnsupported {
            summary: format!(
                "a scalar type {} mapping to non-scalar GraphQL types",
                type_name.clone()
            ),
        }),
        TypeRepresentation::Object(object_type_representation) => Ok(object_type_representation),
    }?;

    let input_fields =
        input_object_type_input_fields(gds, builder, &object_type_representation.fields)?;

    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(
            graphql_type_name,
            object_type_representation.description.clone(),
            input_fields,
            Vec::new(),
        ),
    ))
}
