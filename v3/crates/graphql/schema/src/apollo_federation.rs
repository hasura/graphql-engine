//! Schema of the apollo federation according to <https://www.apollographql.com/docs/federation/subgraph-spec>

use lang_graphql::{
    mk_name,
    schema::{self as gql_schema, RegisteredTypeName},
};
use std::collections::BTreeMap;

use super::{
    permissions,
    types::{self, Annotation, output_type::get_object_type_representation},
};
use crate::{GDS, mk_typename};
use lang_graphql::ast::common as ast;

use super::types::output_type::get_custom_output_type;

pub fn apollo_federation_entities_schema(
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
) -> Result<gql_schema::Union<GDS>, crate::Error> {
    let entity_typename = mk_typename("_Entity")?;
    let mut entity_members = BTreeMap::new();
    for model in gds.metadata.models.values() {
        if model.model.apollo_federation_key_source.is_some() {
            let object_type_representation =
                get_object_type_representation(gds, &model.model.data_type)?;

            let object_typename = get_custom_output_type(gds, builder, &model.model.data_type)?;
            let entity_union_permissions =
                permissions::get_entity_union_permissions(object_type_representation);
            entity_members.insert(
                object_typename.clone(),
                builder.conditional_namespaced((), entity_union_permissions),
            );
        }
    }
    Ok(gql_schema::Union::new(
        builder,
        entity_typename,
        None,
        entity_members,
        Vec::new(),
    ))
}

pub fn apollo_federation_service_schema(
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<gql_schema::Object<GDS>, crate::Error> {
    let service_typename = mk_typename("_Service")?;
    let sdl_name = mk_name!("sdl");
    let sdl_field = gql_schema::Field::new(
        sdl_name.clone(),
        None,
        Annotation::Output(types::OutputAnnotation::SDL),
        ast::TypeContainer::named_non_null(RegisteredTypeName::string()),
        BTreeMap::new(),
        gql_schema::DeprecationStatus::NotDeprecated,
    );
    let service_fields = BTreeMap::from([(sdl_name, builder.allow_all_namespaced(sdl_field))]);
    Ok(gql_schema::Object::new(
        builder,
        service_typename,
        None,
        service_fields,
        BTreeMap::new(),
        Vec::new(),
    ))
}
