//! Schema of the relay according to <https://relay.dev/graphql/objectidentification.htm>

use lang_graphql::schema as gql_schema;
use std::collections::{BTreeMap, HashMap};

use super::permissions;
use crate::types::{
    self, Annotation, OutputAnnotation,
    output_type::{
        ID_TYPE_REFERENCE, get_custom_output_type, get_object_type_representation, get_output_type,
    },
};
use crate::{GDS, Role, mk_typename};

pub fn node_interface_schema(
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
) -> Result<gql_schema::Interface<GDS>, crate::Error> {
    let mut fields = BTreeMap::new();
    let mut implemented_by = BTreeMap::new();
    let mut typename_global_id_mappings = HashMap::new();
    let mut roles_implementing_global_id: HashMap<Role, Option<Box<types::NamespaceAnnotation>>> =
        HashMap::new();
    for model in gds.metadata.models.values() {
        if model.model.global_id_source.is_some() {
            let object_type_representation =
                get_object_type_representation(gds, &model.model.data_type)?;

            let object_typename = get_custom_output_type(gds, builder, &model.model.data_type)?;

            let node_interface_annotations =
                permissions::get_node_interface_annotations(object_type_representation);

            for role in node_interface_annotations.keys() {
                roles_implementing_global_id.insert(role.clone(), None);
            }

            implemented_by.insert(
                object_typename.clone(),
                builder.conditional_namespaced((), node_interface_annotations),
            );

            // Multiple models can be backed by the same type
            typename_global_id_mappings.insert(
                object_typename.type_name().clone(),
                model.model.global_id_fields.clone(),
            );
        }
    }
    let node_id_field = gql_schema::Field::new(
        lang_graphql::mk_name!("id"),
        None,
        Annotation::Output(OutputAnnotation::RelayNodeInterfaceID {
            typename_mappings: typename_global_id_mappings,
        }),
        get_output_type(gds, builder, &ID_TYPE_REFERENCE)?,
        BTreeMap::new(),
        gql_schema::DeprecationStatus::NotDeprecated,
    );
    fields.insert(
        node_id_field.name.clone(),
        builder.conditional_namespaced(node_id_field, roles_implementing_global_id),
    );
    let node_typename = mk_typename("Node")?;
    Ok(gql_schema::Interface::new(
        builder,
        node_typename,
        None,
        fields,
        BTreeMap::new(),
        implemented_by,
        Vec::new(),
    ))
}
