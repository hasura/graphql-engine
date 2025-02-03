//! Schema of the relay node field according to <https://relay.dev/graphql/objectidentification.htm>

use lang_graphql::{ast::common as ast, schema as gql_schema};
use open_dds::types::CustomTypeName;
use std::collections::{BTreeMap, HashMap};

use crate::permissions::get_node_field_namespace_permissions;
use crate::types::RelayInputAnnotation;
use crate::types::{
    self,
    input_type::get_input_type,
    output_type::{
        get_custom_output_type, get_object_type_representation, node_interface_type,
        ID_TYPE_REFERENCE,
    },
    Annotation, NodeFieldTypeNameMapping, OutputAnnotation, RootFieldAnnotation,
};
use crate::{Role, GDS};
use json_ext::HashMapWithJsonKey;
use metadata_resolve;
use metadata_resolve::Qualified;

pub(crate) struct RelayNodeFieldOutput {
    pub relay_node_gql_field: gql_schema::Field<GDS>,
    /// Roles having access to the `node` field.
    pub relay_node_field_permissions: HashMap<Role, Option<types::NamespaceAnnotation>>,
}

/// Calculates the relay `node` field and also returns the
/// list of roles that have access to the `node` field,
/// for the `node` field to be accessible to a role, the role
/// needs to atleast have access to the global ID of any one
/// object that implements the Node interface.
pub(crate) fn relay_node_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<RelayNodeFieldOutput, crate::Error> {
    let mut arguments = BTreeMap::new();
    let mut typename_mappings = HashMap::new();
    // The node field should only be accessible to a role, if
    // atleast one object implements the global `id` field.
    //
    // For example,
    //
    // Let's say the `user` role doesn't have access to the global
    // ID fields to the types it has access to.
    //
    // Then, the node interface generated would not have any
    // object types because the object types don't expose the
    // global `id` field. GraphQL interfaces expect that the
    // objects implementing the interface define all the fields
    // defined by the interface.

    let mut roles_type_permissions: HashMap<
        Role,
        HashMap<Qualified<CustomTypeName>, metadata_resolve::FilterPermission>,
    > = HashMap::new();
    for model in gds.metadata.models.values() {
        if let Some(global_id_source) = &model.model.global_id_source {
            let output_typename = get_custom_output_type(gds, builder, &model.model.data_type)?;

            let object_type_representation =
                get_object_type_representation(gds, &model.model.data_type)?;

            let node_field_permissions =
                get_node_field_namespace_permissions(object_type_representation, model);

            for (role, model_predicate) in &node_field_permissions {
                let role_type_permissions = roles_type_permissions.entry(role.clone()).or_default();
                role_type_permissions
                    .insert(model.model.data_type.clone(), model_predicate.clone());
            }

            if typename_mappings
                .insert(
                    output_typename.type_name().clone(),
                    NodeFieldTypeNameMapping {
                        model_name: model.model.name.clone(),
                        type_name: model.model.data_type.clone(),
                        model_source: model.model.source.clone(),
                        global_id_fields_ndc_mapping: global_id_source.ndc_mapping.clone(),
                    },
                )
                .is_some()
            {
                // This is declared as an internal error because this error should
                // never happen, because this is validated while resolving the metadata.
                return Err(crate::Error::InternalErrorDuplicateGlobalIdSourceFound {
                    type_name: output_typename.type_name().clone(),
                });
            };
        }
    }
    let id_argument: gql_schema::InputField<GDS> = gql_schema::InputField::new(
        lang_graphql::mk_name!("id"),
        None,
        Annotation::Input(types::InputAnnotation::Relay(
            RelayInputAnnotation::NodeFieldIdArgument,
        )),
        get_input_type(gds, builder, &ID_TYPE_REFERENCE)?,
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    );
    arguments.insert(
        id_argument.name.clone(),
        // If a role implements the `node` field, then
        // it should also have access to the `id` argument,
        // which is why we use `allow_all_namespaced` here.
        builder.allow_all_namespaced(id_argument),
    );
    let mut relay_node_field_permissions = HashMap::new();
    for (role, role_type_permission) in roles_type_permissions {
        relay_node_field_permissions.insert(
            role.clone(),
            Some(types::NamespaceAnnotation::NodeFieldTypeMappings(
                HashMapWithJsonKey(role_type_permission),
            )),
        );
    }
    let relay_node_gql_field = gql_schema::Field::new(
        lang_graphql::mk_name!("node"),
        None,
        Annotation::Output(OutputAnnotation::RootField(
            RootFieldAnnotation::RelayNode { typename_mappings },
        )),
        ast::TypeContainer::named_null(node_interface_type(builder)),
        arguments,
        gql_schema::DeprecationStatus::NotDeprecated,
    );
    Ok(RelayNodeFieldOutput {
        relay_node_gql_field,
        relay_node_field_permissions,
    })
}
