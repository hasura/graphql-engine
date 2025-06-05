use std::collections::{BTreeMap, BTreeSet, HashMap};

use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::{mk_name, schema as gql_schema};
use open_dds::types::CustomTypeName;

use crate::permissions::get_entities_field_namespace_permissions;
use metadata_resolve;
use metadata_resolve::Qualified;

use crate::types::EntityFieldTypeNameMapping;
use crate::types::output_type::{
    apollo_federation_entities_type, apollo_federation_service_type, get_custom_output_type,
    get_object_type_representation, representations_type_reference,
};
use crate::{
    GDS,
    types::{self, Annotation},
};

pub(crate) struct ApolloFederationFieldOutput {
    /// The _entities field.
    pub apollo_federation_entities_field: gql_schema::Field<GDS>,
    /// Roles having access to the `_entities` field.
    pub apollo_federation_entities_field_permissions:
        HashMap<Role, Option<types::NamespaceAnnotation>>,
    /// The _service field.
    pub apollo_federation_service_field: gql_schema::Field<GDS>,
}

pub(crate) fn apollo_federation_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<ApolloFederationFieldOutput, crate::Error> {
    let mut roles_type_permissions: HashMap<Role, BTreeSet<Qualified<CustomTypeName>>> =
        HashMap::new();
    let mut typename_mappings = HashMap::new();
    for model in gds.metadata.models.values() {
        if let Some(apollo_federation_key_source) = &model.model.apollo_federation_key_source {
            let output_typename = get_custom_output_type(gds, builder, &model.model.data_type)?;

            let object_type_representation =
                get_object_type_representation(gds, &model.model.data_type)?;

            let entities_field_permissions =
                get_entities_field_namespace_permissions(object_type_representation, model);

            for role in &entities_field_permissions {
                let role_type_permissions = roles_type_permissions.entry(role.clone()).or_default();
                role_type_permissions.insert(model.model.data_type.clone());
            }

            if typename_mappings
                .insert(
                    output_typename.type_name().clone(),
                    EntityFieldTypeNameMapping {
                        model_name: model.model.name.clone(),
                        type_name: model.model.data_type.clone(),
                        model_source: model.model.source.clone(),
                        key_fields_ndc_mapping: apollo_federation_key_source.ndc_mapping.clone(),
                    },
                )
                .is_some()
            {
                // This is declared as an internal error because this error should
                // never happen, because this is validated while resolving the metadata.
                return Err(crate::Error::InternalErrorDuplicateEntitySourceFound {
                    type_name: output_typename.type_name().clone(),
                });
            }
        }
    }
    let mut apollo_federation_entities_field_permissions = HashMap::new();
    for (role, role_type_permission) in roles_type_permissions {
        apollo_federation_entities_field_permissions.insert(
            role.clone(),
            Some(types::NamespaceAnnotation::EntityTypeMappings(
                role_type_permission,
            )),
        );
    }

    let representations_argument: gql_schema::InputField<GDS> = gql_schema::InputField::new(
        lang_graphql::mk_name!("representations"),
        None,
        Annotation::Input(
            types::InputAnnotation::ApolloFederationRepresentationsInput(
                types::ApolloFederationInputAnnotation::AnyScalarInputAnnotation,
            ),
        ),
        representations_type_reference(builder),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    );
    let entities_arguments = BTreeMap::from([(
        mk_name!("representations"),
        builder.allow_all_namespaced(representations_argument),
    )]);
    let entity_field = gql_schema::Field::new(
        mk_name!("_entities"),
        None,
        Annotation::Output(types::OutputAnnotation::RootField(
            types::RootFieldAnnotation::ApolloFederation(
                types::ApolloFederationRootFields::Entities { typename_mappings },
            ),
        )),
        ast::TypeContainer::named_non_null(apollo_federation_entities_type(builder)),
        entities_arguments,
        gql_schema::DeprecationStatus::NotDeprecated,
    );

    let service_field = gql_schema::Field::new(
        mk_name!("_service"),
        None,
        Annotation::Output(types::OutputAnnotation::RootField(
            types::RootFieldAnnotation::ApolloFederation(
                types::ApolloFederationRootFields::Service,
            ),
        )),
        ast::TypeContainer::named_non_null(apollo_federation_service_type(builder)),
        BTreeMap::new(),
        gql_schema::DeprecationStatus::NotDeprecated,
    );
    Ok(ApolloFederationFieldOutput {
        apollo_federation_service_field: service_field,
        apollo_federation_entities_field: entity_field,
        apollo_federation_entities_field_permissions,
    })
}
