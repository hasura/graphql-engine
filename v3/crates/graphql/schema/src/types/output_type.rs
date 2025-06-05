use lang_graphql::ast::common::{self as ast, TypeContainer};
use lang_graphql::mk_name;
use lang_graphql::schema::{self as gql_schema, Directive, RegisteredType};
use open_dds::commands::DataConnectorCommand;
use open_dds::{
    relationships,
    types::{CustomTypeName, FieldName, InbuiltType},
};
use relationship::ModelAggregateRelationshipAnnotation;
use std::collections::{BTreeMap, HashMap, HashSet};

use self::relationship::{
    CommandRelationshipAnnotation, CommandTargetSource, ModelRelationshipAnnotation,
};
use super::inbuilt_type::base_type_container_for_inbuilt_type;
use super::{Annotation, PossibleApolloFederationTypes, TypeId};
use crate::commands::generate_command_argument;
use crate::field_arguments::generate_field_argument;
use crate::query_root::select_aggregate;
use crate::query_root::select_many::generate_select_many_arguments;
use crate::{GDS, Role};
use crate::{aggregates, mk_deprecation_status, permissions};
use metadata_resolve::{self, mk_name};
use metadata_resolve::{Qualified, QualifiedBaseType, QualifiedTypeName, QualifiedTypeReference};
use metadata_resolve::{TypeRepresentation, get_type_representation};

use crate::Error;

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

pub fn apollo_federation_entities_type(
    builder: &mut gql_schema::Builder<GDS>,
) -> gql_schema::RegisteredTypeName {
    builder.register_type(super::TypeId::ApolloFederationType(
        super::PossibleApolloFederationTypes::Entity,
    ))
}

pub fn apollo_federation_service_type(
    builder: &mut gql_schema::Builder<GDS>,
) -> gql_schema::RegisteredTypeName {
    builder.register_type(super::TypeId::ApolloFederationType(
        super::PossibleApolloFederationTypes::Service,
    ))
}

pub fn get_custom_output_type(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    gds_type: &Qualified<CustomTypeName>,
) -> Result<gql_schema::RegisteredTypeName, Error> {
    match get_type_representation(
        gds_type,
        &gds.metadata.object_types,
        &gds.metadata.scalar_types,
        &gds.metadata.boolean_expression_types,
    )
    .map_err(|_| crate::Error::InternalTypeNotFound {
        type_name: gds_type.clone(),
    })? {
        TypeRepresentation::Object(object_type_representation) => {
            Ok(builder.register_type(super::TypeId::OutputType {
                gds_type_name: gds_type.clone(),
                graphql_type_name: object_type_representation
                    .object_type
                    .graphql_output_type_name
                    .as_ref()
                    .ok_or_else(|| Error::NoGraphQlOutputTypeNameForObject {
                        type_name: gds_type.clone(),
                    })?
                    .clone(),
            }))
        }
        TypeRepresentation::Scalar(scalar_type_representation) => {
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
        TypeRepresentation::BooleanExpressionObject(_)
        | TypeRepresentation::BooleanExpressionScalar(_) => {
            Err(Error::BooleanExpressionUsedAsOutputType)
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
                match get_type_representation(
                    type_name,
                    &gds.metadata.object_types,
                    &gds.metadata.scalar_types,
                    &gds.metadata.boolean_expression_types,
                )
                .map_err(|_| Error::InternalTypeNotFound {
                    type_name: type_name.to_owned(),
                })? {
                    TypeRepresentation::Scalar(_) => Ok(super::TypeKind::Scalar),
                    TypeRepresentation::Object(_)
                    | TypeRepresentation::BooleanExpressionObject(_)
                    | TypeRepresentation::BooleanExpressionScalar(_) => Ok(super::TypeKind::Object),
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
    type_name: &Qualified<CustomTypeName>,
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    parent_graphql_type_name: &ast::TypeName,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>>, Error> {
    let mut graphql_fields = object_type_representation
        .object_type
        .fields
        .iter()
        .map(|(field_name, field_definition)| -> Result<_, Error> {
            let graphql_field_name = mk_name(field_name.as_str())
                .map_err(metadata_resolve::Error::from)
                .map_err(metadata_resolve::WithContext::from)?;

            let field_arguments = field_definition
                .field_arguments
                .iter()
                .map(|(argument_name, argument_type)| {
                    generate_field_argument(gds, builder, argument_name, argument_type)
                })
                .collect::<Result<BTreeMap<_, _>, _>>()?;
            let field_argument_types =
                field_definition
                    .field_arguments
                    .iter()
                    .map(|(argument_name, argument_type)| {
                        let name = ast::Name::new(argument_name.as_str())?;
                        Ok((name, argument_type.argument_type.clone()))
                    })
                    .collect::<Result<
                        BTreeMap<lang_graphql::ast::common::Name, QualifiedTypeReference>,
                        Error,
                    >>()?;
            let field = gql_schema::Field::<GDS>::new(
                graphql_field_name.clone(),
                field_definition.description.clone(),
                Annotation::Output(super::OutputAnnotation::Field {
                    name: field_name.clone(),
                    field_type: field_definition.field_type.clone(),
                    field_base_type_kind: get_type_kind(gds, &field_definition.field_type)?,
                    parent_type: type_name.to_owned(),
                    argument_types: field_argument_types,
                    deprecated: field_definition.deprecated.clone(),
                }),
                get_output_type(gds, builder, &field_definition.field_type)?,
                field_arguments,
                mk_deprecation_status(field_definition.deprecated.as_ref()),
            );
            // if output permissions are defined for this type, we conditionally
            // include fields
            let namespaced_field = {
                let mut role_map = HashMap::new();
                for (role, perms) in &object_type_representation.type_output_permissions {
                    if perms.allowed_fields.contains(field_name) {
                        role_map.insert(Role(role.0.clone()), None);
                    }
                }
                builder.conditional_namespaced(field, role_map)
            };
            Ok((graphql_field_name, namespaced_field))
        })
        .collect::<Result<BTreeMap<_, _>, _>>()?;

    add_relationship_fields(
        &mut graphql_fields,
        builder,
        gds,
        type_name,
        object_type_representation,
        parent_graphql_type_name,
    )?;

    Ok(graphql_fields)
}

/// Add the relationship fields to the `graphql_fields` map
fn add_relationship_fields(
    graphql_fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>>,
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
    type_name: &Qualified<CustomTypeName>,
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    parent_graphql_type_name: &ast::TypeName,
) -> Result<(), Error> {
    for relationship in object_type_representation.relationship_fields.values() {
        let mut relationship_fields = Vec::new();
        match &relationship.target {
            metadata_resolve::RelationshipTarget::Command(command_relationship_target) => {
                let command_relationship = command_relationship_field(
                    command_relationship_target,
                    builder,
                    gds,
                    relationship,
                    object_type_representation,
                )?;
                relationship_fields.push(command_relationship);
            }
            metadata_resolve::RelationshipTarget::Model(model_relationship_target) => {
                let ModelRelationshipFields {
                    field,
                    aggregate_field,
                } = model_relationship_fields(
                    model_relationship_target,
                    builder,
                    gds,
                    relationship,
                    object_type_representation,
                    parent_graphql_type_name,
                )?;
                relationship_fields.push(field);
                if let Some(aggregate_field) = aggregate_field {
                    relationship_fields.push(aggregate_field);
                }
            }
        }
        for relationship_field in relationship_fields {
            let relationship_field_name = relationship_field.data.name.clone();
            if graphql_fields
                .insert(relationship_field_name.clone(), relationship_field)
                .is_some()
            {
                return Err(Error::RelationshipFieldNameConflict {
                    relationship_name: relationship.relationship_name.clone(),
                    field_name: relationship_field_name,
                    type_name: type_name.clone(),
                });
            }
        }
    }

    Ok(())
}

/// Create a command relationship field
fn command_relationship_field(
    command_relationship_target: &metadata_resolve::CommandRelationshipTarget,
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
    relationship: &metadata_resolve::RelationshipField,
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
) -> Result<gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>, Error> {
    let relationship_output_type =
        get_output_type(gds, builder, &command_relationship_target.target_type)?;
    let command = gds
        .metadata
        .commands
        .get(&command_relationship_target.command_name)
        .ok_or_else(|| Error::InternalCommandNotFound {
            command_name: command_relationship_target.command_name.clone(),
        })?;

    let arguments_with_mapping = command_relationship_target
        .mappings
        .iter()
        .map(|mapping| &mapping.argument_name)
        .collect::<HashSet<_>>();

    let arguments = command
        .command
        .arguments
        .iter()
        .filter(|(argument_name, _argument_type)| !arguments_with_mapping.contains(argument_name))
        .map(|(argument_name, argument_type)| {
            generate_command_argument(gds, builder, command, argument_name, argument_type)
        })
        .collect::<Result<BTreeMap<_, _>, _>>()?;

    let field = builder.conditional_namespaced(
        gql_schema::Field::<GDS>::new(
            relationship.field_name.clone(),
            relationship.description.clone(),
            Annotation::Output(super::OutputAnnotation::RelationshipToCommand(
                CommandRelationshipAnnotation {
                    source_type: relationship.source.clone(),
                    relationship_name: relationship.relationship_name.clone(),
                    command_name: command_relationship_target.command_name.clone(),
                    target_source: CommandTargetSource::new(command)?,
                    target_type: command_relationship_target.target_type.clone(),
                    target_base_type_kind: get_type_kind(
                        gds,
                        &command_relationship_target.target_type,
                    )?,
                    mappings: command_relationship_target.mappings.clone(),
                    deprecated: relationship.deprecated.clone(),
                },
            )),
            relationship_output_type,
            arguments,
            mk_deprecation_status(relationship.deprecated.as_ref()),
        ),
        permissions::get_command_relationship_namespace_annotations(
            command,
            object_type_representation,
            &command_relationship_target.mappings,
        ),
    );
    Ok(field)
}

struct ModelRelationshipFields {
    // array or object relationship field
    field: gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>,
    // aggregate relationship field applicable only for an array relationship
    aggregate_field: Option<gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>>,
}

/// Create a model relationship field
fn model_relationship_fields(
    model_relationship_target: &metadata_resolve::ModelRelationshipTarget,
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
    relationship: &metadata_resolve::RelationshipField,
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    parent_graphql_type_name: &ast::TypeName,
) -> Result<ModelRelationshipFields, Error> {
    let relationship_base_output_type =
        get_custom_output_type(gds, builder, &model_relationship_target.target_typename)?;
    let relationship_output_type = match model_relationship_target.relationship_type {
        relationships::RelationshipType::Array => {
            let non_nullable_relationship_base_type =
                ast::TypeContainer::named_non_null(relationship_base_output_type);
            ast::TypeContainer::list_null(non_nullable_relationship_base_type)
        }
        relationships::RelationshipType::Object => {
            ast::TypeContainer::named_null(relationship_base_output_type)
        }
    };

    let model = gds
        .metadata
        .models
        .get(&model_relationship_target.model_name)
        .ok_or_else(|| Error::InternalModelNotFound {
            model_name: model_relationship_target.model_name.clone(),
        })?;

    let arguments = match model_relationship_target.relationship_type {
        relationships::RelationshipType::Array => generate_select_many_arguments(builder, model)?,
        relationships::RelationshipType::Object => BTreeMap::new(),
    };

    let target_object_type_representation =
        get_object_type_representation(gds, &model.model.data_type)?;

    let field = builder.conditional_namespaced(
        gql_schema::Field::<GDS>::new(
            relationship.field_name.clone(),
            relationship.description.clone(),
            Annotation::Output(super::OutputAnnotation::RelationshipToModel(
                ModelRelationshipAnnotation {
                    source_type: relationship.source.clone(),
                    relationship_name: relationship.relationship_name.clone(),
                    target_model_name: model_relationship_target.model_name.clone(),
                    target_capabilities: relationship.target_capabilities.clone(),
                    target_type: model_relationship_target.target_typename.clone(),
                    relationship_type: model_relationship_target.relationship_type.clone(),
                    mappings: model_relationship_target.mappings.clone(),
                    deprecated: relationship.deprecated.clone(),
                },
            )),
            relationship_output_type,
            arguments,
            mk_deprecation_status(relationship.deprecated.as_ref()),
        ),
        permissions::get_model_relationship_namespace_annotations(
            model,
            object_type_representation,
            target_object_type_representation,
            &model_relationship_target.mappings,
        ),
    );
    let aggregate_field = model_relationship_target
        .relationship_aggregate
        .as_ref()
        .map(|aggregate| {
            model_aggregate_relationship_field(
                aggregate,
                builder,
                gds,
                model,
                &aggregate.field_name,
                &model_relationship_target.target_typename,
                &model_relationship_target.mappings,
                relationship,
                object_type_representation,
                parent_graphql_type_name,
            )
        })
        .transpose()?;
    Ok(ModelRelationshipFields {
        field,
        aggregate_field,
    })
}

/// Create a model relationship field
fn model_aggregate_relationship_field(
    aggregate_relationship: &metadata_resolve::AggregateRelationship,
    builder: &mut gql_schema::Builder<GDS>,
    gds: &GDS,
    target_model: &metadata_resolve::ModelWithPermissions,
    aggregate_field_name: &ast::Name,
    target_typename: &Qualified<CustomTypeName>,
    mappings: &[metadata_resolve::RelationshipModelMapping],
    relationship: &metadata_resolve::RelationshipField,
    object_type_representation: &metadata_resolve::ObjectTypeWithRelationships,
    parent_graphql_type_name: &ast::TypeName,
) -> Result<gql_schema::Namespaced<GDS, gql_schema::Field<GDS>>, Error> {
    let aggregate_expression_name = &aggregate_relationship.aggregate_expression;
    let aggregate_expression = gds
        .metadata
        .aggregate_expressions
        .get(aggregate_expression_name)
        .ok_or_else(|| Error::InternalAggregateExpressionNotFound {
            aggregate_expression: aggregate_expression_name.clone(),
        })?;

    let aggregate_select_output_type =
        aggregates::get_aggregate_select_output_type(builder, aggregate_expression)?;

    let arguments = select_aggregate::generate_select_aggregate_arguments(
        builder,
        target_model,
        &aggregate_relationship.filter_input_field_name,
        aggregate_field_name,
        parent_graphql_type_name,
    )?;

    let target_object_type_representation =
        get_object_type_representation(gds, &target_model.model.data_type)?;

    let field = builder.conditional_namespaced(
        gql_schema::Field::<GDS>::new(
            aggregate_field_name.clone(),
            aggregate_relationship.description.clone(),
            Annotation::Output(super::OutputAnnotation::RelationshipToModelAggregate(
                ModelAggregateRelationshipAnnotation {
                    source_type: relationship.source.clone(),
                    relationship_name: relationship.relationship_name.clone(),
                    target_model_name: target_model.model.name.clone(),
                    target_type: target_typename.clone(),
                    mappings: mappings.to_vec(),
                    deprecated: relationship.deprecated.clone(),
                },
            )),
            ast::TypeContainer::named_non_null(aggregate_select_output_type),
            arguments,
            mk_deprecation_status(relationship.deprecated.as_ref()),
        ),
        permissions::get_model_relationship_namespace_annotations(
            target_model,
            object_type_representation,
            target_object_type_representation,
            mappings,
        ),
    );
    Ok(field)
}

fn generate_apollo_federation_directives(
    apollo_federation_config: &metadata_resolve::ResolvedObjectApolloFederationConfig,
) -> Vec<Directive> {
    let mut directives = Vec::new();
    for key in &apollo_federation_config.keys {
        let fields = key
            .fields
            .iter()
            .map(FieldName::as_str)
            .collect::<Vec<_>>()
            .join(" ");
        let key_directive = gql_schema::Directive {
            name: mk_name!("key"),
            arguments: vec![(
                mk_name!("fields"),
                lang_graphql::ast::value::ConstValue::SimpleValue(
                    lang_graphql::ast::value::SimpleValue::String(fields),
                ),
            )]
            .into_iter()
            .collect(),
        };
        directives.push(key_directive);
    }
    directives
}

pub fn output_type_schema(
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

    let mut object_type_fields = object_type_fields(
        gds,
        builder,
        type_name,
        object_type_representation,
        &graphql_type_name,
    )?;
    let directives = match &object_type_representation
        .object_type
        .apollo_federation_config
    {
        Some(apollo_federation_config) => {
            generate_apollo_federation_directives(apollo_federation_config)
        }
        None => Vec::new(),
    };
    if object_type_representation
        .object_type
        .global_id_fields
        .is_empty()
    {
        Ok(gql_schema::TypeInfo::Object(gql_schema::Object::new(
            builder,
            graphql_type_name,
            object_type_representation.object_type.description.clone(),
            object_type_fields,
            BTreeMap::new(),
            directives,
        )))
    } else {
        // Generate the Global object `id` field and insert it
        // into the `object_type_fields`.
        let mut interfaces = BTreeMap::new();
        let global_id_field_name = lang_graphql::mk_name!("id");
        let global_id_field = gql_schema::Field::<GDS>::new(
            global_id_field_name.clone(),
            object_type_representation.object_type.description.clone(),
            Annotation::Output(super::OutputAnnotation::GlobalIDField {
                global_id_fields: object_type_representation
                    .object_type
                    .global_id_fields
                    .clone(),
            }),
            get_output_type(gds, builder, &ID_TYPE_REFERENCE)?,
            BTreeMap::new(),
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
        let directives = match &object_type_representation
            .object_type
            .apollo_federation_config
        {
            Some(apollo_federation_config) => {
                generate_apollo_federation_directives(apollo_federation_config)
            }
            None => Vec::new(),
        };
        Ok(gql_schema::TypeInfo::Object(gql_schema::Object::new(
            builder,
            graphql_type_name,
            object_type_representation.object_type.description.clone(),
            object_type_fields,
            interfaces,
            directives,
        )))
    }
}

/// Gets the `ObjectTypeRepresentation` of the type
/// identified with the `gds_type`, it will throw
/// an error if the type is not found to be an object.
pub(crate) fn get_object_type_representation<'s>(
    gds: &'s GDS,
    gds_type: &Qualified<CustomTypeName>,
) -> Result<&'s metadata_resolve::ObjectTypeWithRelationships, crate::Error> {
    gds.metadata
        .object_types
        .get(gds_type)
        .ok_or_else(|| crate::Error::InternalTypeNotFound {
            type_name: gds_type.clone(),
        })
}

pub(crate) fn representations_type_reference(
    builder: &mut gql_schema::Builder<GDS>,
) -> RegisteredType {
    TypeContainer::list_non_null(TypeContainer::named_non_null(builder.register_type(
        TypeId::ApolloFederationType(PossibleApolloFederationTypes::Any),
    )))
}
