use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::data_connector::DataConnectorColumnName;
use open_dds::models::ModelName;
use open_dds::relationships::{RelationshipName, RelationshipType};
use open_dds::types::{CustomTypeName, Deprecated};
use std::collections::{BTreeMap, HashMap};

use super::types::output_type::relationship::OrderByRelationshipAnnotation;
use super::types::{output_type::get_object_type_representation, Annotation, TypeId};
use crate::types::{self};
use crate::{mk_deprecation_status, GDS};
use crate::{permissions, ModelInputAnnotation};
use metadata_resolve::{
    mk_name, ModelWithArgumentPresets, ObjectTypeWithRelationships, OrderByExpressionGraphqlConfig,
    OrderByExpressionIdentifier, OrderableField, OrderableObjectField, OrderableRelationship,
    OrderableRelationships,
};
use metadata_resolve::{Qualified, TypeMapping};

use crate::Error;

// Generates the schema for 'order_by' arguments: Asc/Desc
pub fn build_order_by_enum_type_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    order_by_type_name: &ast::TypeName,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let mut order_by_values = BTreeMap::new();
    let order_by_input_config = gds
        .metadata
        .graphql_config
        .order_by_input
        .as_ref()
        .ok_or_else(|| Error::InternalNoOrderByGraphqlConfigOrderByEnumType {
            type_name: order_by_type_name.clone(),
        })?;

    let asc_ast_name = &order_by_input_config.asc_direction_field_value;
    order_by_values.insert(
        asc_ast_name.clone(),
        builder.allow_all_namespaced(gql_schema::EnumValue {
            value: asc_ast_name.clone(),
            description: Some("Sorts the data in ascending order".to_string()),
            deprecation_status: gql_schema::DeprecationStatus::NotDeprecated,
            info: types::Annotation::Input(types::InputAnnotation::Model(
                types::ModelInputAnnotation::ModelOrderByDirection {
                    direction: types::ModelOrderByDirection::Asc,
                },
            )),
        }),
    );

    let desc_ast_name = &order_by_input_config.desc_direction_field_value;
    order_by_values.insert(
        desc_ast_name.clone(),
        builder.allow_all_namespaced(gql_schema::EnumValue {
            value: desc_ast_name.clone(),
            description: Some("Sorts the data in descending order".to_string()),
            deprecation_status: gql_schema::DeprecationStatus::NotDeprecated,
            info: types::Annotation::Input(types::InputAnnotation::Model(
                types::ModelInputAnnotation::ModelOrderByDirection {
                    direction: types::ModelOrderByDirection::Desc,
                },
            )),
        }),
    );

    Ok(gql_schema::TypeInfo::Enum(gql_schema::Enum {
        name: order_by_type_name.clone(),
        description: None,
        values: order_by_values,
        directives: Vec::new(),
    }))
}

pub fn get_order_by_expression_input_field(
    builder: &mut gql_schema::Builder<GDS>,
    model_name: Qualified<ModelName>,
    order_by_expression_info: &metadata_resolve::ModelOrderByExpression,
) -> gql_schema::InputField<GDS> {
    gql_schema::InputField::new(
        order_by_expression_info.order_by_field_name.clone(),
        None,
        types::Annotation::Input(types::InputAnnotation::Model(
            types::ModelInputAnnotation::ModelOrderByExpression,
        )),
        ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(
            builder.register_type(types::TypeId::ModelOrderByExpression {
                model_name,
                order_by_expression_identifier: order_by_expression_info
                    .order_by_expression_identifier
                    .clone(),
                graphql_type_name: order_by_expression_info.order_by_type_name.clone(),
            }),
        )),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    )
}

fn get_order_by_expression_nested_object_input_field(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    ndc_column: DataConnectorColumnName,
    model_name: Qualified<ModelName>,
    order_by_type_name: &ast::TypeName,
    order_by_field_name: &ast::Name,
    order_by_expression_identifier: &Qualified<OrderByExpressionIdentifier>,
    deprecated: Option<&Deprecated>,
) -> gql_schema::InputField<GDS> {
    let raw_field_type = builder.register_type(types::TypeId::ModelOrderByExpression {
        model_name,
        order_by_expression_identifier: order_by_expression_identifier.clone(),
        graphql_type_name: order_by_type_name.clone(),
    });

    // The type of the field should just be a nullable version of the raw field type, but this was implemented
    // incorrectly as a list of the raw field type originally, so we need to maintain that incorrect form behind
    // a flag for backwards compatibility.
    let field_type = match gds
        .metadata
        .graphql_config
        .multiple_order_by_input_object_fields
    {
        metadata_resolve::MultipleOrderByInputObjectFields::Disallow => {
            ast::TypeContainer::named_null(raw_field_type)
        }
        metadata_resolve::MultipleOrderByInputObjectFields::Allow => {
            ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(raw_field_type))
        }
    };

    gql_schema::InputField::new(
        order_by_field_name.clone(),
        None,
        types::Annotation::Input(types::InputAnnotation::Model(
            ModelInputAnnotation::ModelOrderByNestedExpression {
                ndc_column,
                multiple_input_properties: gds
                    .metadata
                    .graphql_config
                    .multiple_order_by_input_object_fields,
            },
        )),
        field_type,
        None,
        mk_deprecation_status(deprecated),
    )
}

pub fn build_model_order_by_input_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    model_name: &Qualified<ModelName>,
    order_by_expression_identifier: &Qualified<OrderByExpressionIdentifier>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let model =
        gds.metadata
            .models
            .get(model_name)
            .ok_or_else(|| Error::InternalModelNotFound {
                model_name: model_name.clone(),
            })?;

    let order_by_expression = gds
        .metadata
        .order_by_expressions
        .objects
        .get(order_by_expression_identifier)
        .ok_or_else(|| Error::InternalOrderByExpressionNotFound {
            order_by_expression_identifier: order_by_expression_identifier.clone(),
        })?;

    let object_type_representation =
        get_object_type_representation(gds, &order_by_expression.ordered_type)?;

    let mut fields = BTreeMap::new();

    let order_by_input_config = gds
        .metadata
        .graphql_config
        .order_by_input
        .as_ref()
        .ok_or_else(|| Error::InternalNoOrderByGraphqlConfigOrderByExpression {
            order_by_expression_identifier: order_by_expression_identifier.clone(),
        })?;

    let field_mappings = model
        .model
        .source
        .as_ref()
        .and_then(|source| source.type_mappings.get(&order_by_expression.ordered_type))
        .map(|TypeMapping::Object { field_mappings, .. }| field_mappings)
        .ok_or_else(|| Error::InternalTypeMappingNotFound {
            type_name: order_by_expression.ordered_type.clone(),
        })?;

    for (field_name, orderable_field) in &order_by_expression.orderable_fields {
        let graphql_field_name = mk_name(field_name.as_str())
            .map_err(metadata_resolve::Error::from)
            .map_err(metadata_resolve::WithContext::from)?;

        // Get internal field definition
        let field_definition = object_type_representation
            .object_type
            .fields
            .get(field_name)
            .ok_or_else(|| Error::InternalObjectTypeFieldNotFound {
                field_name: field_name.clone(),
                type_name: order_by_expression.ordered_type.clone(),
            })?;

        let field_permissions: HashMap<Role, Option<types::NamespaceAnnotation>> =
            permissions::get_allowed_roles_for_field(object_type_representation, field_name)
                .map(|role| (role.clone(), None))
                .collect();

        let ndc_column = field_mappings
            .get(field_name)
            .map(|field_mapping| field_mapping.column.clone())
            .ok_or_else(|| Error::InternalMappingNotFound {
                type_name: order_by_expression.ordered_type.clone(),
                field_name: field_name.clone(),
            })?;
        let input_field = match orderable_field {
            OrderableField::Scalar(_) => {
                let input_type = ast::TypeContainer::named_null(builder.register_type(
                    TypeId::OrderByEnumType {
                        graphql_type_name: order_by_input_config.enum_type_name.clone(),
                    },
                ));
                builder.conditional_namespaced(
                    gql_schema::InputField::new(
                        graphql_field_name.clone(),
                        None,
                        Annotation::Input(types::InputAnnotation::Model(
                            types::ModelInputAnnotation::ModelOrderByArgument {
                                field_name: field_name.clone(),
                                parent_type: order_by_expression.ordered_type.clone(),
                                ndc_column,
                                deprecated: field_definition.deprecated.clone(),
                            },
                        )),
                        input_type,
                        None,
                        gql_schema::DeprecationStatus::NotDeprecated,
                    ),
                    field_permissions,
                )
            }
            OrderableField::Object(OrderableObjectField {
                order_by_expression_identifier,
            }) => {
                let nested_order_by_expression = gds
                    .metadata
                    .order_by_expressions
                    .objects
                    .get(order_by_expression_identifier)
                    .ok_or_else(|| Error::InternalOrderByExpressionNotFound {
                        order_by_expression_identifier: order_by_expression_identifier.clone(),
                    })?;
                let graphql_type_name = nested_order_by_expression
                    .graphql
                    .as_ref()
                    .map(
                        |OrderByExpressionGraphqlConfig {
                             expression_type_name,
                         }| expression_type_name,
                    )
                    .ok_or_else(|| Error::InternalNoOrderByGraphqlConfigOrderByExpression {
                        order_by_expression_identifier: order_by_expression_identifier.clone(),
                    })?;
                let input_field = get_order_by_expression_nested_object_input_field(
                    gds,
                    builder,
                    ndc_column,
                    model_name.clone(),
                    graphql_type_name,
                    &graphql_field_name,
                    order_by_expression_identifier,
                    field_definition.deprecated.as_ref(),
                );
                builder.conditional_namespaced(input_field, field_permissions)
            }
        };
        fields.insert(graphql_field_name, input_field);
    }

    // relationship fields
    match &order_by_expression.orderable_relationships {
        OrderableRelationships::ModelV2(orderable_relationships) => {
            build_orderable_relationships(
                gds,
                &mut fields,
                builder,
                model,
                &order_by_expression.ordered_type,
                object_type_representation,
                orderable_relationships,
            )?;
        }
        OrderableRelationships::ModelV1AllowAll => {
            build_all_relationships(
                gds,
                &mut fields,
                builder,
                model,
                &order_by_expression.ordered_type,
                object_type_representation,
            )?;
        }
    }

    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(type_name.clone(), None, fields, Vec::new()),
    ))
}

// when using `ModelV1` we do not explicitly allow relationships, so any `Relationship` on the type
// to a valid model is allowed. When we deprecate `ModelV1`, this function should simply be
// removed.
fn build_all_relationships(
    gds: &GDS,
    fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    builder: &mut gql_schema::Builder<GDS>,
    model: &ModelWithArgumentPresets,
    object_type_name: &Qualified<CustomTypeName>,
    object_type_representation: &ObjectTypeWithRelationships,
) -> Result<(), Error> {
    for (rel_name, relationship) in &object_type_representation.relationship_fields {
        if let metadata_resolve::RelationshipTarget::Model(
            metadata_resolve::ModelRelationshipTarget {
                model_name,
                relationship_type,
                target_typename,
                mappings,
            },
        ) = &relationship.target
        {
            let target_model = gds.metadata.models.get(model_name).ok_or_else(|| {
                crate::Error::InternalModelNotFound {
                    model_name: model_name.clone(),
                }
            })?;

            let target_object_type_representation =
                get_object_type_representation(gds, &target_model.model.data_type)?;

            // Build relationship field in filter expression only when both
            // the target_model and source model are backed by a source
            if let (Some(target_source), Some(model_source)) =
                (&target_model.model.source, &model.model.source)
            {
                let target_model_source = metadata_resolve::ModelTargetSource::from_model_source(
                    target_source,
                    relationship,
                )
                .map_err(metadata_resolve::Error::from)
                .map_err(metadata_resolve::WithContext::from)?;

                let relationship_field_nestedness = if model.model.data_type == *object_type_name {
                    metadata_resolve::FieldNestedness::NotNested
                } else {
                    metadata_resolve::FieldNestedness::ObjectNested
                };

                // order_by expression with relationships is currently only supported for local relationships
                if let metadata_resolve::RelationshipExecutionCategory::Local =
                    metadata_resolve::relationship_execution_category(
                        relationship_field_nestedness,
                        &model_source.data_connector,
                        &target_source.data_connector,
                        &target_model_source.capabilities,
                    )
                {
                    // TODO(naveen): Support Array relationships in order_by when the support for aggregates is implemented
                    if let RelationshipType::Object = relationship_type {
                        // If the relationship target model does not have orderByExpressionType do not include
                        // it in the source model order_by input type.
                        if let Some(target_model_order_by_expression) =
                            target_model.graphql_api.order_by_expression.as_ref()
                        {
                            let target_model_order_by_expression_type_name =
                                &target_model_order_by_expression.order_by_type_name;

                            let annotation = OrderByRelationshipAnnotation {
                                source_type: relationship.source.clone(),
                                relationship_name: relationship.relationship_name.clone(),
                                target_model_name: model_name.clone(),
                                target_source: target_model_source.clone(),
                                target_type: target_typename.clone(),
                                relationship_type: relationship_type.clone(),
                                mappings: mappings.clone(),
                                source_data_connector: model_source.data_connector.clone(),
                                source_type_mappings: model_source.type_mappings.clone(),
                                deprecated: relationship.deprecated.clone(),
                                multiple_input_properties: gds
                                    .metadata
                                    .graphql_config
                                    .multiple_order_by_input_object_fields,
                            };

                            fields.insert(
                                    rel_name.clone(),
                                    builder.conditional_namespaced(
                                        gql_schema::InputField::new(
                                            rel_name.clone(),
                                            None,
                                            types::Annotation::Input(types::InputAnnotation::Model(
                                                types::ModelInputAnnotation::ModelOrderByRelationshipArgument(annotation),
                                            )),
                                            ast::TypeContainer::named_null(
                                                gql_schema::RegisteredTypeName::new(
                                                    target_model_order_by_expression_type_name.0.clone(),
                                                ),
                                            ),
                                            None,
                                            gql_schema::DeprecationStatus::NotDeprecated,
                                        ),
                                        permissions::get_model_relationship_namespace_annotations(
                                            target_model,
                                            object_type_representation,
                                            target_object_type_representation,
                                            mappings,
                                        )?,
                                    ),
                                );
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

// when using `OrderByExpressions` the user explicitly lets us know which relationships
// are allowed, and which `OrderByExpression` to use
fn build_orderable_relationships(
    gds: &GDS,
    fields: &mut BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>,
    builder: &mut gql_schema::Builder<GDS>,
    model: &ModelWithArgumentPresets,
    object_type_name: &Qualified<CustomTypeName>,
    object_type_representation: &ObjectTypeWithRelationships,
    orderable_relationships: &BTreeMap<RelationshipName, OrderableRelationship>,
) -> Result<(), Error> {
    for (rel_name, orderable_relationship) in orderable_relationships {
        let field_name = mk_name(rel_name.as_str())
            .map_err(metadata_resolve::Error::from)
            .map_err(metadata_resolve::WithContext::from)?;

        // lookup the relationship used in the underlying object type
        let relationship = object_type_representation
            .relationship_fields
            .get(&field_name)
            .ok_or_else(|| Error::InternalRelationshipNotFound {
                relationship_name: rel_name.clone(),
            })?;

        if let metadata_resolve::RelationshipTarget::Model(
            metadata_resolve::ModelRelationshipTarget {
                model_name,
                relationship_type,
                target_typename,
                mappings,
            },
        ) = &relationship.target
        {
            let target_model = gds.metadata.models.get(model_name).ok_or_else(|| {
                crate::Error::InternalModelNotFound {
                    model_name: model_name.clone(),
                }
            })?;

            let target_object_type_representation =
                get_object_type_representation(gds, &target_model.model.data_type)?;

            // Build relationship field in filter expression only when both
            // the target_model and source model are backed by a source
            if let (Some(target_source), Some(model_source)) =
                (&target_model.model.source, &model.model.source)
            {
                let target_model_source = metadata_resolve::ModelTargetSource::from_model_source(
                    target_source,
                    relationship,
                )
                .map_err(metadata_resolve::Error::from)
                .map_err(metadata_resolve::WithContext::from)?;

                let relationship_field_nestedness = if model.model.data_type == *object_type_name {
                    metadata_resolve::FieldNestedness::NotNested
                } else {
                    metadata_resolve::FieldNestedness::ObjectNested
                };

                // order_by expression with relationships is currently only supported for local relationships
                if let metadata_resolve::RelationshipExecutionCategory::Local =
                    metadata_resolve::relationship_execution_category(
                        relationship_field_nestedness,
                        &model_source.data_connector,
                        &target_source.data_connector,
                        &target_model_source.capabilities,
                    )
                {
                    // TODO(naveen): Support Array relationships in order_by when the support for aggregates is implemented
                    if let RelationshipType::Object = relationship_type {
                        // which type to use for the inner ordering?
                        // if there is one designated by the orderable relationship, use that
                        // otherwise use whatever the target models feels like
                        if let Some(target_model_order_by_expression_type_name) =
                            match &orderable_relationship.order_by_expression {
                                Some(target_model_order_by_expression_name) => {
                                    let qualified_target_order_by_identifier = Qualified::new(
                                        target_model_order_by_expression_name.subgraph.clone(),
                                        OrderByExpressionIdentifier::FromOrderByExpression(
                                            target_model_order_by_expression_name.name.clone(),
                                        ),
                                    );

                                    let target_order_by_expression = gds
                                        .metadata
                                        .order_by_expressions
                                        .objects
                                        .get(&qualified_target_order_by_identifier)
                                        .ok_or_else(|| {
                                            Error::InternalOrderByExpressionNotFound {
                                                order_by_expression_identifier:
                                                    qualified_target_order_by_identifier.clone(),
                                            }
                                        })?;

                                    // lookup graphql type name if it has one defined
                                    let maybe_graphql_type = target_order_by_expression
                                        .graphql
                                        .as_ref()
                                        .map(|graphql| &graphql.expression_type_name);

                                    // we add the target order by expression to the schema
                                    // as it might not be attached to anything else
                                    if let Some(graphql_type) = maybe_graphql_type {
                                        builder.register_type(
                                            types::TypeId::ModelOrderByExpression {
                                                model_name: model_name.clone(),
                                                order_by_expression_identifier:
                                                    qualified_target_order_by_identifier.clone(),
                                                graphql_type_name: graphql_type.clone(),
                                            },
                                        );
                                    }

                                    maybe_graphql_type
                                }
                                None => target_model
                                    .graphql_api
                                    .order_by_expression
                                    .as_ref()
                                    .map(|graphql| &graphql.order_by_type_name),
                            }
                        {
                            let annotation = OrderByRelationshipAnnotation {
                                source_type: relationship.source.clone(),
                                relationship_name: relationship.relationship_name.clone(),
                                target_model_name: model_name.clone(),
                                target_source: target_model_source.clone(),
                                target_type: target_typename.clone(),
                                relationship_type: relationship_type.clone(),
                                mappings: mappings.clone(),
                                source_data_connector: model_source.data_connector.clone(),
                                source_type_mappings: model_source.type_mappings.clone(),
                                deprecated: relationship.deprecated.clone(),
                                multiple_input_properties: gds
                                    .metadata
                                    .graphql_config
                                    .multiple_order_by_input_object_fields,
                            };

                            fields.insert(
                                    field_name.clone(),
                                    builder.conditional_namespaced(
                                        gql_schema::InputField::new(
                                            field_name.clone(),
                                            None,
                                            types::Annotation::Input(types::InputAnnotation::Model(
                                                types::ModelInputAnnotation::ModelOrderByRelationshipArgument(annotation),
                                            )),
                                            ast::TypeContainer::named_null(
                                                gql_schema::RegisteredTypeName::new(
                                                    target_model_order_by_expression_type_name.0.clone(),
                                                ),
                                            ),
                                            None,
                                            gql_schema::DeprecationStatus::NotDeprecated,
                                        ),
                                        permissions::get_model_relationship_namespace_annotations(
                                            target_model,
                                            object_type_representation,
                                            target_object_type_representation,
                                            mappings,
                                        )?,
                                    ),
                                );
                        };
                    }
                }
            }
        }
    }
    Ok(())
}
