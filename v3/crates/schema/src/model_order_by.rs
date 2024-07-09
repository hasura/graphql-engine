use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::models::ModelName;
use open_dds::relationships::RelationshipType;
use std::collections::{BTreeMap, HashMap};

use super::types::output_type::relationship::OrderByRelationshipAnnotation;
use super::types::{output_type::get_object_type_representation, Annotation, TypeId};
use crate::permissions;
use crate::types;
use crate::GDS;
use metadata_resolve::mk_name;
use metadata_resolve::Qualified;

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
        ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(builder.register_type(
            types::TypeId::ModelOrderByExpression {
                model_name,
                graphql_type_name: order_by_expression_info.order_by_type_name.clone(),
            },
        ))),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    )
}

pub fn build_model_order_by_input_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    model_name: &Qualified<ModelName>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let model =
        gds.metadata
            .models
            .get(model_name)
            .ok_or_else(|| Error::InternalModelNotFound {
                model_name: model_name.clone(),
            })?;

    let object_type_representation = get_object_type_representation(gds, &model.model.data_type)?;

    let mut fields = BTreeMap::new();

    let order_by_input_config = gds
        .metadata
        .graphql_config
        .order_by_input
        .as_ref()
        .ok_or_else(|| Error::InternalNoOrderByGraphqlConfig {
            model_name: model_name.clone(),
        })?;

    if let Some(model_order_by_expression) = model.graphql_api.order_by_expression.as_ref() {
        for (field_name, order_by_expression) in &model_order_by_expression.order_by_fields {
            let graphql_field_name =
                mk_name(field_name.as_str()).map_err(metadata_resolve::Error::from)?;

            let input_type =
                ast::TypeContainer::named_null(builder.register_type(TypeId::OrderByEnumType {
                    graphql_type_name: order_by_input_config.enum_type_name.clone(),
                }));
            let field_permissions: HashMap<Role, Option<types::NamespaceAnnotation>> =
                permissions::get_allowed_roles_for_field(object_type_representation, field_name)
                    .map(|role| (role.clone(), None))
                    .collect();
            let input_field = builder.conditional_namespaced(
                gql_schema::InputField::new(
                    graphql_field_name.clone(),
                    None,
                    Annotation::Input(types::InputAnnotation::Model(
                        types::ModelInputAnnotation::ModelOrderByArgument {
                            field_name: field_name.clone(),
                            parent_type: model.model.data_type.clone(),
                            ndc_column: order_by_expression.ndc_column.clone(),
                        },
                    )),
                    input_type,
                    None,
                    gql_schema::DeprecationStatus::NotDeprecated,
                ),
                field_permissions,
            );
            fields.insert(graphql_field_name, input_field);
        }

        // relationship fields
        // TODO(naveen): Add support for command relationships.
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
                    let target_model_source =
                        metadata_resolve::ModelTargetSource::from_model_source(
                            target_source,
                            relationship,
                        )?;

                    // order_by expression with relationships is currently only supported for local relationships
                    if let metadata_resolve::RelationshipExecutionCategory::Local =
                        metadata_resolve::relationship_execution_category(
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
                                            &gds.metadata.object_types,
                                        )?,
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }

        Ok(gql_schema::TypeInfo::InputObject(
            gql_schema::InputObject::new(type_name.clone(), None, fields, Vec::new()),
        ))
    } else {
        Err(Error::NoOrderByExpression {
            model_name: model_name.clone(),
        })
    }
}
