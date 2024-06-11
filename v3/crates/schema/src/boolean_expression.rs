use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::schema::{self as gql_schema};
use open_dds::{relationships::RelationshipType, types::CustomTypeName};
use std::collections::{BTreeMap, HashMap};

use super::types::output_type::get_object_type_representation;
use super::types::output_type::relationship::FilterRelationshipAnnotation;
use super::types::{BooleanExpressionAnnotation, InputAnnotation, TypeId};
use metadata_resolve::{
    mk_name, BooleanExpressionGraphqlConfig, ModelExpressionType, ModelWithPermissions,
    ObjectBooleanExpressionDataConnector, ObjectBooleanExpressionType, ObjectTypeWithRelationships,
    Qualified, Relationship, RelationshipModelMapping, ResolvedObjectBooleanExpressionType,
};

use crate::permissions;
use crate::types;
use crate::GDS;

use crate::Error;

// add input fields for `_and`, `_or`, etc
fn build_builtin_operator_schema(
    boolean_expression_info: &BooleanExpressionGraphqlConfig,
    type_name: &ast::TypeName,
    builder: &mut gql_schema::Builder<GDS>,
) -> BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>> {
    let mut input_fields = BTreeMap::new();

    // `_and`, `_or` or `_not` fields are available for all roles
    let not_field_name = &boolean_expression_info.graphql_config.not_operator_name;

    input_fields.insert(
        not_field_name.clone(),
        builder.allow_all_namespaced(
            gql_schema::InputField::<GDS>::new(
                not_field_name.clone(),
                None,
                types::Annotation::Input(InputAnnotation::BooleanExpression(
                    BooleanExpressionAnnotation::BooleanExpressionArgument {
                        field: types::ModelFilterArgument::NotOp,
                    },
                )),
                ast::TypeContainer::named_null(gql_schema::RegisteredTypeName::new(
                    type_name.0.clone(),
                )),
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            ),
            None,
        ),
    );

    let and_field_name = &boolean_expression_info.graphql_config.and_operator_name;

    input_fields.insert(
        and_field_name.clone(),
        builder.allow_all_namespaced(
            gql_schema::InputField::<GDS>::new(
                and_field_name.clone(),
                None,
                types::Annotation::Input(InputAnnotation::BooleanExpression(
                    BooleanExpressionAnnotation::BooleanExpressionArgument {
                        field: types::ModelFilterArgument::AndOp,
                    },
                )),
                ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(
                    gql_schema::RegisteredTypeName::new(type_name.0.clone()),
                )),
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            ),
            None,
        ),
    );

    let or_field_name = &boolean_expression_info.graphql_config.or_operator_name;
    input_fields.insert(
        or_field_name.clone(),
        builder.allow_all_namespaced(
            gql_schema::InputField::<GDS>::new(
                or_field_name.clone(),
                None,
                types::Annotation::Input(InputAnnotation::BooleanExpression(
                    BooleanExpressionAnnotation::BooleanExpressionArgument {
                        field: types::ModelFilterArgument::OrOp,
                    },
                )),
                ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(
                    gql_schema::RegisteredTypeName::new(type_name.0.clone()),
                )),
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            ),
            None,
        ),
    );

    input_fields
}

// Build input fields for fields that we are allowed to compare
fn build_comparable_fields_schema(
    gds: &GDS,
    object_type_name: &Qualified<CustomTypeName>,
    object_type_representation: &ObjectTypeWithRelationships,
    boolean_expression_info: &BooleanExpressionGraphqlConfig,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, Error> {
    let mut input_fields = BTreeMap::new();

    // scalar column fields
    for (field_name, comparison_expression) in &boolean_expression_info.scalar_fields {
        let field_graphql_name = mk_name(field_name.clone().0.as_str())?;
        let registered_type_name =
            get_scalar_comparison_input_type(builder, comparison_expression)?;
        let field_type = ast::TypeContainer::named_null(registered_type_name);
        let annotation = types::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: types::ModelFilterArgument::Field {
                    field_name: field_name.clone(),
                    object_type: object_type_name.clone(),
                },
            },
        ));
        let field_permissions: HashMap<Role, Option<types::NamespaceAnnotation>> =
            permissions::get_allowed_roles_for_field(object_type_representation, field_name)
                .map(|role| (role.clone(), None))
                .collect();

        let input_field = builder.conditional_namespaced(
            gql_schema::InputField::<GDS>::new(
                field_graphql_name.clone(),
                None,
                annotation,
                field_type,
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            ),
            field_permissions,
        );
        input_fields.insert(field_graphql_name, input_field);
    }

    // object column fields
    for (field_name, object_comparison_expression) in &boolean_expression_info.object_fields {
        let field_graphql_name = mk_name(field_name.clone().0.as_str())?;

        let registered_type_name =
            builder.register_type(TypeId::InputObjectBooleanExpressionType {
                graphql_type_name: object_comparison_expression.graphql_type_name.clone(),
                gds_type_name: object_comparison_expression.object_type_name.clone(),
            });

        let field_object_type_representation = gds
            .metadata
            .object_types
            .get(&object_comparison_expression.underlying_object_type_name)
            .unwrap();

        let field_type = ast::TypeContainer::named_null(registered_type_name);

        let annotation = types::Annotation::Input(InputAnnotation::BooleanExpression(
            BooleanExpressionAnnotation::BooleanExpressionArgument {
                field: types::ModelFilterArgument::Field {
                    field_name: field_name.clone(),
                    object_type: object_type_name.clone(),
                },
            },
        ));

        let field_permissions: HashMap<Role, Option<types::NamespaceAnnotation>> =
            permissions::get_allowed_roles_for_type(field_object_type_representation)
                .map(|role| (role.clone(), None))
                .collect();

        let input_field = builder.conditional_namespaced(
            gql_schema::InputField::<GDS>::new(
                field_graphql_name.clone(),
                None,
                annotation,
                field_type,
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            ),
            field_permissions,
        );
        input_fields.insert(field_graphql_name, input_field);
    }

    Ok(input_fields)
}

// build comparable relationships input fields
// TODO(naveen): Add support for command relationships
fn build_comparable_relationships_schema(
    gds: &GDS,
    object_type_representation: &ObjectTypeWithRelationships,
    object_boolean_expression_data_connector: &Option<ObjectBooleanExpressionDataConnector>,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, Error> {
    let mut input_fields = BTreeMap::new();

    for relationship in object_type_representation.relationships.values() {
        if let metadata_resolve::RelationshipTarget::Model {
            model_name,
            relationship_type,
            target_typename: _,
            mappings,
        } = &relationship.target
        {
            // lookup target model for relationship
            let target_model = gds.metadata.models.get(model_name).ok_or_else(|| {
                Error::InternalModelNotFound {
                    model_name: model_name.clone(),
                }
            })?;

            // lookup type underlying target model
            let target_object_type_representation =
                get_object_type_representation(gds, &target_model.model.data_type)?;

            // try and create a new input field
            if let Some((name, schema)) = build_model_relationship_schema(
                object_type_representation,
                object_boolean_expression_data_connector,
                target_object_type_representation,
                target_model,
                relationship,
                relationship_type,
                mappings,
                gds,
                builder,
            )? {
                input_fields.insert(name, schema);
            }
        }
    }
    Ok(input_fields)
}

type InputField = (
    ast::Name,
    gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>,
);

// build comparable relationships input fields
fn build_model_relationship_schema(
    source_object_type_representation: &ObjectTypeWithRelationships,
    source_object_boolean_expression_data_connector: &Option<ObjectBooleanExpressionDataConnector>,
    target_object_type_representation: &ObjectTypeWithRelationships,
    target_model: &ModelWithPermissions,
    relationship: &Relationship,
    relationship_type: &RelationshipType,
    relationship_model_mappings: &[RelationshipModelMapping],
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<Option<InputField>, Error> {
    // Build relationship field in filter expression only when
    // both the source boolean expression and target_model are backed by a source
    // We'll need to find a way of getting this information for BooleanExpressionTypes
    // that are uncoupled from their data connector source
    if let (Some(source_data_connector), Some(target_source)) = (
        &source_object_boolean_expression_data_connector,
        &target_model.model.source,
    ) {
        let target_model_source =
            metadata_resolve::ModelTargetSource::from_model_source(target_source, relationship)?;

        // filter expression with relationships is currently only supported for local relationships
        if let metadata_resolve::RelationshipExecutionCategory::Local =
            metadata_resolve::relationship_execution_category(
                &source_data_connector.link,
                &target_source.data_connector,
                &target_model_source.capabilities,
            )
        {
            if target_source.data_connector.name == source_data_connector.name {
                match &target_model.model.filter_expression_type {
                    None => {}
                    Some(ModelExpressionType::BooleanExpressionType(_)) => {
                        todo!("relationships with BooleanExpressionType")
                    }
                    Some(ModelExpressionType::ObjectBooleanExpressionType(
                        target_model_filter_expression,
                    )) => {
                        // If the relationship target model does not have filterExpressionType do not include
                        // it in the source model filter expression input type.
                        if let Some(ref target_model_filter_expression_graphql) =
                            &target_model_filter_expression.graphql
                        {
                            let annotation = FilterRelationshipAnnotation {
                                source_type: relationship.source.clone(),
                                relationship_name: relationship.name.clone(),
                                target_source: target_model_source.clone(),
                                target_type: target_model.model.data_type.clone(),
                                target_model_name: target_model.model.name.clone(),
                                relationship_type: relationship_type.clone(),
                                mappings: relationship_model_mappings.to_vec(),
                            };

                            let namespace_annotations =
                                permissions::get_model_relationship_namespace_annotations(
                                    target_model,
                                    source_object_type_representation,
                                    target_object_type_representation,
                                    relationship_model_mappings,
                                    &gds.metadata.object_types,
                                )?;

                            return Ok(Some((
                                    relationship.field_name.clone(),
                                    builder.conditional_namespaced(
                                        gql_schema::InputField::<GDS>::new(
                                            relationship.field_name.clone(),
                                            None,
                                            types::Annotation::Input(InputAnnotation::BooleanExpression(
                                                BooleanExpressionAnnotation
                                                ::BooleanExpressionArgument {
                                                    field:
                                                        types::ModelFilterArgument::RelationshipField(
                                                            annotation,
                                                        ),
                                                },
                                            )),
                                            ast::TypeContainer::named_null(
                                                gql_schema::RegisteredTypeName::new(
                                                    target_model_filter_expression_graphql.type_name.0.clone(),
                                                ),
                                            ),
                                            None,
                                            gql_schema::DeprecationStatus::NotDeprecated,
                                        ),
                                        namespace_annotations
                                    ),
                                )));
                        }
                    }
                }
            }
        }
    }

    Ok(None)
}
/// There are two types of BooleanExpressionType now, we try to build with both
pub fn build_boolean_expression_input_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    gds_type_name: &Qualified<CustomTypeName>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    match gds
        .metadata
        .object_boolean_expression_types
        .get(gds_type_name)
    {
        Some(object_boolean_expression_type) => build_schema_with_object_boolean_expression_type(
            object_boolean_expression_type,
            gds,
            builder,
            type_name,
            gds_type_name,
        ),
        None => {
            match gds
                .metadata
                .boolean_expression_types
                .objects
                .get(gds_type_name)
            {
                Some(boolean_expression_object_type) => build_schema_with_boolean_expression_type(
                    boolean_expression_object_type,
                    gds,
                    builder,
                    type_name,
                    gds_type_name,
                ),
                None => Err(Error::InternalTypeNotFound {
                    type_name: gds_type_name.clone(),
                }),
            }
        }
    }
}

// build the schema using the old `ObjectBooleanExpressionType` metadata kind
fn build_schema_with_object_boolean_expression_type(
    object_boolean_expression_type: &ObjectBooleanExpressionType,
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    gds_type_name: &Qualified<CustomTypeName>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    if let Some(boolean_expression_info) = &object_boolean_expression_type.graphql {
        let mut input_fields = BTreeMap::new();

        // add `_and`, `_not` etc
        input_fields.extend(build_builtin_operator_schema(
            boolean_expression_info,
            type_name,
            builder,
        ));

        let object_type_representation =
            get_object_type_representation(gds, &object_boolean_expression_type.object_type)?;

        // add in all fields that are directly comparable
        input_fields.extend(build_comparable_fields_schema(
            gds,
            &object_boolean_expression_type.object_type,
            object_type_representation,
            boolean_expression_info,
            builder,
        )?);

        // add in relationship fields
        input_fields.extend(build_comparable_relationships_schema(
            gds,
            object_type_representation,
            &object_boolean_expression_type.data_connector,
            builder,
        )?);

        Ok(gql_schema::TypeInfo::InputObject(
            gql_schema::InputObject::new(type_name.clone(), None, input_fields, Vec::new()),
        ))
    } else {
        Err(Error::InternalBooleanExpressionNotFound {
            type_name: gds_type_name.clone(),
        })
    }
}

// build the schema using the new `BooleanExpressionType` metadata kind
fn build_schema_with_boolean_expression_type(
    boolean_expression_object_type: &ResolvedObjectBooleanExpressionType,
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    gds_type_name: &Qualified<CustomTypeName>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    if let Some(boolean_expression_info) = &boolean_expression_object_type.graphql {
        let mut input_fields = BTreeMap::new();

        // add `_and`, `_not` etc
        input_fields.extend(build_builtin_operator_schema(
            boolean_expression_info,
            type_name,
            builder,
        ));

        let object_type_representation =
            get_object_type_representation(gds, &boolean_expression_object_type.object_type)?;

        // add in all fields that are directly comparable
        input_fields.extend(build_comparable_fields_schema(
            gds,
            &boolean_expression_object_type.object_type,
            object_type_representation,
            boolean_expression_info,
            builder,
        )?);

        // TODO: add in relationship fields
        /*
            input_fields.extend(build_comparable_relationships_schema(
                gds,
                object_type_representation,
                &object_boolean_expression_type.data_connector,
                builder,
            )?);
        */

        Ok(gql_schema::TypeInfo::InputObject(
            gql_schema::InputObject::new(type_name.clone(), None, input_fields, Vec::new()),
        ))
    } else {
        Err(Error::InternalBooleanExpressionNotFound {
            type_name: gds_type_name.clone(),
        })
    }
}

fn get_scalar_comparison_input_type(
    builder: &mut gql_schema::Builder<GDS>,
    comparison_expression: &metadata_resolve::ComparisonExpressionInfo,
) -> Result<gql_schema::RegisteredTypeName, Error> {
    let graphql_type_name = comparison_expression.type_name.clone();
    let mut operators = Vec::new();
    for (op_name, input_type) in &comparison_expression.operators {
        let op_name = mk_name(op_name.0.as_str())?;
        operators.push((op_name, input_type.clone()));
    }
    Ok(
        builder.register_type(TypeId::ScalarTypeComparisonExpression {
            graphql_type_name,
            operators,
            operator_mapping: comparison_expression.operator_mapping.clone(),
            is_null_operator_name: comparison_expression.is_null_operator_name.clone(),
        }),
    )
}
