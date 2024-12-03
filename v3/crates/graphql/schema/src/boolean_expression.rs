use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::schema::{self as gql_schema};
use open_dds::data_connector::DataConnectorName;
use open_dds::types::Deprecated;
use open_dds::{
    relationships::RelationshipType,
    types::{CustomTypeName, FieldName},
};
use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use super::types::input_type;
use super::types::output_type::get_object_type_representation;
use super::types::output_type::relationship::FilterRelationshipAnnotation;
use super::types::{ObjectFieldKind, TypeId};
use metadata_resolve::{
    mk_name, BooleanExpressionComparableRelationship, ComparisonExpressionInfo,
    GlobalGraphqlConfig, IncludeLogicalOperators, ModelExpressionType, ModelWithArgumentPresets,
    ObjectBooleanExpressionGraphqlConfig, ObjectBooleanExpressionType,
    ObjectComparisonExpressionInfo, ObjectComparisonKind, ObjectTypeWithRelationships,
    OperatorMapping, Qualified, QualifiedTypeReference, RelationshipCapabilities,
    RelationshipField, RelationshipModelMapping, ResolvedObjectBooleanExpressionType,
    ScalarBooleanExpressionGraphqlConfig,
};

use crate::mk_deprecation_status;
use crate::permissions;
use crate::types;
use crate::GDS;

use crate::Error;

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

// add input fields for `_and`, `_or`, etc
fn build_logical_operators_schema<
    FMkAnnotation: Fn(types::LogicalOperatorField) -> types::BooleanExpressionAnnotation,
>(
    field_config: &metadata_resolve::LogicalOperatorsGraphqlConfig,
    type_name: &ast::TypeName,
    builder: &mut gql_schema::Builder<GDS>,
    mk_annotation: FMkAnnotation,
) -> BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>> {
    let mut input_fields = BTreeMap::new();

    // `_and`, `_or` or `_not` fields are available for all roles
    let not_field_name = &field_config.not_operator_name;

    input_fields.insert(
        not_field_name.clone(),
        builder.allow_all_namespaced(gql_schema::InputField::<GDS>::new(
            not_field_name.clone(),
            None,
            types::Annotation::Input(types::InputAnnotation::BooleanExpression(mk_annotation(
                types::LogicalOperatorField::NotOp,
            ))),
            ast::TypeContainer::named_null(gql_schema::RegisteredTypeName::new(
                type_name.0.clone(),
            )),
            None,
            gql_schema::DeprecationStatus::NotDeprecated,
        )),
    );

    let and_field_name = &field_config.and_operator_name;

    input_fields.insert(
        and_field_name.clone(),
        builder.allow_all_namespaced(gql_schema::InputField::<GDS>::new(
            and_field_name.clone(),
            None,
            types::Annotation::Input(types::InputAnnotation::BooleanExpression(mk_annotation(
                types::LogicalOperatorField::AndOp,
            ))),
            ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(
                gql_schema::RegisteredTypeName::new(type_name.0.clone()),
            )),
            None,
            gql_schema::DeprecationStatus::NotDeprecated,
        )),
    );

    let or_field_name = &field_config.or_operator_name;
    input_fields.insert(
        or_field_name.clone(),
        builder.allow_all_namespaced(gql_schema::InputField::<GDS>::new(
            or_field_name.clone(),
            None,
            types::Annotation::Input(types::InputAnnotation::BooleanExpression(mk_annotation(
                types::LogicalOperatorField::OrOp,
            ))),
            ast::TypeContainer::list_null(ast::TypeContainer::named_non_null(
                gql_schema::RegisteredTypeName::new(type_name.0.clone()),
            )),
            None,
            gql_schema::DeprecationStatus::NotDeprecated,
        )),
    );

    input_fields
}

// Build input fields for fields that we are allowed to compare
fn build_comparable_fields_schema(
    gds: &GDS,
    object_type_name: &Qualified<CustomTypeName>,
    object_type_representation: &ObjectTypeWithRelationships,
    scalar_fields: &BTreeMap<FieldName, ComparisonExpressionInfo>,
    scalar_fields_graphql: &BTreeMap<FieldName, ScalarBooleanExpressionGraphqlConfig>,
    object_fields: &BTreeMap<FieldName, ObjectComparisonExpressionInfo>,
    object_fields_graphql: &BTreeMap<FieldName, ObjectBooleanExpressionGraphqlConfig>,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, Error> {
    let mut input_fields = BTreeMap::new();

    // scalar column fields
    for (field_name, comparison_expression) in scalar_fields {
        let field_graphql_name = mk_name(field_name.as_str())
            .map_err(metadata_resolve::Error::from)
            .map_err(metadata_resolve::WithContext::from)?;

        if let Some(scalar_boolean_expression_graphql) = scalar_fields_graphql.get(field_name) {
            let registered_type_name = get_scalar_boolean_expression_type(
                builder,
                comparison_expression,
                scalar_boolean_expression_graphql,
            )?;
            let field_type = ast::TypeContainer::named_null(registered_type_name);

            // Get internal field definition
            let field_definition = object_type_representation
                .object_type
                .fields
                .get(field_name)
                .ok_or_else(|| Error::InternalObjectTypeFieldNotFound {
                    field_name: field_name.clone(),
                    type_name: object_type_name.clone(),
                })?;

            // create Field annotation for this field
            let annotation = types::Annotation::Input(types::InputAnnotation::BooleanExpression(
                types::BooleanExpressionAnnotation::ObjectBooleanExpressionField(
                    types::ObjectBooleanExpressionField::Field {
                        field_name: field_name.clone(),
                        object_field_kind: ObjectFieldKind::Scalar,
                        object_type: object_type_name.clone(),
                        deprecated: field_definition.deprecated.clone(),
                    },
                ),
            ));

            // calculate permissions
            let field_permissions: HashMap<Role, Option<types::NamespaceAnnotation>> =
                permissions::get_allowed_roles_for_field(object_type_representation, field_name)
                    .map(|role| (role.clone(), None))
                    .collect();

            // create schema for this field, using `field_permissions` to make this appear
            // only for roles that are allowed to see it
            let input_field = builder.conditional_namespaced(
                gql_schema::InputField::<GDS>::new(
                    field_graphql_name.clone(),
                    None,
                    annotation,
                    field_type,
                    None,
                    if gds
                        .metadata
                        .graphql_config
                        .propagate_boolean_expression_deprecation_status
                    {
                        mk_deprecation_status(field_definition.deprecated.as_ref())
                    } else {
                        gql_schema::DeprecationStatus::NotDeprecated
                    },
                ),
                field_permissions,
            );
            input_fields.insert(field_graphql_name, input_field);
        }
    }

    // object column fields
    for (field_name, object_comparison_expression) in object_fields {
        if let Some(object_boolean_expression_graphql) = object_fields_graphql.get(field_name) {
            let field_graphql_name = mk_name(field_name.as_str())
                .map_err(metadata_resolve::Error::from)
                .map_err(metadata_resolve::WithContext::from)?;

            let registered_type_name =
                builder.register_type(TypeId::InputObjectBooleanExpressionType {
                    graphql_type_name: object_boolean_expression_graphql.graphql_type_name.clone(),
                    gds_type_name: object_comparison_expression.object_type_name.clone(),
                });

            let field_object_type_representation = gds
                .metadata
                .object_types
                .get(&object_comparison_expression.underlying_object_type_name)
                .unwrap();

            let field_type = ast::TypeContainer::named_null(registered_type_name);

            // Get internal field definition
            let field_definition = object_type_representation
                .object_type
                .fields
                .get(field_name)
                .ok_or_else(|| Error::InternalObjectTypeFieldNotFound {
                    field_name: field_name.clone(),
                    type_name: object_type_name.clone(),
                })?;

            // create Field annotation for field
            let annotation = types::Annotation::Input(types::InputAnnotation::BooleanExpression(
                types::BooleanExpressionAnnotation::ObjectBooleanExpressionField(
                    types::ObjectBooleanExpressionField::Field {
                        field_name: field_name.clone(),
                        object_field_kind: match object_comparison_expression.field_kind {
                            ObjectComparisonKind::Object => ObjectFieldKind::Object,
                            ObjectComparisonKind::ObjectArray => ObjectFieldKind::ObjectArray,
                        },
                        object_type: object_type_name.clone(),
                        deprecated: field_definition.deprecated.clone(),
                    },
                ),
            ));

            // calculate permissions
            let field_permissions: HashMap<Role, Option<types::NamespaceAnnotation>> =
                permissions::get_allowed_roles_for_type(field_object_type_representation)
                    .map(|role| (role.clone(), None))
                    .collect();

            // add annotation to input field
            let input_field = builder.conditional_namespaced(
                gql_schema::InputField::<GDS>::new(
                    field_graphql_name.clone(),
                    None,
                    annotation,
                    field_type,
                    None,
                    if gds
                        .metadata
                        .graphql_config
                        .propagate_boolean_expression_deprecation_status
                    {
                        mk_deprecation_status(field_definition.deprecated.as_ref())
                    } else {
                        gql_schema::DeprecationStatus::NotDeprecated
                    },
                ),
                field_permissions,
            );
            input_fields.insert(field_graphql_name, input_field);
        }
    }

    Ok(input_fields)
}

// build relationships that use the new `BooleanExpressionType` style
fn build_new_comparable_relationships_schema(
    gds: &GDS,
    object_type_representation: &ObjectTypeWithRelationships,
    relationship_fields: &BTreeMap<FieldName, BooleanExpressionComparableRelationship>,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, Error> {
    let mut input_fields = BTreeMap::new();

    for (relationship_name, comparable_relationship) in relationship_fields {
        let field_name = mk_name(relationship_name.as_str())
            .map_err(metadata_resolve::Error::from)
            .map_err(metadata_resolve::WithContext::from)?;

        // lookup the relationship used in the underlying object type
        let relationship = object_type_representation
            .relationship_fields
            .get(&field_name)
            .ok_or_else(|| Error::InternalRelationshipNotFound {
                relationship_name: comparable_relationship.relationship_name.clone(),
            })?;

        // Check whether the relationship is allowed to be compared
        if !include_relationship_field(
            relationship.target_capabilities.as_ref(),
            &gds.metadata.graphql_config,
        ) {
            continue;
        }

        // we haven't thought about Command relationship targets yet
        if let metadata_resolve::RelationshipTarget::Model(
            metadata_resolve::ModelRelationshipTarget {
                model_name,
                relationship_type,
                target_typename: _,
                mappings,
            },
        ) = &relationship.target
        {
            // lookup target model for relationship
            let target_model = gds.metadata.models.get(model_name).ok_or_else(|| {
                Error::InternalModelNotFound {
                    model_name: model_name.clone(),
                }
            })?;

            let target_boolean_expression_type_name =
                &comparable_relationship.boolean_expression_type;

            let target_boolean_expression_graphql_type = {
                let target_boolean_expression_graphql_type = match gds
                    .metadata
                    .boolean_expression_types
                    .objects
                    .get(target_boolean_expression_type_name)
                {
                    Some(bool_exp) => {
                        Ok(bool_exp.graphql.as_ref().map(|graphql| &graphql.type_name))
                    }
                    None => {
                        // perhaps it's referring to an old-style ObjectBooleanExpressionType
                        match gds
                            .metadata
                            .object_boolean_expression_types
                            .get(target_boolean_expression_type_name)
                        {
                            Some(object_bool_exp) => Ok(object_bool_exp
                                .graphql
                                .as_ref()
                                .map(|graphql| &graphql.type_name)),
                            None => Err(Error::InternalBooleanExpressionNotFound {
                                type_name: target_boolean_expression_type_name.clone(),
                            }),
                        }
                    }
                }?;

                // if we find a type, make sure it's added to the schema
                if let Some(type_name) = target_boolean_expression_graphql_type {
                    let _registered_type_name =
                        builder.register_type(TypeId::InputObjectBooleanExpressionType {
                            graphql_type_name: type_name.clone(),
                            gds_type_name: target_boolean_expression_type_name.clone(),
                        });
                }
                // return type name
                target_boolean_expression_graphql_type
            };

            // lookup type underlying target model
            let target_object_type_representation =
                get_object_type_representation(gds, &target_model.model.data_type)?;

            // if our target model has a boolean expression type to use, and a source,
            if let (Some(target_boolean_expression_graphql_type), Some(target_source)) = (
                target_boolean_expression_graphql_type,
                &target_model.model.source,
            ) {
                // create a new input field
                let (name, schema) = build_model_relationship_schema(
                    object_type_representation,
                    target_object_type_representation,
                    target_boolean_expression_graphql_type,
                    target_model,
                    target_source,
                    relationship,
                    relationship_type,
                    mappings,
                    relationship.deprecated.as_ref(),
                    gds,
                    builder,
                )?;

                input_fields.insert(name, schema);
            }
        }
    }
    Ok(input_fields)
}

// build comparable relationships input fields for the older `ObjectBooleanExpressionType`
// TODO(naveen): Add support for command relationships
fn build_comparable_relationships_schema(
    gds: &GDS,
    object_type_representation: &ObjectTypeWithRelationships,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<BTreeMap<ast::Name, gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>>, Error> {
    let mut input_fields = BTreeMap::new();

    for relationship in object_type_representation.relationship_fields.values() {
        if let metadata_resolve::RelationshipTarget::Model(
            metadata_resolve::ModelRelationshipTarget {
                model_name,
                relationship_type,
                target_typename: _,
                mappings,
            },
        ) = &relationship.target
        {
            // Check whether the relationship is allowed to be compared
            if !include_relationship_field(
                relationship.target_capabilities.as_ref(),
                &gds.metadata.graphql_config,
            ) {
                continue;
            }

            // lookup target model for relationship
            let target_model = gds.metadata.models.get(model_name).ok_or_else(|| {
                Error::InternalModelNotFound {
                    model_name: model_name.clone(),
                }
            })?;

            // lookup type underlying target model
            let target_object_type_representation =
                get_object_type_representation(gds, &target_model.model.data_type)?;

            // lookup filter expression graphql for target model
            let target_boolean_expression_graphql_type = match &target_model.filter_expression_type
            {
                None => None,
                Some(ModelExpressionType::BooleanExpressionType(
                    target_boolean_expression_type,
                )) => target_boolean_expression_type
                    .graphql
                    .as_ref()
                    .map(|graphql| graphql.type_name.clone()),
                Some(ModelExpressionType::ObjectBooleanExpressionType(
                    target_model_filter_expression,
                )) => target_model_filter_expression
                    .graphql
                    .as_ref()
                    .map(|graphql| graphql.type_name.clone()),
            };

            // if our target model has a boolean expression type to use, and a source,
            if let (Some(target_boolean_expression_graphql_type), Some(target_source)) = (
                target_boolean_expression_graphql_type,
                &target_model.model.source,
            ) {
                // add an input field for the relationship
                let (name, schema) = build_model_relationship_schema(
                    object_type_representation,
                    target_object_type_representation,
                    &target_boolean_expression_graphql_type,
                    target_model,
                    target_source,
                    relationship,
                    relationship_type,
                    mappings,
                    relationship.deprecated.as_ref(),
                    gds,
                    builder,
                )?;
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
    target_object_type_representation: &ObjectTypeWithRelationships,
    target_filter_expression_graphql_type: &ast::TypeName,
    target_model: &ModelWithArgumentPresets,
    target_source: &Arc<metadata_resolve::ModelSource>,
    relationship: &RelationshipField,
    relationship_type: &RelationshipType,
    relationship_model_mappings: &[RelationshipModelMapping],
    relationship_deprecated: Option<&Deprecated>,
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<InputField, Error> {
    // Build relationship field in filter expression only when
    // the target_model is backed by a source
    let target_model_source =
        metadata_resolve::ModelTargetSource::from_model_source(target_source, relationship)
            .map_err(metadata_resolve::WithContext::from)?;

    let annotation = FilterRelationshipAnnotation {
        source_type: relationship.source.clone(),
        relationship_name: relationship.relationship_name.clone(),
        target_source: target_model_source,
        target_type: target_model.model.data_type.clone(),
        target_model_name: target_model.model.name.clone(),
        relationship_type: relationship_type.clone(),
        mappings: relationship_model_mappings.to_vec(),
        deprecated: relationship_deprecated.cloned(),
    };

    let namespace_annotations = permissions::get_model_relationship_namespace_annotations(
        target_model,
        source_object_type_representation,
        target_object_type_representation,
        relationship_model_mappings,
    )?;

    Ok((
        relationship.field_name.clone(),
        builder.conditional_namespaced(
            gql_schema::InputField::<GDS>::new(
                relationship.field_name.clone(),
                None,
                types::Annotation::Input(types::InputAnnotation::BooleanExpression(
                    types::BooleanExpressionAnnotation::ObjectBooleanExpressionField(
                        types::ObjectBooleanExpressionField::RelationshipField(annotation),
                    ),
                )),
                ast::TypeContainer::named_null(gql_schema::RegisteredTypeName::new(
                    target_filter_expression_graphql_type.0.clone(),
                )),
                None,
                if gds
                    .metadata
                    .graphql_config
                    .propagate_boolean_expression_deprecation_status
                {
                    mk_deprecation_status(relationship_deprecated)
                } else {
                    gql_schema::DeprecationStatus::NotDeprecated
                },
            ),
            namespace_annotations,
        ),
    ))
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
        input_fields.extend(build_logical_operators_schema(
            &boolean_expression_info.field_config.logical_operators,
            type_name,
            builder,
            |ann| {
                types::BooleanExpressionAnnotation::ObjectBooleanExpressionField(
                    types::ObjectBooleanExpressionField::LogicalOperatorField(ann),
                )
            },
        ));

        let object_type_representation =
            get_object_type_representation(gds, &object_boolean_expression_type.object_type)?;

        // add in all fields that are directly comparable
        input_fields.extend(build_comparable_fields_schema(
            gds,
            &object_boolean_expression_type.object_type,
            object_type_representation,
            &object_boolean_expression_type.scalar_fields,
            &boolean_expression_info.scalar_fields,
            &BTreeMap::new(), // no object fields
            &BTreeMap::new(),
            builder,
        )?);

        // add in relationship fields
        input_fields.extend(build_comparable_relationships_schema(
            gds,
            object_type_representation,
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

        if boolean_expression_object_type.include_logical_operators == IncludeLogicalOperators::Yes
        {
            // add `_and`, `_not` etc
            input_fields.extend(build_logical_operators_schema(
                &boolean_expression_info.field_config.logical_operators,
                type_name,
                builder,
                |ann| {
                    types::BooleanExpressionAnnotation::ObjectBooleanExpressionField(
                        types::ObjectBooleanExpressionField::LogicalOperatorField(ann),
                    )
                },
            ));
        }

        let object_type_representation =
            get_object_type_representation(gds, &boolean_expression_object_type.object_type)?;

        // add in all fields that are directly comparable
        input_fields.extend(build_comparable_fields_schema(
            gds,
            &boolean_expression_object_type.object_type,
            object_type_representation,
            &boolean_expression_object_type.fields.scalar_fields,
            &boolean_expression_info.scalar_fields,
            &boolean_expression_object_type.fields.object_fields,
            &boolean_expression_info.object_fields,
            builder,
        )?);

        // add in relationship fields
        input_fields.extend(build_new_comparable_relationships_schema(
            gds,
            object_type_representation,
            &boolean_expression_object_type.fields.relationship_fields,
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

fn get_scalar_boolean_expression_type(
    builder: &mut gql_schema::Builder<GDS>,
    comparison_expression: &metadata_resolve::ComparisonExpressionInfo,
    scalar_boolean_expression_graphql: &metadata_resolve::ScalarBooleanExpressionGraphqlConfig,
) -> Result<gql_schema::RegisteredTypeName, Error> {
    let graphql_type_name = scalar_boolean_expression_graphql.type_name.clone();
    let mut operators = Vec::new();
    for (op_name, input_type) in &comparison_expression.operators {
        let op_name = mk_name(op_name.as_str())
            .map_err(metadata_resolve::Error::from)
            .map_err(metadata_resolve::WithContext::from)?;

        operators.push((op_name, input_type.clone()));
    }
    Ok(
        builder.register_type(TypeId::InputScalarBooleanExpressionType {
            graphql_type_name,
            operators,
            operator_mapping: comparison_expression.operator_mapping.clone(),
            is_null_operator_name: scalar_boolean_expression_graphql
                .is_null_operator_name
                .clone(),
            logical_operators: comparison_expression.logical_operators.clone(),
        }),
    )
}

fn include_relationship_field(
    target_capabilities: Option<&RelationshipCapabilities>,
    global_graphql_config: &GlobalGraphqlConfig,
) -> bool {
    // Include relationship if bypassing  relation_comparison NDC capability is set to true
    global_graphql_config.bypass_relation_comparisons_ndc_capability
        // Else, check for NDC capability
        || target_capabilities.is_some_and(|capabilities| capabilities.supports_relationships.as_ref().is_some_and(|r| r.supports_relation_comparisons))
}

pub fn build_scalar_boolean_expression_input(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    operators: &Vec<(ast::Name, QualifiedTypeReference)>,
    operator_mapping: &BTreeMap<Qualified<DataConnectorName>, OperatorMapping>,
    maybe_is_null_operator_name: Option<&ast::Name>,
    logical_operator: &metadata_resolve::LogicalOperators,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let mut input_fields: BTreeMap<
        ast::Name,
        gql_schema::Namespaced<GDS, gql_schema::InputField<GDS>>,
    > = BTreeMap::new();

    if let Some(is_null_operator_name) = maybe_is_null_operator_name {
        // Add is_null field
        let is_null_input_type = ast::TypeContainer {
            base: ast::BaseTypeContainer::Named(gql_schema::RegisteredTypeName::boolean()),
            nullable: true,
        };

        input_fields.insert(
            is_null_operator_name.clone(),
            builder.allow_all_namespaced(gql_schema::InputField::new(
                is_null_operator_name.clone(),
                None,
                types::Annotation::Input(types::InputAnnotation::BooleanExpression(
                    types::BooleanExpressionAnnotation::ScalarBooleanExpressionField(
                        types::ScalarBooleanExpressionField::IsNullOperation,
                    ),
                )),
                is_null_input_type,
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            )),
        );
    }

    match logical_operator {
        metadata_resolve::LogicalOperators::Include {
            graphql: Some(graphql_config),
        } => {
            input_fields.extend(build_logical_operators_schema(
                graphql_config,
                type_name,
                builder,
                |ann| {
                    types::BooleanExpressionAnnotation::ScalarBooleanExpressionField(
                        types::ScalarBooleanExpressionField::LogicalOperatorField(ann),
                    )
                },
            ));
        }
        metadata_resolve::LogicalOperators::Include { graphql: None }
        | metadata_resolve::LogicalOperators::Exclude => {}
    }

    for (op_name, input_type) in operators {
        // comparison_operator: input_type
        let input_type = input_type::get_input_type(gds, builder, input_type)?;
        // Presence of all scalar fields in the comparison expression is not compulsory. Users can filter rows based on
        // scalar fields of their choice. Hence, the input type of each scalar field is nullable.
        let nullable_input_type = ast::TypeContainer {
            base: input_type.base,
            nullable: true,
        };

        // this feels a bit loose, we're depending on the fact the ast::Name and
        // OperatorName should be the same
        let operator_name = open_dds::types::OperatorName::new(op_name.as_str().into());

        // for each set of mappings, only return the mapping we actually need
        // default to existing mapping where one is missing
        let this_operator_mapping = operator_mapping
            .iter()
            .map(|(data_connector_name, mappings)| {
                (
                    data_connector_name.clone(),
                    mappings.get(&operator_name).clone(),
                )
            })
            .collect();

        input_fields.insert(
            op_name.clone(),
            builder.allow_all_namespaced(gql_schema::InputField::new(
                op_name.clone(),
                None,
                types::Annotation::Input(types::InputAnnotation::BooleanExpression(
                    types::BooleanExpressionAnnotation::ScalarBooleanExpressionField(
                        types::ScalarBooleanExpressionField::ComparisonOperation {
                            operator_mapping: this_operator_mapping,
                        },
                    ),
                )),
                nullable_input_type,
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            )),
        );
    }

    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(type_name.clone(), None, input_fields, Vec::new()),
    ))
}
