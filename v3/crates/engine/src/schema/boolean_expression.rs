use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::schema::{self as gql_schema};
use open_dds::types::CustomTypeName;
use std::collections::{BTreeMap, HashMap};

use super::types::output_type::get_object_type_representation;
use super::types::output_type::relationship::{FilterRelationshipAnnotation, ModelTargetSource};
use super::types::{BooleanExpressionAnnotation, InputAnnotation, TypeId};
use crate::metadata::resolved;
use crate::metadata::resolved::relationship::{
    relationship_execution_category, RelationshipExecutionCategory, RelationshipTarget,
};
use crate::metadata::resolved::subgraph::Qualified;
use crate::metadata::resolved::types::mk_name;

use crate::schema::permissions;
use crate::schema::types;
use crate::schema::GDS;

type Error = crate::schema::Error;

pub fn build_boolean_expression_input_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    gds_type_name: &Qualified<CustomTypeName>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let object_boolean_expression_type =
        gds.metadata
            .boolean_expression_types
            .get(gds_type_name)
            .ok_or_else(|| crate::schema::Error::InternalTypeNotFound {
                type_name: gds_type_name.clone(),
            })?;

    if let Some(boolean_expression_info) = &object_boolean_expression_type.graphql {
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

        let object_type_representation =
            get_object_type_representation(gds, &object_boolean_expression_type.object_type)?;

        // column fields
        for (field_name, comparison_expression) in &boolean_expression_info.scalar_fields {
            let field_graphql_name = mk_name(field_name.clone().0.as_str())?;
            let registered_type_name =
                get_scalar_comparison_input_type(builder, comparison_expression)?;
            let field_type = ast::TypeContainer::named_null(registered_type_name);
            let annotation = types::Annotation::Input(InputAnnotation::BooleanExpression(
                BooleanExpressionAnnotation::BooleanExpressionArgument {
                    field: types::ModelFilterArgument::Field {
                        ndc_column: comparison_expression.ndc_column.clone(),
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

        // relationship fields
        // TODO(naveen): Add support for command relationships
        for (rel_name, relationship) in object_type_representation.object_type.relationships.iter()
        {
            if let RelationshipTarget::Model {
                model_name,
                relationship_type,
                target_typename,
                mappings,
            } = &relationship.target
            {
                let target_model = gds.metadata.models.get(model_name).ok_or_else(|| {
                    crate::schema::Error::InternalModelNotFound {
                        model_name: model_name.clone(),
                    }
                })?;

                let target_object_type_representation =
                    get_object_type_representation(gds, &target_model.data_type)?;

                // Build relationship field in filter expression only when
                // the target_model is backed by a source
                if let Some(target_source) = &target_model.source {
                    let target_model_source =
                        ModelTargetSource::from_model_source(target_source, relationship)?;

                    // filter expression with relationships is currently only supported for local relationships
                    if let RelationshipExecutionCategory::Local = relationship_execution_category(
                        &object_boolean_expression_type.data_connector_link,
                        &target_source.data_connector,
                        &target_model_source.capabilities,
                    ) {
                        if target_source.data_connector.name
                            == object_boolean_expression_type.data_connector_name
                        {
                            // If the relationship target model does not have filterExpressionType do not include
                            // it in the source model filter expression input type.
                            if let Some(ref target_model_filter_expression) = &target_model
                                .clone()
                                .filter_expression_type
                                .and_then(|ref object_boolean_expression_type| {
                                    object_boolean_expression_type.clone().graphql
                                })
                            {
                                let target_model_filter_expression_type_name =
                                    &target_model_filter_expression.type_name;

                                let annotation = FilterRelationshipAnnotation {
                                    source_type: relationship.source.clone(),
                                    relationship_name: relationship.name.clone(),
                                    target_source: target_model_source.clone(),
                                    target_type: target_typename.clone(),
                                    target_model_name: target_model.name.clone(),
                                    relationship_type: relationship_type.clone(),
                                    mappings: mappings.clone(),
                                    source_data_connector: object_boolean_expression_type
                                        .data_connector_link
                                        .clone(),
                                    source_type_mappings: object_boolean_expression_type
                                        .type_mappings
                                        .clone(),
                                };

                                let namespace_annotations =
                                    permissions::get_model_relationship_namespace_annotations(
                                        target_model,
                                        object_type_representation,
                                        target_object_type_representation,
                                        mappings,
                                        &gds.metadata.object_types,
                                    )?;

                                input_fields.insert(
                                    rel_name.clone(),
                                    builder.conditional_namespaced(
                                        gql_schema::InputField::<GDS>::new(
                                            rel_name.clone(),
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
                                                    target_model_filter_expression_type_name.0.clone(),
                                                ),
                                            ),
                                            None,
                                            gql_schema::DeprecationStatus::NotDeprecated,
                                        ),
                                        namespace_annotations
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }
        Ok(gql_schema::TypeInfo::InputObject(
            gql_schema::InputObject::new(type_name.clone(), None, input_fields, Vec::new()),
        ))
    } else {
        Err(crate::schema::Error::InternalBooleanExpressionNotFound {
            type_name: gds_type_name.clone(),
        })
    }
}

fn get_scalar_comparison_input_type(
    builder: &mut gql_schema::Builder<GDS>,
    comparison_expression: &resolved::boolean_expression::ComparisonExpressionInfo,
) -> Result<gql_schema::RegisteredTypeName, Error> {
    let graphql_type_name = comparison_expression.type_name.clone();
    let mut operators = Vec::new();
    for (op_name, input_type) in &comparison_expression.operators {
        let op_name = mk_name(op_name.as_str())?;
        operators.push((op_name, input_type.clone()))
    }
    Ok(
        builder.register_type(TypeId::ScalarTypeComparisonExpression {
            scalar_type_name: comparison_expression.scalar_type_name.clone(),
            graphql_type_name,
            operators,
            is_null_operator_name: mk_name(&comparison_expression.is_null_operator_name)?,
        }),
    )
}
