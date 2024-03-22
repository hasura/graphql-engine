use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::schema::{self as gql_schema, InputField, Namespaced};
use open_dds::models::ModelName;
use std::collections::{BTreeMap, HashMap};

use super::types::output_type::get_object_type_representation;
use super::types::output_type::relationship::{FilterRelationshipAnnotation, ModelTargetSource};
use super::types::{input_type, InputAnnotation, ModelInputAnnotation, TypeId};
use crate::metadata::resolved;
use crate::metadata::resolved::model::ComparisonExpressionInfo;
use crate::metadata::resolved::relationship::{
    relationship_execution_category, RelationshipExecutionCategory, RelationshipTarget,
};
use crate::metadata::resolved::subgraph::{Qualified, QualifiedTypeReference};
use crate::metadata::resolved::types::mk_name;
use crate::schema::permissions;
use crate::schema::types;
use crate::schema::GDS;

type Error = crate::schema::Error;

pub fn get_where_expression_input_field(
    builder: &mut gql_schema::Builder<GDS>,
    model_name: Qualified<ModelName>,
    boolean_expression_info: &resolved::model::ModelFilterExpression,
) -> gql_schema::InputField<GDS> {
    gql_schema::InputField::new(
        boolean_expression_info
            .filter_graphql_config
            .where_field_name
            .clone(),
        None,
        types::Annotation::Input(types::InputAnnotation::Model(
            types::ModelInputAnnotation::ModelFilterExpression,
        )),
        ast::TypeContainer::named_null(builder.register_type(
            types::TypeId::ModelBooleanExpression {
                model_name,
                graphql_type_name: boolean_expression_info.where_type_name.clone(),
            },
        )),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    )
}

pub fn build_model_filter_expression_input_schema(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    model_name: &Qualified<ModelName>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let model = gds.metadata.models.get(model_name).ok_or_else(|| {
        crate::schema::Error::InternalModelNotFound {
            model_name: model_name.clone(),
        }
    })?;
    if let Some(boolean_expression_info) = &model.graphql_api.filter_expression {
        let mut input_fields = BTreeMap::new();

        // `_and`, `_or` or `_not` fields are available for all roles
        let not_field_name = &boolean_expression_info
            .filter_graphql_config
            .not_operator_name;

        input_fields.insert(
            not_field_name.clone(),
            builder.allow_all_namespaced(
                gql_schema::InputField::<GDS>::new(
                    not_field_name.clone(),
                    None,
                    types::Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::ModelFilterArgument {
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

        let and_field_name = &boolean_expression_info
            .filter_graphql_config
            .and_operator_name;

        input_fields.insert(
            and_field_name.clone(),
            builder.allow_all_namespaced(
                gql_schema::InputField::<GDS>::new(
                    and_field_name.clone(),
                    None,
                    types::Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::ModelFilterArgument {
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

        let or_field_name = &boolean_expression_info
            .filter_graphql_config
            .or_operator_name;
        input_fields.insert(
            or_field_name.clone(),
            builder.allow_all_namespaced(
                gql_schema::InputField::<GDS>::new(
                    or_field_name.clone(),
                    None,
                    types::Annotation::Input(InputAnnotation::Model(
                        ModelInputAnnotation::ModelFilterArgument {
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

        let object_type_representation = get_object_type_representation(gds, &model.data_type)?;

        // column fields
        if let Some(model_filter_expression) = model.graphql_api.filter_expression.as_ref() {
            for (field_name, comparison_expression) in &model_filter_expression.scalar_fields {
                let field_graphql_name = mk_name(field_name.clone().0.as_str())?;
                let registered_type_name =
                    get_scalar_comparison_input_type(builder, comparison_expression)?;
                let field_type = ast::TypeContainer::named_null(registered_type_name);
                let annotation = types::Annotation::Input(InputAnnotation::Model(
                    ModelInputAnnotation::ModelFilterArgument {
                        field: types::ModelFilterArgument::Field {
                            ndc_column: comparison_expression.ndc_column.clone(),
                        },
                    },
                ));
                let field_permissions: HashMap<Role, Option<types::NamespaceAnnotation>> =
                    permissions::get_allowed_roles_for_field(
                        object_type_representation,
                        field_name,
                    )
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
        }

        // relationship fields
        // TODO(naveen): Add support for command relationships
        for (rel_name, relationship) in object_type_representation.relationships.iter() {
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

                // Build relationship field in filter expression only when both
                // the target_model and source model are backed by a source
                if let (Some(target_source), Some(model_source)) =
                    (&target_model.source, &model.source)
                {
                    let target_model_source =
                        ModelTargetSource::from_model_source(target_source, relationship)?;

                    // filter expression with relationships is currently only supported for local relationships
                    if let RelationshipExecutionCategory::Local = relationship_execution_category(
                        &model_source.data_connector,
                        &target_source.data_connector,
                        &target_model_source.capabilities,
                    ) {
                        if target_source.data_connector.name == model_source.data_connector.name {
                            // If the relationship target model does not have filterExpressionType do not include
                            // it in the source model filter expression input type.
                            if let Some(target_model_filter_expression) =
                                target_model.graphql_api.filter_expression.as_ref()
                            {
                                let target_model_filter_expression_type_name =
                                    &target_model_filter_expression.where_type_name;

                                let annotation = FilterRelationshipAnnotation {
                                    source_type: relationship.source.clone(),
                                    relationship_name: relationship.name.clone(),
                                    target_source: target_model_source.clone(),
                                    target_type: target_typename.clone(),
                                    target_model_name: target_model.name.clone(),
                                    relationship_type: relationship_type.clone(),
                                    mappings: mappings.clone(),
                                    source_data_connector: model_source.data_connector.clone(),
                                    source_type_mappings: model_source.type_mappings.clone(),
                                };

                                input_fields.insert(
                                    rel_name.clone(),
                                    builder.conditional_namespaced(
                                        gql_schema::InputField::<GDS>::new(
                                            rel_name.clone(),
                                            None,
                                            types::Annotation::Input(InputAnnotation::Model(
                                                ModelInputAnnotation::ModelFilterArgument {
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
                                        permissions::get_model_relationship_namespace_annotations(
                                            target_model,
                                            object_type_representation,
                                            target_object_type_representation,
                                            mappings,
                                        ),
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
        Err(
            crate::schema::Error::InternalModelFilterExpressionNotFound {
                model_name: model_name.clone(),
            },
        )
    }
}

fn get_scalar_comparison_input_type(
    builder: &mut gql_schema::Builder<GDS>,
    comparison_expression: &ComparisonExpressionInfo,
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

pub fn build_scalar_comparison_input(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    operators: &Vec<(ast::Name, QualifiedTypeReference)>,
    is_null_operator_name: &ast::Name,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let mut input_fields: BTreeMap<ast::Name, Namespaced<GDS, InputField<GDS>>> = BTreeMap::new();

    // Add is_null field
    let is_null_input_type = ast::TypeContainer {
        base: ast::BaseTypeContainer::Named(gql_schema::RegisteredTypeName::boolean()),
        nullable: true,
    };

    input_fields.insert(
        is_null_operator_name.clone(),
        builder.allow_all_namespaced(
            gql_schema::InputField::new(
                is_null_operator_name.clone(),
                None,
                types::Annotation::Input(types::InputAnnotation::Model(
                    types::ModelInputAnnotation::IsNullOperation,
                )),
                is_null_input_type,
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            ),
            None,
        ),
    );

    for (op_name, input_type) in operators {
        // comparison_operator: input_type
        let input_type = input_type::get_input_type(gds, builder, input_type)?;
        // Presence of all scalar fields in the comparison expression is not compulsory. Users can filter rows based on
        // scalar fields of their choice. Hence, the input type of each scalar field is nullable.
        let nullable_input_type = ast::TypeContainer {
            base: input_type.base,
            nullable: true,
        };

        input_fields.insert(
            op_name.clone(),
            builder.allow_all_namespaced(
                gql_schema::InputField::new(
                    op_name.clone(),
                    None,
                    types::Annotation::Input(types::InputAnnotation::Model(
                        types::ModelInputAnnotation::ComparisonOperation {
                            operator: op_name.to_string(),
                        },
                    )),
                    nullable_input_type,
                    None,
                    gql_schema::DeprecationStatus::NotDeprecated,
                ),
                None,
            ),
        );
    }

    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(type_name.clone(), None, input_fields, Vec::new()),
    ))
}
