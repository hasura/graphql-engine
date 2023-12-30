use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::models::ModelName;
use std::collections::HashMap;

use super::types::{input_type, output_type, InputAnnotation, ModelInputAnnotation, TypeId};
use crate::metadata::resolved;
use crate::metadata::resolved::model::ComparisonExpressionInfo;
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
        lang_graphql::mk_name!("where"),
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
    let mut input_fields = HashMap::new();

    // `_and`, `_or` or `_not` fields are available for all roles
    input_fields.insert(
        lang_graphql::mk_name!("_not"),
        builder.allow_all_namespaced(
            gql_schema::InputField::<GDS>::new(
                lang_graphql::mk_name!("_not"),
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

    input_fields.insert(
        lang_graphql::mk_name!("_and"),
        builder.allow_all_namespaced(
            gql_schema::InputField::<GDS>::new(
                lang_graphql::mk_name!("_and"),
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

    input_fields.insert(
        lang_graphql::mk_name!("_or"),
        builder.allow_all_namespaced(
            gql_schema::InputField::<GDS>::new(
                lang_graphql::mk_name!("_or"),
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

    let object_type_representation =
        output_type::get_object_type_representation(gds, &model.data_type)?;

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
    }

    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(type_name.clone(), None, input_fields),
    ))
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
        }),
    )
}

pub fn build_scalar_comparison_input(
    gds: &GDS,
    builder: &mut gql_schema::Builder<GDS>,
    type_name: &ast::TypeName,
    operators: &Vec<(ast::Name, QualifiedTypeReference)>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let mut fields = Vec::new();

    for (op_name, input_type) in operators {
        // comparison_operator: input_type
        let input_type = input_type::get_input_type(gds, builder, input_type)?;
        // Presence of all scalar fields in the comparison expression is not compulsory. Users can filter rows based on
        // scalar fields of their choice. Hence, the input type of each scalar field is nullable.
        let nullable_input_type = ast::TypeContainer {
            base: input_type.base,
            nullable: true,
        };
        fields.push((op_name, nullable_input_type))
    }
    let input_fields = fields
        .into_iter()
        .map(|(field_name, field_type)| {
            (
                field_name.clone(),
                builder.allow_all_namespaced(
                    gql_schema::InputField::new(
                        field_name.clone(),
                        None,
                        types::Annotation::Input(types::InputAnnotation::Model(
                            types::ModelInputAnnotation::ModelFilterScalarExpression,
                        )),
                        field_type,
                        None,
                        gql_schema::DeprecationStatus::NotDeprecated,
                    ),
                    None,
                ),
            )
        })
        .collect();
    Ok(gql_schema::TypeInfo::InputObject(
        gql_schema::InputObject::new(type_name.clone(), None, input_fields),
    ))
}
