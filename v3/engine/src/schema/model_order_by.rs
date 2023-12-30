use hasura_authn_core::Role;
use lang_graphql::ast::common as ast;
use lang_graphql::schema as gql_schema;
use open_dds::models::ModelName;
use std::collections::HashMap;

use super::types::{output_type::get_object_type_representation, Annotation, TypeId};
use crate::metadata::resolved;
use crate::metadata::resolved::subgraph::Qualified;
use crate::metadata::resolved::types::mk_name;
use crate::schema::permissions;
use crate::schema::types;
use crate::schema::GDS;

type Error = crate::schema::Error;

// Generates the schema for 'order_by' arguments: Asc/Desc
pub fn build_order_by_enum_type_schema(
    builder: &mut gql_schema::Builder<GDS>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let mut order_by_values = HashMap::new();

    let asc_ast_name = lang_graphql::mk_name!("Asc");
    let desc_ast_name = lang_graphql::mk_name!("Desc");
    order_by_values.insert(
        asc_ast_name.clone(),
        builder.allow_all_namespaced(
            gql_schema::EnumValue {
                value: asc_ast_name,
                description: Some("Sorts the data in ascending order".to_string()),
                deprecation_status: gql_schema::DeprecationStatus::NotDeprecated,
                info: types::Annotation::Input(types::InputAnnotation::Model(
                    types::ModelInputAnnotation::ModelOrderByDirection {
                        direction: types::ModelOrderByDirection::Asc,
                    },
                )),
            },
            None,
        ),
    );

    order_by_values.insert(
        desc_ast_name.clone(),
        builder.allow_all_namespaced(
            gql_schema::EnumValue {
                value: desc_ast_name,
                description: Some("Sorts the data in descending order".to_string()),
                deprecation_status: gql_schema::DeprecationStatus::NotDeprecated,
                info: types::Annotation::Input(types::InputAnnotation::Model(
                    types::ModelInputAnnotation::ModelOrderByDirection {
                        direction: types::ModelOrderByDirection::Desc,
                    },
                )),
            },
            None,
        ),
    );

    Ok(gql_schema::TypeInfo::Enum(gql_schema::Enum {
        name: ast::TypeName(lang_graphql::mk_name!("order_by")),
        description: None,
        values: order_by_values,
    }))
}

pub fn get_order_by_expression_input_field(
    builder: &mut gql_schema::Builder<GDS>,
    model_name: Qualified<ModelName>,
    order_by_expression_info: &resolved::model::ModelOrderByExpression,
) -> gql_schema::InputField<GDS> {
    gql_schema::InputField::new(
        lang_graphql::mk_name!("order_by"),
        None,
        types::Annotation::Input(types::InputAnnotation::Model(
            types::ModelInputAnnotation::ModelOrderByExpression,
        )),
        ast::TypeContainer::named_null(builder.register_type(
            types::TypeId::ModelOrderByExpression {
                model_name,
                graphql_type_name: order_by_expression_info.order_by_type_name.clone(),
            },
        )),
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

    let object_type_representation = get_object_type_representation(gds, &model.data_type)?;

    let mut fields = HashMap::new();
    if let Some(model_order_by_expression) = model.graphql_api.order_by_expression.as_ref() {
        for (field_name, order_by_expression) in &model_order_by_expression.order_by_fields {
            let graphql_field_name = mk_name(field_name.clone().0.as_str())?;
            let input_type =
                ast::TypeContainer::named_null(builder.register_type(TypeId::OrderByEnumType));
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
        Ok(gql_schema::TypeInfo::InputObject(
            gql_schema::InputObject::new(type_name.clone(), None, fields),
        ))
    } else {
        Err(Error::NoOrderByExpression {
            model_name: model_name.clone(),
        })
    }
}
