use lang_graphql::ast::common as ast;
use lang_graphql::schema::{self as gql_schema, InputField, Namespaced};
use open_dds::types::CustomTypeName;
use std::collections::BTreeMap;

use super::types::input_type;

use metadata_resolve::{Qualified, QualifiedTypeReference};

use crate::types;
use crate::GDS;

use crate::Error;

pub fn get_where_expression_input_field(
    builder: &mut gql_schema::Builder<GDS>,
    gds_type_name: Qualified<CustomTypeName>,
    boolean_expression_info: &metadata_resolve::BooleanExpressionInfo,
) -> gql_schema::InputField<GDS> {
    gql_schema::InputField::new(
        boolean_expression_info
            .graphql_config
            .where_field_name
            .clone(),
        None,
        types::Annotation::Input(types::InputAnnotation::BooleanExpression(
            types::BooleanExpressionAnnotation::BooleanExpression,
        )),
        ast::TypeContainer::named_null(builder.register_type(
            types::TypeId::InputObjectBooleanExpressionType {
                gds_type_name,
                graphql_type_name: boolean_expression_info.type_name.clone(),
            },
        )),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
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
