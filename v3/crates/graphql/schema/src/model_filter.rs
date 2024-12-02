use lang_graphql::ast::common as ast;
use lang_graphql::schema::{self as gql_schema};
use open_dds::types::CustomTypeName;

use metadata_resolve::Qualified;

use crate::types;
use crate::GDS;

pub fn get_where_expression_input_field(
    builder: &mut gql_schema::Builder<GDS>,
    gds_type_name: Qualified<CustomTypeName>,
    boolean_expression_graphql_field_config: &metadata_resolve::BooleanExpressionGraphqlFieldConfig,
    boolean_expression_type_name: &ast::TypeName,
) -> gql_schema::InputField<GDS> {
    gql_schema::InputField::new(
        boolean_expression_graphql_field_config
            .where_field_name
            .clone(),
        None,
        types::Annotation::Input(types::InputAnnotation::BooleanExpression(
            types::BooleanExpressionAnnotation::BooleanExpressionRootField,
        )),
        ast::TypeContainer::named_null(builder.register_type(
            types::TypeId::InputObjectBooleanExpressionType {
                gds_type_name,
                graphql_type_name: boolean_expression_type_name.clone(),
            },
        )),
        None,
        gql_schema::DeprecationStatus::NotDeprecated,
    )
}
