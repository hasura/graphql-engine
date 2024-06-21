use lang_graphql::ast::common as ast;
use lang_graphql::schema::{self as gql_schema, InputField, Namespaced};
use open_dds::{
    data_connector::{DataConnectorName, DataConnectorOperatorName},
    types::{CustomTypeName, OperatorName},
};
use std::collections::BTreeMap;

use super::types::input_type;

use metadata_resolve::{Qualified, QualifiedTypeReference};

use crate::types;
use crate::GDS;

use crate::Error;

pub fn get_where_expression_input_field(
    builder: &mut gql_schema::Builder<GDS>,
    gds_type_name: Qualified<CustomTypeName>,
    boolean_expression_graphql_config: &metadata_resolve::BooleanExpressionGraphqlConfig,
) -> gql_schema::InputField<GDS> {
    gql_schema::InputField::new(
        boolean_expression_graphql_config
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
                graphql_type_name: boolean_expression_graphql_config.type_name.clone(),
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
    operator_mapping: &BTreeMap<
        Qualified<DataConnectorName>,
        BTreeMap<OperatorName, DataConnectorOperatorName>,
    >,
    maybe_is_null_operator_name: &Option<ast::Name>,
) -> Result<gql_schema::TypeInfo<GDS>, Error> {
    let mut input_fields: BTreeMap<ast::Name, Namespaced<GDS, InputField<GDS>>> = BTreeMap::new();

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
                types::Annotation::Input(types::InputAnnotation::Model(
                    types::ModelInputAnnotation::IsNullOperation,
                )),
                is_null_input_type,
                None,
                gql_schema::DeprecationStatus::NotDeprecated,
            )),
        );
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
        let operator_name = OperatorName(op_name.to_string());

        // for each set of mappings, only return the mapping we actually need
        // default to existing mapping where one is missing
        let this_operator_mapping = operator_mapping
            .iter()
            .map(
                |(data_connector_name, mappings)| match mappings.get(&operator_name) {
                    Some(data_connector_operator_name) => (
                        data_connector_name.clone(),
                        data_connector_operator_name.clone(),
                    ),
                    None => (
                        data_connector_name.clone(),
                        DataConnectorOperatorName(operator_name.0.clone()),
                    ),
                },
            )
            .collect();

        input_fields.insert(
            op_name.clone(),
            builder.allow_all_namespaced(gql_schema::InputField::new(
                op_name.clone(),
                None,
                types::Annotation::Input(types::InputAnnotation::Model(
                    types::ModelInputAnnotation::ComparisonOperation {
                        operator_mapping: this_operator_mapping,
                    },
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
