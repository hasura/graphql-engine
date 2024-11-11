use super::error::BooleanExpressionError;
use super::helpers;
pub use super::{
    BooleanExpressionGraphqlConfig, BooleanExpressionGraphqlFieldConfig, ComparableFieldKind,
    ObjectBooleanExpressionGraphqlConfig, ScalarBooleanExpressionGraphqlConfig,
};
use crate::helpers::types::{mk_name, store_new_graphql_type};
use crate::stages::{graphql_config, scalar_boolean_expressions};
use crate::Qualified;
use lang_graphql::ast::common::{self as ast};
use open_dds::{
    boolean_expression::BooleanExpressionTypeGraphQlConfiguration,
    types::{CustomTypeName, FieldName},
};
use std::collections::{BTreeMap, BTreeSet};

// validate graphql config
// we use the raw boolean expression types for lookup
pub(crate) fn resolve_object_boolean_graphql(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    boolean_expression_graphql_config: &BooleanExpressionTypeGraphQlConfiguration,
    comparable_fields: &BTreeMap<FieldName, (ComparableFieldKind, Qualified<CustomTypeName>)>,
    scalar_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &super::object::RawBooleanExpressionTypes,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut BTreeSet<ast::TypeName>,
) -> Result<BooleanExpressionGraphqlConfig, BooleanExpressionError> {
    let boolean_expression_graphql_name =
        mk_name(boolean_expression_graphql_config.type_name.as_ref()).map(ast::TypeName)?;

    store_new_graphql_type(graphql_types, Some(&boolean_expression_graphql_name))?;

    let mut scalar_fields = BTreeMap::new();

    let mut object_fields = BTreeMap::new();

    let filter_graphql_config = graphql_config
        .query
        .filter_input_config
        .as_ref()
        .ok_or_else(|| {
            graphql_config::GraphqlConfigError::MissingFilterInputFieldInGraphqlConfig
        })?;

    for (comparable_field_name, (comparable_field_kind, comparable_field_type_name)) in
        comparable_fields
    {
        match comparable_field_kind {
            ComparableFieldKind::Scalar => {
                if let Some(scalar_boolean_expression_type) =
                    scalar_boolean_expression_types.get(comparable_field_type_name)
                {
                    // Generate comparison expression for fields mapped to simple scalar type
                    if let Some(graphql_name) = &scalar_boolean_expression_type.graphql_name {
                        let graphql_type_name =
                            mk_name(graphql_name.as_str()).map(ast::TypeName)?;

                        // Register scalar comparison field only if it contains non-zero operators.
                        if !scalar_boolean_expression_type
                            .comparison_operators
                            .is_empty()
                            || scalar_boolean_expression_type.include_is_null
                                == scalar_boolean_expressions::IncludeIsNull::Yes
                        {
                            scalar_fields.insert(
                                comparable_field_name.clone(),
                                ScalarBooleanExpressionGraphqlConfig {
                                    type_name: graphql_type_name.clone(),
                                    is_null_operator_name: match scalar_boolean_expression_type
                                        .include_is_null
                                    {
                                        scalar_boolean_expressions::IncludeIsNull::Yes => Some(
                                            filter_graphql_config.operator_names.is_null.clone(),
                                        ),
                                        scalar_boolean_expressions::IncludeIsNull::No => None,
                                    },
                                },
                            );
                        };
                    }
                }
            }
            ComparableFieldKind::Object | ComparableFieldKind::Array => {
                // if this field isn't a scalar, let's see if it's an object instead
                let (_field_subgraph, raw_boolean_expression_type) =
                    helpers::lookup_raw_boolean_expression(
                        boolean_expression_type_name,
                        comparable_field_type_name,
                        raw_boolean_expression_types,
                    )?;

                if let Some(graphql_name) = raw_boolean_expression_type
                    .graphql
                    .as_ref()
                    .map(|gql| gql.type_name.clone())
                {
                    let graphql_type_name = mk_name(graphql_name.as_str()).map(ast::TypeName)?;

                    object_fields.insert(
                        comparable_field_name.clone(),
                        ObjectBooleanExpressionGraphqlConfig {
                            graphql_type_name: graphql_type_name.clone(),
                        },
                    );
                }
            }
        }
    }

    Ok(BooleanExpressionGraphqlConfig {
        type_name: boolean_expression_graphql_name,
        scalar_fields,
        object_fields,
        field_config: (BooleanExpressionGraphqlFieldConfig {
            where_field_name: filter_graphql_config.where_field_name.clone(),
            and_operator_name: filter_graphql_config.operator_names.and.clone(),
            or_operator_name: filter_graphql_config.operator_names.or.clone(),
            not_operator_name: filter_graphql_config.operator_names.not.clone(),
        }),
    })
}
