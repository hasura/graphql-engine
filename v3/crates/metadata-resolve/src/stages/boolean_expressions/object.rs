pub use super::{
    BooleanExpressionComparableRelationship, BooleanExpressionGraphqlConfig,
    BooleanExpressionGraphqlFieldConfig, ComparisonExpressionInfo, ObjectComparisonExpressionInfo,
    ResolvedObjectBooleanExpressionType, ResolvedScalarBooleanExpressionType,
};
use crate::helpers::types::mk_name;
use crate::stages::{graphql_config, object_types, type_permissions};
use crate::types::error::{BooleanExpressionError, Error, GraphqlConfigError};
use crate::types::subgraph::mk_qualified_type_reference;
use crate::Qualified;
use lang_graphql::ast::common::{self as ast};
use open_dds::{
    boolean_expression::{
        BooleanExpressionComparableField, BooleanExpressionObjectOperand, BooleanExpressionOperand,
        BooleanExpressionTypeGraphQlConfiguration, DataConnectorOperatorMapping,
    },
    data_connector::{DataConnectorName, DataConnectorOperatorName},
    types::{CustomTypeName, FieldName, OperatorName},
};
use std::collections::BTreeMap;

/// Resolves a given object boolean expression type
pub(crate) fn resolve_object_boolean_expression_type(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    object_boolean_expression_operand: &BooleanExpressionObjectOperand,
    subgraph: &str,
    graphql: &Option<BooleanExpressionTypeGraphQlConfiguration>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        (
            &String,
            &open_dds::boolean_expression::BooleanExpressionTypeV1,
        ),
    >,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ResolvedObjectBooleanExpressionType, Error> {
    let qualified_object_type_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression_operand.r#type.clone(),
    );

    // get the underlying object type
    let object_type_representation =
        object_types
            .get(&qualified_object_type_name)
            .ok_or_else(|| {
                Error::from(
                    BooleanExpressionError::UnknownTypeInObjectBooleanExpressionType {
                        type_name: qualified_object_type_name.clone(),
                    },
                )
            })?;

    // resolve any comparable fields
    let comparable_fields = resolve_comparable_fields(
        &object_boolean_expression_operand.comparable_fields,
        &object_type_representation.object_type,
        boolean_expression_type_name,
        subgraph,
        raw_boolean_expression_types,
    )?;

    // resolve any comparable relationships
    let comparable_relationships = resolve_comparable_relationships(
        boolean_expression_type_name,
        &object_boolean_expression_operand.comparable_relationships,
        subgraph,
        raw_boolean_expression_types,
    )?;

    // resolve graphql schema information
    let resolved_graphql = graphql
        .as_ref()
        .map(|object_boolean_graphql_config| {
            resolve_object_boolean_graphql(
                boolean_expression_type_name,
                object_boolean_graphql_config,
                &comparable_fields,
                &comparable_relationships,
                scalar_boolean_expression_types,
                raw_boolean_expression_types,
                subgraph,
                graphql_config,
            )
        })
        .transpose()?;

    let resolved_boolean_expression = ResolvedObjectBooleanExpressionType {
        name: boolean_expression_type_name.clone(),
        object_type: qualified_object_type_name.clone(),
        graphql: resolved_graphql,
    };
    Ok(resolved_boolean_expression)
}

// validate graphql config
// we use the raw boolean expression types for lookup
fn resolve_object_boolean_graphql(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    boolean_expression_graphql_config: &BooleanExpressionTypeGraphQlConfiguration,
    comparable_fields: &BTreeMap<FieldName, Qualified<CustomTypeName>>,
    comparable_relationships: &BTreeMap<FieldName, BooleanExpressionComparableRelationship>,
    scalar_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        (
            &String,
            &open_dds::boolean_expression::BooleanExpressionTypeV1,
        ),
    >,
    subgraph: &str,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<BooleanExpressionGraphqlConfig, Error> {
    let boolean_expression_graphql_name =
        mk_name(boolean_expression_graphql_config.type_name.0.as_ref()).map(ast::TypeName)?;

    let mut scalar_fields = BTreeMap::new();

    let mut object_fields = BTreeMap::new();

    let filter_graphql_config = graphql_config
        .query
        .filter_input_config
        .as_ref()
        .ok_or_else(|| Error::GraphqlConfigError {
            graphql_config_error: GraphqlConfigError::MissingFilterInputFieldInGraphqlConfig,
        })?;

    for (comparable_field_name, comparable_field_type_name) in comparable_fields {
        if let Some(scalar_boolean_expression_type) =
            scalar_boolean_expression_types.get(comparable_field_type_name)
        {
            // Generate comparison expression for fields mapped to simple scalar type
            if let Some(graphql_name) = &scalar_boolean_expression_type.graphql_name {
                let mut operators = BTreeMap::new();
                for (op_name, op_definition) in &scalar_boolean_expression_type.comparison_operators
                {
                    operators.insert(
                        op_name.clone(),
                        mk_qualified_type_reference(op_definition, subgraph),
                    );
                }
                let graphql_type_name = mk_name(&graphql_name.0).map(ast::TypeName)?;

                let operator_mapping = resolve_operator_mapping_for_scalar_type(
                    &scalar_boolean_expression_type.data_connector_operator_mappings,
                );

                // Register scalar comparison field only if it contains non-zero operators.
                if !operators.is_empty() {
                    scalar_fields.insert(
                        comparable_field_name.clone(),
                        ComparisonExpressionInfo {
                            object_type_name: Some(comparable_field_type_name.clone()),
                            type_name: graphql_type_name.clone(),
                            operators: operators.clone(),
                            operator_mapping,
                            is_null_operator_name: filter_graphql_config
                                .operator_names
                                .is_null
                                .clone(),
                        },
                    );
                };
            }
        } else {
            // if this field isn't a scalar, let's see if it's an object instead
            let (field_subgraph, raw_boolean_expression_type) = lookup_raw_boolean_expression(
                boolean_expression_type_name,
                comparable_field_type_name,
                raw_boolean_expression_types,
            )?;

            if let (Some(graphql_name), BooleanExpressionOperand::Object(object_operand)) = (
                raw_boolean_expression_type
                    .graphql
                    .as_ref()
                    .map(|gql| gql.type_name.clone()),
                &raw_boolean_expression_type.operand,
            ) {
                let graphql_type_name = mk_name(&graphql_name.0).map(ast::TypeName)?;

                object_fields.insert(
                    comparable_field_name.clone(),
                    ObjectComparisonExpressionInfo {
                        object_type_name: comparable_field_type_name.clone(),
                        underlying_object_type_name: Qualified::new(
                            (*field_subgraph).to_string(),
                            object_operand.r#type.clone(),
                        ),
                        graphql_type_name: graphql_type_name.clone(),
                    },
                );
            }
        }
    }

    Ok(BooleanExpressionGraphqlConfig {
        type_name: boolean_expression_graphql_name,
        scalar_fields,
        object_fields,
        relationship_fields: comparable_relationships.clone(),
        graphql_config: (BooleanExpressionGraphqlFieldConfig {
            where_field_name: filter_graphql_config.where_field_name.clone(),
            and_operator_name: filter_graphql_config.operator_names.and.clone(),
            or_operator_name: filter_graphql_config.operator_names.or.clone(),
            not_operator_name: filter_graphql_config.operator_names.not.clone(),
        }),
    })
}

fn resolve_operator_mapping_for_scalar_type(
    data_connector_operator_mappings: &BTreeMap<
        Qualified<DataConnectorName>,
        DataConnectorOperatorMapping,
    >,
) -> BTreeMap<Qualified<DataConnectorName>, BTreeMap<OperatorName, DataConnectorOperatorName>> {
    let mut operator_mapping = BTreeMap::new();

    for (data_connector_name, data_connector_operator_mapping) in data_connector_operator_mappings {
        operator_mapping.insert(
            data_connector_name.clone(),
            data_connector_operator_mapping.operator_mapping.clone(),
        );
    }

    operator_mapping
}

// resolve comparable relationships. These should only be local relationships (ie, in the same data
// connector), however we are unable to ascertain this early in the pipeline. More indepth checks
// should occur when resolving models, when the model source is known.
fn resolve_comparable_relationships(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    comparable_relationships: &Vec<
        open_dds::boolean_expression::BooleanExpressionComparableRelationship,
    >,
    subgraph: &str,
    raw_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        (
            &String,
            &open_dds::boolean_expression::BooleanExpressionTypeV1,
        ),
    >,
) -> Result<BTreeMap<FieldName, BooleanExpressionComparableRelationship>, Error> {
    let mut resolved_comparable_relationships = BTreeMap::new();

    for comparable_relationship in comparable_relationships {
        // if the relationship has provided an optional boolean_expression_type...
        if let Some(target_boolean_expression_type_name) =
            &comparable_relationship.boolean_expression_type
        {
            // ...check it exists
            let _raw_boolean_expression_type = lookup_raw_boolean_expression(
                boolean_expression_type_name,
                &Qualified::new(
                    subgraph.to_string(),
                    target_boolean_expression_type_name.clone(),
                ),
                raw_boolean_expression_types,
            )?;
        }
        let resolved_comparable_relationship = BooleanExpressionComparableRelationship {
            relationship_name: comparable_relationship.relationship_name.clone(),
            boolean_expression_type: comparable_relationship
                .boolean_expression_type
                .as_ref()
                .map(|bool_exp| Qualified::new(subgraph.to_string(), bool_exp.clone())),
        };
        resolved_comparable_relationships.insert(
            FieldName(comparable_relationship.relationship_name.0.clone()),
            resolved_comparable_relationship,
        );
    }

    Ok(resolved_comparable_relationships)
}
// comparable_fields don't do much, all we can do is ensure that the other BooleanExpressionTypes
// they refer to exist
fn resolve_comparable_fields(
    comparable_fields: &Vec<BooleanExpressionComparableField>,
    object_type_representation: &object_types::ObjectTypeRepresentation,
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    subgraph: &str,
    raw_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        (
            &String,
            &open_dds::boolean_expression::BooleanExpressionTypeV1,
        ),
    >,
) -> Result<BTreeMap<FieldName, Qualified<CustomTypeName>>, Error> {
    let mut resolved_comparable_fields = BTreeMap::new();

    // validate comparable fields all exist in underlying object
    for comparable_field in comparable_fields {
        // fields with field arguments are not allowed in boolean expressions
        if let Some(field_definition) = object_type_representation
            .fields
            .get(&comparable_field.field_name)
        {
            if !field_definition.field_arguments.is_empty() {
                continue;
            }
        }
        if !object_type_representation
            .fields
            .contains_key(&comparable_field.field_name)
        {
            return Err(
                BooleanExpressionError::UnknownFieldInObjectBooleanExpressionType {
                    field_name: comparable_field.field_name.clone(),
                    object_boolean_expression_type: boolean_expression_type_name.clone(),
                }
                .into(),
            );
        }

        let field_boolean_expression_type = Qualified::new(
            subgraph.to_string(),
            comparable_field.boolean_expression_type.clone(),
        );

        // lookup the boolean expression type to check it exists
        let _raw_boolean_expression_type = lookup_raw_boolean_expression(
            boolean_expression_type_name,
            &field_boolean_expression_type,
            raw_boolean_expression_types,
        )?;

        resolved_comparable_fields.insert(
            comparable_field.field_name.clone(),
            field_boolean_expression_type,
        );
    }

    Ok(resolved_comparable_fields)
}

fn lookup_raw_boolean_expression<'a>(
    parent_boolean_expression_name: &Qualified<CustomTypeName>,
    boolean_expression_name: &Qualified<CustomTypeName>,
    raw_boolean_expression_types: &'a BTreeMap<
        Qualified<CustomTypeName>,
        (
            &String,
            &open_dds::boolean_expression::BooleanExpressionTypeV1,
        ),
    >,
) -> Result<
    &'a (
        &'a String,
        &'a open_dds::boolean_expression::BooleanExpressionTypeV1,
    ),
    Error,
> {
    raw_boolean_expression_types
        .get(boolean_expression_name)
        .ok_or_else(|| {
            BooleanExpressionError::BooleanExpressionCouldNotBeFound {
                parent_boolean_expression: parent_boolean_expression_name.clone(),
                child_boolean_expression: boolean_expression_name.clone(),
            }
            .into()
        })
}
