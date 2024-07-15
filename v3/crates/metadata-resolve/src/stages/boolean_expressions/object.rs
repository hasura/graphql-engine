use super::error::BooleanExpressionError;
use super::graphql;
use super::helpers;
pub use super::{BooleanExpressionComparableRelationship, ResolvedObjectBooleanExpressionType};
use crate::stages::{graphql_config, object_types, scalar_boolean_expressions, type_permissions};
use crate::types::subgraph::mk_qualified_type_name;
use crate::Qualified;
use open_dds::{
    boolean_expression::{
        BooleanExpressionComparableField, BooleanExpressionLogicalOperators,
        BooleanExpressionObjectOperand, BooleanExpressionOperand, BooleanExpressionScalarOperand,
        BooleanExpressionTypeGraphQlConfiguration,
    },
    types::{CustomTypeName, FieldName, TypeName},
};
use std::collections::BTreeMap;

pub(crate) type RawBooleanExpressionTypes<'a> = BTreeMap<
    Qualified<CustomTypeName>,
    (
        &'a open_dds::identifier::SubgraphIdentifier,
        &'a open_dds::boolean_expression::BooleanExpressionTypeV1,
    ),
>;

/// Resolves a given object boolean expression type
pub(crate) fn resolve_object_boolean_expression_type(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    object_boolean_expression_operand: &BooleanExpressionObjectOperand,
    logical_operators: &BooleanExpressionLogicalOperators,
    subgraph: &str,
    graphql: &Option<BooleanExpressionTypeGraphQlConfiguration>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &RawBooleanExpressionTypes,
    graphql_config: &graphql_config::GraphqlConfig,
) -> Result<ResolvedObjectBooleanExpressionType, BooleanExpressionError> {
    let qualified_object_type_name = Qualified::new(
        subgraph.to_string(),
        object_boolean_expression_operand.r#type.clone(),
    );

    // get the underlying object type
    let object_type_representation =
        object_types
            .get(&qualified_object_type_name)
            .ok_or_else(
                || BooleanExpressionError::UnknownTypeInObjectBooleanExpressionType {
                    type_name: qualified_object_type_name.clone(),
                },
            )?;

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
            graphql::resolve_object_boolean_graphql(
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
        include_logical_operators: helpers::resolve_logical_operators(logical_operators),
        object_type: qualified_object_type_name.clone(),
        graphql: resolved_graphql,
    };
    Ok(resolved_boolean_expression)
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
    raw_boolean_expression_types: &RawBooleanExpressionTypes,
) -> Result<BTreeMap<FieldName, BooleanExpressionComparableRelationship>, BooleanExpressionError> {
    let mut resolved_comparable_relationships = BTreeMap::new();

    for comparable_relationship in comparable_relationships {
        // if the relationship has provided an optional boolean_expression_type...
        if let Some(target_boolean_expression_type_name) =
            &comparable_relationship.boolean_expression_type
        {
            // ...check it exists
            let _raw_boolean_expression_type = helpers::lookup_raw_boolean_expression(
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
            FieldName::new(comparable_relationship.relationship_name.inner().clone()),
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
    raw_boolean_expression_types: &RawBooleanExpressionTypes,
) -> Result<BTreeMap<FieldName, Qualified<CustomTypeName>>, BooleanExpressionError> {
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

        let field = object_type_representation
            .fields
            .get(&comparable_field.field_name)
            .ok_or_else(
                || BooleanExpressionError::UnknownFieldInObjectBooleanExpressionType {
                    field_name: comparable_field.field_name.clone(),
                    object_boolean_expression_type: boolean_expression_type_name.clone(),
                },
            )?;

        let field_boolean_expression_type_name = Qualified::new(
            subgraph.to_string(),
            comparable_field.boolean_expression_type.clone(),
        );

        // lookup the boolean expression type to check it exists
        let (_, raw_boolean_expression_type) = helpers::lookup_raw_boolean_expression(
            boolean_expression_type_name,
            &field_boolean_expression_type_name,
            raw_boolean_expression_types,
        )?;

        // get type of field
        let field_type = field.field_type.get_underlying_type_name();

        // get type underlying boolean expression
        let boolean_expression_underlying_type = match &raw_boolean_expression_type.operand {
            BooleanExpressionOperand::Object(BooleanExpressionObjectOperand { r#type, .. }) => {
                TypeName::Custom(r#type.clone())
            }
            BooleanExpressionOperand::Scalar(BooleanExpressionScalarOperand { r#type, .. }) => {
                r#type.clone()
            }
        };

        let qualified_boolean_expression_type =
            mk_qualified_type_name(&boolean_expression_underlying_type, subgraph);

        // ensure the two types are the same
        if qualified_boolean_expression_type != *field_type {
            return Err(BooleanExpressionError::FieldTypeMismatch {
                field_boolean_expression_type_name: field_boolean_expression_type_name.clone(),
                field_name: comparable_field.field_name.clone(),
                field_type: field_type.clone(),
                underlying_type: qualified_boolean_expression_type,
            });
        }

        resolved_comparable_fields.insert(
            comparable_field.field_name.clone(),
            field_boolean_expression_type_name,
        );
    }

    Ok(resolved_comparable_fields)
}
