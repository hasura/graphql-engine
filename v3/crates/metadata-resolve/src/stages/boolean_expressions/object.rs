use super::error::BooleanExpressionError;
use super::graphql;
use super::helpers;
use super::BooleanExpressionIssue;
pub use super::{
    BooleanExpressionComparableRelationship, ComparableFieldKind,
    ResolvedObjectBooleanExpressionType,
};
use crate::stages::{
    graphql_config, object_types, relationships, scalar_boolean_expressions, type_permissions,
};
use crate::types::subgraph::mk_qualified_type_name;
use crate::{Qualified, QualifiedBaseType};
use lang_graphql::ast::common::{self as ast};
use open_dds::identifier::SubgraphName;
use open_dds::{
    boolean_expression::{
        BooleanExpressionComparableField, BooleanExpressionLogicalOperators,
        BooleanExpressionObjectAggregateOperand, BooleanExpressionObjectOperand,
        BooleanExpressionOperand, BooleanExpressionScalarAggregateOperand,
        BooleanExpressionScalarOperand, BooleanExpressionTypeGraphQlConfiguration,
    },
    types::{CustomTypeName, FieldName, TypeName},
};
use std::collections::{BTreeMap, BTreeSet};

pub(crate) type RawBooleanExpressionTypes<'a> = BTreeMap<
    Qualified<CustomTypeName>,
    (
        &'a open_dds::identifier::SubgraphName,
        &'a open_dds::boolean_expression::BooleanExpressionTypeV1,
    ),
>;

/// Resolves a given object boolean expression type
pub(crate) fn resolve_object_boolean_expression_type(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    object_boolean_expression_operand: &BooleanExpressionObjectOperand,
    logical_operators: &BooleanExpressionLogicalOperators,
    subgraph: &SubgraphName,
    graphql: &Option<BooleanExpressionTypeGraphQlConfiguration>,
    object_types: &BTreeMap<Qualified<CustomTypeName>, type_permissions::ObjectTypeWithPermissions>,
    scalar_boolean_expression_types: &BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    raw_boolean_expression_types: &RawBooleanExpressionTypes,
    relationships: &relationships::Relationships,
    graphql_config: &graphql_config::GraphqlConfig,
    graphql_types: &mut BTreeSet<ast::TypeName>,
) -> Result<
    (
        ResolvedObjectBooleanExpressionType,
        Vec<BooleanExpressionIssue>,
    ),
    BooleanExpressionError,
> {
    let qualified_object_type_name = Qualified::new(
        subgraph.clone(),
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
    let ComparableFieldsOutput {
        comparable_fields,
        issues,
    } = resolve_comparable_fields(
        &object_boolean_expression_operand.comparable_fields,
        &object_type_representation.object_type,
        boolean_expression_type_name,
        subgraph,
        raw_boolean_expression_types,
    )?;

    // resolve any comparable relationships
    let comparable_relationships = resolve_comparable_relationships(
        boolean_expression_type_name,
        &qualified_object_type_name,
        &object_boolean_expression_operand.comparable_relationships,
        relationships,
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
                graphql_config,
                graphql_types,
            )
        })
        .transpose()?;

    Ok((
        ResolvedObjectBooleanExpressionType {
            name: boolean_expression_type_name.clone(),
            include_logical_operators: helpers::resolve_logical_operators(logical_operators),
            object_type: qualified_object_type_name.clone(),
            graphql: resolved_graphql,
        },
        issues,
    ))
}

// resolve comparable relationships. More indepth checks
// should occur when resolving models, when the model source is known.
fn resolve_comparable_relationships(
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    underlying_object_type_name: &Qualified<CustomTypeName>,
    comparable_relationships: &Vec<
        open_dds::boolean_expression::BooleanExpressionComparableRelationship,
    >,
    relationships: &relationships::Relationships,
    subgraph: &SubgraphName,
    raw_boolean_expression_types: &RawBooleanExpressionTypes,
) -> Result<BTreeMap<FieldName, BooleanExpressionComparableRelationship>, BooleanExpressionError> {
    let mut resolved_comparable_relationships = BTreeMap::new();

    for comparable_relationship in comparable_relationships {
        let relationship = relationships.get(
            underlying_object_type_name,
            &comparable_relationship.relationship_name,
        )?;

        match relationship {
            relationships::Relationship::Relationship(relationship) => {
                // if the relationship has provided an optional boolean_expression_type, let's
                // check it makes sense
                let target_boolean_expression_type = comparable_relationship
                    .boolean_expression_type
                    .as_ref()
                    .map(
                        |target_boolean_expression_type_name| -> Result<_, BooleanExpressionError> {
                            // create target boolean expression name
                            let target_boolean_expression_type = Qualified::new(
                                crate::helpers::relationship::get_target_subgraph(relationship)
                                    .unwrap_or(subgraph.clone()),
                                target_boolean_expression_type_name.clone(),
                            );

                            // ...and ensure it exists
                            let _raw_boolean_expression_type =
                                helpers::lookup_raw_boolean_expression(
                                    boolean_expression_type_name,
                                    &target_boolean_expression_type,
                                    raw_boolean_expression_types,
                                )?;

                            Ok(target_boolean_expression_type)
                        },
                    )
                    .transpose()?;

                resolved_comparable_relationships.insert(
                    FieldName::new(comparable_relationship.relationship_name.inner().clone()),
                    BooleanExpressionComparableRelationship {
                        relationship_name: comparable_relationship.relationship_name.clone(),
                        boolean_expression_type: target_boolean_expression_type,
                    },
                );
            }

            // If the relationship is to an unknown subgraph, skip it because we're in
            // allow unknown subgraphs mode
            relationships::Relationship::RelationshipToUnknownSubgraph => {}
        };
    }

    Ok(resolved_comparable_relationships)
}

pub struct ComparableFieldsOutput {
    comparable_fields: BTreeMap<FieldName, (ComparableFieldKind, Qualified<CustomTypeName>)>,
    issues: Vec<BooleanExpressionIssue>,
}

// comparable_fields don't do much, all we can do is ensure that the other BooleanExpressionTypes
// they refer to exist
fn resolve_comparable_fields(
    comparable_fields: &Vec<BooleanExpressionComparableField>,
    object_type_representation: &object_types::ObjectTypeRepresentation,
    boolean_expression_type_name: &Qualified<CustomTypeName>,
    subgraph: &SubgraphName,
    raw_boolean_expression_types: &RawBooleanExpressionTypes,
) -> Result<ComparableFieldsOutput, BooleanExpressionError> {
    let mut resolved_comparable_fields = BTreeMap::new();
    let mut issues = Vec::new();

    // validate comparable fields all exist in underlying object
    for comparable_field in comparable_fields {
        let field = object_type_representation
            .fields
            .get(&comparable_field.field_name)
            .ok_or_else(
                || BooleanExpressionError::UnknownFieldInObjectBooleanExpressionType {
                    field_name: comparable_field.field_name.clone(),
                    object_boolean_expression_type: boolean_expression_type_name.clone(),
                },
            )?;

        // fields with field arguments are not allowed in boolean expressions
        if !field.field_arguments.is_empty() {
            continue;
        }

        let field_boolean_expression_type_name = Qualified::new(
            subgraph.clone(),
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
        let (field_kind, boolean_expression_underlying_type) = match &raw_boolean_expression_type
            .operand
        {
            BooleanExpressionOperand::Object(BooleanExpressionObjectOperand { r#type, .. })
            | BooleanExpressionOperand::ObjectAggregate(
                BooleanExpressionObjectAggregateOperand { r#type, .. },
            ) => {
                let field_kind = match field.field_type.underlying_type {
                    QualifiedBaseType::List(_) => ComparableFieldKind::Array,
                    QualifiedBaseType::Named(_) => ComparableFieldKind::Object,
                };
                (field_kind, TypeName::Custom(r#type.clone()))
            }
            BooleanExpressionOperand::Scalar(BooleanExpressionScalarOperand { r#type, .. })
            | BooleanExpressionOperand::ScalarAggregate(
                BooleanExpressionScalarAggregateOperand { r#type, .. },
            ) => (ComparableFieldKind::Scalar, r#type.clone()),
        };
        if let QualifiedBaseType::List(_) = field.field_type.underlying_type {
            if field_kind == ComparableFieldKind::Scalar {
                issues.push(
                    BooleanExpressionIssue::BooleanExpressionArrayFieldComparedWithScalarType {
                        field_name: comparable_field.field_name.clone(),
                        boolean_expression_type_name: field_boolean_expression_type_name,
                    },
                );
                continue;
            }
        }

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
            (field_kind, field_boolean_expression_type_name),
        );
    }

    Ok(ComparableFieldsOutput {
        comparable_fields: resolved_comparable_fields,
        issues,
    })
}
