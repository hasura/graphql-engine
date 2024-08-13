use std::collections::{BTreeMap, BTreeSet};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::identifier::SubgraphName;
use open_dds::order_by_expression::{self, OrderByExpressionName};
use open_dds::types::{CustomTypeName, FieldName};
mod error;
use crate::helpers::types::store_new_graphql_type;
use crate::{mk_name, Error, Qualified, QualifiedBaseType, QualifiedTypeName};
pub use error::OrderByExpressionError;

mod types;
pub use types::*;

use super::{object_types, type_permissions};

/// Resolve order by expressions.
/// Resturns the mape of OrderByExpressions and updated graphql_types.
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    mut graphql_types: BTreeSet<ast::TypeName>,
) -> Result<OrderByExpressionsOutput, Error> {
    let mut resolved_order_by_expressions = OrderByExpressions(BTreeMap::new());
    let order_by_expression_names_and_types: BTreeMap<OrderByExpressionName, CustomTypeName> =
        metadata_accessor
            .order_by_expressions
            .iter()
            .map(|o| (o.object.name.clone(), o.object.ordered_type.clone()))
            .collect();

    for open_dds::accessor::QualifiedObject {
        subgraph,
        object: order_by_expression,
    } in &metadata_accessor.order_by_expressions
    {
        let resolved_order_by_expression = resolve_order_by_expression(
            subgraph,
            object_types,
            &order_by_expression_names_and_types,
            order_by_expression,
            &mut graphql_types,
        )
        .map_err(|error| Error::OrderByExpressionError {
            order_by_expression_name: Qualified::new(
                subgraph.clone(),
                order_by_expression.name.clone(),
            ),
            error,
        })?;
        resolved_order_by_expressions.0.insert(
            resolved_order_by_expression.identifier.clone(),
            resolved_order_by_expression,
        );
    }
    Ok(OrderByExpressionsOutput {
        order_by_expressions: resolved_order_by_expressions,
        graphql_types,
    })
}

/// Resolve an order by expression.
/// Resolves all orderable fields, orderable relationships, and
/// checks graphql type names for validity and uniqueness.
fn resolve_order_by_expression(
    subgraph: &SubgraphName,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, CustomTypeName>,
    order_by_expression: &order_by_expression::OrderByExpressionV1,
    graphql_types: &mut BTreeSet<ast::TypeName>,
) -> Result<OrderByExpression, OrderByExpressionError> {
    let identifier = Qualified::new(
        subgraph.clone(),
        OrderByExpressionIdentifier::FromOrderByExpression(order_by_expression.name.clone()),
    );
    let ordered_type = Qualified::new(subgraph.clone(), order_by_expression.ordered_type.clone());

    let orderable_fields = resolve_orderable_fields(
        subgraph,
        object_types,
        &ordered_type,
        order_by_expression_names_and_types,
        &order_by_expression.orderable_fields,
    )?;

    let orderable_relationships = order_by_expression
        .orderable_relationships
        .iter()
        .map(|o| resolve_orderable_relationship(subgraph, order_by_expression_names_and_types, o))
        .collect::<Result<_, _>>()?;

    let graphql = order_by_expression
        .graphql
        .as_ref()
        .map(|config| {
            let expression_type_name =
                mk_name(config.expression_type_name.as_str()).map(ast::TypeName)?;
            store_new_graphql_type(graphql_types, Some(&expression_type_name))?;
            Ok::<_, OrderByExpressionError>(OrderByExpressionGraphqlConfig {
                expression_type_name,
            })
        })
        .transpose()?;

    Ok(OrderByExpression {
        identifier,
        ordered_type,
        orderable_fields,
        orderable_relationships,
        graphql,
        description: order_by_expression.description.clone(),
    })
}

/// Resolve the orderable fields of an order by expression.
pub fn resolve_orderable_fields(
    subgraph: &SubgraphName,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    ordered_type: &Qualified<CustomTypeName>,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, CustomTypeName>,
    orderable_fields: &[order_by_expression::OrderByExpressionOrderableField],
) -> Result<Vec<OrderableField>, OrderByExpressionError> {
    let object_type_representation = get_object_type_representation(object_types, ordered_type)?;
    orderable_fields
        .iter()
        .map(|o| {
            resolve_orderable_field(
                subgraph,
                &object_type_representation.object_type.fields,
                order_by_expression_names_and_types,
                o,
            )
        })
        .collect::<Result<_, OrderByExpressionError>>()
}

fn get_object_type_representation<'s>(
    object_types: &'s type_permissions::ObjectTypesWithPermissions,
    data_type: &Qualified<open_dds::types::CustomTypeName>,
) -> Result<&'s type_permissions::ObjectTypeWithPermissions, OrderByExpressionError> {
    object_types
        .get(data_type)
        .map_err(|_| OrderByExpressionError::UnknownOrderableType {
            data_type: data_type.clone(),
        })
}

/// Resolve an orderable field.
/// Checks that the orderable field is validly specified as either a scalar or object field.
/// For scalar fields it currently requires `enableAll: true` in `enableOrderByDirections`.
/// For object fields, checks that the field name exists in the object type, that the nested order by expression
/// exists, and that the nested order by expression is of the correct type for the field.
fn resolve_orderable_field(
    subgraph: &SubgraphName,
    type_fields: &IndexMap<FieldName, object_types::FieldDefinition>,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, CustomTypeName>,
    orderable_field: &order_by_expression::OrderByExpressionOrderableField,
) -> Result<OrderableField, OrderByExpressionError> {
    // Check for unknown orderable field
    let field_definition = type_fields
        .get(&orderable_field.field_name)
        .ok_or_else(|| OrderByExpressionError::UnknownFieldInOrderByExpression {
            field_name: orderable_field.field_name.clone(),
        })?;

    // Return an error if OrderByExpressionOrderableField
    // does not have exactly one of enable_order_by_directions and order_by_expression_name.
    match &orderable_field.order_by_expression {
        None => {
            match &orderable_field.enable_order_by_directions {
                None => Err(OrderByExpressionError::InvalidOrderByExpressionOrderableField {
                    field_name: orderable_field.field_name.clone(),
                }),
                Some(dir@open_dds::models::EnableAllOrSpecific::EnableAll(true)) => {
                    Ok(OrderableField::Scalar(OrderableScalarField {
                        field_name: orderable_field.field_name.clone(),
                        enable_order_by_directions: dir.clone(),
                    }))
                },
                _ =>Err(OrderByExpressionError::UnsupportedFeature {
                    message: "Field level order by configuration is not fully supported yet. Please use \"enableAll\":true.".to_string(),
                })
            }
        }
        Some(order_by_expression_name) => {
            if orderable_field.enable_order_by_directions.is_some() {
                return Err(OrderByExpressionError::InvalidOrderByExpressionOrderableField {
                        field_name: orderable_field.field_name.clone(),
                    });
            }
            match order_by_expression_names_and_types.get(order_by_expression_name) {
                None => Err(OrderByExpressionError::UnknownOrderByExpressionNameInOrderableField {
                    order_by_expression_name: order_by_expression_name.clone(),
                    field_name: orderable_field.field_name.clone(),
                }),
                Some(order_by_expression_type) =>  {
                    match &field_definition.field_type.underlying_type {
                        QualifiedBaseType::Named(QualifiedTypeName::Custom(Qualified{name: t, ..})) if t == order_by_expression_type =>
                        Ok(OrderableField::Object(OrderableObjectField {
                            field_name: orderable_field.field_name.clone(),
                            order_by_expression: Qualified::new( subgraph.clone(), order_by_expression_name.clone(),)})),
                        _ => Err(OrderByExpressionError::OrderableFieldTypeError { order_by_expression_name: order_by_expression_name.clone(),
                            order_by_expression_type: order_by_expression_type.clone(),
                            field_type: field_definition.field_type.underlying_type.clone(),
                            field_name: orderable_field.field_name.clone() } ),
                    }
                }
            }
        },
    }
}

/// Resolve an orderable relationship.
/// Verifies that the order by expression for the relationship exists.
/// Does _not_ check that the relationship itself exists as we have not yet resolved relationships.
fn resolve_orderable_relationship(
    subgraph: &SubgraphName,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, CustomTypeName>,
    orderable_relationship: &order_by_expression::OrderByExpressionOrderableRelationship,
) -> Result<OrderableRelationship, OrderByExpressionError> {
    match orderable_relationship.order_by_expression.as_ref() {
        None => Ok(OrderableRelationship {
            relationship_name: orderable_relationship.relationship_name.clone(),
            order_by_expression: None,
        }),
        Some(order_by_expression_name) => {
            if order_by_expression_names_and_types.contains_key(order_by_expression_name) {
                Ok(OrderableRelationship {
                    relationship_name: orderable_relationship.relationship_name.clone(),
                    order_by_expression: Some(Qualified::new(
                        subgraph.clone(),
                        order_by_expression_name.clone(),
                    )),
                })
            } else {
                Err(
                    OrderByExpressionError::UnknownOrderByExpressionNameInOrderableRelationship {
                        order_by_expression_name: order_by_expression_name.clone(),
                        relationship_name: orderable_relationship.relationship_name.clone(),
                    },
                )
            }
        }
    }
}
