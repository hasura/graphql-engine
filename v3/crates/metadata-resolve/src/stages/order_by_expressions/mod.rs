use std::collections::BTreeMap;

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::data_connector::DataConnectorName;
use open_dds::identifier::SubgraphName;
use open_dds::models::EnableAllOrSpecific;
use open_dds::order_by_expression::{self, OrderByExpressionName, OrderByExpressionOperand};
use open_dds::relationships::{
    ModelRelationshipTarget, RelationshipName, RelationshipTarget, RelationshipType,
};
use open_dds::types::{CustomTypeName, FieldName, TypeName};
mod error;
use crate::stages::data_connectors;
use crate::{mk_name, Error, Qualified, QualifiedBaseType, QualifiedTypeName};

use crate::types::subgraph::mk_qualified_type_name;
pub use error::{OrderByExpressionError, OrderableRelationshipError};

mod types;
pub use types::*;

use crate::stages::{graphql_config, object_types, relationships, scalar_types, type_permissions};

/// Resolve order by expressions.
/// Returns the map of OrderByExpressions and updated graphql_types.
pub fn resolve(
    metadata_accessor: &open_dds::accessor::MetadataAccessor,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    relationships: &relationships::Relationships,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<OrderByExpressionsOutput, Error> {
    let mut resolved_order_by_expressions = OrderByExpressions {
        objects: BTreeMap::new(),
        scalars: BTreeMap::new(),
    };
    let mut issues = Vec::new();

    // static list of order by expressions and their types so we can resolve without
    // being careful about dependencies between order by expressions
    let order_by_expression_names_and_types: BTreeMap<OrderByExpressionName, TypeName> =
        metadata_accessor
            .order_by_expressions
            .iter()
            .map(|o| match &o.object.operand {
                OrderByExpressionOperand::Object(object_operand) => (
                    o.object.name.clone(),
                    TypeName::Custom(object_operand.ordered_type.clone()),
                ),
                OrderByExpressionOperand::Scalar(scalar_operand) => {
                    (o.object.name.clone(), scalar_operand.ordered_type.clone())
                }
            })
            .collect();

    for open_dds::accessor::QualifiedObject {
        path: _,
        subgraph,
        object: order_by_expression,
    } in &metadata_accessor.order_by_expressions
    {
        match &order_by_expression.operand {
            OrderByExpressionOperand::Scalar(scalar_operand) => {
                let resolved_scalar_order_by_expression = resolve_scalar_order_by_expression(
                    subgraph,
                    &order_by_expression.name,
                    scalar_operand,
                    order_by_expression.graphql.as_ref(),
                    order_by_expression.description.as_ref(),
                    graphql_types,
                )
                .map_err(|error| Error::OrderByExpressionError {
                    order_by_expression_name: Qualified::new(
                        subgraph.clone(),
                        order_by_expression.name.clone(),
                    ),
                    error,
                })?;

                if let Some(existing_order_by_expression) =
                    resolved_order_by_expressions.scalars.insert(
                        Qualified::new(
                            subgraph.clone(),
                            OrderByExpressionIdentifier::FromOrderByExpression(
                                order_by_expression.name.clone(),
                            ),
                        ),
                        resolved_scalar_order_by_expression,
                    )
                {
                    issues.push(OrderByExpressionIssue::DuplicateOrderByExpression {
                        order_by_expression: existing_order_by_expression.identifier,
                    });
                };
            }
            OrderByExpressionOperand::Object(object_operand) => {
                let (resolved_order_by_expression, new_issues) =
                    resolve_object_order_by_expression(
                        subgraph,
                        object_types,
                        scalar_types,
                        &order_by_expression_names_and_types,
                        &order_by_expression.name,
                        object_operand,
                        order_by_expression.graphql.as_ref(),
                        order_by_expression.description.as_ref(),
                        relationships,
                        graphql_types,
                    )
                    .map_err(|error| Error::OrderByExpressionError {
                        order_by_expression_name: Qualified::new(
                            subgraph.clone(),
                            order_by_expression.name.clone(),
                        ),
                        error,
                    })?;

                issues.extend(new_issues);
                if let Some(existing_order_by_expression) =
                    resolved_order_by_expressions.objects.insert(
                        resolved_order_by_expression.identifier.clone(),
                        resolved_order_by_expression,
                    )
                {
                    issues.push(OrderByExpressionIssue::DuplicateOrderByExpression {
                        order_by_expression: existing_order_by_expression.identifier,
                    });
                };
            }
        }
    }

    Ok(OrderByExpressionsOutput {
        order_by_expressions: resolved_order_by_expressions,
        issues,
    })
}

// these don't do much, and we just copy the result into the object order by expressions
// rather than flattening everything out
fn resolve_scalar_order_by_expression(
    subgraph: &SubgraphName,
    order_by_expression_name: &order_by_expression::OrderByExpressionName,
    scalar_operand: &open_dds::order_by_expression::OrderByExpressionScalarOperand,
    order_by_expression_graphql: Option<
        &order_by_expression::OrderByExpressionGraphQlConfiguration,
    >,
    description: Option<&String>,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<ScalarOrderByExpression, OrderByExpressionError> {
    // because we essentially enforce all orderable fields have 'allow all', we don't actually
    // generate GraphQL types for ordering. If we change this we will need to actually generate all
    // the enums in the GraphQL schema.
    let resolved_enable_order_by_directions = match &scalar_operand.enable_order_by_directions {
        EnableAllOrSpecific::EnableAll(true) => {
            Ok(scalar_operand.enable_order_by_directions.clone())
        }
        _ => Err(OrderByExpressionError::UnsupportedFeature {
            message:
                "Order by configuration is not fully supported yet. Please use \"enableAll\":true."
                    .to_string(),
        }),
    }?;

    let identifier = Qualified::new(
        subgraph.clone(),
        OrderByExpressionIdentifier::FromOrderByExpression(order_by_expression_name.clone()),
    );

    Ok(ScalarOrderByExpression {
        identifier,
        enable_order_by_directions: resolved_enable_order_by_directions,
        graphql: resolve_graphql(order_by_expression_graphql, graphql_types)?,
        description: description.cloned(),
    })
}

fn resolve_graphql(
    order_by_expression_graphql: Option<
        &order_by_expression::OrderByExpressionGraphQlConfiguration,
    >,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<Option<OrderByExpressionGraphqlConfig>, OrderByExpressionError> {
    order_by_expression_graphql
        .as_ref()
        .map(|config| {
            let expression_type_name =
                mk_name(config.expression_type_name.as_str()).map(ast::TypeName)?;
            graphql_types.store(Some(&expression_type_name))?;
            Ok::<_, OrderByExpressionError>(OrderByExpressionGraphqlConfig {
                expression_type_name,
            })
        })
        .transpose()
}

/// Resolve an order by expression.
/// Resolves all orderable fields, orderable relationships, and
/// checks graphql type names for validity and uniqueness.
fn resolve_object_order_by_expression(
    subgraph: &SubgraphName,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, TypeName>,
    order_by_expression_name: &order_by_expression::OrderByExpressionName,
    object_operand: &order_by_expression::OrderByExpressionObjectOperand,
    order_by_expression_graphql: Option<
        &order_by_expression::OrderByExpressionGraphQlConfiguration,
    >,
    description: Option<&String>,
    relationships: &relationships::Relationships,
    graphql_types: &mut graphql_config::GraphqlTypeNames,
) -> Result<(ObjectOrderByExpression, Vec<OrderByExpressionIssue>), OrderByExpressionError> {
    let identifier = Qualified::new(
        subgraph.clone(),
        OrderByExpressionIdentifier::FromOrderByExpression(order_by_expression_name.clone()),
    );
    let ordered_type = Qualified::new(subgraph.clone(), object_operand.ordered_type.clone());
    let mut issues = Vec::new();
    let orderable_fields = resolve_orderable_fields(
        subgraph,
        object_types,
        scalar_types,
        &ordered_type,
        order_by_expression_names_and_types,
        &object_operand.orderable_fields,
        &mut issues,
    )?;

    let mut orderable_relationships = BTreeMap::new();
    for orderable_relationship in &object_operand.orderable_relationships {
        let ResolvedOrderableRelationship {
            orderable_relationship: maybe_relationship,
            issues: new_issues,
        } = resolve_orderable_relationship(
            subgraph,
            &ordered_type,
            order_by_expression_names_and_types,
            &orderable_relationship.relationship_name,
            orderable_relationship.order_by_expression.as_ref(),
            &identifier,
            relationships,
        )?;

        issues.extend(new_issues);

        if let Some((_, resolved_orderable_relationship)) = maybe_relationship {
            orderable_relationships.insert(
                orderable_relationship.relationship_name.clone(),
                resolved_orderable_relationship,
            );
        }
    }

    Ok((
        ObjectOrderByExpression {
            identifier,
            ordered_type,
            orderable_fields,
            orderable_relationships,
            graphql: resolve_graphql(order_by_expression_graphql, graphql_types)?,
            description: description.cloned(),
        },
        issues,
    ))
}

/// Resolve the orderable fields of an order by expression.
pub fn resolve_orderable_fields(
    subgraph: &SubgraphName,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    ordered_type: &Qualified<CustomTypeName>,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, TypeName>,
    orderable_fields: &[order_by_expression::OrderByExpressionOrderableField],
    issues: &mut Vec<OrderByExpressionIssue>,
) -> Result<BTreeMap<FieldName, OrderableField>, OrderByExpressionError> {
    let object_type_representation = get_object_type_representation(object_types, ordered_type)?;
    orderable_fields
        .iter()
        .map(|o| {
            resolve_orderable_field(
                subgraph,
                &object_type_representation.object_type.fields,
                object_types,
                scalar_types,
                order_by_expression_names_and_types,
                o,
                issues,
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

/// Resolve an object orderable field.
/// Checks that the orderable field is validly specified as either a scalar or object field.
/// For scalar fields it currently requires `enableAll: true` in `enableOrderByDirections`.
/// For object fields, checks that the field name exists in the object type, that the nested order by expression
/// exists, and that the nested order by expression is of the correct type for the field.
fn resolve_orderable_field(
    subgraph: &SubgraphName,
    type_fields: &IndexMap<FieldName, object_types::FieldDefinition>,
    object_types: &type_permissions::ObjectTypesWithPermissions,
    scalar_types: &BTreeMap<Qualified<CustomTypeName>, scalar_types::ScalarTypeRepresentation>,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, TypeName>,
    orderable_field: &order_by_expression::OrderByExpressionOrderableField,
    issues: &mut Vec<OrderByExpressionIssue>,
) -> Result<(FieldName, OrderableField), OrderByExpressionError> {
    // Check for unknown orderable field
    let field_definition = type_fields
        .get(&orderable_field.field_name)
        .ok_or_else(|| OrderByExpressionError::UnknownFieldInOrderByExpression {
            field_name: orderable_field.field_name.clone(),
        })?;

    // fields with field arguments are not allowed in order by expressions
    if !field_definition.field_arguments.is_empty() {
        issues.push(OrderByExpressionIssue::OrderByFieldWithFieldArguments {
            field_name: orderable_field.field_name.clone(),
        });
    }

    let resolved_orderable_field =
        match order_by_expression_names_and_types.get(&orderable_field.order_by_expression) {
            None => Err(
                OrderByExpressionError::UnknownOrderByExpressionNameInOrderableField {
                    order_by_expression_name: orderable_field.order_by_expression.clone(),
                    field_name: orderable_field.field_name.clone(),
                },
            ),
            Some(order_by_expression_type) => {
                match (
                    &field_definition.field_type.underlying_type,
                    mk_qualified_type_name(order_by_expression_type, subgraph),
                ) {
                    (
                        QualifiedBaseType::Named(QualifiedTypeName::Custom(field_custom_type)),
                        QualifiedTypeName::Custom(order_by_expression_custom_type),
                    ) if *field_custom_type == order_by_expression_custom_type => {
                        // lookup custom type name to see what kind of type we have
                        if object_types.get(field_custom_type).is_ok() {
                            // if it's an object type, store a reference to it's name
                            Ok(OrderableField::Object(OrderableObjectField {
                                order_by_expression_identifier: Qualified::new(
                                    subgraph.clone(),
                                    OrderByExpressionIdentifier::FromOrderByExpression(
                                        orderable_field.order_by_expression.clone(),
                                    ),
                                ),
                            }))
                        } else if scalar_types.get(field_custom_type).is_some() {
                            // it's a custom scalar type
                            Ok(OrderableField::Scalar(OrderableScalarField {
                                order_by_expression_identifier: Qualified::new(
                                    subgraph.clone(),
                                    OrderByExpressionIdentifier::FromOrderByExpression(
                                        orderable_field.order_by_expression.clone(),
                                    ),
                                ),
                            }))
                        } else {
                            Err(OrderByExpressionError::UnknownOrderableType {
                                data_type: field_custom_type.clone(),
                            })
                        }
                    }

                    (
                        QualifiedBaseType::Named(QualifiedTypeName::Inbuilt(field_ty)),
                        QualifiedTypeName::Inbuilt(order_by_ty),
                    ) if *field_ty == order_by_ty => {
                        // inbuilt types are always scalars
                        Ok(OrderableField::Scalar(OrderableScalarField {
                            order_by_expression_identifier: Qualified::new(
                                subgraph.clone(),
                                OrderByExpressionIdentifier::FromOrderByExpression(
                                    orderable_field.order_by_expression.clone(),
                                ),
                            ),
                        }))
                    }

                    _ => Err(OrderByExpressionError::OrderableFieldTypeError {
                        order_by_expression_name: orderable_field.order_by_expression.clone(),
                        order_by_expression_type: order_by_expression_type.clone(),
                        field_type: field_definition.field_type.underlying_type.clone(),
                        field_name: orderable_field.field_name.clone(),
                    }),
                }
            }
        }?;
    Ok((orderable_field.field_name.clone(), resolved_orderable_field))
}

struct ResolvedOrderableRelationship {
    orderable_relationship: Option<(RelationshipName, OrderableRelationship)>,
    issues: Vec<OrderByExpressionIssue>,
}

/// Resolve an orderable relationship.
/// Verifies that the order by expression for the relationship exists.
/// Checks that we only accept object relationships
fn resolve_orderable_relationship(
    subgraph: &SubgraphName,
    ordered_type: &Qualified<CustomTypeName>,
    order_by_expression_names_and_types: &BTreeMap<OrderByExpressionName, TypeName>,
    relationship_name: &RelationshipName,
    relationship_order_by_expression: Option<&OrderByExpressionName>,
    order_by_expression_identifier: &Qualified<OrderByExpressionIdentifier>,
    relationships: &relationships::Relationships,
) -> Result<ResolvedOrderableRelationship, OrderByExpressionError> {
    let mut issues = Vec::new();

    // does the relationship exist?
    let relationship = relationships
        .get(ordered_type, relationship_name)
        .map_err(|_| OrderByExpressionError::UnknownRelationship {
            relationship_name: relationship_name.clone(),
            object_type_name: ordered_type.clone(),
        })?;

    // if the relationship is to unknown subgraph, drop this `orderable_relationship` (this will
    // only happen in partial supergraph resolve mode)
    match relationship {
        relationships::Relationship::RelationshipToUnknownSubgraph => {
            Ok(ResolvedOrderableRelationship {
                orderable_relationship: None,
                issues,
            })
        }
        relationships::Relationship::Relationship(relationship) => {
            // if relationship is an array, raise a warning (that will become an error)
            if let RelationshipTarget::Model(ModelRelationshipTarget {
                relationship_type: RelationshipType::Array,
                ..
            }) = relationship.target
            {
                issues.push(OrderByExpressionIssue::CannotOrderByAnArrayRelationship {
                    order_by_expression: order_by_expression_identifier.clone(),
                    relationship_name: relationship_name.clone(),
                });
            };

            let resolved_orderable_relationship = match relationship_order_by_expression {
                None => Ok(OrderableRelationship {
                    order_by_expression: None,
                }),
                Some(order_by_expression_name) => {
                    if order_by_expression_names_and_types.contains_key(order_by_expression_name) {
                        Ok(OrderableRelationship {
                            order_by_expression: Some(Qualified::new(
                                subgraph.clone(),
                                order_by_expression_name.clone(),
                            )),
                        })
                    } else {
                        Err(
                    OrderByExpressionError::UnknownOrderByExpressionNameInOrderableRelationship {
                        order_by_expression_name: order_by_expression_name.clone(),
                        relationship_name: relationship_name.clone(),
                    },
                )
                    }
                }
            }?;
            Ok(ResolvedOrderableRelationship {
                orderable_relationship: Some((
                    relationship_name.clone(),
                    resolved_orderable_relationship,
                )),
                issues,
            })
        }
    }
}

pub fn validate_orderable_relationship(
    orderable_type: &Qualified<CustomTypeName>,
    relationship_name: &RelationshipName,
    relationship_field_nestedness: OrderableFieldNestedness,
    source_connector: &data_connectors::DataConnectorLink,
    target_connector_name: &Qualified<DataConnectorName>,
) -> Result<(), OrderableRelationshipError> {
    if source_connector.name != *target_connector_name {
        return Err(
            OrderableRelationshipError::RemoteRelationshipsNotSupported {
                orderable_type: orderable_type.clone(),
                relationship_name: relationship_name.clone(),
            },
        );
    }

    if let Some(relationship_capabilities) = &source_connector.capabilities.supports_relationships {
        if relationship_field_nestedness == OrderableFieldNestedness::ObjectNested
            && !relationship_capabilities
                .supports_nested_relationships
                .as_ref()
                .is_some_and(|n| n.supports_nested_in_ordering)
        {
            return Err(
                OrderableRelationshipError::NestedRelationshipsNotSupported {
                    orderable_type: orderable_type.clone(),
                    relationship_name: relationship_name.clone(),
                    data_connector_name: source_connector.name.clone(),
                },
            );
        }
    } else {
        return Err(OrderableRelationshipError::RelationshipsNotSupported {
            orderable_type: orderable_type.clone(),
            relationship_name: relationship_name.clone(),
            data_connector_name: source_connector.name.clone(),
        });
    }

    Ok(())
}
