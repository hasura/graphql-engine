use std::collections::{BTreeMap, BTreeSet};

use open_dds::{
    models::{EnableAllOrSpecific, ModelName, OrderByDirection},
    order_by_expression::OrderByExpressionName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};

use lang_graphql::ast::common::{self as ast};
use serde::{Deserialize, Serialize};

use crate::Qualified;

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, derive_more::Display,
)]
pub enum OrderByExpressionIdentifier {
    FromOrderByExpression(OrderByExpressionName),
    FromModel(ModelName),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct OrderByExpressions(
    pub BTreeMap<Qualified<OrderByExpressionIdentifier>, OrderByExpression>,
);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct OrderByExpressionsOutput {
    pub order_by_expressions: OrderByExpressions,
    pub graphql_types: BTreeSet<ast::TypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct OrderByExpression {
    pub identifier: Qualified<OrderByExpressionIdentifier>,
    pub ordered_type: Qualified<CustomTypeName>,
    pub orderable_fields: Vec<OrderableField>,
    pub orderable_relationships: Vec<OrderableRelationship>,
    pub graphql: Option<OrderByExpressionGraphqlConfig>,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum OrderableField {
    Scalar(OrderableScalarField),
    Object(OrderableObjectField),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct OrderableScalarField {
    pub field_name: FieldName,
    pub enable_order_by_directions: EnableAllOrSpecific<OrderByDirection>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct OrderableObjectField {
    pub field_name: FieldName,
    pub order_by_expression: Qualified<OrderByExpressionName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct OrderableRelationship {
    pub relationship_name: RelationshipName,

    /// order_by_expression is optional.
    /// If not present we will use order_by_expression from the model
    /// that the relationship targets.
    pub order_by_expression: Option<Qualified<OrderByExpressionName>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct OrderByExpressionGraphqlConfig {
    pub expression_type_name: ast::TypeName,
}
