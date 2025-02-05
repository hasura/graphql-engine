use crate::types::error::ShouldBeAnError;
use lang_graphql::ast::common::{self as ast};
use open_dds::{
    models::{EnableAllOrSpecific, ModelName, OrderByDirection},
    order_by_expression::OrderByExpressionName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::collections::BTreeMap;
use std::fmt::Display;

use crate::Qualified;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OrderByExpressionIdentifier {
    FromOrderByExpression(OrderByExpressionName),
    FromModel(ModelName),
    FromModelField(ModelName, FieldName),
}

impl Display for OrderByExpressionIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            OrderByExpressionIdentifier::FromOrderByExpression(inner) => {
                write!(f, "{inner}")
            }
            OrderByExpressionIdentifier::FromModel(model_name) => {
                write!(f, "{model_name}")
            }
            OrderByExpressionIdentifier::FromModelField(model_name, field_name) => {
                write!(f, "{model_name}.{field_name}")
            }
        }
    }
}

#[serde_as]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct OrderByExpressions {
    #[serde_as(as = "Vec<(_, _)>")]
    pub objects: BTreeMap<Qualified<OrderByExpressionIdentifier>, ObjectOrderByExpression>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub scalars: BTreeMap<Qualified<OrderByExpressionIdentifier>, ScalarOrderByExpression>,
}

#[derive(Debug)]
pub struct OrderByExpressionsOutput {
    pub order_by_expressions: OrderByExpressions,
    pub issues: Vec<OrderByExpressionIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectOrderByExpression {
    pub identifier: Qualified<OrderByExpressionIdentifier>,
    pub ordered_type: Qualified<CustomTypeName>,
    pub orderable_fields: BTreeMap<FieldName, OrderableField>,
    pub orderable_relationships: BTreeMap<RelationshipName, OrderableRelationship>,
    pub graphql: Option<OrderByExpressionGraphqlConfig>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarOrderByExpression {
    pub identifier: Qualified<OrderByExpressionIdentifier>,
    pub enable_order_by_directions: EnableAllOrSpecific<OrderByDirection>,
    pub graphql: Option<OrderByExpressionGraphqlConfig>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum OrderableField {
    Scalar(OrderableScalarField),
    Object(OrderableObjectField),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderableScalarField {
    pub order_by_expression_identifier: Qualified<OrderByExpressionIdentifier>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderableObjectField {
    pub order_by_expression_identifier: Qualified<OrderByExpressionIdentifier>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderableRelationship {
    /// order_by_expression is optional.
    /// If not present we will use order_by_expression from the model
    /// that the relationship targets.
    pub order_by_expression: Option<Qualified<OrderByExpressionName>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct OrderByExpressionGraphqlConfig {
    pub expression_type_name: ast::TypeName,
}

#[derive(Debug, thiserror::Error)]
pub enum OrderByExpressionIssue {
    #[error("Duplicate order by expression found: {order_by_expression}")]
    DuplicateOrderByExpression {
        order_by_expression: Qualified<OrderByExpressionIdentifier>,
    },
    #[error("Cannot order by array relationship {relationship_name} in order by expression {order_by_expression}")]
    CannotOrderByAnArrayRelationship {
        order_by_expression: Qualified<OrderByExpressionIdentifier>,
        relationship_name: RelationshipName,
    },
    #[error("The orderable field \"{field_name}\" has field arguments and cannot be used in order by expressions.")]
    OrderByFieldWithFieldArguments { field_name: FieldName },
}

impl ShouldBeAnError for OrderByExpressionIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            OrderByExpressionIssue::DuplicateOrderByExpression { .. } => flags
                .contains(open_dds::flags::Flag::DisallowDuplicateNamesAcrossTypesAndExpressions),
            OrderByExpressionIssue::CannotOrderByAnArrayRelationship { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowArrayRelationshipInOrderBy)
            }
            OrderByExpressionIssue::OrderByFieldWithFieldArguments { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowOrderByFieldsWithFieldArguments)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OrderableFieldNestedness {
    NotNested,
    ObjectNested,
}
