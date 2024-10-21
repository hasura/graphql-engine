use crate::stages::{aggregate_boolean_expressions, scalar_boolean_expressions};
use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use lang_graphql::ast::common as ast;
use open_dds::{
    data_connector::{DataConnectorName, DataConnectorOperatorName},
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, OperatorName},
};
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, thiserror::Error, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum BooleanExpressionIssue {
    #[error("The data connector {data_connector_name} cannot be used for filtering nested array {nested_type_name:} within {parent_type_name:} as it has not defined any capabilities for nested array filtering")]
    NoNestedArrayFilteringCapabilitiesDefined {
        parent_type_name: Qualified<CustomTypeName>,
        nested_type_name: Qualified<CustomTypeName>,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error("The underlying type for the field {field_name} is array, but the boolean expression type used for the field is {boolean_expression_type_name}, which is a scalar boolean expression type")]
    BooleanExpressionArrayFieldComparedWithScalarType {
        field_name: FieldName,
        boolean_expression_type_name: Qualified<CustomTypeName>,
    },
}

impl ShouldBeAnError for BooleanExpressionIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::Flags) -> bool {
        match self {
            BooleanExpressionIssue::NoNestedArrayFilteringCapabilitiesDefined { .. } => {
                flags.require_nested_array_filtering_capability
            }
            BooleanExpressionIssue::BooleanExpressionArrayFieldComparedWithScalarType {
                ..
            } => flags.disallow_array_field_compared_with_scalar_boolean_type,
        }
    }
}

#[serde_as]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionTypes {
    #[serde_as(as = "Vec<(_, _)>")]
    pub objects: BTreeMap<Qualified<CustomTypeName>, ResolvedObjectBooleanExpressionType>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub scalars: BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
    #[serde_as(as = "Vec<(_, _)>")]
    pub object_aggregates: BTreeMap<
        Qualified<CustomTypeName>,
        aggregate_boolean_expressions::ObjectAggregateBooleanExpression,
    >,
    #[serde_as(as = "Vec<(_, _)>")]
    pub scalar_aggregates: BTreeMap<
        Qualified<CustomTypeName>,
        aggregate_boolean_expressions::ScalarAggregateBooleanExpression,
    >,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionsOutput {
    pub boolean_expression_types: BooleanExpressionTypes,
    pub graphql_types: BTreeSet<ast::TypeName>,
    pub issues: Vec<BooleanExpressionIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ComparableFieldKind {
    Scalar,
    Object,
    Array,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedObjectBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,
    pub object_type: Qualified<CustomTypeName>,
    pub graphql: Option<BooleanExpressionGraphqlConfig>,
    // do we allow _and, _or, etc for this type?
    pub include_logical_operators: IncludeLogicalOperators,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum IncludeLogicalOperators {
    Yes,
    No,
}

#[serde_as]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparisonExpressionInfo {
    // we reuse this type for ObjectBooleanExpressionType and BooleanExpressionType
    // the former does not use this, hence partial
    // it will be good to get rid of `Option` in future
    pub object_type_name: Option<Qualified<CustomTypeName>>,
    pub type_name: ast::TypeName,
    pub operators: BTreeMap<OperatorName, QualifiedTypeReference>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub operator_mapping:
        BTreeMap<Qualified<DataConnectorName>, BTreeMap<OperatorName, DataConnectorOperatorName>>,
    pub is_null_operator_name: Option<ast::Name>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ObjectComparisonKind {
    Object,
    Array,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectComparisonExpressionInfo {
    pub graphql_type_name: ast::TypeName,
    pub object_type_name: Qualified<CustomTypeName>,
    pub underlying_object_type_name: Qualified<CustomTypeName>,
    pub field_kind: ObjectComparisonKind,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlFieldConfig {
    pub where_field_name: ast::Name,
    pub and_operator_name: ast::Name,
    pub or_operator_name: ast::Name,
    pub not_operator_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionComparableRelationship {
    /// The name of the relationship to use for comparison
    pub relationship_name: RelationshipName,

    /// The boolean expression type to use for comparison. This is optional for relationships to
    /// models, and defaults to the filterExpressionType of the model
    pub boolean_expression_type: Option<Qualified<CustomTypeName>>,
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlConfig {
    pub type_name: ast::TypeName,
    pub object_fields: BTreeMap<FieldName, ObjectComparisonExpressionInfo>,
    pub scalar_fields: BTreeMap<FieldName, ComparisonExpressionInfo>,
    pub relationship_fields: BTreeMap<FieldName, BooleanExpressionComparableRelationship>,
    pub field_config: BooleanExpressionGraphqlFieldConfig,
}
