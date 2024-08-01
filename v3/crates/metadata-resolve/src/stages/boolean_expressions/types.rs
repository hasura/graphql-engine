use crate::stages::scalar_boolean_expressions;
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use lang_graphql::ast::common as ast;
use open_dds::{
    data_connector::{DataConnectorName, DataConnectorOperatorName},
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, OperatorName},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, thiserror::Error, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum BooleanExpressionIssue {
    #[error("field '{field_name:}' in object boolean expression type {object_boolean_expression_type:} is a nested array and cannot be used for comparison")]
    CannotCompareNestedArray {
        field_name: FieldName,
        object_boolean_expression_type: Qualified<CustomTypeName>,
    },
}
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionTypes {
    pub objects: BTreeMap<Qualified<CustomTypeName>, ResolvedObjectBooleanExpressionType>,
    pub scalars: BTreeMap<
        Qualified<CustomTypeName>,
        scalar_boolean_expressions::ResolvedScalarBooleanExpressionType,
    >,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionsOutput {
    pub boolean_expression_types: BooleanExpressionTypes,
    pub graphql_types: BTreeSet<ast::TypeName>,
    pub issues: Vec<BooleanExpressionIssue>,
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

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparisonExpressionInfo {
    // we reuse this type for ObjectBooleanExpressionType and BooleanExpressionType
    // the former does not use this, hence partial
    // it will be good to get rid of `Option` in future
    pub object_type_name: Option<Qualified<CustomTypeName>>,
    pub type_name: ast::TypeName,
    pub operators: BTreeMap<OperatorName, QualifiedTypeReference>,
    pub operator_mapping:
        BTreeMap<Qualified<DataConnectorName>, BTreeMap<OperatorName, DataConnectorOperatorName>>,
    pub is_null_operator_name: Option<ast::Name>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectComparisonExpressionInfo {
    pub graphql_type_name: ast::TypeName,
    pub object_type_name: Qualified<CustomTypeName>,
    pub underlying_object_type_name: Qualified<CustomTypeName>,
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
    pub graphql_config: BooleanExpressionGraphqlFieldConfig,
}
