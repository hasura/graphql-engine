use crate::stages::{
    aggregate_boolean_expressions,
    scalar_boolean_expressions::{self, LogicalOperators, LogicalOperatorsGraphqlConfig},
};
use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use lang_graphql::ast::common as ast;
use open_dds::{
    data_connector::{DataConnectorName, DataConnectorOperatorName},
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName, OperatorName},
};
use ref_cast::RefCast;
use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
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
    #[error("the comparable field '{name}' is defined more than once in the boolean expression type '{type_name}'")]
    DuplicateComparableFieldFound {
        type_name: Qualified<CustomTypeName>,
        name: FieldName,
    },
    #[error("the comparable relationship '{name}' is defined more than once in the boolean expression type '{type_name}'")]
    DuplicateComparableRelationshipFound {
        type_name: Qualified<CustomTypeName>,
        name: RelationshipName,
    },
    #[error("the boolean expression '{type_name}' has a GraphQL field name conflict between the '{name}' {name_source_1} and the '{name}' {name_source_2}. One of these will need to be renamed.")]
    GraphqlFieldNameConflict {
        type_name: Qualified<CustomTypeName>,
        name: String,
        name_source_1: FieldNameSource,
        name_source_2: FieldNameSource,
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
            BooleanExpressionIssue::DuplicateComparableFieldFound { .. }
            | BooleanExpressionIssue::DuplicateComparableRelationshipFound { .. }
            | BooleanExpressionIssue::GraphqlFieldNameConflict { .. } => {
                flags.disallow_duplicate_names_in_boolean_expressions
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, derive_more::Display)]
pub enum FieldNameSource {
    #[display("comparable field")]
    ComparableField,
    #[display("comparable relationship")]
    ComparableRelationship,
    #[display("logical operator")]
    LogicalOperator,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionsOutput {
    pub boolean_expression_types: BooleanExpressionTypes,
    pub graphql_types: BTreeSet<ast::TypeName>,
    pub issues: Vec<BooleanExpressionIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ComparableFieldKind {
    Scalar,
    Object,
    ObjectArray,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedObjectBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,
    pub object_type: Qualified<CustomTypeName>,
    pub graphql: Option<BooleanExpressionGraphqlConfig>,
    pub fields: ResolvedObjectBooleanExpressionTypeFields,
    // do we allow _and, _or, etc for this type?
    pub include_logical_operators: IncludeLogicalOperators,
}

impl ResolvedObjectBooleanExpressionType {
    // we should only return fields if a) `graphql` config is provided
    // or b) flag is passed that allows us to expose it to other frontends
    pub fn get_fields(
        &self,
        flags: &open_dds::flags::Flags,
    ) -> Option<&ResolvedObjectBooleanExpressionTypeFields> {
        if self.graphql.is_some() || flags.allow_boolean_expression_fields_without_graphql {
            Some(&self.fields)
        } else {
            None
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedObjectBooleanExpressionTypeFields {
    pub object_fields: BTreeMap<FieldName, ObjectComparisonExpressionInfo>,
    pub scalar_fields: BTreeMap<FieldName, ComparisonExpressionInfo>,
    pub relationship_fields: BTreeMap<FieldName, BooleanExpressionComparableRelationship>,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum IncludeLogicalOperators {
    Yes,
    No,
}

#[derive(Serialize, Deserialize, Clone, Debug, Hash, PartialEq, Eq)]
pub struct OperatorMapping(pub BTreeMap<OperatorName, DataConnectorOperatorName>);

impl OperatorMapping {
    // if we cannot find an operator name in the look up, we assume it's the same as the operator
    // name
    pub fn get<'a>(&'a self, operator_name: &'a OperatorName) -> &'a DataConnectorOperatorName {
        self.0
            .get(operator_name)
            .unwrap_or_else(move || DataConnectorOperatorName::ref_cast(operator_name.inner()))
    }

    pub fn new() -> Self {
        OperatorMapping(BTreeMap::new())
    }
}

impl Default for OperatorMapping {
    fn default() -> Self {
        Self::new()
    }
}

#[serde_as]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparisonExpressionInfo {
    // we reuse this type for ObjectBooleanExpressionType and BooleanExpressionType
    // the former does not use this, hence partial
    // it will be good to get rid of `Option` in future
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub object_type_name: Option<Qualified<CustomTypeName>>,
    pub operators: BTreeMap<OperatorName, QualifiedTypeReference>,
    #[serde_as(as = "Vec<(_, _)>")]
    pub operator_mapping: BTreeMap<Qualified<DataConnectorName>, OperatorMapping>,
    pub logical_operators: LogicalOperators,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ObjectComparisonKind {
    Object,
    ObjectArray,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectComparisonExpressionInfo {
    pub object_type_name: Qualified<CustomTypeName>,
    pub underlying_object_type_name: Qualified<CustomTypeName>,
    pub field_kind: ObjectComparisonKind,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlFieldConfig {
    pub where_field_name: ast::Name,
    pub logical_operators: LogicalOperatorsGraphqlConfig,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionComparableRelationship {
    /// The name of the relationship to use for comparison
    pub relationship_name: RelationshipName,

    /// The boolean expression type to use for comparison. This is optional for relationships to
    /// models, and defaults to the filterExpressionType of the model
    pub boolean_expression_type: Qualified<CustomTypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarBooleanExpressionGraphqlConfig {
    pub type_name: ast::TypeName,
    pub is_null_operator_name: Option<ast::Name>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionGraphqlConfig {
    pub graphql_type_name: ast::TypeName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlConfig {
    pub type_name: ast::TypeName,
    pub object_fields: BTreeMap<FieldName, ObjectBooleanExpressionGraphqlConfig>,
    pub scalar_fields: BTreeMap<FieldName, ScalarBooleanExpressionGraphqlConfig>,
    pub field_config: BooleanExpressionGraphqlFieldConfig,
}
