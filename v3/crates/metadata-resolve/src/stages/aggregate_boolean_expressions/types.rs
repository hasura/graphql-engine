use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use lang_graphql::ast::common as ast;
use open_dds::{
    aggregates::{AggregateExpressionName, AggregationFunctionName},
    models::ModelName,
    order_by_expression::OrderByExpressionName,
    relationships::RelationshipName,
    types::{CustomTypeName, FieldName},
};

use crate::{
    stages::aggregates::CountAggregateType,
    stages::boolean_expressions::BooleanExpressionTypeIdentifier,
    stages::graphql_config::GraphqlConfigError,
    stages::scalar_boolean_expressions::LogicalOperators, Qualified, QualifiedTypeName,
    QualifiedTypeReference,
};

#[derive(Debug)]
pub struct AggregateBooleanExpressionsOutput {
    pub scalar_aggregates: BTreeMap<Qualified<CustomTypeName>, ScalarAggregateBooleanExpression>,
    pub object_aggregates: BTreeMap<Qualified<CustomTypeName>, ObjectAggregateBooleanExpression>,

    pub issues: Vec<NamedAggregateBooleanExpressionIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarAggregateBooleanExpression {
    pub operand_type: QualifiedTypeName,
    pub aggregate_expression: Qualified<AggregateExpressionName>,
    pub aggregation_functions: Vec<ComparableAggregationFunction>,
    pub count_aggregation: Option<ComparableCountAggregation>,
    pub count_distinct_aggregation: Option<ComparableCountAggregation>,
    pub logical_operators: LogicalOperators,
    pub graphql: Option<AggregateBooleanExpressionGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparableAggregationFunction {
    pub aggregate_function_name: AggregationFunctionName,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    pub boolean_expression_type: BooleanExpressionTypeIdentifier,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparableCountAggregation {
    pub boolean_expression_type: BooleanExpressionTypeIdentifier,
    pub graphql: Option<ComparableCountGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparableCountGraphqlConfig {
    pub field_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CustomFilterInput {
    pub filter_boolean_expression_type: Qualified<CustomTypeName>,
    pub order_by_expression: Qualified<OrderByExpressionName>,
    pub graphql: Option<CustomFilterInputGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CustomFilterInputGraphqlConfig {
    pub type_name: ast::TypeName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregateBooleanExpressionGraphqlConfig {
    pub type_name: ast::TypeName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectAggregateBooleanExpression {
    pub operand_type: Qualified<CustomTypeName>,
    pub aggregate_expression: Qualified<AggregateExpressionName>,
    pub comparable_fields: Vec<ComparableAggregatableField>,
    pub comparable_relationships: Vec<ComparableAggregatableRelationship>,
    pub count_aggregation: Option<ComparableCountAggregation>,
    pub count_distinct_aggregation: Option<ComparableCountAggregation>,
    pub filter_input: Option<FilterInputDefinition>,
    pub logical_operators: LogicalOperators,
    pub graphql: Option<AggregateBooleanExpressionGraphqlConfig>,
    pub predicate_graphql: Option<AggregatePredicateGraphqlConfig>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparableAggregatableField {
    pub field_name: FieldName,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    pub aggregate_boolean_expression_type: Qualified<CustomTypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparableAggregatableRelationship {
    pub relationship_name: RelationshipName,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    pub aggregate_boolean_expression_type: Qualified<CustomTypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum FilterInputDefinition {
    FromModel(FromModelFilterInputDefinition),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct FromModelFilterInputDefinition {
    pub model_name: Qualified<ModelName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregatePredicateGraphqlConfig {
    pub type_name: ast::TypeName,
}

#[derive(Debug, thiserror::Error)]
#[error("error in boolean expression type '{type_name}': {error}")]
pub struct NamedAggregateBooleanExpressionError {
    pub type_name: Qualified<CustomTypeName>,
    pub error: AggregateBooleanExpressionError,
}

#[derive(Debug, thiserror::Error)]
pub enum AggregateBooleanExpressionError {
    #[error("boolean expressions with aggregate operands are not supported")]
    AggregateBooleanExpressionsNotSupported,

    #[error("boolean expressions with aggregate operands do not support isNull comparisons")]
    IsNullComparisonsNotSupported,

    #[error("{0}")]
    GraphqlConfigError(#[from] GraphqlConfigError),

    #[error("the operand type '{operand_type}' must be a scalar type")]
    OperandTypeIsNotAScalarType {
        operand_type: Qualified<CustomTypeName>,
    },

    #[error("the operand type '{operand_type}' must be an object type")]
    OperandTypeIsNotAnObjectType {
        operand_type: Qualified<CustomTypeName>,
    },

    #[error("the aggregate expression '{aggregate_expression}' could not be found")]
    AggregateExpressionNotFound {
        aggregate_expression: Qualified<AggregateExpressionName>,
    },

    #[error("the operand type '{operand_type}' does not match the operand type '{aggregate_operand}' from the aggregate expression '{aggregate_expression}'")]
    AggregateOperandTypeMismatch {
        operand_type: QualifiedTypeName,
        aggregate_expression: Qualified<AggregateExpressionName>,
        aggregate_operand: QualifiedTypeName,
    },

    #[error("the aggregate function '{aggregation_function_name}' is defined more than once in the comparable aggregation functions")]
    DuplicateAggregationFunctionFound {
        aggregation_function_name: AggregationFunctionName,
    },

    #[error("the aggregation function '{aggregation_function_name}' is not defined in the aggregate expression '{aggregate_expression}'")]
    AggregationFunctionNotFound {
        aggregation_function_name: AggregationFunctionName,
        aggregate_expression: Qualified<AggregateExpressionName>,
    },

    #[error(
        "could not find a scalar-operanded boolean expression type named '{boolean_expression_type}'"
    )]
    ScalarBooleanExpressionTypeNotFound {
        boolean_expression_type: BooleanExpressionTypeIdentifier,
    },

    #[error("type mismatch between the aggregation function '{aggregation_function_name}' (return type: '{aggregation_function_return_type}') and the specified boolean expression type '{boolean_expression_type}' (operand type: '{boolean_expression_operand_type}')")]
    AggregationFunctionTypeMismatch {
        aggregation_function_name: AggregationFunctionName,
        aggregation_function_return_type: QualifiedTypeName,
        boolean_expression_type: BooleanExpressionTypeIdentifier,
        boolean_expression_operand_type: QualifiedTypeName,
    },

    #[error("the aggregation function '{aggregation_function_name}' returns an array type ({aggregation_function_return_type}). Array types are not supported in aggregate comparisons")]
    ComparisonAgainstArrayTypesNotSupported {
        aggregation_function_name: AggregationFunctionName,
        aggregation_function_return_type: QualifiedTypeReference,
    },

    #[error("the {count_type} aggregation cannot be compared to because it is not enabled on the aggregate expression '{aggregate_expression}'")]
    CountAggregateNotEnabled {
        count_type: CountAggregateType,
        aggregate_expression: Qualified<AggregateExpressionName>,
    },

    #[error("type mismatch between the '{count_type}' aggregate (return type: '{count_return_type}') and the specified boolean expression type '{boolean_expression_type}' (operand type: '{boolean_expression_operand_type}')")]
    CountAggregateTypeMismatch {
        count_type: CountAggregateType,
        count_return_type: QualifiedTypeName,
        boolean_expression_type: BooleanExpressionTypeIdentifier,
        boolean_expression_operand_type: QualifiedTypeName,
    },

    #[error("the operand type '{operand_type}' does not match the operand type of the filter boolean expression type '{boolean_expression_type}': '{boolean_expression_operand_type}'")]
    FilterInputFilterExpressionTypeMismatch {
        operand_type: QualifiedTypeName,
        boolean_expression_type: Qualified<CustomTypeName>,
        boolean_expression_operand_type: QualifiedTypeName,
    },

    #[error("the order by expression '{order_by_expression}' could not be found")]
    OrderByExpressionNotFound {
        order_by_expression: Qualified<OrderByExpressionName>,
    },

    #[error("the field '{field_name}' is defined more than once in the comparable fields")]
    DuplicateComparableFieldsFound { field_name: FieldName },

    #[error("the field '{field_name}' used in comparable fields is not an aggregatable field defined in the aggregate expression '{aggregate_expression}'")]
    ComparableFieldNotFound {
        field_name: FieldName,
        aggregate_expression: Qualified<AggregateExpressionName>,
    },

    #[error("the operand object type '{operand_type}' does not contain the field '{field_name}' used in the comparable fields")]
    ComparableFieldNotFoundOnObjectType {
        operand_type: Qualified<CustomTypeName>,
        field_name: FieldName,
    },

    #[error("the type of the comparable field '{field_name}' ({field_type}) is an array type. Nested aggregation over array types is not supported")]
    ComparableFieldNestedArrayTypeNotSupported {
        field_name: FieldName,
        field_type: QualifiedTypeReference,
    },

    #[error("the boolean expression type ({boolean_expression_type}) used in the comparable field '{field_name}' could not be found")]
    ComparableFieldBooleanExpressionNotFound {
        field_name: FieldName,
        boolean_expression_type: Qualified<CustomTypeName>,
    },

    #[error("the boolean expression type ({boolean_expression_type}) used in the comparable field '{field_name}' must have a '{aggregate_operand_type}' operand, to match the field type '{field_type}'")]
    ComparableFieldBooleanExpressionIncorrectOperandType {
        boolean_expression_type: Qualified<CustomTypeName>,
        aggregate_operand_type: AggregateOperandType,
        field_name: FieldName,
        field_type: QualifiedTypeName,
    },

    #[error("the type of the comparable field '{field_name}' ({field_type}) does not match the operand type of the boolean expression type '{boolean_expression_type}': '{boolean_expression_operand_type}'")]
    ComparableFieldBooleanExpressionTypeMismatch {
        field_name: FieldName,
        field_type: QualifiedTypeName,
        boolean_expression_type: Qualified<CustomTypeName>,
        boolean_expression_operand_type: QualifiedTypeName,
    },

    #[error("the relationship '{relationship_name}' is defined more than once in the comparable relationships")]
    DuplicateComparableRelationshipsFound {
        relationship_name: open_dds::relationships::RelationshipName,
    },

    #[error("the comparable relationship '{relationship_name}' was not found for the operand type '{operand_type}'")]
    ComparableRelationshipNotFound {
        operand_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error("the comparable relationship '{relationship_name}' for the operand type '{operand_type}' targets a command. This is not supported")]
    ComparableRelationshipCommandTargetNotSupported {
        operand_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error("the comparable relationship '{relationship_name}' for the operand type '{operand_type}' is an array relationship. This is not supported")]
    ComparableArrayRelationshipNotSupported {
        operand_type: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
    },

    #[error("the comparable relationship '{relationship_name}' for the operand type '{operand_type}' targets a model than cannot be found: '{target_model_name}'")]
    ComparableRelationshipTargetModelNotFound {
        operand_type: Qualified<CustomTypeName>,
        relationship_name: open_dds::relationships::RelationshipName,
        target_model_name: Qualified<open_dds::models::ModelName>,
    },

    #[error("the comparable relationship '{relationship_name}' for the operand type '{operand_type}' references a boolean expression type that cannot be found: '{boolean_expression_type}'")]
    ComparableRelationshipBooleanExpressionNotFound {
        operand_type: Qualified<CustomTypeName>,
        relationship_name: open_dds::relationships::RelationshipName,
        boolean_expression_type: Qualified<CustomTypeName>,
    },

    #[error("the boolean expression type '{boolean_expression_type}' used in the comparable relationship '{relationship_name}' does not have an object aggregate operand")]
    ComparableRelationshipBooleanExpressionIncorrectOperandType {
        relationship_name: open_dds::relationships::RelationshipName,
        boolean_expression_type: Qualified<CustomTypeName>,
    },

    #[error("the object type of the target of the relationship '{relationship_name}' ({relationship_target_object_type}) does not match the operand type of the boolean expression type '{boolean_expression_type}': '{boolean_expression_operand_type}'")]
    ComparableRelationshipBooleanExpressionTypeMismatch {
        relationship_name: open_dds::relationships::RelationshipName,
        relationship_target_object_type: Qualified<CustomTypeName>,
        boolean_expression_type: Qualified<CustomTypeName>,
        boolean_expression_operand_type: Qualified<CustomTypeName>,
    },

    #[error("the filter input model '{model_name}' cannot be found")]
    FilterInputModelNotFound { model_name: Qualified<ModelName> },

    #[error("the operand type '{operand_type}' does not match the type of the model '{model_name}': '{model_type}'")]
    FilterInputModelTypeMismatch {
        operand_type: Qualified<CustomTypeName>,
        model_name: Qualified<ModelName>,
        model_type: Qualified<CustomTypeName>,
    },

    #[error("a GraphQL field name conflict exists between the '{name}' {name_source_1} and the '{name}' {name_source_2}. One of these will need to be renamed.")]
    GraphqlNameConflict {
        name: String,
        name_source_1: NameSource,
        name_source_2: NameSource,
    },
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, derive_more::Display)]
pub enum AggregateOperandType {
    #[display("object aggregate")]
    ObjectAggregate,
    #[display("scalar aggregate")]
    ScalarAggregate,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, derive_more::Display)]
pub enum NameSource {
    #[display("comparable aggregation function")]
    ComparableAggregationFunction,
    #[display("comparable aggregatable field")]
    ComparableAggregatableField,
    #[display("comparable aggregatable relationship")]
    ComparableAggregatableRelationship,
    #[display("logical operator")]
    LogicalOperator,
    #[display("count aggregation function")]
    CountAggregationFunction,
    #[display("count distinct aggregation function")]
    CountDistinctAggregationFunction,
}

#[derive(Debug, thiserror::Error)]
#[error("issue in boolean expression type '{type_name}': {issue}")]
pub struct NamedAggregateBooleanExpressionIssue {
    pub type_name: Qualified<CustomTypeName>,
    pub issue: AggregateBooleanExpressionIssue,
}

#[derive(Debug, thiserror::Error)]
pub enum AggregateBooleanExpressionIssue {
    #[error("a graphql section is defined but it will not appear in the GraphQL API unless logical operator field names are also configured in the GraphqlConfig in query.filterInputConfig")]
    MissingLogicalOperatorNamesInGraphqlConfig,

    #[error("a {count_type} aggregation is defined but it will not appear in the GraphQL API unless count aggregate field names are also configured in the GraphqlConfig in query.aggregate")]
    MissingCountAggregationNamesInGraphqlConfig { count_type: CountAggregateType },
}
