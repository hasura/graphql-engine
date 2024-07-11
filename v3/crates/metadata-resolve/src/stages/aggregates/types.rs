use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

use open_dds::{
    aggregates::{
        AggregateExpressionName, AggregationFunctionName, DataConnectorAggregationFunctionName,
    },
    data_connector::{DataConnectorName, DataConnectorScalarType},
    types::{CustomTypeName, FieldName},
};

use lang_graphql::ast::common::{self as ast};

use crate::{Qualified, QualifiedTypeName, QualifiedTypeReference};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AggregateExpressionsOutput {
    pub aggregate_expressions: BTreeMap<Qualified<AggregateExpressionName>, AggregateExpression>,
    pub graphql_types: BTreeSet<ast::TypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregateExpression {
    pub name: Qualified<AggregateExpressionName>,
    pub operand: AggregateOperand,
    pub count: AggregateCountDefinition,
    pub count_distinct: AggregateCountDefinition,
    pub graphql: Option<AggregateExpressionGraphqlConfig>,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregateCountDefinition {
    pub enable: bool,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregateOperand {
    pub aggregated_type: QualifiedTypeName,
    pub aggregatable_fields: Vec<AggregatableFieldInfo>,
    pub aggregation_functions: Vec<AggregationFunctionInfo>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregatableFieldInfo {
    pub field_name: FieldName,
    pub description: Option<String>,
    pub aggregate_expression: Qualified<AggregateExpressionName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregationFunctionInfo {
    pub name: AggregationFunctionName,
    pub description: Option<String>,
    pub return_type: QualifiedTypeReference,
    pub data_connector_functions: Vec<DataConnectorAggregationFunctionInfo>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorAggregationFunctionInfo {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub function_name: DataConnectorAggregationFunctionName,
    pub operand_scalar_type: DataConnectorScalarType,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregateExpressionGraphqlConfig {
    pub count_field_name: ast::Name,
    pub count_distinct_field_name: ast::Name,

    pub select_output_type_name: ast::TypeName,
}

#[derive(Debug, thiserror::Error)]
pub enum AggregateExpressionError {
    #[error("the following aggregate expression is defined more than once: {name}")]
    DuplicateAggregateExpressionDefinition {
        name: Qualified<AggregateExpressionName>,
    },

    #[error("the aggregate expression {name} defines a graphql section and so {config_name} must be set in the GraphqlConfig")]
    ConfigMissingFromGraphQlConfig {
        name: Qualified<AggregateExpressionName>,
        config_name: String,
    },

    #[error("the name used by {config_name} from the GraphqlConfig conflicts with the aggregatable field name {aggregatable_field_name} in the aggregate expression {name}")]
    AggregatableFieldNameConflict {
        name: Qualified<AggregateExpressionName>,
        config_name: String,
        aggregatable_field_name: FieldName,
    },

    #[error("the name used by {config_name} from the GraphqlConfig conflicts with the aggregation function name {function_name} in the aggregate expression {name}")]
    AggregationFunctionNameConflict {
        name: Qualified<AggregateExpressionName>,
        config_name: String,
        function_name: AggregationFunctionName,
    },

    #[error("the aggregate expression {name} specifies an operand object type that cannot be found: {type_name}")]
    AggregateOperandObjectTypeNotFound {
        name: Qualified<AggregateExpressionName>,
        type_name: Qualified<CustomTypeName>,
    },

    #[error("the aggregate expression {name} has duplicate definitions of the aggregatable field '{field_name}'")]
    AggregateOperandObjectFieldDuplicated {
        name: Qualified<AggregateExpressionName>,
        field_name: FieldName,
    },

    #[error("the aggregate expression {name} specifies an aggregatable field '{field_name}' that does not exist on its operand type {operand_type}")]
    AggregateOperandObjectFieldNotFound {
        name: Qualified<AggregateExpressionName>,
        operand_type: Qualified<CustomTypeName>,
        field_name: FieldName,
    },

    #[error("the aggregate expression {name} specifies an aggregatable field '{field_name}' that references an aggregate expression that cannot be found: {field_aggregate_expression}")]
    AggregateOperandObjectFieldAggregateExpressionNotFound {
        name: Qualified<AggregateExpressionName>,
        field_name: FieldName,
        field_aggregate_expression: Qualified<AggregateExpressionName>,
    },

    #[error("the aggregate expression {name} specifies an aggregatable field '{field_name}' of type {field_type}, however arrays of arrays are not supported for aggregation")]
    MultipleNestedArrayAggregationNotSupported {
        name: Qualified<AggregateExpressionName>,
        field_name: FieldName,
        field_type: QualifiedTypeReference,
    },

    #[error("the aggregate expression {name} specifies an aggregatable field '{field_name}' of type {field_type}, however the aggregation expression used to aggregate that field ({field_aggregate_exp_name}) is for aggregating a different type: {field_aggregate_exp_operand_type}")]
    AggregateOperandObjectFieldTypeMismatch {
        name: Qualified<AggregateExpressionName>,
        operand_type: Qualified<CustomTypeName>,
        field_name: FieldName,
        field_type: QualifiedTypeReference,
        field_aggregate_exp_name: Qualified<AggregateExpressionName>,
        field_aggregate_exp_operand_type: QualifiedTypeName,
    },

    #[error("the aggregate expression {name} specifies an operand scalar type that cannot be found: {type_name}")]
    AggregateOperandScalarTypeNotFound {
        name: Qualified<AggregateExpressionName>,
        type_name: Qualified<CustomTypeName>,
    },

    #[error("the aggregate expression {name} has duplicate definitions of the aggregation function '{function_name}'")]
    AggregateOperandFunctionDuplicated {
        name: Qualified<AggregateExpressionName>,
        function_name: AggregationFunctionName,
    },

    #[error("the aggregate expression {name} specifies an aggregation function '{function_name}' that uses an unknown type for its return type: {type_name}")]
    AggregateOperandFunctionUnknownReturnType {
        name: Qualified<AggregateExpressionName>,
        function_name: AggregationFunctionName,
        type_name: CustomTypeName,
    },

    #[error("the aggregate expression {name} defines an aggregation function mapping to an unknown data connector: {data_connector_name}")]
    AggregateOperandDataConnectorMissing {
        name: Qualified<AggregateExpressionName>,
        data_connector_name: Qualified<DataConnectorName>,
    },

    #[error("the aggregate expression {name} defines an aggregation function mapping to a data connector that does not support aggregates: {data_connector_name}")]
    AggregateOperandDataConnectorNotSupported {
        name: Qualified<AggregateExpressionName>,
        data_connector_name: Qualified<DataConnectorName>,
    },

    #[error("the aggregate expression {name} specifies an aggregation function '{function_name}' but there is no mapping defined to an aggregation function in the data connector '{data_connector_name}'")]
    AggregateOperandDataConnectorFunctionMappingMissing {
        name: Qualified<AggregateExpressionName>,
        function_name: AggregationFunctionName,
        data_connector_name: Qualified<DataConnectorName>,
    },

    #[error("the aggregate expression {name} specifies an aggregation function '{function_name}' but the mapping to the data connector '{data_connector_name}' specifies a data connector scalar type that does not exist: {scalar_type}")]
    AggregateOperandDataConnectorFunctionUnknownScalarType {
        name: Qualified<AggregateExpressionName>,
        function_name: AggregationFunctionName,
        data_connector_name: Qualified<DataConnectorName>,
        scalar_type: DataConnectorScalarType,
    },

    #[error("the aggregate expression {name} specifies an aggregation function '{function_name}' which is mapped to the data connector '{data_connector_name}', however the mapped data connector aggregate function cannot be found: {data_connector_aggregate_function_name}")]
    AggregateOperandDataConnectorFunctionNotFound {
        name: Qualified<AggregateExpressionName>,
        function_name: AggregationFunctionName,
        data_connector_name: Qualified<DataConnectorName>,
        data_connector_aggregate_function_name: String,
    },

    #[error("the aggregate expression {name} specifies an aggregation function '{function_name}' which is mapped to the data connector '{data_connector_name}' but the Open DD return type {return_type} is not compatible with the data connector's return type. Reason: {reason}")]
    AggregateOperandDataConnectorFunctionReturnTypeIncompatible {
        name: Qualified<AggregateExpressionName>,
        function_name: AggregationFunctionName,
        return_type: QualifiedTypeReference,
        data_connector_name: Qualified<DataConnectorName>,
        reason: String,
    },

    #[error("the data connector {data_connector_name} does not support aggregates over nested object fields, such as the field {field_name} used in aggregate expression {name}")]
    NestedObjectAggregatesNotSupportedByDataConnector {
        name: Qualified<AggregateExpressionName>,
        data_connector_name: Qualified<DataConnectorName>,
        field_name: FieldName,
    },
}
