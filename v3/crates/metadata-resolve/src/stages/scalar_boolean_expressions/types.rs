use crate::stages::boolean_expressions;
use crate::{
    types::subgraph::{Qualified, QualifiedTypeReference},
    QualifiedTypeName,
};
use open_dds::types::{GraphQlTypeName, OperatorName};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use lang_graphql::ast::common as ast;
use serde_with::serde_as;

#[derive(Debug)]
pub struct ScalarBooleanExpressionsOutput {
    pub boolean_expression_scalar_types: BTreeMap<
        boolean_expressions::BooleanExpressionTypeIdentifier,
        ResolvedScalarBooleanExpressionType,
    >,
    pub issues: Vec<super::error::ScalarBooleanExpressionTypeIssue>,
}

#[serde_as]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedScalarBooleanExpressionType {
    /// The OpenDD type this scalar refers to
    pub operand_type: QualifiedTypeName,

    /// The list of comparison operators that can used on this scalar type
    pub comparison_operators: BTreeMap<OperatorName, QualifiedTypeReference>,

    /// The list of mappings between OpenDD operator names and the names used in the data
    /// connector schema
    #[serde_as(as = "Vec<(_, _)>")]
    pub data_connector_operator_mappings: BTreeMap<
        Qualified<open_dds::data_connector::DataConnectorName>,
        open_dds::boolean_expression::DataConnectorOperatorMapping,
    >,

    // optional name for exposing this in the GraphQL schema
    pub graphql_name: Option<GraphQlTypeName>,

    pub logical_operators: LogicalOperators,

    // do we allow _is_null comparisons for this type?
    pub is_null_operator: IsNullOperator,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum IsNullOperator {
    Include {
        graphql: Option<IsNullOperatorGraphqlConfig>,
    },
    Exclude,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct IsNullOperatorGraphqlConfig {
    pub is_null_operator_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LogicalOperators {
    Include {
        graphql: Option<LogicalOperatorsGraphqlConfig>,
    },
    Exclude,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct LogicalOperatorsGraphqlConfig {
    pub and_operator_name: ast::Name,
    pub or_operator_name: ast::Name,
    pub not_operator_name: ast::Name,
}
