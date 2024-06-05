use crate::stages::{boolean_expressions, data_connectors};

use crate::types::subgraph::Qualified;
use std::collections::BTreeMap;

use lang_graphql::ast::common::{self as ast};
use std::collections::BTreeSet;

use open_dds::{
    data_connector::{DataConnectorName, DataConnectorObjectType},
    types::CustomTypeName,
};
use serde::{Deserialize, Serialize};

pub struct ObjectBooleanExpressionsOutput {
    pub object_boolean_expression_types:
        BTreeMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
    pub graphql_types: BTreeSet<ast::TypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionDataConnector {
    pub name: Qualified<DataConnectorName>,
    pub link: data_connectors::DataConnectorLink,
    pub object_type: DataConnectorObjectType,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,
    pub object_type: Qualified<CustomTypeName>,
    pub graphql: Option<boolean_expressions::BooleanExpressionGraphqlConfig>,

    /// in future we'll not be using this at all, for now it is here, and we use it only to check
    /// the user has not included something that does not make sense
    pub data_connector: Option<ObjectBooleanExpressionDataConnector>,
}
