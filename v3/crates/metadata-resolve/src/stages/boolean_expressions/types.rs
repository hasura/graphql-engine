use crate::stages::{data_connectors, object_types};

use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use std::collections::BTreeMap;

use lang_graphql::ast::common::{self as ast};
use std::collections::BTreeSet;

use open_dds::{
    data_connector::{DataConnectorName, DataConnectorObjectType},
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};

pub struct BooleanExpressionsOutput {
    pub object_boolean_expression_types:
        BTreeMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
    pub graphql_types: BTreeSet<ast::TypeName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionType {
    pub name: Qualified<CustomTypeName>,
    pub object_type: Qualified<CustomTypeName>,
    pub data_connector_name: Qualified<DataConnectorName>,
    pub data_connector_link: data_connectors::DataConnectorLink,
    // this will be removed in future and we'll work this type out from elsewhere
    // made optional to make it harder to use, but we still want to use it for throwing errors if
    // the user provides something nonsensical
    pub data_connector_object_type_dont_use_please: Option<DataConnectorObjectType>,
    pub type_mappings:
        BTreeMap<Qualified<open_dds::types::CustomTypeName>, object_types::TypeMapping>,
    pub graphql: Option<BooleanExpressionInfo>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ComparisonExpressionInfo {
    pub data_connector_name: Qualified<DataConnectorName>,
    pub scalar_type_name: String,
    pub type_name: ast::TypeName,
    pub ndc_column: String,
    pub operators: BTreeMap<String, QualifiedTypeReference>,
    pub is_null_operator_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionGraphqlConfig {
    pub where_field_name: ast::Name,
    pub and_operator_name: ast::Name,
    pub or_operator_name: ast::Name,
    pub not_operator_name: ast::Name,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpressionInfo {
    pub type_name: ast::TypeName,
    pub scalar_fields: BTreeMap<FieldName, ComparisonExpressionInfo>,
    pub graphql_config: BooleanExpressionGraphqlConfig,
}
