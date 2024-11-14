use crate::stages::{boolean_expressions, data_connectors};
use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::Qualified;
use std::collections::BTreeMap;

use lang_graphql::ast::common::{self as ast};
use std::collections::BTreeSet;

use open_dds::{
    data_connector::{DataConnectorName, DataConnectorObjectType},
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};

pub struct ObjectBooleanExpressionsOutput {
    pub object_boolean_expression_types:
        BTreeMap<Qualified<CustomTypeName>, ObjectBooleanExpressionType>,
    pub graphql_types: BTreeSet<ast::TypeName>,
    pub issues: Vec<ObjectBooleanExpressionIssue>,
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
    pub scalar_fields: BTreeMap<FieldName, boolean_expressions::ComparisonExpressionInfo>,
    pub graphql: Option<ObjectBooleanExpressionGraphqlConfig>,
    pub data_connector: ObjectBooleanExpressionDataConnector,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectBooleanExpressionGraphqlConfig {
    pub type_name: ast::TypeName,
    pub scalar_fields:
        BTreeMap<FieldName, boolean_expressions::ScalarBooleanExpressionGraphqlConfig>,
    pub field_config: boolean_expressions::BooleanExpressionGraphqlFieldConfig,
}

#[derive(Debug, thiserror::Error)]
pub enum ObjectBooleanExpressionIssue {
    #[error("ObjectBooleanExpressionType is deprecated. Please consider upgrading {name:} to a BooleanExpressionType. https://hasura.io/docs/3.0/cli/commands/ddn_codemod_upgrade-object-boolean-expression-types/")]
    PleaseUpgradeToBooleanExpression { name: Qualified<CustomTypeName> },
}

impl ShouldBeAnError for ObjectBooleanExpressionIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::Flags) -> bool {
        match self {
            ObjectBooleanExpressionIssue::PleaseUpgradeToBooleanExpression { .. } => {
                flags.disallow_object_boolean_expression_type
            }
        }
    }
}
