use crate::types::subgraph::Qualified;
use lang_graphql::ast::common as ast;
use ndc_models;
use open_dds::data_connector::{
    DataConnectorName, DataConnectorOperatorName, DataConnectorScalarType,
};
use open_dds::types::TypeName;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

pub struct DataConnectorWithScalarsOutput<'a> {
    pub data_connector_scalars:
        BTreeMap<Qualified<DataConnectorName>, ScalarTypeWithRepresentationInfoMap<'a>>,
    pub graphql_types: BTreeSet<ast::TypeName>,
    pub warnings: Vec<DataConnectorScalarTypesWarning>,
}
// basic scalar type info
#[derive(Debug)]
pub struct ScalarTypeWithRepresentationInfo<'a> {
    pub scalar_type: &'a ndc_models::ScalarType,
    pub representation: Option<TypeName>,
    pub comparison_expression_name: Option<ast::TypeName>,
    pub comparison_operators: ComparisonOperators,
    pub aggregate_functions:
        &'a BTreeMap<ndc_models::AggregateFunctionName, ndc_models::AggregateFunctionDefinition>,
}

#[derive(Debug)]
pub struct ScalarTypeWithRepresentationInfoMap<'a>(
    pub BTreeMap<DataConnectorScalarType, ScalarTypeWithRepresentationInfo<'a>>,
);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct ComparisonOperators {
    pub equal_operators: Vec<DataConnectorOperatorName>,
    pub in_operators: Vec<DataConnectorOperatorName>,
}

#[derive(Debug, thiserror::Error)]
pub enum DataConnectorScalarTypesWarning {
    #[error("DataConnectorScalarRepresentation is deprecated in favour of BooleanExpressionType. Please consider upgrading {scalar_type:} for data connector {data_connector_name}.")]
    PleaseUpgradeToBooleanExpressionType {
        scalar_type: DataConnectorScalarType,
        data_connector_name: Qualified<DataConnectorName>,
    },
}
