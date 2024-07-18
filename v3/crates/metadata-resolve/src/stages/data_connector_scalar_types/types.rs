use crate::types::subgraph::Qualified;
use lang_graphql::ast::common as ast;
use ndc_models;
use open_dds::data_connector::{DataConnectorName, DataConnectorScalarType};
use open_dds::types::TypeName;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

pub struct DataConnectorWithScalarsOutput<'a> {
    pub data_connector_scalars:
        BTreeMap<Qualified<DataConnectorName>, ScalarTypeWithRepresentationInfoMap<'a>>,
    pub graphql_types: BTreeSet<ast::TypeName>,
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
    pub equal_operators: Vec<ndc_models::ComparisonOperatorName>,
    pub in_operators: Vec<ndc_models::ComparisonOperatorName>,
}
