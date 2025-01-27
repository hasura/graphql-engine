use crate::types::subgraph::Qualified;
use lang_graphql::ast::common as ast;
use ndc_models;
use open_dds::data_connector::DataConnectorScalarType;
use open_dds::types::{CustomTypeName, TypeName};
use std::collections::BTreeMap;

// basic scalar type info
#[derive(Debug)]
pub struct ScalarTypeWithRepresentationInfo<'a> {
    pub scalar_type: &'a ndc_models::ScalarType,
    pub representation: Option<TypeName>,
    pub comparison_expression_name: Option<ast::TypeName>,
    pub aggregate_functions:
        &'a BTreeMap<ndc_models::AggregateFunctionName, ndc_models::AggregateFunctionDefinition>,
}

#[derive(Debug)]
pub struct DataConnectorScalars<'a> {
    pub by_ndc_type: BTreeMap<DataConnectorScalarType, ScalarTypeWithRepresentationInfo<'a>>,
    pub by_custom_type_name: BTreeMap<Qualified<CustomTypeName>, ndc_models::TypeRepresentation>,
}
