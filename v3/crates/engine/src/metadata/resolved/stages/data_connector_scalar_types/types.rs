use crate::metadata::resolved::subgraph::Qualified;

use crate::metadata::resolved::stages::data_connectors;
use indexmap::IndexMap;
use lang_graphql::ast::common as ast;

use ndc_models;
use open_dds::data_connector::{self, DataConnectorName};
use open_dds::types::TypeName;
use std::collections::HashMap;

/// information about a data connector
/// currently this contains partial ScalarTypeInfo, which we add to later
pub struct DataConnectorWithScalarsContext<'a> {
    pub url: &'a data_connector::DataConnectorUrl,
    pub headers: &'a IndexMap<String, open_dds::EnvironmentValue>,
    pub schema: &'a ndc_models::SchemaResponse,
    pub capabilities: &'a ndc_models::CapabilitiesResponse,
    pub scalars: HashMap<&'a str, ScalarTypeWithRepresentationInfo<'a>>,
}

// basic scalar type info
pub struct ScalarTypeWithRepresentationInfo<'a> {
    pub scalar_type: &'a ndc_models::ScalarType,
    pub representation: Option<TypeName>,
    pub comparison_expression_name: Option<ast::TypeName>,
    pub comparison_operators: data_connectors::ComparisonOperators,
}

pub struct DataConnectorsWithScalars<'a> {
    pub data_connectors_with_scalars:
        HashMap<Qualified<DataConnectorName>, DataConnectorWithScalarsContext<'a>>,
}
