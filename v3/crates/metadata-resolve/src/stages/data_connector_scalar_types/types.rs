use crate::stages::data_connectors;
use lang_graphql::ast::common as ast;

use ndc_models;
use open_dds::data_connector::DataConnectorScalarType;
use open_dds::types::TypeName;
use std::collections::BTreeMap;

// basic scalar type info
pub struct ScalarTypeWithRepresentationInfo<'a> {
    pub scalar_type: &'a ndc_models::ScalarType,
    pub representation: Option<TypeName>,
    pub comparison_expression_name: Option<ast::TypeName>,
    pub comparison_operators: data_connectors::ComparisonOperators,
}

pub struct ScalarTypeWithRepresentationInfoMap<'a>(
    pub BTreeMap<DataConnectorScalarType, ScalarTypeWithRepresentationInfo<'a>>,
);
