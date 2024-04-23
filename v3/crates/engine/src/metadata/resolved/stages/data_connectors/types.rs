use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::subgraph::Qualified;
use serde::{Deserialize, Serialize};

use indexmap::IndexMap;

use ndc_models;
use open_dds::data_connector::{self, DataConnectorName, VersionedSchemaAndCapabilities};
use std::collections::HashMap;

/// information that does not change between resolver stages
#[derive(Clone, Copy)]
pub struct DataConnectorCoreInfo<'a> {
    pub url: &'a data_connector::DataConnectorUrl,
    pub headers: &'a IndexMap<String, open_dds::EnvironmentValue>,
    pub schema: &'a ndc_models::SchemaResponse,
    pub capabilities: &'a ndc_models::CapabilitiesResponse,
}

/// information about a data connector
/// currently this contains partial ScalarTypeInfo, which we add to later
pub struct DataConnectorContext<'a> {
    pub inner: DataConnectorCoreInfo<'a>,
    pub scalars: HashMap<&'a str, ScalarTypeInfo<'a>>,
}

impl<'a> DataConnectorContext<'a> {
    pub fn new(data_connector: &'a data_connector::DataConnectorLinkV1) -> Result<Self, Error> {
        let VersionedSchemaAndCapabilities::V01(schema_and_capabilities) = &data_connector.schema;
        Ok(DataConnectorContext {
            inner: DataConnectorCoreInfo {
                url: &data_connector.url,
                headers: &data_connector.headers,
                schema: &schema_and_capabilities.schema,
                capabilities: &schema_and_capabilities.capabilities,
            },
            scalars: schema_and_capabilities
                .schema
                .scalar_types
                .iter()
                .map(|(k, v)| (k.as_str(), ScalarTypeInfo::new(v)))
                .collect(),
        })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct ComparisonOperators {
    pub equal_operators: Vec<String>,
    pub in_operators: Vec<String>,
}

// basic scalar type info
#[derive(Debug)]
pub struct ScalarTypeInfo<'a> {
    pub scalar_type: &'a ndc_models::ScalarType,
    pub comparison_operators: ComparisonOperators,
}

impl<'a> ScalarTypeInfo<'a> {
    pub(crate) fn new(source_scalar: &'a ndc_models::ScalarType) -> Self {
        let mut comparison_operators = ComparisonOperators::default();
        for (operator_name, operator_definition) in &source_scalar.comparison_operators {
            match operator_definition {
                ndc_models::ComparisonOperatorDefinition::Equal => {
                    comparison_operators
                        .equal_operators
                        .push(operator_name.clone());
                }
                ndc_models::ComparisonOperatorDefinition::In => {
                    comparison_operators
                        .in_operators
                        .push(operator_name.clone());
                }
                ndc_models::ComparisonOperatorDefinition::Custom { argument_type: _ } => {}
            };
        }
        ScalarTypeInfo {
            scalar_type: source_scalar,
            comparison_operators,
        }
    }
}

pub struct DataConnectors<'a> {
    pub data_connectors: HashMap<Qualified<DataConnectorName>, DataConnectorContext<'a>>,
}
