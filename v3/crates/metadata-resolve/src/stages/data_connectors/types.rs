use crate::helpers::http::{HeaderError, SerializableHeaderMap, SerializableUrl};
use crate::types::error::Error;
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use lang_graphql::ast::common::OperationType;
use ndc_models;
use open_dds::{
    commands::{FunctionName, ProcedureName},
    data_connector::{
        self, DataConnectorName, DataConnectorScalarType, DataConnectorUrl, ReadWriteUrls,
        VersionedSchemaAndCapabilities,
    },
    EnvironmentValue,
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// information that does not change between resolver stages
#[derive(Clone)]
pub struct DataConnectorCoreInfo<'a> {
    pub url: &'a data_connector::DataConnectorUrl,
    pub headers: &'a IndexMap<String, open_dds::EnvironmentValue>,
    pub schema: DataConnectorSchema,
    pub capabilities: &'a ndc_models::CapabilitiesResponse,
}

/// information provided in the `ndc_models::SchemaResponse`, processed to make it easier to work
/// with
#[derive(Clone)]
pub struct DataConnectorSchema {
    /// A list of scalar types which will be used as the types of collection columns
    pub scalar_types: BTreeMap<String, ndc_models::ScalarType>,
    /// A list of object types which can be used as the types of arguments, or return types of procedures.
    /// Names should not overlap with scalar type names.
    pub object_types: BTreeMap<String, ndc_models::ObjectType>,
    /// Collections which are available for queries
    pub collections: BTreeMap<String, ndc_models::CollectionInfo>,
    /// Functions (i.e. collections which return a single column and row)
    pub functions: BTreeMap<FunctionName, ndc_models::FunctionInfo>,
    /// Procedures which are available for execution as part of mutations
    pub procedures: BTreeMap<ProcedureName, ndc_models::ProcedureInfo>,
}

/// information about a data connector
/// currently this contains partial ScalarTypeInfo, which we add to later
pub struct DataConnectorContext<'a> {
    pub inner: DataConnectorCoreInfo<'a>,
    pub scalars: BTreeMap<DataConnectorScalarType, ScalarTypeInfo<'a>>,
}

fn create_data_connector_schema(schema: &ndc_models::SchemaResponse) -> DataConnectorSchema {
    DataConnectorSchema {
        scalar_types: schema.scalar_types.clone(),
        object_types: schema.object_types.clone(),
        collections: schema
            .collections
            .iter()
            .map(|collection_info| (collection_info.name.clone(), collection_info.clone()))
            .collect(),
        functions: schema
            .functions
            .iter()
            .map(|function_info| {
                (
                    FunctionName(function_info.name.clone()),
                    function_info.clone(),
                )
            })
            .collect(),
        procedures: schema
            .procedures
            .iter()
            .map(|procedure_info| {
                (
                    ProcedureName(procedure_info.name.clone()),
                    procedure_info.clone(),
                )
            })
            .collect(),
    }
}

impl<'a> DataConnectorContext<'a> {
    pub fn new(data_connector: &'a data_connector::DataConnectorLinkV1) -> Result<Self, Error> {
        let VersionedSchemaAndCapabilities::V01(schema_and_capabilities) = &data_connector.schema;
        let resolved_schema = create_data_connector_schema(&schema_and_capabilities.schema);

        Ok(DataConnectorContext {
            inner: DataConnectorCoreInfo {
                url: &data_connector.url,
                headers: &data_connector.headers,
                schema: resolved_schema,
                capabilities: &schema_and_capabilities.capabilities,
            },
            scalars: schema_and_capabilities
                .schema
                .scalar_types
                .iter()
                .map(|(k, v)| (DataConnectorScalarType(k.clone()), ScalarTypeInfo::new(v)))
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

pub struct DataConnectors<'a>(pub BTreeMap<Qualified<DataConnectorName>, DataConnectorContext<'a>>);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorLink {
    pub name: Qualified<DataConnectorName>,
    pub url: ResolvedDataConnectorUrl,
    pub headers: SerializableHeaderMap,
}

impl std::hash::Hash for DataConnectorLink {
    fn hash<H>(&self, h: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.name.hash(h)
    }
}

impl DataConnectorLink {
    pub(crate) fn new(
        name: Qualified<DataConnectorName>,
        url: DataConnectorUrl,
        headers: &IndexMap<String, EnvironmentValue>,
    ) -> Result<Self, Error> {
        let url = match url {
            DataConnectorUrl::SingleUrl(url) => ResolvedDataConnectorUrl::SingleUrl(
                SerializableUrl::new(&url.value).map_err(|e| Error::InvalidDataConnectorUrl {
                    data_connector_name: name.clone(),
                    error: e,
                })?,
            ),
            DataConnectorUrl::ReadWriteUrls(ReadWriteUrls { read, write }) => {
                ResolvedDataConnectorUrl::ReadWriteUrls(ResolvedReadWriteUrls {
                    read: SerializableUrl::new(&read.value).map_err(|e| {
                        Error::InvalidDataConnectorUrl {
                            data_connector_name: name.clone(),
                            error: e,
                        }
                    })?,
                    write: SerializableUrl::new(&write.value).map_err(|e| {
                        Error::InvalidDataConnectorUrl {
                            data_connector_name: name.clone(),
                            error: e,
                        }
                    })?,
                })
            }
        };
        let headers = SerializableHeaderMap::new(headers).map_err(|e| match e {
            HeaderError::InvalidHeaderName { header_name } => Error::InvalidHeaderName {
                data_connector: name.clone(),
                header_name,
            },
            HeaderError::InvalidHeaderValue { header_name } => Error::InvalidHeaderValue {
                data_connector: name.clone(),
                header_name,
            },
        })?;
        Ok(Self { name, url, headers })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct ResolvedReadWriteUrls {
    pub read: SerializableUrl,
    pub write: SerializableUrl,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum ResolvedDataConnectorUrl {
    SingleUrl(SerializableUrl),
    ReadWriteUrls(ResolvedReadWriteUrls),
}

impl ResolvedDataConnectorUrl {
    pub fn get_url(&self, operation: OperationType) -> reqwest::Url {
        match self {
            ResolvedDataConnectorUrl::SingleUrl(url) => url.0.clone(),
            ResolvedDataConnectorUrl::ReadWriteUrls(ResolvedReadWriteUrls { read, write }) => {
                match operation {
                    OperationType::Query => read.0.clone(),
                    OperationType::Mutation => write.0.clone(),
                    OperationType::Subscription => write.0.clone(),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ndc_models;
    use open_dds::data_connector::DataConnectorLinkV1;

    use crate::stages::data_connectors::types::DataConnectorContext;

    #[test]
    fn test_url_serialization_deserialization() {
        let actual_url_str = r#"{"singleUrl":"http://hasura.io/"}"#;

        // Testing deserialization of url with trailing `/`
        let url: super::ResolvedDataConnectorUrl = serde_json::from_str(actual_url_str).unwrap();
        let serialized_url = serde_json::to_string(&url).unwrap();
        assert_eq!(actual_url_str, serialized_url);

        // Testing deserialization of url without trailing `/`
        let url_str = r#"{"singleUrl":"http://hasura.io"}"#;
        let url: super::ResolvedDataConnectorUrl = serde_json::from_str(url_str).unwrap();
        let serialized_url = serde_json::to_string(&url).unwrap();
        assert_eq!(actual_url_str, serialized_url);
    }

    #[test]
    fn test_data_connector_context_capablities() {
        let data_connector_with_capabilities: DataConnectorLinkV1 =
            open_dds::traits::OpenDd::deserialize(serde_json::json!(
                {
                    "name": "foo",
                    "url": { "singleUrl": { "value": "http://test.com" } },
                    "schema": {
                        "version": "v0.1",
                        "capabilities": { "version": "1", "capabilities": { "query": {}, "mutation": {} }},
                        "schema": {
                            "scalar_types": {},
                            "object_types": {},
                            "collections": [],
                            "functions": [],
                            "procedures": []
                        }
                    }
                }
            ))
            .unwrap();

        let explicit_capabilities: ndc_models::CapabilitiesResponse = serde_json::from_str(
            r#" { "version": "1", "capabilities": { "query": {}, "mutation": {} } }"#,
        )
        .unwrap();

        // With explicit capabilities specified, we should use them
        assert_eq!(
            DataConnectorContext::new(&data_connector_with_capabilities)
                .unwrap()
                .inner
                .capabilities,
            &explicit_capabilities
        );
    }
}
