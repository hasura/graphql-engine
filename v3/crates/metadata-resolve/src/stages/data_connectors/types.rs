use crate::helpers::http::{
    HeaderError, SerializableHeaderMap, SerializableHeaderName, SerializableUrl,
};
use crate::helpers::ndc_validation::validate_ndc_argument_presets;
use crate::types::error::Error;
use crate::types::permission::ValueExpression;
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use lang_graphql::ast::common::OperationType;
use ndc_models;
use open_dds::arguments::ArgumentName;
use open_dds::data_connector::SchemaAndCapabilitiesV01;
use open_dds::{
    commands::{FunctionName, ProcedureName},
    data_connector::{
        self, DataConnectorName, DataConnectorScalarType, DataConnectorUrl, ReadWriteUrls,
        VersionedSchemaAndCapabilities,
    },
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Map of resolved data connectors information that are used in the later
/// stages of metadata resolving. This structure is not kept in the finally
/// resolved metadata.
pub struct DataConnectors<'a>(pub BTreeMap<Qualified<DataConnectorName>, DataConnectorContext<'a>>);

/// information about a data connector
/// currently this contains partial ScalarTypeInfo, which we add to later
pub struct DataConnectorContext<'a> {
    pub inner: DataConnectorCoreInfo<'a>,
    // resolved scalar types
    pub scalars: BTreeMap<DataConnectorScalarType, ScalarTypeInfo<'a>>,
}

impl<'a> DataConnectorContext<'a> {
    pub fn new(
        data_connector: &'a data_connector::DataConnectorLinkV1,
        data_connector_name: &Qualified<DataConnectorName>,
    ) -> Result<Self, Error> {
        let VersionedSchemaAndCapabilities::V01(schema_and_capabilities) = &data_connector.schema;
        let data_connector_core_info = DataConnectorCoreInfo::new(
            data_connector,
            schema_and_capabilities,
            data_connector_name,
        )?;
        Ok(DataConnectorContext {
            inner: data_connector_core_info,
            scalars: schema_and_capabilities
                .schema
                .scalar_types
                .iter()
                .map(|(k, v)| (DataConnectorScalarType(k.clone()), ScalarTypeInfo::new(v)))
                .collect(),
        })
    }
}

/// information that does not change between resolver stages
#[derive(Clone)]
pub struct DataConnectorCoreInfo<'a> {
    pub url: &'a data_connector::DataConnectorUrl,
    pub headers: &'a IndexMap<String, open_dds::EnvironmentValue>,
    pub schema: DataConnectorSchema,
    pub capabilities: &'a ndc_models::CapabilitiesResponse,
    pub argument_presets: Vec<ArgumentPreset>,
    pub response_headers: Option<ResponseHeaders>,
}

impl<'a> DataConnectorCoreInfo<'a> {
    pub fn new(
        data_connector: &'a data_connector::DataConnectorLinkV1,
        schema_and_capabilities: &'a SchemaAndCapabilitiesV01,
        data_connector_name: &Qualified<DataConnectorName>,
    ) -> Result<Self, Error> {
        let resolved_schema = DataConnectorSchema::new(&schema_and_capabilities.schema);

        let argument_presets = data_connector
            .argument_presets
            .iter()
            .map(|argument_preset| -> Result<_, Error> {
                let header_presets = HttpHeadersPreset::new(
                    &argument_preset.value.http_headers,
                    data_connector_name,
                )?;
                Ok(ArgumentPreset {
                    name: argument_preset.argument.clone(),
                    value: ArgumentPresetValue {
                        http_headers: header_presets,
                    },
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        // validate the argument presets with NDC schema
        validate_ndc_argument_presets(&argument_presets, &resolved_schema)?;

        let response_headers = if let Some(headers) = &data_connector.response_headers {
            Some(ResponseHeaders::new(headers, data_connector_name)?)
        } else {
            None
        };

        Ok(DataConnectorCoreInfo {
            url: &data_connector.url,
            headers: &data_connector.headers,
            schema: resolved_schema,
            capabilities: &schema_and_capabilities.capabilities,
            argument_presets,
            response_headers,
        })
    }
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

impl DataConnectorSchema {
    fn new(schema: &ndc_models::SchemaResponse) -> Self {
        Self {
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
}

// basic scalar type info
#[derive(Debug)]
pub struct ScalarTypeInfo<'a> {
    pub scalar_type: &'a ndc_models::ScalarType,
    pub comparison_operators: ComparisonOperators,
    pub aggregate_functions: &'a BTreeMap<String, ndc_models::AggregateFunctionDefinition>,
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
            aggregate_functions: &source_scalar.aggregate_functions,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct ComparisonOperators {
    pub equal_operators: Vec<String>,
    pub in_operators: Vec<String>,
}

/// This represents part of resolved data connector info that is eventually kept
/// in the resolved metadata. This is used inside model/command sources, and
/// this info is required only during execution.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorLink {
    pub name: Qualified<DataConnectorName>,
    pub url: ResolvedDataConnectorUrl,
    /// These are headers used in the protocol level
    pub headers: SerializableHeaderMap,
    /// Presets for arguments that applies to all functions/procedures of the
    /// data connector
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub argument_presets: Vec<ArgumentPreset>,
    /// HTTP response headers configuration that is forwarded from a NDC
    /// function/procedure to the client.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub response_headers: Option<ResponseHeaders>,
    pub capabilities: DataConnectorCapabilities,
}

impl std::hash::Hash for DataConnectorLink {
    fn hash<H>(&self, h: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.name.hash(h);
    }
}

impl DataConnectorLink {
    pub(crate) fn new(
        name: Qualified<DataConnectorName>,
        info: &DataConnectorCoreInfo<'_>,
    ) -> Result<Self, Error> {
        let url = match info.url {
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
        let headers = SerializableHeaderMap::new(info.headers).map_err(|e| match e {
            HeaderError::InvalidHeaderName { header_name } => Error::InvalidHeaderName {
                data_connector: name.clone(),
                header_name,
            },
            HeaderError::InvalidHeaderValue { header_name } => Error::InvalidHeaderValue {
                data_connector: name.clone(),
                header_name,
            },
        })?;
        let capabilities = DataConnectorCapabilities {
            supports_explaining_queries: info.capabilities.capabilities.query.explain.is_some(),
            supports_explaining_mutations: info
                .capabilities
                .capabilities
                .mutation
                .explain
                .is_some(),
            supports_nested_object_aggregations: info
                .capabilities
                .capabilities
                .query
                .nested_fields
                .aggregates
                .is_some(),
        };
        Ok(Self {
            name,
            url,
            headers,
            capabilities,
            argument_presets: info.argument_presets.clone(),
            response_headers: info.response_headers.clone(),
        })
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
    pub fn get_url(&self, operation: OperationType) -> &reqwest::Url {
        match self {
            ResolvedDataConnectorUrl::SingleUrl(url) => &url.0,
            ResolvedDataConnectorUrl::ReadWriteUrls(ResolvedReadWriteUrls { read, write }) => {
                match operation {
                    OperationType::Query | OperationType::Subscription => &read.0,
                    OperationType::Mutation => &write.0,
                }
            }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentPreset {
    pub name: ArgumentName,
    pub value: ArgumentPresetValue,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ArgumentPresetValue {
    /// HTTP headers that can be preset from request
    pub http_headers: HttpHeadersPreset,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct HttpHeadersPreset {
    /// List of HTTP headers that should be forwarded from HTTP requests
    pub forward: Vec<SerializableHeaderName>,
    /// Additional (header, value) pairs that should be forwarded
    pub additional: IndexMap<SerializableHeaderName, ValueExpression>,
}

impl HttpHeadersPreset {
    fn new(
        headers_preset: &open_dds::data_connector::HttpHeadersPreset,
        data_connector_name: &Qualified<DataConnectorName>,
    ) -> Result<Self, Error> {
        let forward = headers_preset
            .forward
            .iter()
            .map(|header| {
                SerializableHeaderName::new(header.to_string())
                    .map_err(|err| to_error(err, data_connector_name))
            })
            .collect::<Result<Vec<_>, Error>>()?;

        let additional = headers_preset
            .additional
            .iter()
            .map(|(header_name, header_val)| {
                let key = SerializableHeaderName::new(header_name.to_string())
                    .map_err(|err| to_error(err, data_connector_name))?;
                let val = resolve_value_expression(header_val.clone())?;
                Ok((key, val))
            })
            .collect::<Result<IndexMap<_, _>, Error>>()?;

        Ok(Self {
            forward,
            additional,
        })
    }
}

fn resolve_value_expression(
    value_expression_input: open_dds::permissions::ValueExpression,
) -> Result<ValueExpression, Error> {
    match value_expression_input {
        open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
            Ok::<ValueExpression, Error>(ValueExpression::SessionVariable(session_variable.clone()))
        }
        open_dds::permissions::ValueExpression::Literal(json_value) => {
            Ok(ValueExpression::Literal(json_value.clone()))
        }
        open_dds::permissions::ValueExpression::BooleanExpression(_) => {
            Err(Error::BooleanExpressionInValueExpressionForHeaderPresetsNotSupported)
        }
    }
}

fn to_error(err: HeaderError, data_connector_name: &Qualified<DataConnectorName>) -> Error {
    match err {
        HeaderError::InvalidHeaderName { header_name } => Error::InvalidHeaderName {
            data_connector: data_connector_name.clone(),
            header_name,
        },
        HeaderError::InvalidHeaderValue { header_name } => Error::InvalidHeaderValue {
            data_connector: data_connector_name.clone(),
            header_name,
        },
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResponseHeaders {
    pub headers_field: String,
    pub result_field: String,
    pub forward_headers: Vec<SerializableHeaderName>,
}

impl ResponseHeaders {
    fn new(
        response_headers: &open_dds::data_connector::ResponseHeaders,
        data_connector_name: &Qualified<DataConnectorName>,
    ) -> Result<Self, Error> {
        let forward_headers = response_headers
            .forward_headers
            .iter()
            .map(|header| {
                SerializableHeaderName::new(header.to_string())
                    .map_err(|err| to_error(err, data_connector_name))
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(Self {
            headers_field: response_headers.headers_field.clone(),
            result_field: response_headers.result_field.clone(),
            forward_headers,
        })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorCapabilities {
    pub supports_explaining_queries: bool,
    pub supports_explaining_mutations: bool,
    pub supports_nested_object_aggregations: bool,
}

#[cfg(test)]
mod tests {
    use ndc_models;
    use open_dds::data_connector::DataConnectorLinkV1;

    use crate::{stages::data_connectors::types::DataConnectorContext, Qualified};

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
                        "capabilities": { "version": "0.1.3", "capabilities": { "query": { "nested_fields": {} }, "mutation": {} }},
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

        let explicit_capabilities: ndc_models::CapabilitiesResponse = serde_json::from_value(serde_json::json!(
            { "version": "0.1.3", "capabilities": { "query": { "nested_fields": {} }, "mutation": {} } }
        )).unwrap();

        // With explicit capabilities specified, we should use them
        let dc_name = Qualified::new(
            "foo".to_string(),
            data_connector_with_capabilities.name.clone(),
        );
        assert_eq!(
            DataConnectorContext::new(&data_connector_with_capabilities, &dc_name)
                .unwrap()
                .inner
                .capabilities,
            &explicit_capabilities
        );
    }
}
