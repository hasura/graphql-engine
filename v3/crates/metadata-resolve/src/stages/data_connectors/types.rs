use super::error::DataConnectorError;
use crate::configuration::UnstableFeatures;
use crate::helpers::http::{
    HeaderError, SerializableHeaderMap, SerializableHeaderName, SerializableUrl,
};
use crate::helpers::ndc_validation::validate_ndc_argument_presets;
use crate::ndc_migration;
use crate::types::permission::ValueExpression;
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use lang_graphql::ast::common::OperationType;
use ndc_models;
use open_dds::arguments::ArgumentName;
use open_dds::{
    commands::{FunctionName, ProcedureName},
    data_connector::{
        self, DataConnectorName, DataConnectorUrl, ReadWriteUrls, VersionedSchemaAndCapabilities,
    },
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum NdcVersion {
    V01,
    V02,
}

/// Map of resolved data connectors information that are used in the later
/// stages of metadata resolving. This structure is not kept in the finally
/// resolved metadata.
pub struct DataConnectors<'a>(pub BTreeMap<Qualified<DataConnectorName>, DataConnectorContext<'a>>);

/// information about a data connector
pub struct DataConnectorContext<'a> {
    pub url: &'a data_connector::DataConnectorUrl,
    pub headers: IndexMap<String, String>,
    pub schema: DataConnectorSchema,
    pub capabilities: ndc_models::Capabilities,
    pub supported_ndc_version: NdcVersion,
    pub argument_presets: Vec<ArgumentPreset>,
    pub response_headers: Option<CommandsResponseConfig>,
}

impl<'a> DataConnectorContext<'a> {
    pub fn new(
        data_connector: &'a data_connector::DataConnectorLinkV1,
        data_connector_name: &Qualified<DataConnectorName>,
        unstable_features: &UnstableFeatures,
    ) -> Result<Self, DataConnectorError> {
        let (resolved_schema, capabilities, supported_ndc_version) = match &data_connector.schema {
            VersionedSchemaAndCapabilities::V01(schema_and_capabilities) => {
                let schema =
                    DataConnectorSchema::new(ndc_migration::v02::migrate_schema_response_from_v01(
                        schema_and_capabilities.schema.clone(),
                    ));
                let capabilities = ndc_migration::v02::migrate_capabilities_from_v01(
                    schema_and_capabilities.capabilities.capabilities.clone(),
                );
                (schema, capabilities, NdcVersion::V01)
            }
            VersionedSchemaAndCapabilities::V02(schema_and_capabilities) => {
                let schema = DataConnectorSchema::new(schema_and_capabilities.schema.clone());
                let capabilities = schema_and_capabilities.capabilities.capabilities.clone();
                (schema, capabilities, NdcVersion::V02)
            }
        };

        if !unstable_features.enable_ndc_v02_support && supported_ndc_version == NdcVersion::V02 {
            return Err(DataConnectorError::NdcV02DataConnectorNotSupported {
                data_connector: data_connector_name.clone(),
            });
        }

        let argument_presets = data_connector
            .argument_presets
            .iter()
            .map(|argument_preset| -> Result<_, DataConnectorError> {
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
        validate_ndc_argument_presets(&argument_presets, &resolved_schema)
            .map_err(DataConnectorError::NdcValidationError)?;

        let response_headers = if let Some(headers) = &data_connector.response_headers {
            Some(CommandsResponseConfig::new(headers, data_connector_name)?)
        } else {
            None
        };

        Ok(DataConnectorContext {
            url: &data_connector.url,
            headers: data_connector
                .headers
                .iter()
                .map(|(k, v)| (k.clone(), v.value.clone()))
                .collect(),
            schema: resolved_schema,
            capabilities,
            supported_ndc_version,
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
    pub scalar_types: BTreeMap<ndc_models::ScalarTypeName, ndc_models::ScalarType>,
    /// A list of object types which can be used as the types of arguments, or return types of procedures.
    /// Names should not overlap with scalar type names.
    pub object_types: BTreeMap<ndc_models::ObjectTypeName, ndc_models::ObjectType>,
    /// Collections which are available for queries
    pub collections: BTreeMap<ndc_models::CollectionName, ndc_models::CollectionInfo>,
    /// Functions (i.e. collections which return a single column and row)
    pub functions: BTreeMap<FunctionName, ndc_models::FunctionInfo>,
    /// Procedures which are available for execution as part of mutations
    pub procedures: BTreeMap<ProcedureName, ndc_models::ProcedureInfo>,
}

impl DataConnectorSchema {
    fn new(schema: ndc_models::SchemaResponse) -> Self {
        Self {
            scalar_types: schema.scalar_types,
            object_types: schema.object_types,
            collections: schema
                .collections
                .into_iter()
                .map(|collection_info| (collection_info.name.clone(), collection_info))
                .collect(),
            functions: schema
                .functions
                .into_iter()
                .map(|function_info| {
                    (
                        FunctionName::from(function_info.name.as_str()),
                        function_info,
                    )
                })
                .collect(),
            procedures: schema
                .procedures
                .into_iter()
                .map(|procedure_info| {
                    (
                        ProcedureName::from(procedure_info.name.as_str()),
                        procedure_info,
                    )
                })
                .collect(),
        }
    }
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
    pub response_config: Option<CommandsResponseConfig>,
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
        context: &DataConnectorContext<'_>,
    ) -> Result<Self, DataConnectorError> {
        let url = match context.url {
            DataConnectorUrl::SingleUrl(url) => {
                ResolvedDataConnectorUrl::SingleUrl(SerializableUrl::new(&url.value).map_err(
                    |e| DataConnectorError::InvalidDataConnectorUrl {
                        data_connector_name: name.clone(),
                        error: e,
                    },
                )?)
            }
            DataConnectorUrl::ReadWriteUrls(ReadWriteUrls { read, write }) => {
                ResolvedDataConnectorUrl::ReadWriteUrls(ResolvedReadWriteUrls {
                    read: SerializableUrl::new(&read.value).map_err(|e| {
                        DataConnectorError::InvalidDataConnectorUrl {
                            data_connector_name: name.clone(),
                            error: e,
                        }
                    })?,
                    write: SerializableUrl::new(&write.value).map_err(|e| {
                        DataConnectorError::InvalidDataConnectorUrl {
                            data_connector_name: name.clone(),
                            error: e,
                        }
                    })?,
                })
            }
        };
        let headers = SerializableHeaderMap::new(&context.headers).map_err(|e| match e {
            HeaderError::InvalidHeaderName { header_name } => {
                DataConnectorError::InvalidHeaderName {
                    data_connector: name.clone(),
                    header_name,
                }
            }
            HeaderError::InvalidHeaderValue { header_name } => {
                DataConnectorError::InvalidHeaderValue {
                    data_connector: name.clone(),
                    header_name,
                }
            }
        })?;
        let capabilities = DataConnectorCapabilities {
            supported_ndc_version: context.supported_ndc_version,
            supports_explaining_queries: context.capabilities.query.explain.is_some(),
            supports_explaining_mutations: context.capabilities.mutation.explain.is_some(),
            supports_nested_object_filtering: context
                .capabilities
                .query
                .nested_fields
                .filter_by
                .is_some(),
            supports_nested_object_aggregations: context
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
            argument_presets: context.argument_presets.clone(),
            response_config: context.response_headers.clone(),
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
    ) -> Result<Self, DataConnectorError> {
        let forward = headers_preset
            .forward
            .iter()
            .map(|header| {
                SerializableHeaderName::new(header.to_string())
                    .map_err(|err| to_error(err, data_connector_name))
            })
            .collect::<Result<Vec<_>, DataConnectorError>>()?;

        let additional = headers_preset
            .additional
            .iter()
            .map(|(header_name, header_val)| {
                let key = SerializableHeaderName::new(header_name.to_string())
                    .map_err(|err| to_error(err, data_connector_name))?;
                let val = resolve_value_expression(header_val.clone());
                Ok((key, val))
            })
            .collect::<Result<IndexMap<_, _>, DataConnectorError>>()?;

        Ok(Self {
            forward,
            additional,
        })
    }
}

fn resolve_value_expression(
    value_expression_input: open_dds::permissions::ValueExpression,
) -> ValueExpression {
    match value_expression_input {
        open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
            ValueExpression::SessionVariable(session_variable)
        }
        open_dds::permissions::ValueExpression::Literal(json_value) => {
            ValueExpression::Literal(json_value)
        }
    }
}

fn to_error(
    err: HeaderError,
    data_connector_name: &Qualified<DataConnectorName>,
) -> DataConnectorError {
    match err {
        HeaderError::InvalidHeaderName { header_name } => DataConnectorError::InvalidHeaderName {
            data_connector: data_connector_name.clone(),
            header_name,
        },
        HeaderError::InvalidHeaderValue { header_name } => DataConnectorError::InvalidHeaderValue {
            data_connector: data_connector_name.clone(),
            header_name,
        },
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
/// Configure how NDC functions/procedures should be processed to extract
/// headers and result
pub struct CommandsResponseConfig {
    pub headers_field: ndc_models::FieldName,
    pub result_field: ndc_models::FieldName,
    pub forward_headers: Vec<SerializableHeaderName>,
}

impl CommandsResponseConfig {
    fn new(
        response_headers: &open_dds::data_connector::ResponseHeaders,
        data_connector_name: &Qualified<DataConnectorName>,
    ) -> Result<Self, DataConnectorError> {
        let forward_headers = response_headers
            .forward_headers
            .iter()
            .map(|header| {
                SerializableHeaderName::new(header.to_string())
                    .map_err(|err| to_error(err, data_connector_name))
            })
            .collect::<Result<Vec<_>, DataConnectorError>>()?;
        Ok(Self {
            headers_field: ndc_models::FieldName::from(response_headers.headers_field.as_str()),
            result_field: ndc_models::FieldName::from(response_headers.result_field.as_str()),
            forward_headers,
        })
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[allow(clippy::struct_excessive_bools)]
pub struct DataConnectorCapabilities {
    pub supported_ndc_version: NdcVersion,
    pub supports_explaining_queries: bool,
    pub supports_explaining_mutations: bool,
    pub supports_nested_object_filtering: bool,
    pub supports_nested_object_aggregations: bool,
}

#[cfg(test)]
mod tests {
    use ndc_models;
    use open_dds::data_connector::DataConnectorLinkV1;

    use crate::{
        configuration::UnstableFeatures, data_connectors::types::NdcVersion,
        stages::data_connectors::types::DataConnectorContext, Qualified,
    };

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
    fn test_data_connector_context_v01_capablities() {
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

        let explicit_capabilities: ndc_models::Capabilities =
            serde_json::from_value(serde_json::json!(
                { "query": { "nested_fields": {} }, "mutation": {} }
            ))
            .unwrap();

        // With explicit capabilities specified, we should use them
        let dc_name = Qualified::new(
            "foo".to_string(),
            data_connector_with_capabilities.name.clone(),
        );
        let unstable_features = UnstableFeatures {
            enable_ndc_v02_support: true,
            ..Default::default()
        };
        let context = DataConnectorContext::new(
            &data_connector_with_capabilities,
            &dc_name,
            &unstable_features,
        )
        .unwrap();
        assert_eq!(context.capabilities, explicit_capabilities);
        assert_eq!(context.supported_ndc_version, NdcVersion::V01);
    }

    #[test]
    fn test_data_connector_context_v02_capablities() {
        let data_connector_with_capabilities: DataConnectorLinkV1 =
            open_dds::traits::OpenDd::deserialize(serde_json::json!(
                {
                    "name": "foo",
                    "url": { "singleUrl": { "value": "http://test.com" } },
                    "schema": {
                        "version": "v0.2",
                        "capabilities": { "version": "0.2.0", "capabilities": { "query": { "nested_fields": {} }, "mutation": {} }},
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

        let explicit_capabilities: ndc_models::Capabilities =
            serde_json::from_value(serde_json::json!(
                { "query": { "nested_fields": {} }, "mutation": {} }
            ))
            .unwrap();

        // With explicit capabilities specified, we should use them
        let dc_name = Qualified::new(
            "foo".to_string(),
            data_connector_with_capabilities.name.clone(),
        );
        let unstable_features = UnstableFeatures {
            enable_ndc_v02_support: true,
            ..Default::default()
        };
        let context = DataConnectorContext::new(
            &data_connector_with_capabilities,
            &dc_name,
            &unstable_features,
        )
        .unwrap();
        assert_eq!(context.capabilities, explicit_capabilities);
        assert_eq!(context.supported_ndc_version, NdcVersion::V02);
    }
}
