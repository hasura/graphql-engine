use super::error::{
    DataConnectorError, DataConnectorIssue, NamedDataConnectorError, NamedDataConnectorIssue,
};
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
use open_dds::accessor::MetadataAccessor;
use open_dds::data_connector::DataConnectorColumnName;
use open_dds::types::DataConnectorArgumentName;
use open_dds::{
    commands::{FunctionName, ProcedureName},
    data_connector::{
        self, DataConnectorName, DataConnectorUrl, ReadWriteUrls, VersionedSchemaAndCapabilities,
    },
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::sync::Arc;
use strum_macros::EnumIter;

pub struct DataConnectorsOutput<'a> {
    pub data_connectors: DataConnectors<'a>,
    pub issues: Vec<NamedDataConnectorIssue>,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Serialize, Deserialize, EnumIter)]
pub enum NdcVersion {
    V01,
    V02,
}

impl std::fmt::Display for NdcVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NdcVersion::V01 => write!(f, "v0.1.x"),
            NdcVersion::V02 => write!(f, "v0.2.x"),
        }
    }
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
        metadata_accessor: &MetadataAccessor,
        data_connector: &'a data_connector::DataConnectorLinkV1,
        unstable_features: &UnstableFeatures,
    ) -> Result<(Self, Vec<DataConnectorIssue>), DataConnectorError> {
        let (resolved_schema, capabilities, supported_ndc_version, issues) = match &data_connector
            .schema
        {
            VersionedSchemaAndCapabilities::V01(schema_and_capabilities) => {
                let issues = validate_ndc_version(
                    NdcVersion::V01,
                    &schema_and_capabilities.capabilities.version,
                )?;
                let schema =
                    DataConnectorSchema::new(ndc_migration::v02::migrate_schema_response_from_v01(
                        schema_and_capabilities.schema.clone(),
                    ));
                let capabilities = ndc_migration::v02::migrate_capabilities_from_v01(
                    schema_and_capabilities.capabilities.capabilities.clone(),
                );
                (schema, capabilities, NdcVersion::V01, issues)
            }
            VersionedSchemaAndCapabilities::V02(schema_and_capabilities) => {
                let issues = validate_ndc_version(
                    NdcVersion::V02,
                    &schema_and_capabilities.capabilities.version,
                )?;
                let schema = DataConnectorSchema::new(schema_and_capabilities.schema.clone());
                let capabilities = schema_and_capabilities.capabilities.capabilities.clone();
                (schema, capabilities, NdcVersion::V02, issues)
            }
        };

        if !unstable_features.enable_ndc_v02_support && supported_ndc_version == NdcVersion::V02 {
            return Err(DataConnectorError::NdcV02DataConnectorNotSupported);
        }

        let argument_presets = data_connector
            .argument_presets
            .iter()
            .map(|argument_preset| -> Result<_, DataConnectorError> {
                let header_presets =
                    HttpHeadersPreset::new(metadata_accessor, &argument_preset.value.http_headers)?;
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
            Some(CommandsResponseConfig::new(headers)?)
        } else {
            None
        };

        let context = DataConnectorContext {
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
        };

        Ok((context, issues))
    }
}

fn validate_ndc_version(
    ndc_version: NdcVersion,
    capabilities_version: &str,
) -> Result<Vec<DataConnectorIssue>, DataConnectorError> {
    let capabilities_semver_result = semver::Version::parse(capabilities_version);

    let capabilities_semver = match ndc_version {
        NdcVersion::V01 => match capabilities_semver_result {
            Ok(semver) => semver,
            // Old builds with V01 capabilities contain invalid capabilities versions such as
            // "", "*", "^0.1.1". In order to be backwards compatible we'll allow these and assume
            // they are NDC v0.1.x connectors (they were back then, there were no v0.2.x connectors)
            // and we'll just log an issue
            Err(error) => {
                return Ok(vec![DataConnectorIssue::InvalidNdcV01Version {
                    version: capabilities_version.to_owned(),
                    error,
                }]);
            }
        },
        NdcVersion::V02 => {
            capabilities_semver_result.map_err(|error| DataConnectorError::InvalidNdcVersion {
                version: capabilities_version.to_owned(),
                error,
            })?
        }
    };

    let requirement = match ndc_version {
        NdcVersion::V01 => semver::VersionReq::parse("^0.1.0").unwrap(),
        NdcVersion::V02 => semver::VersionReq::parse("^0.2.0").unwrap(),
    };

    if requirement.matches(&capabilities_semver) {
        Ok(vec![])
    } else {
        Err(DataConnectorError::IncompatibleNdcVersion {
            version: capabilities_version.to_owned(),
            requirement,
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
    /// HTTP response headers configuration that is forwarded from a NDC
    /// function/procedure to the client.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub response_config: Option<Arc<CommandsResponseConfig>>,
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
    ) -> Result<Self, NamedDataConnectorError> {
        let url = match context.url {
            DataConnectorUrl::SingleUrl(url) => ResolvedDataConnectorUrl::SingleUrl(
                SerializableUrl::new(&url.value).map_err(|error| NamedDataConnectorError {
                    data_connector_name: name.clone(),
                    error: DataConnectorError::InvalidDataConnectorUrl { error },
                })?,
            ),
            DataConnectorUrl::ReadWriteUrls(ReadWriteUrls { read, write }) => {
                ResolvedDataConnectorUrl::ReadWriteUrls(ResolvedReadWriteUrls {
                    read: SerializableUrl::new(&read.value).map_err(|error| {
                        NamedDataConnectorError {
                            data_connector_name: name.clone(),
                            error: DataConnectorError::InvalidDataConnectorUrl { error },
                        }
                    })?,
                    write: SerializableUrl::new(&write.value).map_err(|error| {
                        NamedDataConnectorError {
                            data_connector_name: name.clone(),
                            error: DataConnectorError::InvalidDataConnectorUrl { error },
                        }
                    })?,
                })
            }
        };
        let headers =
            SerializableHeaderMap::new(&context.headers).map_err(|e| NamedDataConnectorError {
                data_connector_name: name.clone(),
                error: to_error(e),
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
            supports_nested_array_filtering: context
                .capabilities
                .query
                .exists
                .nested_collections
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
            response_config: context.response_headers.clone().map(Arc::new),
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
    pub name: DataConnectorArgumentName,
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
        metadata_accessor: &MetadataAccessor,
        headers_preset: &open_dds::data_connector::HttpHeadersPreset,
    ) -> Result<Self, DataConnectorError> {
        let forward = headers_preset
            .forward
            .iter()
            .map(|header| SerializableHeaderName::new(header.to_string()).map_err(to_error))
            .collect::<Result<Vec<_>, DataConnectorError>>()?;

        let additional = headers_preset
            .additional
            .iter()
            .map(|(header_name, header_val)| {
                let key = SerializableHeaderName::new(header_name.to_string()).map_err(to_error)?;
                let val = resolve_value_expression(metadata_accessor, header_val.clone());
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
    metadata_accessor: &MetadataAccessor,
    value_expression_input: open_dds::permissions::ValueExpression,
) -> ValueExpression {
    match value_expression_input {
        open_dds::permissions::ValueExpression::SessionVariable(session_variable) => {
            ValueExpression::SessionVariable(hasura_authn_core::SessionVariableReference {
                name: session_variable,
                passed_as_json: metadata_accessor.flags.json_session_variables,
            })
        }
        open_dds::permissions::ValueExpression::Literal(json_value) => {
            ValueExpression::Literal(json_value)
        }
    }
}

fn to_error(err: HeaderError) -> DataConnectorError {
    match err {
        HeaderError::InvalidHeaderName { header_name } => {
            DataConnectorError::InvalidHeaderName { header_name }
        }
        HeaderError::InvalidHeaderValue { header_name } => {
            DataConnectorError::InvalidHeaderValue { header_name }
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
/// Configure how NDC functions/procedures should be processed to extract
/// headers and result
pub struct CommandsResponseConfig {
    pub headers_field: DataConnectorColumnName,
    pub result_field: DataConnectorColumnName,
    pub forward_headers: Vec<SerializableHeaderName>,
}

impl CommandsResponseConfig {
    fn new(
        response_headers: &open_dds::data_connector::ResponseHeaders,
    ) -> Result<Self, DataConnectorError> {
        let forward_headers = response_headers
            .forward_headers
            .iter()
            .map(|header| SerializableHeaderName::new(header.to_string()).map_err(to_error))
            .collect::<Result<Vec<_>, DataConnectorError>>()?;
        Ok(Self {
            headers_field: response_headers.headers_field.clone(),
            result_field: response_headers.result_field.clone(),
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
    pub supports_nested_array_filtering: bool,
}

#[cfg(test)]
mod tests {
    use ndc_models;
    use open_dds::{accessor::MetadataAccessor, data_connector::DataConnectorLinkV1, Metadata};
    use strum::IntoEnumIterator;

    use crate::{
        configuration::UnstableFeatures,
        data_connectors::{error::DataConnectorIssue, types::NdcVersion},
        stages::data_connectors::types::DataConnectorContext,
    };

    use super::validate_ndc_version;

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
    fn test_data_connector_context_v01_capabilities() {
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
            ), jsonpath::JSONPath::new())
            .unwrap();

        let explicit_capabilities: ndc_models::Capabilities =
            serde_json::from_value(serde_json::json!(
                { "query": { "nested_fields": {}, "exists": { "unrelated": {} } }, "mutation": {} }
            ))
            .unwrap();

        // With explicit capabilities specified, we should use them
        let unstable_features = UnstableFeatures {
            enable_ndc_v02_support: true,
            ..Default::default()
        };
        let metadata_accessor = MetadataAccessor::new(Metadata::WithoutNamespaces(vec![]));
        let (context, issues) = DataConnectorContext::new(
            &metadata_accessor,
            &data_connector_with_capabilities,
            &unstable_features,
        )
        .unwrap();
        assert_eq!(context.capabilities, explicit_capabilities);
        assert_eq!(context.supported_ndc_version, NdcVersion::V01);
        assert_eq!(issues.len(), 0, "Issues: {issues:#?}");
    }

    #[test]
    fn test_data_connector_context_v02_capabilities() {
        let data_connector_with_capabilities: DataConnectorLinkV1 =
            open_dds::traits::OpenDd::deserialize(serde_json::json!(
                {
                    "name": "foo",
                    "url": { "singleUrl": { "value": "http://test.com" } },
                    "schema": {
                        "version": "v0.2",
                        "capabilities": { "version": "0.2.0", "capabilities": { "query": { "nested_fields": {}, "exists": {} }, "mutation": {} }},
                        "schema": {
                            "scalar_types": {},
                            "object_types": {},
                            "collections": [],
                            "functions": [],
                            "procedures": []
                        }
                    }
                }
            ), jsonpath::JSONPath::new())
            .unwrap();

        let explicit_capabilities: ndc_models::Capabilities =
            serde_json::from_value(serde_json::json!(
                { "query": { "nested_fields": {}, "exists": {} }, "mutation": {} }
            ))
            .unwrap();

        // With explicit capabilities specified, we should use them
        let unstable_features = UnstableFeatures {
            enable_ndc_v02_support: true,
            ..Default::default()
        };
        let metadata_accessor = MetadataAccessor::new(Metadata::WithoutNamespaces(vec![]));
        let (context, issues) = DataConnectorContext::new(
            &metadata_accessor,
            &data_connector_with_capabilities,
            &unstable_features,
        )
        .unwrap();
        assert_eq!(context.capabilities, explicit_capabilities);
        assert_eq!(context.supported_ndc_version, NdcVersion::V02);
        assert_eq!(issues.len(), 0, "Issues: {issues:#?}");
    }

    enum AssertNdcVersionShould {
        Validate,
        HaveIssue,
        FailToValidate,
    }

    #[test]
    fn test_validate_ndc_version() {
        for ndc_version in NdcVersion::iter() {
            // This might seem like a convoluted way of writing the test cases,
            // but the match ensures that new versions that get added produce warnings
            // here that tell the developer to add more test cases here
            let test_cases = match ndc_version {
                NdcVersion::V01 => vec![
                    ("0.1.0", AssertNdcVersionShould::Validate),
                    ("0.1.1", AssertNdcVersionShould::Validate),
                    ("0.2.0", AssertNdcVersionShould::FailToValidate),
                    ("0.2.1", AssertNdcVersionShould::FailToValidate),
                    ("1.0.0", AssertNdcVersionShould::FailToValidate),
                    ("", AssertNdcVersionShould::HaveIssue),
                    ("*", AssertNdcVersionShould::HaveIssue),
                    ("^0.1.1", AssertNdcVersionShould::HaveIssue),
                ],
                NdcVersion::V02 => vec![
                    ("0.1.0", AssertNdcVersionShould::FailToValidate),
                    ("0.1.1", AssertNdcVersionShould::FailToValidate),
                    ("0.2.0", AssertNdcVersionShould::Validate),
                    ("0.2.1", AssertNdcVersionShould::Validate),
                    ("1.0.0", AssertNdcVersionShould::FailToValidate),
                    ("", AssertNdcVersionShould::FailToValidate),
                    ("*", AssertNdcVersionShould::FailToValidate),
                    ("^0.1.1", AssertNdcVersionShould::FailToValidate),
                ],
            };

            for (version, assertion) in test_cases {
                let result = validate_ndc_version(ndc_version, version);
                match assertion {
                    AssertNdcVersionShould::Validate => {
                        match result {
                            Ok(issues) => {
                                assert!(issues.is_empty(), "When testing ndc version {ndc_version:?}, the version '{version}' validated but had issues: {issues:#?}");
                            },
                            Err(error) => panic!("When testing ndc version {ndc_version:?}, the version '{version}' failed to validate: {error}"),
                        }
                    },
                    AssertNdcVersionShould::HaveIssue => {
                        match result {
                            Ok(issues) => {
                                assert!(issues.iter().any(|i| matches!(i, DataConnectorIssue::InvalidNdcV01Version { .. })), "When testing ndc version {ndc_version:?}, the version '{version}' validated but did not have the InvalidNdcV1Version issue. Issues: {issues:#?}");
                            },
                            Err(error) => panic!("When testing ndc version {ndc_version:?}, the version '{version}' failed to validate: {error}"),
                        }
                    },
                    AssertNdcVersionShould::FailToValidate => assert!(result.is_err(), "When testing ndc version {ndc_version:?}, the version '{version}' validated when it shouldn't have: {result:#?}"),
                }
            }
        }
    }
}
