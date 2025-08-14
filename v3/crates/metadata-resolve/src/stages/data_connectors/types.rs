use super::error::{
    DataConnectorError, DataConnectorIssue, NamedDataConnectorError, NamedDataConnectorIssue,
};
use crate::helpers::http::{
    HeaderError, SerializableHeaderMap, SerializableHeaderName, SerializableUrl,
};
use crate::helpers::ndc_validation::validate_ndc_argument_presets;
use crate::ndc_migration;
use crate::types::permission::{ValueExpression, resolve_value_expression};
use crate::types::subgraph::Qualified;
use indexmap::IndexMap;
use lang_graphql::ast::common::OperationType;
use ndc_models;
use open_dds::accessor::MetadataAccessor;
use open_dds::data_connector::{DataConnectorColumnName, DataConnectorScalarType};
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
    pub capabilities: DataConnectorCapabilities,
    pub argument_presets: Vec<ArgumentPreset>,
    pub response_headers: Option<CommandsResponseConfig>,
}

impl<'a> DataConnectorContext<'a> {
    pub fn new(
        metadata_accessor: &MetadataAccessor,
        data_connector: &'a data_connector::DataConnectorLinkV1,
    ) -> Result<(Self, Vec<DataConnectorIssue>), DataConnectorError> {
        let (resolved_schema, capabilities, issues) = match &data_connector.schema {
            VersionedSchemaAndCapabilities::V01(schema_and_capabilities) => {
                let issues = validate_ndc_version(
                    NdcVersion::V01,
                    &schema_and_capabilities.capabilities.version,
                )?;
                let schema =
                    DataConnectorSchema::new(ndc_migration::v02::migrate_schema_response_from_v01(
                        schema_and_capabilities.schema.clone(),
                    ));
                let capabilities =
                    mk_ndc_01_capabilities(&schema_and_capabilities.capabilities.capabilities);
                (schema, capabilities, issues)
            }
            VersionedSchemaAndCapabilities::V02(schema_and_capabilities) => {
                let issues = validate_ndc_version(
                    NdcVersion::V02,
                    &schema_and_capabilities.capabilities.version,
                )?;
                let schema = DataConnectorSchema::new(schema_and_capabilities.schema.clone());
                let capabilities = mk_ndc_02_capabilities(
                    &schema_and_capabilities.capabilities.capabilities,
                    schema_and_capabilities.schema.capabilities.as_ref(),
                );
                (schema, capabilities, issues)
            }
        };

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
#[derive(Debug, Clone)]
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
        Ok(Self {
            name,
            url,
            headers,
            capabilities: context.capabilities.clone(),
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
                let val = resolve_value_expression(&metadata_accessor.flags, header_val.clone());
                Ok((key, val))
            })
            .collect::<Result<IndexMap<_, _>, DataConnectorError>>()?;

        Ok(Self {
            forward,
            additional,
        })
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

    /// Whether not explaining queries is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_explaining_queries: bool,

    /// Whether not explaining mutations is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_explaining_mutations: bool,

    /// Whether not filtering by nested object fields is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_object_filtering: bool,

    /// Whether not ordering by nested object fields is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_object_ordering: bool,

    /// Whether not filtering using 'exists' over nested object arrays is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_object_array_filtering: bool,

    /// Whether not filtering using 'exists' over nested scalar arrays is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_scalar_array_filtering: bool,

    /// Whether or not aggregates are supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_aggregates: Option<DataConnectorAggregateCapabilities>,

    /// Whether or not query variables are supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_query_variables: bool,

    /// Whether or not relationships are supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_relationships: Option<DataConnectorRelationshipCapabilities>,

    /// Whether or not relational queries are supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_relational_queries: Option<DataConnectorRelationalQueryCapabilities>,

    /// Whether or not relational mutations are supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_relational_mutations: Option<DataConnectorRelationalMutationCapabilities>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorAggregateCapabilities {
    /// Whether not aggregating over nested object fields is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_object_aggregations: bool,

    /// The scalar type used for any count aggregates. Optional because NDC 0.1.x did not specify this.
    /// If unspecified, one should assume that it will be something that has an integer representation.
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub aggregate_count_scalar_type: Option<DataConnectorScalarType>,

    /// Whether or not grouping operations are supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_grouping: Option<DataConnectorGroupingCapabilities>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorGroupingCapabilities {
    /// Whether not paginating groups is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_pagination: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationshipCapabilities {
    /// Whether or not filters can compare a column against another column that is across a relationship
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_relation_comparisons: bool,

    /// Whether or not relationships can start from or end with columns in nested objects. Implies support in field selection.
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_relationships: Option<DataConnectorNestedRelationshipCapabilities>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorNestedRelationshipCapabilities {
    /// Whether or not relationships can start from columns inside nested objects inside nested arrays
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_array_selection: bool,

    /// Whether or not nested relationships can be used in filtering
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_in_filtering: bool,

    /// Whether or not nested relationships can be used in ordering
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nested_in_ordering: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalQueryCapabilities {
    pub supports_project: DataConnectorRelationalProjectionCapabilities,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_filter: Option<DataConnectorRelationalExpressionCapabilities>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_sort: Option<DataConnectorRelationalSortCapabilities>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_join: Option<DataConnectorRelationalJoinCapabilities>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_aggregate: Option<DataConnectorRelationalAggregateCapabilities>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_window: Option<DataConnectorRelationalWindowCapabilities>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_union: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalProjectionCapabilities {
    pub expression_capabilities: DataConnectorRelationalExpressionCapabilities,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalSortCapabilities {
    pub expression_capabilities: DataConnectorRelationalExpressionCapabilities,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalJoinCapabilities {
    pub expression_capabilities: DataConnectorRelationalExpressionCapabilities,

    pub supports_join_types: DataConnectorRelationalJoinTypeCapabilities,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalJoinTypeCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_left: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_right: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_inner: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_full: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_left_semi: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_left_anti: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_right_semi: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_right_anti: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalAggregateCapabilities {
    pub expression_capabilities: DataConnectorRelationalExpressionCapabilities,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_group_by: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalWindowCapabilities {
    pub expression_capabilities: DataConnectorRelationalExpressionCapabilities,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalExpressionCapabilities {
    pub supports_conditional: DataConnectorRelationalConditionalExpressionCapabilities,

    pub supports_comparison: DataConnectorRelationalComparisonExpressionCapabilities,

    pub supports_scalar: DataConnectorRelationalScalarExpressionCapabilities,

    pub supports_aggregate: DataConnectorRelationalAggregateExpressionCapabilities,

    pub supports_window: DataConnectorRelationalWindowExpressionCapabilities,

    pub supports_scalar_types: Option<DataConnectorRelationalScalarTypeCapabilities>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalScalarTypeCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_interval: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_from_type: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalCastCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_from_type: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalCaseExpressionCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_scrutinee: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalConditionalExpressionCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_case: Option<DataConnectorRelationalCaseExpressionCapabilities>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nullif: bool,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalComparisonExpressionCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_like: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_ilike: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_between: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_contains: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_is_nan: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_is_zero: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_greater_than_eq: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_greater_than: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_in_list: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_is_false: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_is_null: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_is_true: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_less_than: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_less_than_eq: bool,

    /// Whether the is distinct from comparison is supported
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_is_distinct_from: bool,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalScalarExpressionCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_abs: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_array_element: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_btrim: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_ceil: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_character_length: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_concat: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_binary_concat: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_cos: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_current_date: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_current_time: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_current_timestamp: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_date_part: Option<DatePartScalarExpressionCapability>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_date_trunc: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_exp: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_floor: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_get_field: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_greatest: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_least: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_left: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_ln: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_log: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_log10: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_log2: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_lpad: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_ltrim: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nvl: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_power: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_random: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_replace: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_reverse: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_right: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_round: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_rpad: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_rtrim: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_sqrt: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_str_pos: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_substr: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_substr_index: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_tan: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_to_date: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_to_timestamp: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_trunc: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_to_lower: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_to_upper: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_and: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_coalesce: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_divide: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_minus: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_modulo: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_multiply: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_negate: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_not: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_or: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_plus: bool,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DatePartScalarExpressionCapability {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_year: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_quarter: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_month: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_week: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_day_of_week: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_day_of_year: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_day: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_hour: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_minute: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_second: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_microsecond: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_millisecond: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_nanosecond: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_epoch: bool,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalAggregateExpressionCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_bool_and: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_bool_or: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_count: Option<DataConnectorRelationalAggregateFunctionCapabilities>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_first_value: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_last_value: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_median: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_string_agg: Option<DataConnectorRelationalOrderedAggregateFunctionCapabilities>,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_var: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_avg: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_sum: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_min: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_max: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_stddev: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_stddev_pop: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_approx_percentile_cont: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_approx_distinct: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_array_agg: Option<DataConnectorRelationalOrderedAggregateFunctionCapabilities>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalAggregateFunctionCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_distinct: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalOrderedAggregateFunctionCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_distinct: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_order_by: bool,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalWindowExpressionCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_row_number: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_dense_rank: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_ntile: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_rank: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_cume_dist: bool,

    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_percent_rank: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct DataConnectorRelationalMutationCapabilities {
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_insert: bool,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_update: bool,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub supports_delete: bool,
}

fn mk_ndc_01_capabilities(
    capabilities: &ndc_models_v01::Capabilities,
) -> DataConnectorCapabilities {
    DataConnectorCapabilities {
        supported_ndc_version: NdcVersion::V01,
        supports_explaining_queries: capabilities.query.explain.is_some(),
        supports_explaining_mutations: capabilities.mutation.explain.is_some(),
        supports_nested_object_filtering: capabilities.query.nested_fields.filter_by.is_some(),
        supports_nested_object_ordering: capabilities.query.nested_fields.order_by.is_some(),
        supports_nested_object_array_filtering: capabilities
            .query
            .exists
            .nested_collections
            .is_some(),
        // Filtering by nested scalar arrays is not supported in NDC 0.1.x
        supports_nested_scalar_array_filtering: false,
        supports_aggregates: capabilities.query.aggregates.as_ref().map(|_agg| {
            DataConnectorAggregateCapabilities {
                supports_nested_object_aggregations: capabilities
                    .query
                    .nested_fields
                    .aggregates
                    .is_some(),
                aggregate_count_scalar_type: None,
                supports_grouping: None,
            }
        }),
        supports_query_variables: capabilities.query.variables.is_some(),
        supports_relationships: capabilities.relationships.as_ref().map(|rel| {
            DataConnectorRelationshipCapabilities {
                supports_relation_comparisons: rel.relation_comparisons.is_some(),
                // Selection of nested relationships is assumed supported in NDC 0.1.x
                supports_nested_relationships: Some(DataConnectorNestedRelationshipCapabilities {
                    supports_nested_array_selection: true,
                    // Selection of nested filtering/ordering is not supported in NDC 0.1.x
                    supports_nested_in_filtering: false,
                    supports_nested_in_ordering: false,
                }),
            }
        }),
        supports_relational_queries: None,
        supports_relational_mutations: None, // v0.1.x did not have relational mutations
    }
}

fn mk_ndc_02_capabilities(
    capabilities: &ndc_models::Capabilities,
    schema_capabilities: Option<&ndc_models::CapabilitySchemaInfo>,
) -> DataConnectorCapabilities {
    DataConnectorCapabilities {
        supported_ndc_version: NdcVersion::V02,
        supports_explaining_queries: capabilities.query.explain.is_some(),
        supports_explaining_mutations: capabilities.mutation.explain.is_some(),
        supports_nested_object_filtering: capabilities.query.nested_fields.filter_by.is_some(),
        supports_nested_object_ordering: capabilities.query.nested_fields.order_by.is_some(),
        supports_nested_object_array_filtering: capabilities
            .query
            .exists
            .nested_collections
            .is_some(),
        supports_nested_scalar_array_filtering: capabilities
            .query
            .exists
            .nested_scalar_collections
            .is_some(),
        supports_aggregates: capabilities.query.aggregates.as_ref().map(|aggregates| {
            DataConnectorAggregateCapabilities {
                supports_nested_object_aggregations: capabilities
                    .query
                    .nested_fields
                    .aggregates
                    .is_some(),

                aggregate_count_scalar_type: schema_capabilities
                    .and_then(|capabilities| capabilities.query.as_ref())
                    .and_then(|query_capabilities| query_capabilities.aggregates.as_ref())
                    .map(|aggregate_capabilities| {
                        DataConnectorScalarType::from(
                            aggregate_capabilities.count_scalar_type.as_str(),
                        )
                    }),

                supports_grouping: aggregates.group_by.as_ref().map(|groupby_capabilities| {
                    DataConnectorGroupingCapabilities {
                        supports_pagination: groupby_capabilities.paginate.is_some(),
                    }
                }),
            }
        }),
        supports_query_variables: capabilities.query.variables.is_some(),
        supports_relationships: capabilities.relationships.as_ref().map(|rel| {
            DataConnectorRelationshipCapabilities {
                supports_relation_comparisons: rel.relation_comparisons.is_some(),
                supports_nested_relationships: rel.nested.as_ref().map(|n| {
                    DataConnectorNestedRelationshipCapabilities {
                        supports_nested_array_selection: n.array.is_some(),
                        supports_nested_in_filtering: n.filtering.is_some(),
                        supports_nested_in_ordering: n.ordering.is_some(),
                    }
                }),
            }
        }),
        supports_relational_queries: capabilities.relational_query.as_ref().map(|r| {
            DataConnectorRelationalQueryCapabilities {
                supports_project: DataConnectorRelationalProjectionCapabilities {
                    expression_capabilities: mk_relational_expression_capabilities(
                        &r.project.expression,
                    ),
                },
                supports_filter: r.filter.as_ref().map(mk_relational_expression_capabilities),
                supports_sort: r
                    .sort
                    .as_ref()
                    .map(|c| DataConnectorRelationalSortCapabilities {
                        expression_capabilities: mk_relational_expression_capabilities(
                            &c.expression,
                        ),
                    }),
                supports_join: r
                    .join
                    .as_ref()
                    .map(|c| DataConnectorRelationalJoinCapabilities {
                        expression_capabilities: mk_relational_expression_capabilities(
                            &c.expression,
                        ),
                        supports_join_types: DataConnectorRelationalJoinTypeCapabilities {
                            supports_left: c.join_types.left.is_some(),
                            supports_right: c.join_types.right.is_some(),
                            supports_inner: c.join_types.inner.is_some(),
                            supports_full: c.join_types.full.is_some(),
                            supports_left_semi: c.join_types.left_semi.is_some(),
                            supports_left_anti: c.join_types.left_anti.is_some(),
                            supports_right_semi: c.join_types.right_semi.is_some(),
                            supports_right_anti: c.join_types.right_anti.is_some(),
                        },
                    }),
                supports_aggregate: r.aggregate.as_ref().map(|c| {
                    DataConnectorRelationalAggregateCapabilities {
                        expression_capabilities: mk_relational_expression_capabilities(
                            &c.expression,
                        ),
                        supports_group_by: c.group_by.is_some(),
                    }
                }),
                supports_window: r.window.as_ref().map(|c| {
                    DataConnectorRelationalWindowCapabilities {
                        expression_capabilities: mk_relational_expression_capabilities(
                            &c.expression,
                        ),
                    }
                }),
                supports_union: r.union.is_some(),
            }
        }),
        supports_relational_mutations: capabilities.relational_mutation.as_ref().map(|r| {
            DataConnectorRelationalMutationCapabilities {
                supports_insert: r.insert.is_some(),
                supports_update: r.update.is_some(),
                supports_delete: r.delete.is_some(),
            }
        }),
    }
}

fn mk_relational_expression_capabilities(
    capabilities: &ndc_models::RelationalExpressionCapabilities,
) -> DataConnectorRelationalExpressionCapabilities {
    let data_connector_relational_expression_capabilities =
        DataConnectorRelationalExpressionCapabilities {
            supports_conditional: DataConnectorRelationalConditionalExpressionCapabilities {
                supports_case: capabilities.conditional.case.as_ref().map(|c| {
                    DataConnectorRelationalCaseExpressionCapabilities {
                        supports_scrutinee: c.scrutinee.is_some(),
                    }
                }),
                supports_nullif: capabilities.conditional.nullif.is_some(),
            },
            supports_comparison: DataConnectorRelationalComparisonExpressionCapabilities {
                supports_like: capabilities.comparison.like.is_some(),
                supports_ilike: capabilities.comparison.ilike.is_some(),
                supports_between: capabilities.comparison.between.is_some(),
                supports_contains: capabilities.comparison.contains.is_some(),
                supports_is_distinct_from: capabilities.comparison.is_distinct_from.is_some(),
                supports_is_nan: capabilities.comparison.is_nan.is_some(),
                supports_is_zero: capabilities.comparison.is_zero.is_some(),
                supports_greater_than_eq: capabilities.comparison.greater_than_eq.is_some(),
                supports_greater_than: capabilities.comparison.greater_than.is_some(),
                supports_in_list: capabilities.comparison.in_list.is_some(),
                supports_is_false: capabilities.comparison.is_false.is_some(),
                supports_is_null: capabilities.comparison.is_null.is_some(),
                supports_is_true: capabilities.comparison.is_true.is_some(),
                supports_less_than: capabilities.comparison.less_than.is_some(),
                supports_less_than_eq: capabilities.comparison.less_than.is_some(),
            },
            supports_scalar: DataConnectorRelationalScalarExpressionCapabilities {
                supports_abs: capabilities.scalar.abs.is_some(),
                supports_array_element: capabilities.scalar.array_element.is_some(),
                supports_binary_concat: capabilities.scalar.binary_concat.is_some(),
                supports_btrim: capabilities.scalar.btrim.is_some(),
                supports_ceil: capabilities.scalar.ceil.is_some(),
                supports_character_length: capabilities.scalar.character_length.is_some(),
                supports_concat: capabilities.scalar.concat.is_some(),
                supports_cos: capabilities.scalar.cos.is_some(),
                supports_current_date: capabilities.scalar.current_date.is_some(),
                supports_current_time: capabilities.scalar.current_time.is_some(),
                supports_current_timestamp: capabilities.scalar.current_timestamp.is_some(),
                supports_date_part: capabilities.scalar.date_part.as_ref().map(|c| {
                    DatePartScalarExpressionCapability {
                        supports_year: c.year.is_some(),
                        supports_quarter: c.quarter.is_some(),
                        supports_month: c.month.is_some(),
                        supports_week: c.week.is_some(),
                        supports_day_of_week: c.day_of_week.is_some(),
                        supports_day_of_year: c.day_of_year.is_some(),
                        supports_day: c.day.is_some(),
                        supports_hour: c.hour.is_some(),
                        supports_minute: c.minute.is_some(),
                        supports_second: c.second.is_some(),
                        supports_microsecond: c.microsecond.is_some(),
                        supports_millisecond: c.millisecond.is_some(),
                        supports_nanosecond: c.nanosecond.is_some(),
                        supports_epoch: c.epoch.is_some(),
                    }
                }),
                supports_date_trunc: capabilities.scalar.date_trunc.is_some(),
                supports_exp: capabilities.scalar.exp.is_some(),
                supports_floor: capabilities.scalar.floor.is_some(),
                supports_get_field: capabilities.scalar.get_field.is_some(),
                supports_greatest: capabilities.scalar.greatest.is_some(),
                supports_least: capabilities.scalar.least.is_some(),
                supports_left: capabilities.scalar.left.is_some(),
                supports_ln: capabilities.scalar.ln.is_some(),
                supports_log: capabilities.scalar.log.is_some(),
                supports_log10: capabilities.scalar.log10.is_some(),
                supports_log2: capabilities.scalar.log2.is_some(),
                supports_lpad: capabilities.scalar.lpad.is_some(),
                supports_ltrim: capabilities.scalar.ltrim.is_some(),
                supports_nvl: capabilities.scalar.nvl.is_some(),
                supports_power: capabilities.scalar.power.is_some(),
                supports_random: capabilities.scalar.random.is_some(),
                supports_replace: capabilities.scalar.replace.is_some(),
                supports_reverse: capabilities.scalar.reverse.is_some(),
                supports_right: capabilities.scalar.right.is_some(),
                supports_round: capabilities.scalar.round.is_some(),
                supports_rpad: capabilities.scalar.rpad.is_some(),
                supports_rtrim: capabilities.scalar.rtrim.is_some(),
                supports_sqrt: capabilities.scalar.sqrt.is_some(),
                supports_str_pos: capabilities.scalar.str_pos.is_some(),
                supports_substr: capabilities.scalar.substr.is_some(),
                supports_substr_index: capabilities.scalar.substr_index.is_some(),
                supports_tan: capabilities.scalar.tan.is_some(),
                supports_to_date: capabilities.scalar.to_date.is_some(),
                supports_to_timestamp: capabilities.scalar.to_timestamp.is_some(),
                supports_trunc: capabilities.scalar.trunc.is_some(),
                supports_to_lower: capabilities.scalar.to_lower.is_some(),
                supports_to_upper: capabilities.scalar.to_upper.is_some(),
                supports_and: capabilities.scalar.and.is_some(),
                supports_coalesce: capabilities.scalar.coalesce.is_some(),
                supports_divide: capabilities.scalar.divide.is_some(),
                supports_minus: capabilities.scalar.minus.is_some(),
                supports_modulo: capabilities.scalar.modulo.is_some(),
                supports_multiply: capabilities.scalar.multiply.is_some(),
                supports_negate: capabilities.scalar.negate.is_some(),
                supports_not: capabilities.scalar.not.is_some(),
                supports_or: capabilities.scalar.or.is_some(),
                supports_plus: capabilities.scalar.plus.is_some(),
            },
            supports_aggregate: DataConnectorRelationalAggregateExpressionCapabilities {
                supports_bool_and: capabilities.aggregate.bool_and.is_some(),
                supports_bool_or: capabilities.aggregate.bool_or.is_some(),
                supports_count: capabilities.aggregate.count.as_ref().map(|c| {
                    DataConnectorRelationalAggregateFunctionCapabilities {
                        supports_distinct: c.distinct.is_some(),
                    }
                }),
                supports_first_value: capabilities.aggregate.first_value.is_some(),
                supports_last_value: capabilities.aggregate.last_value.is_some(),
                supports_median: capabilities.aggregate.median.is_some(),
                supports_string_agg: capabilities
                    .aggregate
                    .string_agg_with_separator
                    .as_ref()
                    .map(
                        |c| DataConnectorRelationalOrderedAggregateFunctionCapabilities {
                            supports_distinct: c.distinct.is_some(),
                            supports_order_by: c.order_by.is_some(),
                        },
                    ),
                supports_var: capabilities.aggregate.var.is_some(),
                supports_avg: capabilities.aggregate.avg.is_some(),
                supports_sum: capabilities.aggregate.sum.is_some(),
                supports_min: capabilities.aggregate.min.is_some(),
                supports_max: capabilities.aggregate.max.is_some(),
                supports_stddev: capabilities.aggregate.stddev.is_some(),
                supports_stddev_pop: capabilities.aggregate.stddev_pop.is_some(),
                supports_approx_percentile_cont: capabilities
                    .aggregate
                    .approx_percentile_cont
                    .is_some(),
                supports_approx_distinct: capabilities.aggregate.approx_distinct.is_some(),
                supports_array_agg: capabilities.aggregate.array_agg.as_ref().map(|c| {
                    DataConnectorRelationalOrderedAggregateFunctionCapabilities {
                        supports_distinct: c.distinct.is_some(),
                        supports_order_by: c.order_by.is_some(),
                    }
                }),
            },
            supports_window: DataConnectorRelationalWindowExpressionCapabilities {
                supports_row_number: capabilities.window.row_number.is_some(),
                supports_dense_rank: capabilities.window.dense_rank.is_some(),
                supports_ntile: capabilities.window.ntile.is_some(),
                supports_rank: capabilities.window.rank.is_some(),
                supports_cume_dist: capabilities.window.cume_dist.is_some(),
                supports_percent_rank: capabilities.window.percent_rank.is_some(),
            },
            supports_scalar_types: capabilities.scalar_types.as_ref().map(|scalar_types| {
                DataConnectorRelationalScalarTypeCapabilities {
                    supports_interval: scalar_types.interval.is_some(),
                    supports_from_type: scalar_types.from_type.is_some(),
                }
            }),
        };
    data_connector_relational_expression_capabilities
}

#[cfg(test)]
mod tests {
    use open_dds::{Metadata, accessor::MetadataAccessor, data_connector::DataConnectorLinkV1};
    use strum::IntoEnumIterator;

    use crate::{
        data_connectors::{
            DataConnectorCapabilities, error::DataConnectorIssue, types::NdcVersion,
        },
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

        let data_connector_capabilities = DataConnectorCapabilities {
            supported_ndc_version: NdcVersion::V01,
            supports_explaining_queries: false,
            supports_explaining_mutations: false,
            supports_nested_object_filtering: false,
            supports_nested_object_ordering: false,
            supports_nested_object_array_filtering: false,
            supports_nested_scalar_array_filtering: false,
            supports_aggregates: None,
            supports_query_variables: false,
            supports_relationships: None,
            supports_relational_queries: None,
            supports_relational_mutations: None,
        };

        // With explicit capabilities specified, we should use them
        let metadata_accessor = MetadataAccessor::new(Metadata::WithoutNamespaces(vec![]));
        let (context, issues) =
            DataConnectorContext::new(&metadata_accessor, &data_connector_with_capabilities)
                .unwrap();
        assert_eq!(context.capabilities, data_connector_capabilities);
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

        let data_connector_capabilities = DataConnectorCapabilities {
            supported_ndc_version: NdcVersion::V02,
            supports_explaining_queries: false,
            supports_explaining_mutations: false,
            supports_nested_object_filtering: false,
            supports_nested_object_ordering: false,
            supports_nested_object_array_filtering: false,
            supports_nested_scalar_array_filtering: false,
            supports_aggregates: None,
            supports_query_variables: false,
            supports_relationships: None,
            supports_relational_queries: None,
            supports_relational_mutations: None,
        };

        // With explicit capabilities specified, we should use them
        let metadata_accessor = MetadataAccessor::new(Metadata::WithoutNamespaces(vec![]));
        let (context, issues) =
            DataConnectorContext::new(&metadata_accessor, &data_connector_with_capabilities)
                .unwrap();
        assert_eq!(context.capabilities, data_connector_capabilities);
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
                    AssertNdcVersionShould::Validate => match result {
                        Ok(issues) => {
                            assert!(
                                issues.is_empty(),
                                "When testing ndc version {ndc_version:?}, the version '{version}' validated but had issues: {issues:#?}"
                            );
                        }
                        Err(error) => panic!(
                            "When testing ndc version {ndc_version:?}, the version '{version}' failed to validate: {error}"
                        ),
                    },
                    AssertNdcVersionShould::HaveIssue => match result {
                        Ok(issues) => {
                            assert!(
                                issues.iter().any(|i| matches!(
                                    i,
                                    DataConnectorIssue::InvalidNdcV01Version { .. }
                                )),
                                "When testing ndc version {ndc_version:?}, the version '{version}' validated but did not have the InvalidNdcV1Version issue. Issues: {issues:#?}"
                            );
                        }
                        Err(error) => panic!(
                            "When testing ndc version {ndc_version:?}, the version '{version}' failed to validate: {error}"
                        ),
                    },
                    AssertNdcVersionShould::FailToValidate => assert!(
                        result.is_err(),
                        "When testing ndc version {ndc_version:?}, the version '{version}' validated when it shouldn't have: {result:#?}"
                    ),
                }
            }
        }
    }
}
