use std::sync::Arc;

use crate::data_connectors::ArgumentPresetValue;
use crate::helpers::argument::ArgumentMappingIssue;
use crate::helpers::types::NdcColumnForComparison;
use crate::stages::order_by_expressions::{OrderByExpressionIdentifier, OrderByExpressions};
use crate::stages::{data_connectors, object_types};
use crate::types::subgraph::{
    deserialize_qualified_btreemap, serialize_qualified_btreemap, ArgumentInfo, Qualified,
};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::data_connector::{CollectionName, DataConnectorName};
use open_dds::{
    aggregates::AggregateExpressionName,
    arguments::ArgumentName,
    data_connector::DataConnectorObjectType,
    models::{ModelGraphQlDefinitionV2, ModelName},
    types::{CustomTypeName, DataConnectorArgumentName, FieldName},
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelSource {
    pub data_connector: Arc<data_connectors::DataConnectorLink>,
    pub collection: CollectionName,
    pub collection_type: DataConnectorObjectType,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub type_mappings: BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    pub argument_mappings: BTreeMap<ArgumentName, DataConnectorArgumentName>,
    pub data_connector_link_argument_presets:
        BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    pub source_arguments: BTreeMap<DataConnectorArgumentName, ndc_models::Type>,
}

pub struct ModelsOutput {
    pub models: IndexMap<Qualified<ModelName>, Model>,
    pub global_id_enabled_types: BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    pub apollo_federation_entity_enabled_types:
        BTreeMap<Qualified<CustomTypeName>, Option<Qualified<open_dds::models::ModelName>>>,
    pub order_by_expressions: OrderByExpressions,
    pub graphql_types: BTreeSet<ast::TypeName>,
    pub issues: Vec<ModelsIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Model {
    pub path: jsonpath::JSONPath,
    pub name: Qualified<ModelName>,
    pub data_type: Qualified<CustomTypeName>,
    pub type_fields: IndexMap<FieldName, object_types::FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub source: Option<Arc<ModelSource>>, // wrapped in Arc because we include these in our `Plan`
    pub global_id_source: Option<NDCFieldSourceMapping>,
    pub apollo_federation_key_source: Option<NDCFieldSourceMapping>,
    pub order_by_expression: Option<Qualified<OrderByExpressionIdentifier>>,
    pub aggregate_expression: Option<Qualified<AggregateExpressionName>>,
    pub raw: ModelRaw,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelRaw {
    pub filter_expression_type: Option<Qualified<CustomTypeName>>,
    pub graphql: Option<ModelGraphQlDefinitionV2>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct NDCFieldSourceMapping {
    pub ndc_mapping: BTreeMap<FieldName, NdcColumnForComparison>,
}

#[derive(Debug, thiserror::Error)]
pub enum ModelsIssue {
    #[error("An issue occurred while mapping arguments in the model {model_name:} to the collection {collection_name:} in the data connector {data_connector_name:}: {issue:}")]
    FunctionArgumentMappingIssue {
        data_connector_name: Qualified<DataConnectorName>,
        model_name: Qualified<ModelName>,
        collection_name: CollectionName,
        issue: ArgumentMappingIssue,
    },
}
