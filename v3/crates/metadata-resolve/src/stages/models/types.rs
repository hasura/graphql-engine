use crate::helpers::types::NdcColumnForComparison;
use crate::stages::{data_connectors, object_types};
use crate::types::subgraph::{
    deserialize_qualified_btreemap, serialize_qualified_btreemap, ArgumentInfo, Qualified,
};

use indexmap::IndexMap;
use open_dds::aggregates::AggregateExpressionName;
use open_dds::data_connector::DataConnectorObjectType;
use open_dds::{
    arguments::ArgumentName,
    models::{ModelGraphQlDefinition, ModelName, OrderableField},
    types::{CustomTypeName, FieldName},
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelSource {
    pub data_connector: data_connectors::DataConnectorLink,
    pub collection: String,
    pub collection_type: DataConnectorObjectType,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub type_mappings: BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    pub argument_mappings: BTreeMap<ArgumentName, ConnectorArgumentName>,
    pub source_arguments: BTreeMap<ConnectorArgumentName, ndc_models::Type>,
}

pub struct ModelsOutput {
    pub models: IndexMap<Qualified<ModelName>, Model>,
    pub global_id_enabled_types: BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    pub apollo_federation_entity_enabled_types:
        BTreeMap<Qualified<CustomTypeName>, Option<Qualified<open_dds::models::ModelName>>>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Model {
    pub name: Qualified<ModelName>,
    pub data_type: Qualified<CustomTypeName>,
    pub type_fields: IndexMap<FieldName, object_types::FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub source: Option<ModelSource>,
    pub global_id_source: Option<NDCFieldSourceMapping>,
    pub apollo_federation_key_source: Option<NDCFieldSourceMapping>,
    pub orderable_fields: Vec<OrderableField>,
    pub raw: ModelRaw,
}

#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
    ref_cast::RefCast,
)]
/// The name of an argument as defined by a data connector
#[repr(transparent)]
pub struct ConnectorArgumentName(pub String);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelRaw {
    pub filter_expression_type: Option<Qualified<CustomTypeName>>,
    pub aggregate_expression: Option<Qualified<AggregateExpressionName>>,
    pub graphql: Option<ModelGraphQlDefinition>,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct NDCFieldSourceMapping {
    pub ndc_mapping: BTreeMap<FieldName, NdcColumnForComparison>,
}
