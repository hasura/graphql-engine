use metadata_resolve::{self, Qualified, serialize_qualified_btreemap};
use open_dds::{
    arguments::ArgumentName,
    commands::{CommandName, FunctionName},
    data_connector::DataConnectorColumnName,
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, DataConnectorArgumentName},
};
use serde::Serialize;
use std::collections::BTreeMap;

use crate::NdcRelationshipName;

#[derive(Debug, Serialize, Clone, PartialEq)]
pub struct LocalModelRelationshipInfo<'s> {
    pub relationship_name: &'s RelationshipName,
    pub relationship_type: &'s RelationshipType,
    pub source_type: &'s Qualified<CustomTypeName>,
    #[serde(serialize_with = "serialize_qualified_btreemap")]
    pub source_type_mappings:
        &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    pub target_model_name: &'s Qualified<ModelName>,
    pub target_source: &'s metadata_resolve::ModelSource,
    pub mappings: &'s Vec<metadata_resolve::RelationshipModelMapping>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RelationshipPathElement<TExpression> {
    pub field_path: Vec<DataConnectorColumnName>,
    pub relationship_name: NdcRelationshipName,
    pub filter_predicate: Option<TExpression>,
}

#[derive(Debug, Serialize, Clone, PartialEq)]
pub struct LocalCommandRelationshipInfo<'s> {
    pub relationship_name: &'s RelationshipName,
    pub source_type: &'s Qualified<CustomTypeName>,
    pub source_type_mappings:
        &'s BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
    pub command_name: &'s Qualified<CommandName>,
    pub argument_mappings: &'s BTreeMap<ArgumentName, DataConnectorArgumentName>,
    pub function_name: &'s FunctionName,
    pub mappings: &'s Vec<metadata_resolve::RelationshipCommandMapping>,
}
