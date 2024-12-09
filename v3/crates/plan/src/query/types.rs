use std::collections::BTreeMap;
use std::sync::Arc;

use metadata_resolve::Qualified;
use open_dds::{
    commands::{FunctionName, ProcedureName},
    data_connector::CollectionName,
    types::{CustomTypeName, DataConnectorArgumentName},
};
use plan_types::{
    Argument, Field, NdcFieldAlias, NdcRelationshipName, NestedField, OrderByElement, Relationship,
    ResolvedFilterExpression,
};

use indexmap::IndexMap;

// additional query context which is helpful when processing the response afterwards
pub struct QueryContext {
    pub type_name: Qualified<CustomTypeName>,
}

#[derive(Debug, Clone)]
// intermediate type constructed whilst planning a model selection
pub struct NDCQuery {
    pub collection_name: CollectionName,
    pub arguments: BTreeMap<DataConnectorArgumentName, Argument>,
    pub filter: Option<ResolvedFilterExpression>,
    pub order_by: Vec<OrderByElement<ResolvedFilterExpression>>,
    pub limit: Option<u32>,
    pub offset: Option<u32>,
    pub collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
}

#[derive(Debug, Clone)]
pub struct NDCFunction {
    pub function_name: FunctionName,
    pub fields: IndexMap<NdcFieldAlias, Field>,
    pub arguments: BTreeMap<DataConnectorArgumentName, serde_json::Value>,
    pub collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
}

#[derive(Debug, Clone)]
pub struct NDCProcedure {
    pub procedure_name: ProcedureName,
    pub arguments: BTreeMap<DataConnectorArgumentName, serde_json::Value>,
    pub fields: Option<NestedField>,
    pub collection_relationships: BTreeMap<NdcRelationshipName, Relationship>,
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
}
