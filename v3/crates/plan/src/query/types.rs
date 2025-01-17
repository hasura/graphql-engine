use std::collections::BTreeMap;
use std::sync::Arc;

use open_dds::{data_connector::CollectionName, types::DataConnectorArgumentName};
use plan_types::{
    Argument, NdcRelationshipName, OrderByElement, Relationship, ResolvedFilterExpression,
};

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
