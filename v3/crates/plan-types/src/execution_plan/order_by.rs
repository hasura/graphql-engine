use crate::NdcRelationshipName;
use open_dds::data_connector::DataConnectorColumnName;
use serde::Serialize;

#[derive(Serialize, Clone, Debug, PartialEq, Eq, Hash)]
pub enum OrderByDirection {
    Asc,
    Desc,
}

#[derive(Debug, Serialize, Clone, PartialEq, Eq, Hash)]
pub struct OrderByElement {
    pub order_direction: OrderByDirection,
    pub target: OrderByTarget,
}

#[derive(Debug, Serialize, Clone, PartialEq, Eq, Hash)]
pub enum OrderByTarget {
    Column {
        name: DataConnectorColumnName,
        field_path: Option<Vec<DataConnectorColumnName>>,
        relationship_path: Vec<NdcRelationshipName>,
    },
}
