use open_dds::data_connector::DataConnectorColumnName;
use serde::Serialize;

use super::relationships::RelationshipPathElement;

#[derive(Serialize, Clone, Debug, PartialEq, Eq)]
pub enum OrderByDirection {
    Asc,
    Desc,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct OrderByElement<TExpression> {
    pub order_direction: OrderByDirection,
    pub target: OrderByTarget<TExpression>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum OrderByTarget<TExpression> {
    Column {
        relationship_path: Vec<RelationshipPathElement<TExpression>>,
        name: DataConnectorColumnName,
        field_path: Vec<DataConnectorColumnName>,
    },
}
