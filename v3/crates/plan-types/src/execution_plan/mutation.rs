use crate::NdcRelationshipName;
use open_dds::{commands::ProcedureName, types::DataConnectorArgumentName};
use std::collections::BTreeMap;
use std::sync::Arc;

use super::arguments;
use super::field;
use super::relationships;

#[derive(Debug)]
pub struct MutationExecutionPlan {
    /// The name of a procedure
    pub procedure_name: ProcedureName,
    /// Any named procedure arguments
    pub procedure_arguments: BTreeMap<DataConnectorArgumentName, arguments::MutationArgument>,
    /// The fields to return from the result, or null to return everything
    pub procedure_fields: Option<field::NestedField>,
    /// Any relationships between collections involved in the query request
    pub collection_relationships: BTreeMap<NdcRelationshipName, relationships::Relationship>,
    /// The data connector used to fetch the data
    pub data_connector: Arc<metadata_resolve::DataConnectorLink>,
}
