use crate::NdcFieldAlias;
use indexmap::IndexMap;
use nonempty::NonEmpty;
use open_dds::{
    aggregates::{DataConnectorAggregationFunctionName, DataConnectorExtractionFunctionName},
    data_connector::DataConnectorColumnName,
};
use serde::Serialize;
use std::hash::Hash;

#[derive(Debug, Serialize, Default, PartialEq, Clone, Eq)]
pub struct Grouping {
    pub aggregates: IndexMap<NdcFieldAlias, AggregateFieldSelection>,
    pub dimensions: IndexMap<NdcFieldAlias, Dimension>,
    pub limit: Option<u32>,
    pub offset: Option<u32>,
}

#[derive(Debug, Serialize, PartialEq, Clone, Eq, Hash)]
pub enum Dimension {
    Column {
        column_path: NonEmpty<DataConnectorColumnName>,
        extraction: Option<DataConnectorExtractionFunctionName>,
    },
}

/// IR that represents the selected fields of an output type.
#[derive(Debug, Serialize, Default, PartialEq, Clone, Eq)]
pub struct AggregateSelectionSet {
    // The fields in the selection set. They are stored in the form that would
    // be converted and sent over the wire. Serialized the map as ordered to
    // produce deterministic golden files.
    pub fields: IndexMap<NdcFieldAlias, AggregateFieldSelection>,
}

// FIXME: remove this; this is probably inaccurate.
// https://github.com/indexmap-rs/indexmap/issues/155
// Probably use ordermap (ref: https://github.com/indexmap-rs/indexmap/issues/67#issuecomment-2189801441)
impl Hash for AggregateSelectionSet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self.fields {
            k.hash(state);
            v.hash(state);
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Clone, Eq, Hash)]
pub enum AggregateFieldSelection {
    Count {
        column_path: Vec<DataConnectorColumnName>,
    },
    CountDistinct {
        column_path: Vec<DataConnectorColumnName>,
    },
    AggregationFunction {
        function_name: DataConnectorAggregationFunctionName,
        column_path: NonEmpty<DataConnectorColumnName>,
    },
}
