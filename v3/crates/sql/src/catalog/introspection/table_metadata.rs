use serde::{Deserialize, Serialize};

use crate::catalog::mem_table::MemTable;

pub const TABLE_METADATA: &str = "table_metadata";

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TableMetadata {
    rows: Vec<TableMetadataRow>,
}

impl TableMetadata {
    pub(crate) fn new(rows: Vec<TableMetadataRow>) -> Self {
        TableMetadata { rows }
    }
}

impl TableMetadata {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "schema_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.schema_name.clone()))
                    .collect(),
            ),
            (
                "table_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.table_name.clone()))
                    .collect(),
            ),
            (
                "description",
                true,
                self.rows
                    .iter()
                    .map(|row| row.description.clone())
                    .collect(),
            ),
        ])
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TableMetadataRow {
    schema_name: String,
    table_name: String,
    description: Option<String>,
}

impl TableMetadataRow {
    pub(crate) fn new(
        schema_name: String,
        table_name: String,
        description: Option<String>,
    ) -> Self {
        Self {
            schema_name,
            table_name,
            description,
        }
    }
}
