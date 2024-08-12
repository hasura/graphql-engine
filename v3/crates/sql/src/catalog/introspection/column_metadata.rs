use serde::{Deserialize, Serialize};

use crate::catalog::mem_table::MemTable;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct ColumnMetadataRow {
    schema_name: String,
    table_name: String,
    column_name: String,
    description: Option<String>,
}

impl ColumnMetadataRow {
    pub(crate) fn new(
        schema_name: String,
        table_name: String,
        column_name: String,
        description: Option<String>,
    ) -> Self {
        Self {
            schema_name,
            table_name,
            column_name,
            description,
        }
    }
}

pub const COLUMN_METADATA: &str = "column_metadata";

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(super) struct ColumnMetadata {
    pub(crate) rows: Vec<ColumnMetadataRow>,
}

impl ColumnMetadata {
    pub(super) fn new(rows: Vec<ColumnMetadataRow>) -> Self {
        ColumnMetadata { rows }
    }
}

impl ColumnMetadata {
    pub(super) fn to_table_provider(&self) -> MemTable {
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
                "column_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.column_name.clone()))
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
