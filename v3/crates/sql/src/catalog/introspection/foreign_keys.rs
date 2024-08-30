use serde::{Deserialize, Serialize};

use crate::catalog::mem_table::MemTable;

pub(super) const INFERRED_FOREIGN_KEY_CONSTRAINTS: &str = "inferred_foreign_key_constraints";

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(super) struct InferredForeignKeysRow {
    from_schema_name: String,
    from_table_name: String,
    from_column_name: String,
    to_schema_name: String,
    to_table_name: String,
    to_column_name: String,
}

impl InferredForeignKeysRow {
    pub(super) fn new(
        from_schema_name: String,
        from_table_name: String,
        from_column_name: String,
        to_schema_name: String,
        to_table_name: String,
        to_column_name: String,
    ) -> Self {
        Self {
            from_schema_name,
            from_table_name,
            from_column_name,
            to_schema_name,
            to_table_name,
            to_column_name,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct InferredForeignKeys {
    rows: Vec<InferredForeignKeysRow>,
}

impl InferredForeignKeys {
    pub(super) fn new(rows: Vec<InferredForeignKeysRow>) -> Self {
        InferredForeignKeys { rows }
    }
}

impl InferredForeignKeys {
    pub(super) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "from_schema_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.from_schema_name.clone()))
                    .collect(),
            ),
            (
                "from_table_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.from_table_name.clone()))
                    .collect(),
            ),
            (
                "from_column_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.from_column_name.clone()))
                    .collect(),
            ),
            (
                "to_schema_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.to_schema_name.clone()))
                    .collect(),
            ),
            (
                "to_table_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.to_table_name.clone()))
                    .collect(),
            ),
            (
                "to_column_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.to_column_name.clone()))
                    .collect(),
            ),
        ])
    }
}
