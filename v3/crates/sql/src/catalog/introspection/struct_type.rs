use datafusion::arrow::datatypes::DataType;
use serde::{Deserialize, Serialize};

use crate::catalog::{
    mem_table::MemTable,
    types::{display_data_type, NormalizedType, StructTypeName},
};

pub const STRUCT_TYPE: &str = "struct_type";
pub const STRUCT_TYPE_FIELD: &str = "struct_type_field";

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct StructTypes {
    rows: Vec<StructTypeRow>,
}

impl StructTypes {
    pub(crate) fn new(rows: Vec<StructTypeRow>) -> Self {
        StructTypes { rows }
    }
}

impl StructTypes {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.name.0.clone()))
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
pub(crate) struct StructTypeRow {
    name: StructTypeName,
    description: Option<String>,
}

impl StructTypeRow {
    pub(crate) fn new(name: StructTypeName, description: Option<String>) -> Self {
        Self { name, description }
    }
}

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct StructTypeFields {
    rows: Vec<StructTypeFieldRow>,
}

impl StructTypeFields {
    pub(crate) fn new(rows: Vec<StructTypeFieldRow>) -> Self {
        StructTypeFields { rows }
    }
}

impl StructTypeFields {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "struct_type_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.struct_type_name.0.clone()))
                    .collect(),
            ),
            (
                "field_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.field_name.clone()))
                    .collect(),
            ),
            (
                "field_type",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(display_data_type(&row.field_type)))
                    .collect(),
            ),
            (
                "field_type_normalized",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.field_type_normalized.to_string()))
                    .collect(),
            ),
            (
                "is_nullable",
                false,
                self.rows.iter().map(|row| Some(row.is_nullable)).collect(),
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
pub(crate) struct StructTypeFieldRow {
    struct_type_name: StructTypeName,
    field_name: String,
    field_type: DataType,
    field_type_normalized: NormalizedType,
    is_nullable: bool,
    description: Option<String>,
}

impl StructTypeFieldRow {
    pub(crate) fn new(
        struct_type_name: StructTypeName,
        field_name: String,
        field_type: DataType,
        field_type_normalized: NormalizedType,
        is_nullable: bool,
        description: Option<String>,
    ) -> Self {
        Self {
            struct_type_name,
            field_name,
            field_type,
            field_type_normalized,
            is_nullable,
            description,
        }
    }
}
