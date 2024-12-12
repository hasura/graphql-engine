use crate::catalog::mem_table::MemTable;
use serde::{Deserialize, Serialize};

pub const UNSUPPORTED_MODELS: &str = "unsupported_models";
pub const UNSUPPORTED_COMMANDS: &str = "unsupported_commands";
pub const UNSUPPORTED_OBJECT_TYPES: &str = "unsupported_object_types";
pub const UNSUPPORTED_SCALARS: &str = "unsupported_scalars";
pub const UNSUPPORTED_OBJECT_TYPE_FIELDS: &str = "unsupported_object_type_fields";

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct UnsupportedModels {
    rows: Vec<UnsupportedModelsRow>,
}

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct UnsupportedCommands {
    rows: Vec<UnsupportedCommandsRow>,
}

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct UnsupportedObjectTypes {
    rows: Vec<UnsupportedObjectTypesRow>,
}

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct UnsupportedScalars {
    rows: Vec<UnsupportedScalarsRow>,
}

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct UnsupportedObjectTypeFields {
    rows: Vec<UnsupportedObjectTypeFieldsRow>,
}

// Row structs
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct UnsupportedModelsRow {
    subgraph: String,
    name: String,
    reason: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct UnsupportedCommandsRow {
    subgraph: String,
    name: String,
    reason: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct UnsupportedObjectTypesRow {
    subgraph: String,
    name: String,
    reason: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct UnsupportedScalarsRow {
    subgraph: String,
    name: String,
    reason: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct UnsupportedObjectTypeFieldsRow {
    subgraph: String,
    object: String,
    field_name: String,
    reason: String,
}

// Implementations for constructors
impl UnsupportedModels {
    pub(crate) fn new(rows: Vec<UnsupportedModelsRow>) -> Self {
        UnsupportedModels { rows }
    }
}

impl UnsupportedCommands {
    pub(crate) fn new(rows: Vec<UnsupportedCommandsRow>) -> Self {
        UnsupportedCommands { rows }
    }
}

impl UnsupportedObjectTypes {
    pub(crate) fn new(rows: Vec<UnsupportedObjectTypesRow>) -> Self {
        UnsupportedObjectTypes { rows }
    }
}

impl UnsupportedScalars {
    pub(crate) fn new(rows: Vec<UnsupportedScalarsRow>) -> Self {
        UnsupportedScalars { rows }
    }
}

impl UnsupportedObjectTypeFields {
    pub(crate) fn new(rows: Vec<UnsupportedObjectTypeFieldsRow>) -> Self {
        UnsupportedObjectTypeFields { rows }
    }
}

// Row constructors
impl UnsupportedModelsRow {
    pub(crate) fn new(subgraph: String, name: String, reason: String) -> Self {
        Self {
            subgraph,
            name,
            reason,
        }
    }
}

impl UnsupportedCommandsRow {
    pub(crate) fn new(subgraph: String, name: String, reason: String) -> Self {
        Self {
            subgraph,
            name,
            reason,
        }
    }
}

impl UnsupportedObjectTypesRow {
    pub(crate) fn new(subgraph: String, name: String, reason: String) -> Self {
        Self {
            subgraph,
            name,
            reason,
        }
    }
}

impl UnsupportedScalarsRow {
    pub(crate) fn new(subgraph: String, name: String, reason: String) -> Self {
        Self {
            subgraph,
            name,
            reason,
        }
    }
}

impl UnsupportedObjectTypeFieldsRow {
    pub(crate) fn new(
        subgraph: String,
        object: String,
        field_name: String,
        reason: String,
    ) -> Self {
        Self {
            subgraph,
            object,
            field_name,
            reason,
        }
    }
}

// Implementations for table providers
impl UnsupportedModels {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "subgraph",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.subgraph.clone()))
                    .collect(),
            ),
            (
                "name",
                false,
                self.rows.iter().map(|row| Some(row.name.clone())).collect(),
            ),
            (
                "reason",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.reason.clone()))
                    .collect(),
            ),
        ])
    }
}

impl UnsupportedCommands {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "subgraph",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.subgraph.clone()))
                    .collect(),
            ),
            (
                "name",
                false,
                self.rows.iter().map(|row| Some(row.name.clone())).collect(),
            ),
            (
                "reason",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.reason.clone()))
                    .collect(),
            ),
        ])
    }
}

impl UnsupportedObjectTypes {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "subgraph",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.subgraph.clone()))
                    .collect(),
            ),
            (
                "name",
                false,
                self.rows.iter().map(|row| Some(row.name.clone())).collect(),
            ),
            (
                "reason",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.reason.clone()))
                    .collect(),
            ),
        ])
    }
}

impl UnsupportedScalars {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "subgraph",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.subgraph.clone()))
                    .collect(),
            ),
            (
                "name",
                false,
                self.rows.iter().map(|row| Some(row.name.clone())).collect(),
            ),
            (
                "reason",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.reason.clone()))
                    .collect(),
            ),
        ])
    }
}

impl UnsupportedObjectTypeFields {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "subgraph",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.subgraph.clone()))
                    .collect(),
            ),
            (
                "object",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.object.clone()))
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
                "reason",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.reason.clone()))
                    .collect(),
            ),
        ])
    }
}
