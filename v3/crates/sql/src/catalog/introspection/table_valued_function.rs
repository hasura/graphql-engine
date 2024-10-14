use datafusion::arrow::datatypes::DataType;
use serde::{Deserialize, Serialize};

use crate::catalog::{
    mem_table::MemTable,
    types::{display_data_type, NormalizedType, StructTypeName},
};

pub const TABLE_VALUED_FUNCTION: &str = "table_valued_function";
pub const TABLE_VALUED_FUNCTION_ARGUMENT: &str = "table_valued_function_argument";

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct TableValuedFunction {
    rows: Vec<TableValuedFunctionRow>,
}

impl TableValuedFunction {
    pub(crate) fn new(rows: Vec<TableValuedFunctionRow>) -> Self {
        TableValuedFunction { rows }
    }
}

impl TableValuedFunction {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "function_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.function_name.clone()))
                    .collect(),
            ),
            (
                "return_type",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.return_type.0.clone()))
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
pub(crate) struct TableValuedFunctionRow {
    function_name: String,
    return_type: StructTypeName,
    description: Option<String>,
}

impl TableValuedFunctionRow {
    pub(crate) fn new(
        function_name: String,
        return_type: StructTypeName,
        description: Option<String>,
    ) -> Self {
        Self {
            function_name,
            return_type,
            description,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TableValuedFunctionField {
    rows: Vec<TableValuedFunctionFieldRow>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TableValuedFunctionFieldRow {
    function_name: String,
    field_name: String,
    field_type: String,
    is_nullable: bool,
    description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Default, Debug, PartialEq)]
pub(crate) struct TableValuedFunctionArgument {
    rows: Vec<TableValuedFunctionArgumentRow>,
}

impl TableValuedFunctionArgument {
    pub(crate) fn new(rows: Vec<TableValuedFunctionArgumentRow>) -> Self {
        TableValuedFunctionArgument { rows }
    }
}

impl TableValuedFunctionArgument {
    pub(crate) fn to_table_provider(&self) -> MemTable {
        MemTable::new_from_iter(vec![
            (
                "function_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.function_name.clone()))
                    .collect(),
            ),
            (
                "argument_name",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.argument_name.clone()))
                    .collect(),
            ),
            (
                "argument_position",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.argument_position))
                    .collect(),
            ),
            (
                "argument_type",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(display_data_type(&row.argument_type)))
                    .collect(),
            ),
            (
                "argument_type_normalized",
                false,
                self.rows
                    .iter()
                    .map(|row| Some(row.argument_type_normalized.to_string()))
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
pub(crate) struct TableValuedFunctionArgumentRow {
    function_name: String,
    argument_name: String,
    argument_position: i64,
    argument_type: DataType,
    argument_type_normalized: NormalizedType,
    is_nullable: bool,
    description: Option<String>,
}

impl TableValuedFunctionArgumentRow {
    pub(crate) fn new(
        function_name: String,
        argument_name: String,
        argument_position: i64,
        argument_type: DataType,
        argument_type_normalized: NormalizedType,
        is_nullable: bool,
        description: Option<String>,
    ) -> Self {
        Self {
            function_name,
            argument_name,
            argument_position,
            argument_type,
            argument_type_normalized,
            is_nullable,
            description,
        }
    }
}
