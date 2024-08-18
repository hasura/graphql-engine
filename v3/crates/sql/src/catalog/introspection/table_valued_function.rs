use datafusion::arrow::datatypes::DataType;
use serde::{Deserialize, Serialize};

use crate::catalog::mem_table::MemTable;

pub const TABLE_VALUED_FUNCTION: &str = "table_valued_function";
pub const TABLE_VALUED_FUNCTION_FIELD: &str = "table_valued_function_field";
pub const TABLE_VALUED_FUNCTION_ARGUMENT: &str = "table_valued_function_argument";

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
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
    description: Option<String>,
}

impl TableValuedFunctionRow {
    pub(crate) fn new(function_name: String, description: Option<String>) -> Self {
        Self {
            function_name,
            description,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TableValuedFunctionField {
    rows: Vec<TableValuedFunctionFieldRow>,
}

impl TableValuedFunctionField {
    pub(crate) fn new(rows: Vec<TableValuedFunctionFieldRow>) -> Self {
        TableValuedFunctionField { rows }
    }
}

impl TableValuedFunctionField {
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
                    .map(|row| Some(row.field_type.clone()))
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
pub(crate) struct TableValuedFunctionFieldRow {
    function_name: String,
    field_name: String,
    field_type: String,
    is_nullable: bool,
    description: Option<String>,
}

impl TableValuedFunctionFieldRow {
    pub(crate) fn new(
        function_name: String,
        field_name: String,
        field_type: &DataType,
        is_nullable: bool,
        description: Option<String>,
    ) -> Self {
        Self {
            function_name,
            field_name,
            field_type: display_data_type(field_type),
            is_nullable,
            description,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct TableValuedFunctionArgument {
    rows: Vec<TableValuedFunctionArgumentRow>,
}

impl TableValuedFunctionArgument {
    pub(crate) fn new(rows: Vec<TableValuedFunctionArgumentRow>) -> Self {
        TableValuedFunctionArgument { rows }
    }
}

fn display_data_type(data_type: &DataType) -> String {
    match data_type {
        DataType::Boolean => "BOOL".to_string(),
        DataType::Int8 | DataType::UInt8 => "INT8".to_string(),
        DataType::Int16 | DataType::UInt16 => "INT16".to_string(),
        DataType::Int32 | DataType::UInt32 => "INT32".to_string(),
        DataType::Int64 | DataType::UInt64 => "INT64".to_string(),
        DataType::Float16 => "FLOAT16".to_string(),
        DataType::Float32 => "FLOAT32".to_string(),
        DataType::Float64 => "FLOAT64".to_string(),
        DataType::Utf8 | DataType::LargeUtf8 => "STRING".to_string(),
        DataType::Binary | DataType::LargeBinary => "BYTES".to_string(),
        DataType::Date32 | DataType::Date64 => "DATE".to_string(),
        DataType::Timestamp(_, _) => "TIMESTAMP".to_string(),
        DataType::Time32(_) | DataType::Time64(_) => "TIME".to_string(),
        DataType::Decimal128(precision, scale) => format!("NUMERIC({precision}, {scale})"),
        DataType::Decimal256(precision, scale) => format!("BIGNUMERIC({precision}, {scale})"),
        DataType::Struct(fields) => {
            // let field_types: Vec<String> = fields
            //     .iter()
            //     .map(|f| format!("{} {}", f.name(), display_data_type(f.data_type())))
            //     .collect();
            let field_types: Vec<String> = fields
                .iter()
                .map(|f| {
                    let type_str = display_data_type(f.data_type());
                    if f.is_nullable() {
                        format!("{} {}", f.name(), type_str)
                    } else {
                        format!("{} {} REQUIRED", f.name(), type_str)
                    }
                })
                .collect();
            format!("STRUCT<{}>", field_types.join(", "))
        }
        DataType::List(field) => {
            format!("ARRAY<{}>", display_data_type(field.data_type()))
        }
        t => format!("{t:?}"),
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
                    .map(|row| Some(row.argument_type.clone()))
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
    argument_type: String,
    is_nullable: bool,
    description: Option<String>,
}

impl TableValuedFunctionArgumentRow {
    pub(crate) fn new(
        function_name: String,
        argument_name: String,
        argument_position: i64,
        argument_type: &DataType,
        is_nullable: bool,
        description: Option<String>,
    ) -> Self {
        Self {
            function_name,
            argument_name,
            argument_position,
            argument_type: display_data_type(argument_type),
            is_nullable,
            description,
        }
    }
}
