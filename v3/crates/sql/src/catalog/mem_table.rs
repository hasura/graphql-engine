//! In-memory table implementation for DataFusion
//!
//! This module provides a serializable and deserializable in-memory table implementation.
//! Datafusion's built-in MemTable doesn't support the serializability property
use async_trait::async_trait;
use std::{any::Any, sync::Arc};
mod datafusion {
    pub(super) use datafusion::{
        arrow::{
            array::{ArrayRef, BooleanArray, Int64Array, RecordBatch, StringArray},
            datatypes::{DataType, Field, SchemaBuilder, SchemaRef},
        },
        datasource::{TableProvider, TableType},
        error::Result,
        execution::context::SessionState,
        logical_expr::Expr,
        physical_plan::{values::ValuesExec, ExecutionPlan},
    };
}
use serde::{Deserialize, Serialize};

/// Represents the data for a single column in a table.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) enum ColumnData {
    Utf8 { data: Vec<Option<String>> },
    Int64 { data: Vec<Option<i64>> },
    Bool { data: Vec<Option<bool>> },
}

impl<'a> FromIterator<Option<&'a str>> for ColumnData {
    fn from_iter<I: IntoIterator<Item = Option<&'a str>>>(iter: I) -> Self {
        ColumnData::Utf8 {
            data: iter
                .into_iter()
                .map(|s| s.map(std::borrow::ToOwned::to_owned))
                .collect(),
        }
    }
}

impl FromIterator<Option<String>> for ColumnData {
    fn from_iter<I: IntoIterator<Item = Option<String>>>(iter: I) -> Self {
        ColumnData::Utf8 {
            data: iter.into_iter().collect(),
        }
    }
}

impl FromIterator<Option<i64>> for ColumnData {
    fn from_iter<I: IntoIterator<Item = Option<i64>>>(iter: I) -> Self {
        ColumnData::Int64 {
            data: iter.into_iter().collect(),
        }
    }
}

impl FromIterator<Option<bool>> for ColumnData {
    fn from_iter<I: IntoIterator<Item = Option<bool>>>(iter: I) -> Self {
        ColumnData::Bool {
            data: iter.into_iter().collect(),
        }
    }
}

impl ColumnData {
    fn data_type(&self) -> datafusion::DataType {
        match self {
            ColumnData::Utf8 { .. } => datafusion::DataType::Utf8,
            ColumnData::Int64 { .. } => datafusion::DataType::Int64,
            ColumnData::Bool { .. } => datafusion::DataType::Boolean,
        }
    }
    fn into_array_ref(self) -> datafusion::ArrayRef {
        match self {
            ColumnData::Utf8 { data } => Arc::new(datafusion::StringArray::from(data)),
            ColumnData::Int64 { data } => Arc::new(datafusion::Int64Array::from(data)),
            ColumnData::Bool { data } => Arc::new(datafusion::BooleanArray::from(data)),
        }
    }
}

/// A table with a fixed set rows, stored in memory. Like datafusion's MemTable but serializable
/// and deserializable
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(super) struct MemTable {
    schema: datafusion::SchemaRef,
    columns: Vec<ColumnData>,
}

impl MemTable {
    /// Creates a new MemTable from an iterator of (field_name, nullable, ColumnData) tuples.
    ///
    /// # Arguments
    ///
    /// * `value` - An iterator yielding tuples of (field_name, nullable, ColumnData)
    ///
    /// # Returns
    ///
    /// A new MemTable instance
    ///
    /// Based on try_from_iter_with_nullable on RecordBatch
    pub(crate) fn new_from_iter<I, F>(value: I) -> Self
    where
        I: IntoIterator<Item = (F, bool, ColumnData)>,
        F: AsRef<str>,
    {
        let iter = value.into_iter();
        let capacity = iter.size_hint().0;
        let mut schema = datafusion::SchemaBuilder::with_capacity(capacity);
        let mut columns = Vec::with_capacity(capacity);

        for (field_name, nullable, array) in iter {
            let field_name = field_name.as_ref();
            schema.push(datafusion::Field::new(
                field_name,
                array.data_type().clone(),
                nullable,
            ));
            columns.push(array);
        }

        let schema = Arc::new(schema.finish());
        MemTable { schema, columns }
    }
}

#[async_trait]
impl datafusion::TableProvider for MemTable {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn schema(&self) -> datafusion::SchemaRef {
        self.schema.clone()
    }

    fn table_type(&self) -> datafusion::TableType {
        datafusion::TableType::View
    }
    async fn scan(
        &self,
        _state: &datafusion::SessionState,
        projection: Option<&Vec<usize>>,
        // filters and limit can be used here to inject some push-down operations if needed
        _filters: &[datafusion::Expr],
        _limit: Option<usize>,
    ) -> datafusion::Result<Arc<dyn datafusion::ExecutionPlan>> {
        let projected_schema = Arc::new(self.schema.project(projection.unwrap_or(&vec![]))?);
        let columnar_projection = projection
            .unwrap_or(&vec![])
            .iter()
            .map(|j| self.columns[*j].clone().into_array_ref())
            .collect::<Vec<_>>();
        Ok(Arc::new(datafusion::ValuesExec::try_new_from_batches(
            projected_schema.clone(),
            vec![datafusion::RecordBatch::try_new(
                projected_schema,
                columnar_projection,
            )?],
        )?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ::datafusion::arrow::array::{Array, BooleanArray, Int64Array, StringArray};
    use ::datafusion::prelude::*;

    #[test]
    fn test_column_data_utf8() {
        let utf8_data = vec![Some("hello".to_string()), None, Some("world".to_string())];
        let column = ColumnData::from_iter(utf8_data);
        let array_ref = column.into_array_ref();
        let string_array = array_ref.as_any().downcast_ref::<StringArray>().unwrap();
        assert_eq!(string_array.len(), 3);
        assert_eq!(string_array.value(0), "hello");
        assert!(string_array.is_null(1));
        assert_eq!(string_array.value(2), "world");
    }

    #[test]
    fn test_column_data_int64() {
        let int64_data = vec![Some(1), None, Some(42)];
        let column = ColumnData::from_iter(int64_data);
        let array_ref = column.into_array_ref();
        let int64_array = array_ref.as_any().downcast_ref::<Int64Array>().unwrap();
        assert_eq!(int64_array.len(), 3);
        assert_eq!(int64_array.value(0), 1);
        assert!(int64_array.is_null(1));
        assert_eq!(int64_array.value(2), 42);
    }

    #[test]
    fn test_column_data_bool() {
        let bool_data = vec![Some(true), None, Some(false)];
        let column = ColumnData::from_iter(bool_data);
        let array_ref = column.into_array_ref();
        let bool_array = array_ref.as_any().downcast_ref::<BooleanArray>().unwrap();
        assert_eq!(bool_array.len(), 3);
        assert!(bool_array.value(0));
        assert!(bool_array.is_null(1));
        assert!(!bool_array.value(2));
    }

    #[test]
    fn test_column_data_empty() {
        let empty_data: Vec<Option<String>> = vec![];
        let column = ColumnData::from_iter(empty_data);
        let array_ref = column.into_array_ref();
        let string_array = array_ref.as_any().downcast_ref::<StringArray>().unwrap();
        assert_eq!(string_array.len(), 0);
    }

    #[test]
    fn test_column_data_all_null() {
        let all_null_data: Vec<Option<String>> = vec![None, None, None];
        let column = ColumnData::from_iter(all_null_data);
        let array_ref = column.into_array_ref();
        let string_array = array_ref.as_any().downcast_ref::<StringArray>().unwrap();
        assert_eq!(string_array.len(), 3);
        assert!(string_array.is_null(0));
        assert!(string_array.is_null(1));
        assert!(string_array.is_null(2));
    }

    #[tokio::test]
    async fn test_mem_table_provider_sql_full_scan() -> datafusion::Result<()> {
        let ctx = create_test_context()?;

        let df = ctx.sql("SELECT * FROM test_table").await?;
        let results = df.collect().await?;

        assert_eq!(results.len(), 1); // One batch
        let batch = &results[0];
        assert_eq!(batch.num_columns(), 3);
        assert_eq!(batch.num_rows(), 3);

        // Verify column data
        let col1 = batch
            .column(0)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap();
        assert_eq!(col1.value(0), "a");
        assert!(col1.is_null(1));
        assert_eq!(col1.value(2), "c");

        let col2 = batch
            .column(1)
            .as_any()
            .downcast_ref::<Int64Array>()
            .unwrap();
        assert_eq!(col2.value(0), 1);
        assert_eq!(col2.value(1), 2);
        assert_eq!(col2.value(2), 3);

        let col3 = batch
            .column(2)
            .as_any()
            .downcast_ref::<BooleanArray>()
            .unwrap();
        assert!(col3.value(0));
        assert!(!col3.value(1));
        assert!(col3.is_null(2));

        Ok(())
    }

    #[tokio::test]
    async fn test_mem_table_provider_sql_projection() -> datafusion::Result<()> {
        let ctx = create_test_context()?;

        let df = ctx.sql("SELECT col1, col3 FROM test_table").await?;
        let results = df.collect().await?;

        assert_eq!(results.len(), 1);
        let batch = &results[0];
        assert_eq!(batch.num_columns(), 2);
        assert_eq!(batch.num_rows(), 3);

        // Verify projected columns
        let col1 = batch
            .column(0)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap();
        assert_eq!(col1.value(0), "a");
        assert!(col1.is_null(1));
        assert_eq!(col1.value(2), "c");

        let col3 = batch
            .column(1)
            .as_any()
            .downcast_ref::<BooleanArray>()
            .unwrap();
        assert!(col3.value(0));
        assert!(!col3.value(1));
        assert!(col3.is_null(2));

        Ok(())
    }

    #[tokio::test]
    async fn test_mem_table_provider_sql_filter() -> datafusion::Result<()> {
        let ctx = create_test_context()?;

        let df = ctx
            .sql("SELECT col1, col2 FROM test_table WHERE col2 > 1")
            .await?;
        let results = df.collect().await?;

        assert_eq!(results.len(), 1);
        let batch = &results[0];
        assert_eq!(batch.num_columns(), 2);
        assert_eq!(batch.num_rows(), 2);

        let col1 = batch
            .column(0)
            .as_any()
            .downcast_ref::<StringArray>()
            .unwrap();
        assert!(col1.is_null(0));
        assert_eq!(col1.value(1), "c");

        let col2 = batch
            .column(1)
            .as_any()
            .downcast_ref::<Int64Array>()
            .unwrap();
        assert_eq!(col2.value(0), 2);
        assert_eq!(col2.value(1), 3);

        Ok(())
    }

    #[tokio::test]
    async fn test_mem_table_provider_sql_aggregation() -> datafusion::Result<()> {
        let ctx = create_test_context()?;

        let df = ctx
            .sql("SELECT COUNT(*), SUM(col2) FROM test_table")
            .await?;
        let results = df.collect().await?;

        assert_eq!(results.len(), 1);
        let batch = &results[0];
        assert_eq!(batch.num_columns(), 2);
        assert_eq!(batch.num_rows(), 1);

        let count = batch
            .column(0)
            .as_any()
            .downcast_ref::<Int64Array>()
            .unwrap();
        assert_eq!(count.value(0), 3);

        let sum = batch
            .column(1)
            .as_any()
            .downcast_ref::<Int64Array>()
            .unwrap();
        assert_eq!(sum.value(0), 6);

        Ok(())
    }

    #[tokio::test]
    async fn test_mem_table_provider_sql_empty() -> datafusion::Result<()> {
        let ctx = SessionContext::new();
        let empty_table = MemTable::new_from_iter(vec![(
            "empty_col".to_string(),
            true,
            ColumnData::from_iter(Vec::<Option<String>>::new()),
        )]);

        ctx.register_table("empty_table", Arc::new(empty_table))?;

        let df = ctx.sql("SELECT * FROM empty_table").await?;
        let results = df.collect().await?;

        assert_eq!(results.len(), 1);
        let batch = &results[0];
        assert_eq!(batch.num_columns(), 1);
        assert_eq!(batch.num_rows(), 0);

        Ok(())
    }

    fn create_test_context() -> datafusion::Result<SessionContext> {
        let ctx = SessionContext::new();
        let mem_table = MemTable::new_from_iter(vec![
            (
                "col1".to_string(),
                true,
                ColumnData::from_iter(vec![Some("a"), None, Some("c")]),
            ),
            (
                "col2".to_string(),
                false,
                ColumnData::from_iter(vec![Some(1), Some(2), Some(3)]),
            ),
            (
                "col3".to_string(),
                true,
                ColumnData::from_iter(vec![Some(true), Some(false), None]),
            ),
        ]);

        ctx.register_table("test_table", Arc::new(mem_table))?;
        Ok(ctx)
    }
}
