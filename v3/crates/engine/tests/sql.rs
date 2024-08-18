mod common;
use common::test_sql;

#[test]
fn test_introspection_tables() -> anyhow::Result<()> {
    test_sql("sql/introspection/tables")
}

#[test]
fn test_introspection_columns() -> anyhow::Result<()> {
    test_sql("sql/introspection/columns")
}

#[test]
fn test_introspection_hasura_table_metadata() -> anyhow::Result<()> {
    test_sql("sql/introspection/hasura_table_metadata")
}

#[test]
fn test_select_all_fields() -> anyhow::Result<()> {
    test_sql("sql/select/select_all_fields")
}

#[test]
fn test_select_all_fields_invoice() -> anyhow::Result<()> {
    test_sql("sql/select/select_all_fields_invoice")
}

#[test]
fn test_select_some_fields() -> anyhow::Result<()> {
    test_sql("sql/select/select_some_fields")
}

#[test]
fn test_select_computed_fields() -> anyhow::Result<()> {
    test_sql("sql/select/select_computed_fields")
}

#[test]
fn test_filter_simple_equality() -> anyhow::Result<()> {
    test_sql("sql/filter/simple_equality")
}

#[test]
fn test_filter_timestamp_gt() -> anyhow::Result<()> {
    test_sql("sql/filter/timestamp_gt")
}

#[test]
fn test_filter_nested_fields() -> anyhow::Result<()> {
    test_sql("sql/filter/nested_fields")
}

#[test]
fn test_order_by_single_column() -> anyhow::Result<()> {
    test_sql("sql/order_by/single_column")
}

#[test]
fn test_order_by_by_timestamp() -> anyhow::Result<()> {
    test_sql("sql/order_by/by_timestamp")
}

#[test]
fn test_order_by_nested_fields() -> anyhow::Result<()> {
    test_sql("sql/order_by/nested_fields")
}

#[test]
fn test_aggregate_count_invoice_lines() -> anyhow::Result<()> {
    test_sql("sql/aggregate/count_invoice_lines")
}

#[test]
fn test_aggregate_sum_invoice_lines() -> anyhow::Result<()> {
    test_sql("sql/aggregate/sum_invoice_lines")
}
