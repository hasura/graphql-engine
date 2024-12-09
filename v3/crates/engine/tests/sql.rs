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
fn test_introspection_functions() -> anyhow::Result<()> {
    test_sql("sql/introspection/functions")
}

#[test]
fn test_introspection_struct_types() -> anyhow::Result<()> {
    test_sql("sql/introspection/struct_types")
}

#[test]
fn test_commands_functions() -> anyhow::Result<()> {
    test_sql("sql/commands/functions")
}

#[test]
fn test_commands_functions_forward_headers() -> anyhow::Result<()> {
    test_sql("sql/commands/functions_forward_headers")
}

#[test]
fn test_commands_functions_empty_args() -> anyhow::Result<()> {
    test_sql("sql/commands/functions_empty_args")
}

#[test]
fn test_commands_functions_no_access() -> anyhow::Result<()> {
    test_sql("sql/commands/functions_no_access")
}

#[test]
fn test_commands_functions_no_access_fields() -> anyhow::Result<()> {
    test_sql("sql/commands/functions_no_access_fields")
}

#[test]
fn test_commands_procedures() -> anyhow::Result<()> {
    test_sql("sql/commands/procedures")
}

#[test]
fn test_commands_procedures_empty_args() -> anyhow::Result<()> {
    test_sql("sql/commands/procedures_empty_args")
}

#[test]
fn test_commands_procedures_no_access() -> anyhow::Result<()> {
    test_sql("sql/commands/procedures_no_access")
}

#[test]
fn test_commands_procedures_no_access_fields() -> anyhow::Result<()> {
    test_sql("sql/commands/procedures_no_access_fields")
}

#[test]
fn test_commands_procedures_disallow_mutations() -> anyhow::Result<()> {
    test_sql("sql/commands/procedures_disallow_mutations")
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
fn test_select_nested_field_permission_error() -> anyhow::Result<()> {
    test_sql("sql/select/select_nested_field_permission_error")
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
fn test_paginate_nested_limit_1() -> anyhow::Result<()> {
    test_sql("sql/paginate/nested_limit_1")
}

#[test]
fn test_paginate_nested_limit_2() -> anyhow::Result<()> {
    test_sql("sql/paginate/nested_limit_2")
}

#[test]
fn test_paginate_nested_limit_3() -> anyhow::Result<()> {
    test_sql("sql/paginate/nested_limit_3")
}

#[test]
fn test_paginate_nested_limit_4() -> anyhow::Result<()> {
    test_sql("sql/paginate/nested_limit_4")
}

#[test]
fn test_aggregate_count_invoice_lines() -> anyhow::Result<()> {
    test_sql("sql/aggregate/count_invoice_lines")
}

#[test]
fn test_aggregate_count_distinct() -> anyhow::Result<()> {
    test_sql("sql/aggregate/count_distinct")
}

#[test]
fn test_aggregate_count_no_pushdown() -> anyhow::Result<()> {
    test_sql("sql/aggregate/count_no_pushdown")
}

#[test]
fn test_aggregate_count_nested_field() -> anyhow::Result<()> {
    test_sql("sql/aggregate/count_nested_field")
}

#[test]
fn test_aggregate_count_all_invoice_lines() -> anyhow::Result<()> {
    test_sql("sql/aggregate/count_all_invoice_lines")
}

#[test]
fn test_aggregate_count_all_invoice_lines_aliased() -> anyhow::Result<()> {
    test_sql("sql/aggregate/count_all_invoice_lines_aliased")
}

#[test]
fn test_aggregate_sum_invoice_lines() -> anyhow::Result<()> {
    test_sql("sql/aggregate/sum_invoice_lines")
}

#[test]
fn test_aggregate_group_by_unique_key() -> anyhow::Result<()> {
    test_sql("sql/aggregate/group_by_unique_key")
}
