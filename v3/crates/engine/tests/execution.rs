use std::collections::BTreeMap;

use metadata_resolve::data_connectors::NdcVersion;

mod common;

#[test]
fn test_model_select_one_simple_select() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_one/simple_select",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_one_with_type_permission() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_one/type_permission",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_one_simple_select_introspection() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_one/simple_select/introspection/introspection",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_one_filter() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_one/simple_select/filter",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_one_custom_scalar() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_one/custom_scalar",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Select Many Tests
#[test]
fn test_model_select_many_simple_select() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/simple_select",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_simple_select_introspection() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/simple_select/introspection/introspection",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_simple_select_introspection_user_1() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/simple_select/introspection/introspection_user_1",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_simple_select_introspection_with_graphql_config() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/simple_select/introspection/with_graphql_config",
        &["execute/models/select_many/common_metadata/graphql_config.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_filter() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/simple_select/filter",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_empty_select() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/empty_select";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_field_arguments() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/field_arguments",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_multiple_field_arguments() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/field_arguments/multiple_arguments",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Nested selection tests
#[test]
fn test_model_select_many_nested_select() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/nested_select";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

// Same test as above, but with no type mappings defined in the metadata.
// Tests that the engine will correctly add default type mappings
// When resolving the metadata.
#[test]
fn test_model_select_many_nested_select_no_explicit_type_mapping() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/nested_select/no_explicit_type_mapping";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

// Same test as the nested selection
#[test]
fn test_model_select_many_nested_select_with_relationship() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/nested_select/relationship";
    let common_metadata_paths = [
        "execute/common_metadata/custom_connector_v02_schema.json",
        "execute/models/select_many/nested_select/metadata.json",
    ];
    common::test_execution_expectation(test_path_string, &common_metadata_paths)
}

// nested selection tests, using Postgres
#[test]
fn test_model_select_many_nested_select_postgres() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/nested_select/postgres",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Order By Tests
#[test]
fn test_model_select_many_order_by() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_order_by_empty_ordering() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/empty_ordering",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_order_by_with_model_v2() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/with_model_v2",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_order_by_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/nested";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_order_by_nested_relationships() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/nested_relationships";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_order_by_nested_legacy() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/nested_legacy";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_order_by_filter() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/filter",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_order_by_with_graphql_config() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/with_graphql_config",
        &["execute/models/select_many/common_metadata/graphql_config.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// What is being tested? - We are testing the order_by sorts correctly when
// multiple columns are being ordered.
// We are sorting the `Tracks` models where we want `AlbumId` in ascending order
// and `TrackId` columns in descending order.
// We expect the results to be sorted by `AlbumId`, and then if there are
// multiple rows that have the same value in `AlbumId`, it will then order these
// rows by `TrackId`
#[test]
fn test_model_select_many_order_by_multiple_columns() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/multiple_columns",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_order_by_multiple_columns_validation_check() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/multiple_columns_validation_check",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_order_by_multiple_nested_columns_validation_check() -> anyhow::Result<()>
{
    let test_path_string =
        "execute/models/select_many/order_by/multiple_nested_columns_validation_check";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

// Type Permissions
#[test]
fn test_model_select_many_type_permission_order_by() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/type_permission/order_by",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Relationships in order_by expressions
// What is being tested:
// 1. Object relationships in order_by expressions (Simple, Nested Object relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_order_by_object_relationship_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/relationships/object/simple",
        &["execute/models/select_many/order_by/relationships/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_order_by_object_relationship_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/relationships/object/nested",
        &["execute/models/select_many/order_by/relationships/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_order_by_remote_relationship() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/remote_relationship";
    let common_metadata_path_string =
        "execute/models/select_many/order_by/remote_relationship/combined_metadata.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_order_by_reuse_order_by_expression() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/order_by/reuse_order_by_expression",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_type_permission_where() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/type_permission/where",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Where Tests
#[test]
fn test_model_select_many_where() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/simple",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// With null input
#[test]
fn test_model_select_many_where_null_input() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/null_input",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// the test here is that two Models can both use the same ObjectBooleanExpressionType without
// errors
#[test]
fn test_model_select_many_shared_boolean_expression() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/shared_boolean_expression",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_boolean_expression_type() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/boolean_expression_type",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_nested_select_object() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_select/object",
        &["execute/models/select_many/where/nested_select/common-metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_nested_select_object_is_null() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_select/object_is_null",
        &["execute/models/select_many/where/nested_select/common-metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_nested_select_array() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_select/array",
        &["execute/models/select_many/where/nested_select/common-metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_nested_select_array_is_null() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_select/array_is_null",
        &["execute/models/select_many/where/nested_select/common-metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// is_null tests

// old boolean expressions
#[test]
fn test_model_select_many_where_is_null_object_boolean_expression_type() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/is_null/object_boolean_expression_type",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// new boolean expressions
#[test]
fn test_model_select_many_where_is_null_boolean_expression_type() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/is_null/boolean_expression_type",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// end of is_null tests

#[test]
fn test_model_select_many_where_filter() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/filter",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_with_grapqhl_config() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/with_graphql_config",
        &["execute/models/select_many/common_metadata/graphql_config.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_multiple_fields() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/multiple_fields",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_select_with_args() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/select_with_args",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_select_with_args_filter() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/select_with_args/filter",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_select_with_args_with_graphql_config() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/select_with_args/with_graphql_config",
        &["execute/models/select_many/common_metadata/graphql_config.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_ndc_operators() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/ndc_operators",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Relationships in boolean expressions

// Older style: using `ObjectBooleanExpressionType` and `DataConnectorScalarType`

// What is being tested:
// 1. Array relationships in boolean expressions (Simple, Nested array relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_object_boolean_array_relationship_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/relationships/object_boolean_expression_type/array/simple",
        &[
            "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_object_boolean_array_relationship_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/relationships/object_boolean_expression_type/array/nested",
        &[
            "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Object relationships in boolean expressions (Simple, Nested object relationships). We also test multi column boolean expressions
#[test]
fn test_model_select_many_where_object_boolean_object_relationship_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/relationships/object_boolean_expression_type/object/simple",
        &[
            "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_object_boolean_object_relationship_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/relationships/object_boolean_expression_type/object/nested",
        &[
            "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Newer style: using `BooleanExpressionType`

// What is being tested:
// 1. Array relationships in boolean expressions (Simple, Nested array relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_array_relationship_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/relationships/boolean_expression_type/array/simple",
        &[
            "execute/models/select_many/where/relationships/boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_array_relationship_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/relationships/boolean_expression_type/array/nested",
        &[
            "execute/models/select_many/where/relationships/boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Object relationships in boolean expressions (Simple, Nested object relationships). We also test multi column boolean expressions
#[test]
fn test_model_select_many_where_object_relationship_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/relationships/boolean_expression_type/object/simple",
        &[
            "execute/models/select_many/where/relationships/boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_object_relationship_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/relationships/boolean_expression_type/object/nested",
        &[
            "execute/models/select_many/where/relationships/boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Remote relationships in boolean expressions

// What is being tested:
// 1. Remote array relationships in boolean expressions (Simple, Nested relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_remote_array_relationship_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/array/simple",
        &[
            "execute/models/select_many/where/remote_relationships/boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/where/remote_relationships/boolean_expression_type/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/where/remote_relationships/boolean_expression_type/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_remote_array_relationship_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/array/nested",
        &[
            "execute/models/select_many/where/remote_relationships/boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/where/remote_relationships/boolean_expression_type/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/where/remote_relationships/boolean_expression_type/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

// Remote object relationships in boolean expressions (Simple, Nested rrelationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_remote_object_relationship_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/object/simple",
        &[
            "execute/models/select_many/where/remote_relationships/boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/where/remote_relationships/boolean_expression_type/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/where/remote_relationships/boolean_expression_type/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_remote_object_relationship_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/object/nested",
        &[
            "execute/models/select_many/where/remote_relationships/boolean_expression_type/common_metadata.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/where/remote_relationships/boolean_expression_type/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/where/remote_relationships/boolean_expression_type/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_remote_object_relationship_simple_across_subgraphs()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/object/simple_across_subgraphs",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Relationships in boolean expressions without 'relation_comparisons' capability
// What is being tested:
// 1. Remote and Local array relationships in boolean expressions (Simple, Nested relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_no_capability_array_relationship_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/no_relation_comparisons_capability/boolean_expression_type/array/simple";
    let common_metadata_path_string =
        "execute/common_metadata/two_postgres_connector_same_subgraph_schema.json";
    let test_common_metadata_path_string = "execute/models/select_many/where/no_relation_comparisons_capability/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            test_common_metadata_path_string,
        ],
    )
}

#[test]
fn test_model_select_many_where_no_capability_array_relationship_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/no_relation_comparisons_capability//boolean_expression_type/array/nested";
    let common_metadata_path_string =
        "execute/common_metadata/two_postgres_connector_same_subgraph_schema.json";
    let test_common_metadata_path_string = "execute/models/select_many/where/no_relation_comparisons_capability/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            test_common_metadata_path_string,
        ],
    )
}

// Remote and Local object relationships in boolean expressions (Simple, Nested rrelationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_no_capability_object_relationship_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/no_relation_comparisons_capability/boolean_expression_type/object/simple";
    let common_metadata_path_string =
        "execute/common_metadata/two_postgres_connector_same_subgraph_schema.json";
    let test_common_metadata_path_string = "execute/models/select_many/where/no_relation_comparisons_capability/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            test_common_metadata_path_string,
        ],
    )
}

#[test]
fn test_model_select_many_where_no_capability_object_relationship_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/no_relation_comparisons_capability/boolean_expression_type/object/nested";
    let common_metadata_path_string =
        "execute/common_metadata/two_postgres_connector_same_subgraph_schema.json";
    let test_common_metadata_path_string = "execute/models/select_many/where/no_relation_comparisons_capability/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            test_common_metadata_path_string,
        ],
    )
}

#[test]
fn test_model_select_many_where_nested_relationships() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_relationships",
        &[],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, it does not support nested relationships in predicates
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_nested_remote_relationships() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_remote_relationships",
        &[],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, it does not support nested relationships in predicates
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_nested_relationships_different_names() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_relationships_different_names",
        &[],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, it does not support nested relationships in predicates
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_nested_scalar_array() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_scalar_array",
        &[],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, it does not support nested scalar arrays in predicates
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_where_nested_scalar_array_fails_on_v01() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/where/nested_scalar_array/not_supported",
        &[],
        BTreeMap::from([(
            NdcVersion::V01,
            vec!["execute/common_metadata/custom_connector_v01_schema.json"],
        )]),
    )
}

#[test]
fn test_model_select_many_object_type_input_arguments() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/object_type_input_arguments";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

// Limit Tests
#[test]
fn test_model_select_many_limit() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/limit_offset/limit",
        &["execute/models/select_many/limit_offset/common_metadata/metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Test is_null in model select permissions
#[test]
fn test_model_select_many_predicate_is_null() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/predicate/is_null",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// ---------- Offset Tests
#[test]
fn test_model_select_many_offset() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/limit_offset/offset",
        &["execute/models/select_many/limit_offset/common_metadata/metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_limit_offset_null_values() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/limit_offset/null_values",
        &["execute/models/select_many/limit_offset/common_metadata/metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// ---------- Model Permission Tests
#[test]
fn test_model_select_many_permission_filter_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/permission_filter/simple";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";

    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_permission_filter_relationships_object() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/permission_filter/relationships/object";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";

    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_permission_filter_relationships_array() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/permission_filter/relationships/array";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";

    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_permission_filter_remote_relationships_object() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/permission_filter/remote_relationships/object";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";

    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_permission_filter_remote_relationships_array() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/permission_filter/remote_relationships/array";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";

    common::test_execution_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_model_select_many_permission_filter_nested_select_object() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/permission_filter/nested_select/object",
        &["execute/models/select_many/permission_filter/nested_select/common-metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_permission_filter_nested_select_object_is_null() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/permission_filter/nested_select/object_is_null",
        &["execute/models/select_many/permission_filter/nested_select/common-metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_model_select_many_permission_filter_nested_relationships() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/permission_filter/nested_relationships",
        &[],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, it does not support nested relationships in predicates
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// ---------- Limit and Offset Tests
#[test]
fn test_model_select_many_limit_offset() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/limit_offset/limit_offset",
        &["execute/models/select_many/limit_offset/common_metadata/metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// ----------- Limit and Offset negative value test
#[test]
fn test_model_select_many_negative_limit_offset() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/limit_offset/unexpected_value",
        &["execute/models/select_many/limit_offset/common_metadata/metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relay() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
/// Tests the generation of the `id` field in select queries including relationships.
fn test_relay_id_in_select() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay_id_in_select",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
/// Test querying the relay global ID with a role that doesn't have access to
/// all the global ID fields.
fn test_relay_global_id_permission() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay_global_id_permission",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relay_node_field() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay_node_field",
        &["execute/relay/article_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relay_node_type_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay_node_type_permissions",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_relay_node_field_permission() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay_node_field_permissions",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
/// Tests a role should not be able to access the relay `node` field,
/// if the Node interface doesn't implement any objects for that role.
fn test_relay_node_interface_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay_node_interface_permission",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
/// Tests the `node` root field with a role that has no model select permissions.
fn test_relay_node_model_select_permissions_with_role_without_model_select_permission()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay_node_model_select_permissions/no_select_permission_exists_for_role",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
/// Test the `node` root field with a role that has model select permissions.
fn test_relay_node_model_select_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relay/relay_node_model_select_permissions/successful_model_select_permissions",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_typename() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/typename",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Command Functions

#[test]
fn test_command_functions() -> anyhow::Result<()> {
    let test_path_string = "execute/commands/functions";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_command_object_type_input_arguments() -> anyhow::Result<()> {
    let test_path_string = "execute/commands/object_type_input_arguments";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_command_custom_scalar_inputs() -> anyhow::Result<()> {
    let test_path_string = "execute/commands/custom_scalar_inputs";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

// Tests a query command with scalar (Int) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_functions_scalar_output_type() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/scalar_output_type",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a query command with object (commandActor) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_functions_object_output_type_command_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/object_output_type/command_permissions",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a query command with object (commandActor) output type (different object output subset field permissions for roles: admin,
// user_1, user_2)
#[test]
fn test_command_functions_object_output_type_output_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/object_output_type/output_permissions",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a query command with array of scalar ([String]) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_functions_scalar_array_output_type() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/scalar_array_output_type",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a query command with array of object ([commandActor]) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_functions_object_array_output_type_command_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/object_array_output_type/command_permissions",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, the embedded actors data has changed
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a query command with array of object ([commandActor]) output type (different object output subset field permissions for roles: admin,
// user_1, user_2)
#[test]
fn test_command_functions_object_array_output_type_output_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/object_array_output_type/output_permissions",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, the embedded actors data has changed
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a query command with multiple arguments:
//  arguments: 2 arguments (taken as bounds and return the list of commandActors with movie_id between the bounds)
//  output: array of object ([commandActor]) output type
//  permission: different command permissions for roles: admin, user_1, user_2
#[test]
fn test_command_functions_multiple_arguments() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/multiple_arguments",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Command Procedures

// Tests a mutation command with scalar (String) output type (different command permissions for roles: admin, user_1,
// user_2). This mutation doesn't perform any mutation on the database, it just returns a string
#[test]
fn test_command_procedures_scalar_output_type_command_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/scalar_output_type",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command with object (commandActor) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_procedures_object_output_type_command_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/object_output_type/command_permissions",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command with object (commandActor) output type (different object output subset field permissions for roles: admin,
// user_1, user_2)
#[test]
fn test_command_procedures_object_output_type_output_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/object_output_type/output_permissions",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command with array of scalar ([String]) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_procedures_scalar_array_output_type() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/scalar_array_output_type",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, the embedded actors data has changed
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command with array of object ([commandActor]) output type (different command permissions for roles: admin, user_1, user_2)
#[test]
fn test_command_procedures_object_array_output_type_command_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/object_array_output_type/command_permissions",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, the embedded actors data has changed
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command with array of object ([commandActor]) output type (different object output subset field permissions for roles: admin,
// user_1, user_2)
#[test]
fn test_command_procedures_object_array_output_type_output_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/object_array_output_type/output_permissions",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, the embedded actors data has changed
            // (
            //     NdcVersion::V01,
            //     vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            // ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Test a mutation command with an input object type as an argument
#[test]
fn test_command_procedures_input_object_type() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/object_input_type",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command with multiple arguments:
// arguments: 2 arguments (taken as id and new name for an actor and returns the updated commandActor row )
// output: object (commandActor) output type
// permission: different command permissions for roles: admin, user_1, user_2
#[test]
fn test_command_procedures_multiple_arguments() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/multiple_arguments",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command that uses a preset boolean expression as an argument
#[test]
fn test_command_procedures_boolean_expression_argument() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/boolean_expression_argument",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            // Not supported in v0.1.x because the old custom connector doesn't support the new boolean expression argument to upsert_actor
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command with preset arguments:
// arguments: 2 arguments (lower_bound, upper_bound) - one provided by presets in permissions
// output: object (commandActor) output type
// permission: different permissions and preset arguments for roles: admin, user_1, user_2
#[test]
fn test_command_argument_presets() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/command_argument_presets",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command with a boolean expression as an argument:
// arguments: 1 boolean expression as a preset
// output: object (commandActor) output type
// permission: different permissions and preset arguments for roles: admin, user_1, user_2

// old `object_boolean_expression_type`
#[test]
fn test_boolean_expression_command_argument_presets_object_boolean_expression_type()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/boolean_expression_command_argument/object_boolean_expression_type",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// new `boolean_expression_type`
#[test]
fn test_boolean_expression_command_argument_presets_boolean_expression_type() -> anyhow::Result<()>
{
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/boolean_expression_command_argument/boolean_expression_type",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// new `boolean_expression_type` with boolean expression provided in query
#[test]
fn test_boolean_expression_command_argument_from_user() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/boolean_expression_command_argument/passed_by_user",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_boolean_expression_command_argument_combined_with_type_permissions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/boolean_expression_command_argument/combined_with_type_permissions",
        &[],
        BTreeMap::from([
            // EvalInstitutions function not supported in v0.1.x connector
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_boolean_expression_command_argument_combined_with_type_permissions_more_presets()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/boolean_expression_command_argument/combined_with_type_permissions_more_presets",
        &[],
        BTreeMap::from([
            // EvalInstitutions function not supported in v0.1.x connector
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_boolean_expression_command_argument_combined_with_type_permissions_more_presets_flag_off()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/boolean_expression_command_argument/combined_with_type_permissions_more_presets_flag_off",
        &[],
        BTreeMap::from([
            // EvalInstitutions function not supported in v0.1.x connector
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_boolean_expression_command_argument_combined_with_type_permissions_disallow_bad_values()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/boolean_expression_command_argument/combined_with_type_permissions_disallow_bad_values",
        &[],
        BTreeMap::from([
            // EvalInstitutions function not supported in v0.1.x connector
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a select many query command with preset arguments on the model:
// permission: different permissions and preset arguments for roles: admin, user_1, user_2
#[test]
fn test_model_argument_presets_select_many() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/model_argument_presets_select_many",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Also check that when all fields are nullable or preset, we can omit the 'args' in GraphQL
#[test]
fn test_model_argument_presets_select_many_2() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/model_argument_presets_select_many_nullable_arguments",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            // `actors_by_movie` not supported in v0.1.x connector
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a select one query command with preset arguments on the model:
// permission: different permissions and preset arguments for roles: admin, user_1, user_2
#[test]
fn test_model_argument_presets_select_one() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/model_argument_presets_select_one",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests input type permissions -> field presets, on command
#[test]
fn test_input_type_field_presets_on_command() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/input_types/field_presets/commands",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests input type permissions -> field presets, on model arguments
#[test]
fn test_input_type_field_presets_on_model_arguments() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/input_types/field_presets/models",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests using relationships in predicates
// Array relationship
#[test]
fn test_model_select_many_relationship_predicate_array_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/relationship_predicates/array/simple",
        &["execute/models/select_many/relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Tests using relationships in predicates

// Nested Array relationship
#[test]
fn test_model_select_many_relationship_predicate_array_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/relationship_predicates/array/nested",
        &["execute/models/select_many/relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Nested Array relationship with multiple fields
#[test]
fn test_model_select_many_relationship_predicate_array_nested_multiple_fields() -> anyhow::Result<()>
{
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/relationship_predicates/array/nested_multiple_fields",
        &["execute/models/select_many/relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Tests using relationships in predicates
// Object relationship
#[test]
fn test_model_select_many_relationship_predicate_object_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/relationship_predicates/object/simple",
        &["execute/models/select_many/relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Tests using relationships in predicates
// Nested object relationship
#[test]
fn test_model_select_many_relationship_predicate_object_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/relationship_predicates/object/nested",
        &["execute/models/select_many/relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Tests using relationships in predicates
// We have the following relationships:
//  1.  'Tracks' array relationship to 'Album' model
//  2.  'Album' object relationship to 'Track' model
//
// Predicates using the relationship are defined on both the models as follows:
// 1. The select permission for 'user' role on 'Album' model is defined as:
//      Select only those Album whose `TrackId` from the relationship `Track` is equal to "x-hasura-user-id"
// 2. The select permission for 'user' role on 'Track' model is defined as:
//      Select only those Track whose `Title` from the relationship `Album` is equal to "x-hasura-album-title"
//
// In this test, we test what happens when we query both the `Tracks` and `Album` relationship in the same query.
// The query we make is:
//   query MyQuery {
//      Album(limit: 1) {
//          Tracks {
//              TrackId
//              Name
//              Album {
//                  Title
//              }
//          }
//      }
//  }
// We expect the following results:
//      Fetch all the tracks of the Albums whose `TrackId` is equal to "x-hasura-user-id" and then
//      filter those tracks based on the "x-hasura-album-title" value.
#[test]
fn test_model_select_many_relationship_predicate_on_two_fields() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/relationship_predicates/on_two_fields",
        &["execute/models/select_many/relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Tests using relationships in predicates
// We have the following relationships:
//  1.  'Tracks' object relationship to 'Album' model
//  2.  'Album' object relationship to 'Track' model
//  3.  'Genre' object relationship to 'Track' model
//
// We have the following select permission defined for "user" role
//    It filters only those Albums whose Tracks's Album's AlbumnId is equal to "x-hasura-user-id" and
//    whose Tracks's Genre's GenreId is equal to "x-hasura-genre-name"
#[test]
fn test_model_select_many_relationship_predicate_object_two_relationship_fields()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/relationship_predicates/object/two_relationship_fields",
        &["execute/models/select_many/relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Tests using remote relationships in predicates
// Array relationship
#[test]
fn test_model_select_many_remote_relationship_predicate_array_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/remote_relationship_predicates/array/simple",
        &["execute/models/select_many/remote_relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

// Tests using remote relationships in predicates

// Nested Array relationship
#[test]
fn test_model_select_many_remote_relationship_predicate_array_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/remote_relationship_predicates/array/nested",
        &["execute/models/select_many/remote_relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

// Nested Array relationship with multiple fields
#[test]
fn test_model_select_many_remote_relationship_predicate_array_nested_multiple_fields()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/remote_relationship_predicates/array/nested_multiple_fields",
        &["execute/models/select_many/remote_relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

// Tests using remote relationships in predicates
// Object relationship
#[test]
fn test_model_select_many_remote_relationship_predicate_object_simple() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/remote_relationship_predicates/object/simple",
        &["execute/models/select_many/remote_relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

// Tests using remote relationships in predicates
// Object relationship, but `predicate` is omitted meaning `const True`, i.e. a semi-join (see
// ENG-1747). This started as a fork of `..._object_simple` above.
#[test]
fn test_model_select_many_remote_relationship_predicate_object_empty_predicate()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/remote_relationship_predicates/object/empty_predicate",
        &["execute/models/select_many/remote_relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

// Tests using remote relationships in predicates
// Nested object relationship
#[test]
fn test_model_select_many_remote_relationship_predicate_object_nested() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/remote_relationship_predicates/object/nested",
        &["execute/models/select_many/remote_relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

// Tests using remote relationships in predicates
// We have the following relationships:
//  1.  'TracksRemote' array relationship to 'Album' model
//  2.  'AlbumRemote' object relationship to 'Track' model
//
// Predicates using the relationship are defined on both the models as follows:
// 1. The select permission for 'user' role on 'Album' model is defined as:
//      Select only those Album whose `TrackId` from the relationship `TracksRemote` is equal to "x-hasura-user-id"
// 2. The select permission for 'user' role on 'Track' model is defined as:
//      Select only those Track whose `Title` from the relationship `AlbumRemote` is equal to "x-hasura-album-title"
//
// In this test, we test what happens when we query both the `TracksRemote` and `AlbumRemote` relationship in the same query.
// The query we make is:
//   query MyQuery {
//      Album(limit: 1) {
//          Tracks {
//              TrackId
//              Name
//              Album {
//                  Title
//              }
//          }
//      }
//  }
// We expect the following results:
//      Fetch all the tracks of the Albums whose `TrackId` is equal to "x-hasura-user-id" and then
//      filter those tracks based on the "x-hasura-album-title" value.
#[test]
fn test_model_select_many_remote_relationship_predicate_on_two_fields() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/remote_relationship_predicates/on_two_fields",
        &["execute/models/select_many/remote_relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

// Tests using relationships in predicates
// We have the following relationships:
//  1.  'TracksRemote' object relationship to 'Album' model
//  2.  'AlbumRemote' object relationship to 'Track' model
//  3.  'GenreRemote' object relationship to 'Track' model
//
// We have the following select permission defined for "user" role
//    It filters only those Albums whose Tracks's Album's AlbumnId is equal to "x-hasura-user-id" and
//    whose Tracks's Genre's GenreId is equal to "x-hasura-genre-name"
#[test]
fn test_model_select_many_remote_relationship_predicate_object_two_relationship_fields()
-> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/models/select_many/remote_relationship_predicates/object/two_relationship_fields",
        &["execute/models/select_many/remote_relationship_predicates/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v01.json",
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/models/select_many/remote_relationship_predicates/pg_connector_ndc_v02.json",
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_graphql_descriptions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/description",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                    "execute/common_metadata/custom_connector_v01_schema.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                    "execute/common_metadata/custom_connector_v02_schema.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_apollo_federation_service_sdl() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/apollo_federation_fields/service_sdl",
        &["execute/apollo_federation_fields/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_apollo_federation_entities() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/apollo_federation_fields/entities",
        &["execute/apollo_federation_fields/common_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_aggregates_root_field_simple_select() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/root_field/simple_select",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                    "execute/aggregates/root_field/simple_select/metadata_ndc_v01.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                    "execute/aggregates/root_field/simple_select/metadata_ndc_v02.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_aggregates_root_field_filtering() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/root_field/filtering",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                    "execute/aggregates/root_field/filtering/metadata_ndc_v01.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                    "execute/aggregates/root_field/filtering/metadata_ndc_v02.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_aggregates_root_field_filtering_null_inputs() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/root_field/filtering_null_inputs",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                    "execute/aggregates/root_field/filtering_null_inputs/metadata_ndc_v01.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                    "execute/aggregates/root_field/filtering_null_inputs/metadata_ndc_v02.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_aggregates_root_field_nested_object() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/nested_object";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/custom_connector_vBoth_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_aggregates_root_field_custom_count_return_type() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/custom_count_return_type";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/custom_connector_v02_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([
            // This test can't use the old NDC v0.1.x connector, it does not support custom count return types
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_aggregates_root_field_typename() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/root_field/typename",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                    "execute/aggregates/root_field/typename/metadata_ndc_v01.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                    "execute/aggregates/root_field/typename/metadata_ndc_v02.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_aggregates_relationship_field_simple_select() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/relationship_field/simple_select",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
                    "execute/aggregates/relationship_field/simple_select/metadata_ndc_v01.json",
                ],
            ),
            (
                NdcVersion::V02,
                vec![
                    "execute/common_metadata/postgres_connector_ndc_v02_schema.json",
                    "execute/aggregates/relationship_field/simple_select/metadata_ndc_v02.json",
                ],
            ),
        ]),
    )
}

#[test]
fn test_aggregates_relationship_field_filtering_ndc_v01() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/relationship_field/filtering_ndc_v01",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([(
            NdcVersion::V01,
            vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
        )]),
    )
}

#[test]
fn test_aggregates_relationship_field_filtering_ndc_v02() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/relationship_field/filtering_ndc_v02",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([(
            NdcVersion::V01,
            vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
        )]),
    )
}

#[test]
fn test_aggregates_relationship_field_filtering_null_inputs_ndc_v01() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/relationship_field/filtering_null_inputs_ndc_v01",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([(
            NdcVersion::V01,
            vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
        )]),
    )
}

#[test]
fn test_aggregates_relationship_field_filtering_null_inputs_ndc_v02() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/aggregates/relationship_field/filtering_null_inputs_ndc_v02",
        &[
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([(
            NdcVersion::V02,
            vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
        )]),
    )
}

// Tests of NDC header forwarding

// Tests a mutation command "login", with NDC forward headers configuration.
#[test]
fn test_command_mutation_forwarded_headers() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/procedures/forward_headers",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests a mutation command "login", with NDC forward headers configuration.
#[test]
fn test_command_query_forwarded_headers() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/commands/functions/forward_headers",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Tests of session variables

#[test]
fn test_session_variables_json_enabled_array_session_variable() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/session_variables/json_enabled/array_session_variable",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_session_variables_json_enabled_integer_session_variable() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/session_variables/json_enabled/integer_session_variable",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_session_variables_json_disabled_integer_session_variable() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/session_variables/json_disabled/integer_session_variable",
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// Test a join with an object on RHS, where that object is joined on an argument
#[test]
fn test_relationships_array_target_model_with_arguments() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/relationships/array/target_model_with_arguments",
        &["execute/common_metadata/command_metadata.json"],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// Test of non-null query variables
#[test]
fn test_variables_non_null_type_omit_variable() -> anyhow::Result<()> {
    let test_path_string = "execute/variables/non_null_type_omit_variable";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

// this test is to ensure that omitted enum arguments to functions that _should_ be non-nullable
// are not validated, and `null` is passed to NDC
#[test]
fn test_variables_non_null_type_omit_variable_with_enum() -> anyhow::Result<()> {
    let test_path_string = "execute/variables/non_null_type_omit_variable/with_enum";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[],
        BTreeMap::from([
            // not defined in v01 connector
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

// this test is to ensure that when we turn the flag on, we do indeed validate the missing non-nullanle argument
#[test]
fn test_variables_non_null_type_omit_variable_with_enum_and_validation() -> anyhow::Result<()> {
    let test_path_string = "execute/variables/non_null_type_omit_variable/with_enum_and_validation";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[],
        BTreeMap::from([
            // not defined in v01 connector
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_variables_non_null_type_null_variable() -> anyhow::Result<()> {
    let test_path_string = "execute/variables/non_null_type_null_variable";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_variables_non_null_type_default_value() -> anyhow::Result<()> {
    let test_path_string = "execute/variables/non_null_type_default_value";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}

#[test]
fn test_variables_non_null_type_default_value_null() -> anyhow::Result<()> {
    let test_path_string = "execute/variables/non_null_type_default_value_null";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/common_metadata/postgres_connector_ndc_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/common_metadata/postgres_connector_ndc_v02_schema.json"],
            ),
        ]),
    )
}
