use std::collections::BTreeMap;

use metadata_resolve::data_connectors::NdcVersion;

mod common;

#[test]
fn test_model_select_one_simple_select() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_one/simple_select";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_one_with_type_permission() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_one/type_permission";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_one_simple_select_introspection() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_one/simple_select/introspection/introspection";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_one_filter() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_one/simple_select/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_one_custom_scalar() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_one/custom_scalar";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// Select Many Tests
#[test]
fn test_model_select_many_simple_select() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/simple_select";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

#[test]
fn test_model_select_many_simple_select_introspection() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/simple_select/introspection/introspection";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_simple_select_introspection_user_1() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/simple_select/introspection/introspection_user_1";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_simple_select_introspection_with_graphql_config() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/simple_select/introspection/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let metadata_graphql_json = "execute/models/select_many/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, metadata_graphql_json],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_filter() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/simple_select/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

#[test]
fn test_model_select_many_empty_select() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/empty_select";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
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
        common::TestOpenDDPipeline::TestNDCResponses,
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
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

// Nested selection tests
#[test]
fn test_model_select_many_nested_select() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/nested_select";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

// Same test as above, but with no type mappings defined in the metadata.
// Tests that the engine will correctly add default type mappings
// When resolving the metadata.
#[test]
fn test_model_select_many_nested_select_no_explicit_type_mapping() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/nested_select/no_explicit_type_mapping";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

// Same test as the nested selection
#[test]
fn test_model_select_many_nested_select_with_relationship() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/nested_select/relationship";
    let common_metadata_paths = [
        "execute/common_metadata/custom_connector_v02_schema.json",
        "execute/models/select_many/nested_select/metadata.json",
    ];
    common::test_execution_expectation(
        test_path_string,
        &common_metadata_paths,
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

// nested selection tests, using Postgres
#[test]
fn test_model_select_many_nested_select_postgres() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/nested_select/postgres";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

// Order By Tests
#[test]
fn test_model_select_many_order_by() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_order_by_with_model_v2() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/with_model_v2";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_order_by_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/nested";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_order_by_filter() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_order_by_with_graphql_config() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let metadata_graphql_json = "execute/models/select_many/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, metadata_graphql_json],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
    let test_path_string = "execute/models/select_many/order_by/multiple_columns";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_order_by_multiple_columns_validation_check() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/order_by_validation_check";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// Type Permissions
#[test]
fn test_model_select_many_type_permission_order_by() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/type_permission/order_by";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// Relationships in order_by expressions
// What is being tested:
// 1. Object relationships in order_by expressions (Simple, Nested Object relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_order_by_object_relationship_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/relationships/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/order_by/relationships/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_order_by_object_relationship_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/relationships/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/order_by/relationships/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_type_permission_where() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/type_permission/where";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// Where Tests
#[test]
fn test_model_select_many_where() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// the test here is that two Models can both use the same ObjectBooleanExpressionType without
// errors
#[test]
fn test_model_select_many_shared_boolean_expression() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/shared_boolean_expression";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_boolean_expression_type() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/boolean_expression_type";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_where_nested_select_object() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/nested_select/object";
    let shared_metadata = "execute/models/select_many/where/nested_select/common-metadata.json";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[shared_metadata, common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_where_nested_select_array() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/nested_select/array";
    let shared_metadata = "execute/models/select_many/where/nested_select/common-metadata.json";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[shared_metadata, common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// is_null tests

// old boolean expressions
#[test]
fn test_model_select_many_where_is_null_object_boolean_expression_type() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/where/is_null/object_boolean_expression_type";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// new boolean expressions
#[test]
fn test_model_select_many_where_is_null_boolean_expression_type() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/is_null/boolean_expression_type";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// end of is_null tests

#[test]
fn test_model_select_many_where_filter() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_where_with_grapqhl_config() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let metadata_graphql_json = "execute/models/select_many/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, metadata_graphql_json],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_where_multiple_fields() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/multiple_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_select_with_args() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/select_with_args";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_select_with_args_filter() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/select_with_args/filter";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_select_with_args_with_graphql_config() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/select_with_args/with_graphql_config";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let metadata_graphql_json = "execute/models/select_many/common_metadata/graphql_config.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, metadata_graphql_json],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_model_select_many_where_ndc_operators() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/ndc_operators";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// Relationships in boolean expressions

// Older style: using `ObjectBooleanExpressionType` and `DataConnectorScalarType`

// What is being tested:
// 1. Array relationships in boolean expressions (Simple, Nested array relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_object_boolean_array_relationship_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/relationships/object_boolean_expression_type/array/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_where_object_boolean_array_relationship_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/relationships/object_boolean_expression_type/array/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Object relationships in boolean expressions (Simple, Nested object relationships). We also test multi column boolean expressions
#[test]
fn test_model_select_many_where_object_boolean_object_relationship_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/relationships/object_boolean_expression_type/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_where_object_boolean_object_relationship_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/relationships/object_boolean_expression_type/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Newer style: using `BooleanExpressionType`

// What is being tested:
// 1. Array relationships in boolean expressions (Simple, Nested array relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_array_relationship_simple() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/where/relationships/boolean_expression_type/array/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_where_array_relationship_nested() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/where/relationships/boolean_expression_type/array/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Object relationships in boolean expressions (Simple, Nested object relationships). We also test multi column boolean expressions
#[test]
fn test_model_select_many_where_object_relationship_simple() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/where/relationships/boolean_expression_type/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_where_object_relationship_nested() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/where/relationships/boolean_expression_type/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Remote relationships in boolean expressions

// What is being tested:
// 1. Remote array relationships in boolean expressions (Simple, Nested relationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_remote_array_relationship_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/remote_relationships/boolean_expression_type/array/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_where_remote_array_relationship_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/remote_relationships/boolean_expression_type/array/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Remote object relationships in boolean expressions (Simple, Nested rrelationships). We also test multi column boolean expressions

#[test]
fn test_model_select_many_where_remote_object_relationship_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/remote_relationships/boolean_expression_type/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_where_remote_object_relationship_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/remote_relationships/boolean_expression_type/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/remote_relationships/boolean_expression_type/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_where_remote_object_relationship_simple_across_subgraphs(
) -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/remote_relationships/boolean_expression_type/object/simple_across_subgraphs";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_model_select_many_object_type_input_arguments() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/object_type_input_arguments";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// Limit Tests
#[test]
fn test_model_select_many_limit() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/limit_offset/limit";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_path_string =
        "execute/models/select_many/limit_offset/common_metadata/metadata.json";

    common::test_execution_expectation(
        test_path_string,
        &[ndc_metadata_path_string, common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// Test is_null in model select permissions
#[test]
fn test_model_select_many_predicate_is_null() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/predicate/is_null";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";

    common::test_execution_expectation(
        test_path_string,
        &[ndc_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// ---------- Offset Tests
#[test]
fn test_model_select_many_offset() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/limit_offset/offset";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_path_string =
        "execute/models/select_many/limit_offset/common_metadata/metadata.json";

    common::test_execution_expectation(
        test_path_string,
        &[ndc_metadata_path_string, common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

// ---------- Limit and Offset Tests
#[test]
fn test_model_select_many_limit_offset() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/limit_offset/limit_offset";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_path_string =
        "execute/models/select_many/limit_offset/common_metadata/metadata.json";

    common::test_execution_expectation(
        test_path_string,
        &[ndc_metadata_path_string, common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

// ----------- Limit and Offset negative value test
#[test]
fn test_model_select_many_negative_limit_offset() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/limit_offset/unexpected_value";
    let ndc_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_metadata_path_string =
        "execute/models/select_many/limit_offset/common_metadata/metadata.json";

    common::test_execution_expectation(
        test_path_string,
        &[ndc_metadata_path_string, common_metadata_path_string],
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

#[test]
fn test_relay() -> anyhow::Result<()> {
    let test_path_string = "execute/relay/relay";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
/// Tests the generation of the `id` field in select queries including relationships.
fn test_relay_id_in_select() -> anyhow::Result<()> {
    let test_path_string = "execute/relay/relay_id_in_select";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
/// Test querying the relay global ID with a role that doesn't have access to
/// all the global ID fields.
fn test_relay_global_id_permission() -> anyhow::Result<()> {
    let test_path_string = "execute/relay/relay_global_id_permission";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_relay_node_field() -> anyhow::Result<()> {
    let test_path_string = "execute/relay/relay_node_field";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let common_articles_metadata_path_string = "execute/relay/article_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_articles_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_relay_node_type_permissions() -> anyhow::Result<()> {
    let test_path_string = "execute/relay/relay_node_type_permissions";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_relay_node_field_permission() -> anyhow::Result<()> {
    let test_path_string = "execute/relay/relay_node_field_permissions";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

#[test]
/// Tests a role should not be able to access the relay `node` field,
/// if the Node interface doesn't implement any objects for that role.
fn test_relay_node_interface_permissions() -> anyhow::Result<()> {
    let test_path_string = "execute/relay/relay_node_interface_permission";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

#[test]
/// Tests the `node` root field with a role that has no model select permissions.
fn test_relay_node_model_select_permissions_with_role_without_model_select_permission(
) -> anyhow::Result<()> {
    let test_path_string =
        "execute/relay/relay_node_model_select_permissions/no_select_permission_exists_for_role";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
/// Test the `node` root field with a role that has model select permissions.
fn test_relay_node_model_select_permissions() -> anyhow::Result<()> {
    let test_path_string =
        "execute/relay/relay_node_model_select_permissions/successful_model_select_permissions";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_typename() -> anyhow::Result<()> {
    let test_path_string = "execute/typename";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
        common::TestOpenDDPipeline::TestNDCResponses,
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
        common::TestOpenDDPipeline::TestNDCResponses,
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
        common::TestOpenDDPipeline::TestNDCResponses,
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
        common::TestOpenDDPipeline::TestNDCResponses,
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
        common::TestOpenDDPipeline::TestNDCResponses,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
    )
}

// Tests a mutation command with a boolean expression as an argument:
// arguments: 1 boolean expression as a preset
// output: object (commandActor) output type
// permission: different permissions and preset arguments for roles: admin, user_1, user_2

// old `object_boolean_expression_type`
#[test]
fn test_boolean_expression_command_argument_presets_object_boolean_expression_type(
) -> anyhow::Result<()> {
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,

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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
        common::TestOpenDDPipeline::TestNDCResponses,
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
        common::TestOpenDDPipeline::GenerateOpenDDQuery,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
    )
}

// Tests using relationships in predicates
// Array relationship
#[test]
fn test_model_select_many_relationship_predicate_array_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/relationship_predicates/array/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Tests using relationships in predicates

// Nested Array relationship
#[test]
fn test_model_select_many_relationship_predicate_array_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/relationship_predicates/array/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Nested Array relationship with multiple fields
#[test]
fn test_model_select_many_relationship_predicate_array_nested_multiple_fields() -> anyhow::Result<()>
{
    let test_path_string =
        "execute/models/select_many/relationship_predicates/array/nested_multiple_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Tests using relationships in predicates
// Object relationship
#[test]
fn test_model_select_many_relationship_predicate_object_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/relationship_predicates/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Tests using relationships in predicates
// Nested bject relationship
#[test]
fn test_model_select_many_relationship_predicate_object_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/relationship_predicates/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
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
    let test_path_string = "execute/models/select_many/relationship_predicates/on_two_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
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
fn test_model_select_many_relationship_predicate_object_two_relationship_fields(
) -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/relationship_predicates/object/two_relationship_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Tests using remote relationships in predicates
// Array relationship
#[test]
fn test_model_select_many_remote_relationship_predicate_array_simple() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/remote_relationship_predicates/array/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/remote_relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Tests using remote relationships in predicates

// Nested Array relationship
#[test]
fn test_model_select_many_remote_relationship_predicate_array_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/remote_relationship_predicates/array/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/remote_relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Nested Array relationship with multiple fields
#[test]
fn test_model_select_many_remote_relationship_predicate_array_nested_multiple_fields(
) -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/remote_relationship_predicates/array/nested_multiple_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/remote_relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Tests using remote relationships in predicates
// Object relationship
#[test]
fn test_model_select_many_remote_relationship_predicate_object_simple() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/remote_relationship_predicates/object/simple";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/remote_relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

// Tests using remote relationships in predicates
// Nested object relationship
#[test]
fn test_model_select_many_remote_relationship_predicate_object_nested() -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/remote_relationship_predicates/object/nested";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/remote_relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
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
    let test_path_string =
        "execute/models/select_many/remote_relationship_predicates/on_two_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/remote_relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
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
fn test_model_select_many_remote_relationship_predicate_object_two_relationship_fields(
) -> anyhow::Result<()> {
    let test_path_string =
        "execute/models/select_many/remote_relationship_predicates/object/two_relationship_fields";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/remote_relationship_predicates/common_metadata.json";
    common::test_execution_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_graphql_descriptions() -> anyhow::Result<()> {
    common::test_execution_expectation_for_multiple_ndc_versions(
        "execute/description",
        &[
            "execute/common_metadata/postgres_connector_schema.json",
            "execute/common_metadata/command_metadata.json",
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
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_apollo_federation_service_sdl() -> anyhow::Result<()> {
    let test_path_string = "execute/apollo_federation_fields/service_sdl";
    let common_apollo_metadata = "execute/apollo_federation_fields/common_metadata.json";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, common_apollo_metadata],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_apollo_federation_entities() -> anyhow::Result<()> {
    let test_path_string = "execute/apollo_federation_fields/entities";
    let common_apollo_metadata = "execute/apollo_federation_fields/common_metadata.json";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string, common_apollo_metadata],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_aggregates_root_field_simple_select() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/simple_select";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_aggregates_root_field_filtering() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/filtering";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_aggregates_root_field_nested_object() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/nested_object";
    common::test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/custom_connector_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        BTreeMap::from([
            (
                NdcVersion::V01,
                vec!["execute/aggregates/common_metadata/custom_connector_v01_schema.json"],
            ),
            (
                NdcVersion::V02,
                vec!["execute/aggregates/common_metadata/custom_connector_v02_schema.json"],
            ),
        ]),
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_aggregates_root_field_typename() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/typename";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_aggregates_relationship_field_simple_select() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/relationship_field/simple_select";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        common::TestOpenDDPipeline::Skip,
    )
}

#[test]
fn test_aggregates_relationship_field_filtering() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/relationship_field/filtering";
    common::test_execution_expectation(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::Skip,
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
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

// Tests of session variables

#[test]
fn test_session_variables_json_enabled_array_session_variable() -> anyhow::Result<()> {
    let test_path_string = "execute/session_variables/json_enabled/array_session_variable";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

#[test]
fn test_session_variables_json_enabled_integer_session_variable() -> anyhow::Result<()> {
    let test_path_string = "execute/session_variables/json_enabled/integer_session_variable";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}

#[test]
fn test_session_variables_json_disabled_integer_session_variable() -> anyhow::Result<()> {
    let test_path_string = "execute/session_variables/json_disabled/integer_session_variable";
    let common_metadata_path_string = "execute/common_metadata/postgres_connector_schema.json";
    common::test_execution_expectation(
        test_path_string,
        &[common_metadata_path_string],
        common::TestOpenDDPipeline::TestNDCResponses,
    )
}
