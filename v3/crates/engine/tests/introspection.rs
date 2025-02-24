#[allow(dead_code)]
mod common;

#[test]
fn test_introspect_command_with_preset_arguments() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/command_argument_presets/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_introspect_model_with_preset_arguments_select_many() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/model_argument_presets_select_many/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

// this test has an additional nullable argument
#[test]
fn test_introspect_model_with_preset_arguments_select_many_2() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/model_argument_presets_select_many_nullable_arguments/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_introspect_model_with_preset_arguments_select_one() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/model_argument_presets_select_one/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_introspect_input_type_field_presets_on_command() -> anyhow::Result<()> {
    let test_path_string = "execute/input_types/field_presets/commands";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_introspect_input_type_field_presets_on_model_arguments() -> anyhow::Result<()> {
    let test_path_string = "execute/input_types/field_presets/models";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_graphql_deprecated() -> anyhow::Result<()> {
    let common_custom_connector_path_string =
        "execute/common_metadata/custom_connector_v02_schema.json";

    common::test_introspection_expectation(
        "execute/deprecated",
        &[common_custom_connector_path_string],
    )
}

// command arguments with boolean expressions

// old `object_boolean_expression_type`

#[test]
fn test_introspect_boolean_expression_in_command_object_boolean_expression_type(
) -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/boolean_expression_command_argument/object_boolean_expression_type",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ])
}

// new `boolean_expression_type`

#[test]
fn test_introspect_boolean_expression_in_command_boolean_expression_type() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/boolean_expression_command_argument/boolean_expression_type",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

// new `boolean_expression_type` with manually passed argument

#[test]
fn test_introspect_boolean_expression_in_command_boolean_expression_type_passed_in_query(
) -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/boolean_expression_command_argument/passed_by_user",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

// end of command arguments with boolean expressions

#[test]
fn test_introspect_aggregates_root_field_simple_select() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/simple_select";
    common::test_introspection_expectation(
        test_path_string,
        &[
            "execute/aggregates/root_field/simple_select/metadata_ndc_v01.json",
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
    )
}

#[test]
fn test_introspect_aggregates_root_field_filtering() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/filtering";
    common::test_introspection_expectation(
        test_path_string,
        &[
            "execute/aggregates/root_field/filtering/metadata_ndc_v01.json",
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
    )
}

#[test]
fn test_introspect_aggregates_root_field_nested_object() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/nested_object";
    common::test_introspection_expectation(
        test_path_string,
        &[
            "execute/common_metadata/custom_connector_v02_schema.json",
            "execute/aggregates/common_metadata/custom_connector_vBoth_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
    )
}

#[test]
fn test_introspect_aggregates_root_field_custom_count_return_type() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/root_field/custom_count_return_type";
    common::test_introspection_expectation(
        test_path_string,
        &[
            "execute/common_metadata/custom_connector_v02_schema.json",
            "execute/aggregates/common_metadata/custom_connector_v02_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
    )
}

#[test]
fn test_introspect_aggregates_relationship_field_simple_select() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/relationship_field/simple_select";
    common::test_introspection_expectation(
        test_path_string,
        &[
            "execute/aggregates/relationship_field/simple_select/metadata_ndc_v01.json",
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
    )
}

#[test]
fn test_introspect_aggregates_relationship_field_filtering() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/relationship_field/filtering_ndc_v01";
    common::test_introspection_expectation(
        test_path_string,
        &[
            "execute/common_metadata/postgres_connector_ndc_v01_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
    )
}

#[test]
fn test_introspect_relationship_comparison_capabilities_with_object_boolean_expression_types(
) -> anyhow::Result<()> {
    let test_path_string = "execute/relationships/no_relationship_comparison_capability/with_object_boolean_expression_type";
    common::test_introspection_expectation(
        test_path_string,
        &["execute/relationships/no_relationship_comparison_capability/metadata.json"],
    )
}

#[test]
fn test_introspect_relationship_comparison_capabilities_with_boolean_expression_types(
) -> anyhow::Result<()> {
    let test_path_string =
        "execute/relationships/no_relationship_comparison_capability/with_boolean_expression_type";
    common::test_introspection_expectation(
        test_path_string,
        &["execute/relationships/no_relationship_comparison_capability/metadata.json"],
    )
}

#[test]
fn test_introspect_model_select_many_where_object_boolean_array_relationship_simple(
) -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/where/relationships/object_boolean_expression_type/array/simple";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    let boolean_exp_rel_metadata_path_string =
        "execute/models/select_many/where/relationships/object_boolean_expression_type/common_metadata.json";
    common::test_introspection_expectation(
        test_path_string,
        &[
            common_metadata_path_string,
            boolean_exp_rel_metadata_path_string,
        ],
    )
}

// Tests for order by

#[test]
fn test_introspect_model_select_many_order_by() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    common::test_introspection_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_introspect_model_select_many_order_by_with_model_v2() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/with_model_v2";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    common::test_introspection_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_introspect_model_select_many_order_by_nested() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/nested";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_introspection_expectation(test_path_string, &[common_metadata_path_string])
}

#[test]
fn test_introspect_model_select_many_order_by_nested_legacy() -> anyhow::Result<()> {
    let test_path_string = "execute/models/select_many/order_by/nested_legacy";
    let common_metadata_path_string = "execute/common_metadata/custom_connector_v02_schema.json";
    common::test_introspection_expectation(test_path_string, &[common_metadata_path_string])
}

// Tests for subscriptions

// Tests subscription schema generation with introspection queries
#[test]
fn test_subscription_introspection() -> anyhow::Result<()> {
    let test_path_string = "execute/subscriptions/introspection";
    let common_metadata_path_string =
        "execute/common_metadata/postgres_connector_ndc_v01_schema.json";
    common::test_introspection_expectation(test_path_string, &[common_metadata_path_string])
}
