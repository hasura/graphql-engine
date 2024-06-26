#[allow(dead_code)]
mod common;

#[test]
fn test_introspect_command_with_preset_arguments() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
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
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/model_argument_presets_select_many/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

#[test]
fn test_introspect_model_with_preset_arguments_select_one() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
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
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
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
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
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
        "execute/common_metadata/custom_connector_schema.json";

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
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/boolean_expression_command_argument/object_boolean_expression_type",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}

// new `boolean_expression_type`

#[test]
fn test_introspect_boolean_expression_in_command_boolean_expression_type() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/boolean_expression_command_argument/boolean_expression_type",
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
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
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
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
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
            "execute/aggregates/common_metadata/custom_connector_schema.json",
            "execute/aggregates/common_metadata/custom_connector_types.json",
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
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
    )
}

#[test]
fn test_introspect_aggregates_relationship_field_filtering() -> anyhow::Result<()> {
    let test_path_string = "execute/aggregates/relationship_field/filtering";
    common::test_introspection_expectation(
        test_path_string,
        &[
            "execute/aggregates/common_metadata/postgres_connector_schema.json",
            "execute/aggregates/common_metadata/pg_types.json",
            "execute/aggregates/common_metadata/supergraph.json",
        ],
    )
}
