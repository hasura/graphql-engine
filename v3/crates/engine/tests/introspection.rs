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

#[test]
fn test_introspect_boolean_expression_in_command() -> anyhow::Result<()> {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/boolean_expression_command_argument/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    )
}
