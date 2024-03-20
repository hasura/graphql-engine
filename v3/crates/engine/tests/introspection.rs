#[allow(dead_code)]
mod common;

#[test]
fn test_introspect_command_with_preset_arguments() {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/command_argument_presets/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

#[test]
fn test_introspect_model_with_preset_arguments_select_many() {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/model_argument_presets_select_many/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

#[test]
fn test_introspect_model_with_preset_arguments_select_one() {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/model_argument_presets_select_one/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}

#[test]
fn test_graphql_deprecated() {
    let common_custom_connector_path_string =
        "execute/common_metadata/custom_connector_schema.json";
    common::test_introspection_expectation(
        "execute/deprecated",
        &[common_custom_connector_path_string],
    );
}
