#[allow(dead_code)]
mod common;

#[test]
fn test_introspect_command_with_preset_arguments() {
    let common_metadata_path_string = "execute/common_metadata/custom_connector_schema.json";
    let common_command_metadata_path_string = "execute/common_metadata/command_metadata.json";

    common::test_introspection_expectation(
        "execute/commands/functions/preset_arguments/",
        &[
            common_metadata_path_string,
            common_command_metadata_path_string,
        ],
    );
}
