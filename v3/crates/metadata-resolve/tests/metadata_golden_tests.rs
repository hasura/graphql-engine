//! Tests that attempt to resolve different metadata files and assert that they parse successfully
//! or fail in the expected way.

use metadata_resolve::MetadataResolveFlagsInternal;

#[test]
fn test_passing_metadata() {
    insta::glob!("passing/**/metadata.json", |path| {
        insta::with_settings!({
            snapshot_path => path.parent().unwrap(),
            snapshot_suffix => "",
            prepend_module_to_snapshot => false,
        }, {
            let metadata_resolve_flags_internal = MetadataResolveFlagsInternal {
                enable_boolean_expression_types: true,
            };

            let metadata_json_text = std::fs::read_to_string(path)
                .unwrap_or_else(|error| panic!("Could not read file {path:?}: {error}"));

            let metadata_json_value = serde_json::from_str(&metadata_json_text)
                .unwrap_or_else(|error| panic!("Could not parse JSON: {error}"));

            let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value)
                .unwrap_or_else(|error| panic!("Could not deserialize metadata: {error}"));

            let resolved = metadata_resolve::resolve(metadata, metadata_resolve_flags_internal)
                .unwrap_or_else(|error| panic!("Could not resolve metadata: {error}"));

            insta::assert_debug_snapshot!("resolved", resolved);
        });
    });
}

#[test]
fn test_failing_metadata() {
    insta::glob!("failing/**/metadata.json", |path| {
        insta::with_settings!({
            snapshot_path => path.parent().unwrap(),
            snapshot_suffix => "",
            prepend_module_to_snapshot => false,
        }, {
            let metadata_resolve_flags_internal = MetadataResolveFlagsInternal {
                enable_boolean_expression_types: true,
            };

            let metadata_json_text = std::fs::read_to_string(path)
                .unwrap_or_else(|error| panic!("Could not read file {path:?}: {error}"));

            match serde_json::from_str(&metadata_json_text) {
                Ok(metadata_json_value) => {
                    match open_dds::traits::OpenDd::deserialize(metadata_json_value) {
                        Ok(metadata) => {
                            match metadata_resolve::resolve(metadata, metadata_resolve_flags_internal) {
                                Ok(_) => {
                                    panic!("Unexpected success when resolving {path:?}.");
                                }
                                Err(msg) => {
                                    insta::assert_snapshot!("resolve_error", msg);
                                }
                            }
                        }
                        Err(msg) => {
                            insta::assert_snapshot!("deserialize_error", msg);
                        }
                    };
                }

                Err(msg) => {
                    insta::assert_snapshot!("parse_error", msg);
                }
            };
        });
    });
}
