//! Tests that attempt to resolve different metadata files and assert that they parse successfully
//! or fail in the expected way.

use std::fs;
use std::path::PathBuf;

use metadata_resolve::MetadataResolveFlagsInternal;

#[test_each::file(
    glob = "crates/metadata-resolve/tests/passing/**/metadata.json",
    name(segments = 2)
)]
fn test_passing_metadata(metadata_json_text: &str) -> anyhow::Result<()> {
    let metadata_resolve_flags_internal = MetadataResolveFlagsInternal {
        enable_boolean_expression_types: true,
    };

    let metadata_json_value = serde_json::from_str(metadata_json_text)?;

    let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value)?;
    let resolved = metadata_resolve::resolve(metadata, &metadata_resolve_flags_internal);

    match resolved {
        Ok(_) => Ok(()),
        Err(msg) => panic!("{msg}"),
    }
}

#[test_each::file(
    glob = "crates/metadata-resolve/tests/failing/**/metadata.json",
    name(segments = 3)
)]
#[allow(clippy::needless_pass_by_value)] // must receive a `PathBuf`
fn test_failing_metadata(
    metadata_json_text: &str,
    metadata_json_path: PathBuf,
) -> anyhow::Result<()> {
    let comparison_folder_path = metadata_json_path.parent().unwrap();
    let failing_reason = comparison_folder_path.join("expected_error.txt");

    let metadata_resolve_flags_internal = MetadataResolveFlagsInternal {
        enable_boolean_expression_types: true,
    };

    let error_untrimmed = fs::read_to_string(failing_reason)?;
    let error = error_untrimmed.trim();

    match serde_json::from_str(metadata_json_text) {
        Ok(metadata_json_value) => {
            match open_dds::traits::OpenDd::deserialize(metadata_json_value) {
                Ok(metadata) => {
                    match metadata_resolve::resolve(metadata, &metadata_resolve_flags_internal) {
                        Ok(_) => panic!("Expected to fail with {error}"),
                        Err(msg) => similar_asserts::assert_eq!(error, msg.to_string()),
                    }
                }
                Err(msg) => similar_asserts::assert_eq!(msg.to_string(), error),
            };
        }

        Err(msg) => {
            similar_asserts::assert_eq!(msg.to_string(), error);
        }
    };

    Ok(())
}
