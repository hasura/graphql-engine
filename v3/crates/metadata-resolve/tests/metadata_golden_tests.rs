//! Tests that attempt to resolve different metadata files and assert that they parse successfully
//! or fail in the expected way.

use std::fs;
use std::path::{Path, PathBuf};

use serde_json::Value;

use metadata_resolve::MetadataResolveFlagsInternal;

#[test_each::path(glob = "crates/metadata-resolve/tests/passing/*/", name(segments = 2))]
#[allow(clippy::needless_pass_by_value)] // must receive a `PathBuf`
fn test_passing_metadata(comparison_folder_path: PathBuf) -> anyhow::Result<()> {
    let passing_example = comparison_folder_path.join("example.json");

    let metadata_resolve_flags_internal = MetadataResolveFlagsInternal {
        enable_boolean_expression_types: true,
    };

    let metadata_json_value = read_json(&passing_example)?;

    let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value)?;
    let resolved = metadata_resolve::resolve(metadata, &metadata_resolve_flags_internal);

    match resolved {
        Ok(_) => Ok(()),
        Err(msg) => panic!("{msg}"),
    }
}

#[test_each::path(glob = "crates/metadata-resolve/tests/failing/*/", name(segments = 2))]
#[allow(clippy::needless_pass_by_value)] // must receive a `PathBuf`
fn test_failing_metadata(comparison_folder_path: PathBuf) -> anyhow::Result<()> {
    let failing_example = comparison_folder_path.join("example.json");
    let failing_reason = comparison_folder_path.join("expected_error.txt");

    let metadata_resolve_flags_internal = MetadataResolveFlagsInternal {
        enable_boolean_expression_types: true,
    };

    let error_untrimmed = fs::read_to_string(failing_reason).unwrap();
    let error = error_untrimmed.trim();

    match read_json(&failing_example) {
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

fn read_json(path: &Path) -> anyhow::Result<Value> {
    let json_string = fs::read_to_string(path)?;
    let value = serde_json::from_str(&json_string)?;
    Ok(value)
}
