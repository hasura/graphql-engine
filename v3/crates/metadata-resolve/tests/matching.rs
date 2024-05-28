//! tests to ensure that old and new ways of expressing metadata result in the same outcome
//! we could use a similar technique to ensure that an upgraded version of a metadata kind is
//! equivalent

#![cfg(test)]
use serde_json::Value;
use std::{
    fs::{self},
    path::Path,
    path::PathBuf,
};
extern crate json_value_merge;
use json_value_merge::Merge;
use metadata_resolve::MetadataResolveFlagsInternal;

// given an old-metadata.json and a new-metadata.json, do they both result in the same resolved
// metadata?
// this may stop working if we start tagging resolved Metadata with the paths to the input items
// in which case we'll need to look at removing those before comparing the output
#[test_each::path(glob = "crates/metadata-resolve/tests/examples/*/", name(segments = 2))]
#[allow(clippy::needless_pass_by_value)] // must receive a `PathBuf`
fn test_matching_output_metadata(comparison_folder_path: PathBuf) -> anyhow::Result<()> {
    let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");

    // get metadata shared between all tests
    let mut common_metadata_paths: Vec<PathBuf> = ["examples/postgres-metadata.json"]
        .iter()
        .map(|path| root_test_dir.join(path))
        .collect();

    // add metadata for this particular test
    let mut shared_metadata_path = comparison_folder_path.clone();
    shared_metadata_path.push("shared-metadata.json");

    common_metadata_paths.push(shared_metadata_path);

    let metadata_resolve_flags_internal = MetadataResolveFlagsInternal {
        enable_boolean_expression_types: true,
    };

    // RESOLVE OLD METADATA
    let mut old_path = comparison_folder_path.clone();
    old_path.push("old-metadata.json");

    let old_metadata_json_value =
        merge_with_common_metadata(&old_path, common_metadata_paths.clone().into_iter())?;

    let old_metadata = open_dds::traits::OpenDd::deserialize(old_metadata_json_value)?;

    let old_resolved = metadata_resolve::resolve(old_metadata, &metadata_resolve_flags_internal)
        .expect("resolve old metadata");

    // RESOLVE NEW METADATA
    let mut new_path = comparison_folder_path.clone();
    new_path.push("new-metadata.json");

    let new_metadata_json_value =
        merge_with_common_metadata(&new_path, common_metadata_paths.into_iter())?;

    let new_metadata = open_dds::traits::OpenDd::deserialize(new_metadata_json_value)?;

    let new_resolved = metadata_resolve::resolve(new_metadata, &metadata_resolve_flags_internal)
        .expect("resolve new metadata");

    similar_asserts::assert_eq!(old_resolved, new_resolved);
    Ok(())
}

fn read_json(path: &Path) -> anyhow::Result<Value> {
    let json_string = fs::read_to_string(path)?;
    let value = serde_json::from_str(&json_string)?;
    Ok(value)
}

pub fn merge_with_common_metadata<T: Iterator<Item = PathBuf>>(
    metadata_path: &Path,
    common_metadata_paths: T,
) -> anyhow::Result<Value> {
    let mut metadata = read_json(metadata_path)?;
    for path in common_metadata_paths {
        let common_metadata = read_json(&path)?;
        metadata.merge(&common_metadata);
    }
    Ok(metadata)
}
