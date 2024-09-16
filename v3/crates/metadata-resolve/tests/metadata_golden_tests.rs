//! Tests that attempt to resolve different metadata files and assert that they parse successfully
//! or fail in the expected way.

use std::fs;
use std::path::Path;

use metadata_resolve::configuration;

#[test]
fn test_passing_metadata() {
    insta::glob!("passing/**/metadata.json", |path| {
        let directory = path.parent().unwrap();
        insta::with_settings!({
            snapshot_path => directory,
            snapshot_suffix => "",
            prepend_module_to_snapshot => false,
        }, {
            let configuration = read_test_configuration(directory)
                .unwrap_or_else(|error| panic!("{}: Could not read configuration: {error}",directory.display()));

            let metadata_json_text = std::fs::read_to_string(path)
                .unwrap_or_else(|error| panic!("{}: Could not read file {path:?}: {error}", directory.display()));

            let metadata_json_value = serde_json::from_str(&metadata_json_text)
                .unwrap_or_else(|error| panic!("{}: Could not parse JSON: {error}",directory.display()));

            let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value)
                .unwrap_or_else(|error| panic!("{}: Could not deserialize metadata: {error}", directory.display()));

            let resolved = metadata_resolve::resolve(metadata, &configuration)
                .unwrap_or_else(|error| panic!("{}: Could not resolve metadata: {error}",directory.display()));

            insta::assert_debug_snapshot!("resolved", resolved);
        });
    });
}

#[test]
fn test_failing_metadata() {
    insta::glob!("failing/**/metadata.json", |path| {
        let directory = path.parent().unwrap();
        insta::with_settings!({
            snapshot_path => directory,
            snapshot_suffix => "",
            prepend_module_to_snapshot => false,
        }, {
            let configuration = read_test_configuration(directory)
                .unwrap_or_else(|error| panic!("{}: Could not read configuration: {error}", directory.display()));

            let metadata_json_text = std::fs::read_to_string(path)
                .unwrap_or_else(|error| panic!("{}: Could not read file {path:?}: {error}", directory.display()));

            match serde_json::from_str(&metadata_json_text) {
                Ok(metadata_json_value) => {
                    match open_dds::traits::OpenDd::deserialize(metadata_json_value) {
                        Ok(metadata) => {
                            match metadata_resolve::resolve(metadata, &configuration) {
                                Ok(_) => {
                                    panic!("{}: Unexpected success when resolving {path:?}.", directory.display());
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

fn read_test_configuration(
    directory: &Path,
) -> Result<configuration::Configuration, Box<dyn std::error::Error>> {
    let unstable_features = configuration::UnstableFeatures {
        enable_order_by_expressions: false,
        enable_ndc_v02_support: false,
        enable_subscriptions: false,
        enable_jsonapi: false,
    };

    let configuration_path = directory.join("configuration.json");
    if configuration_path.exists() {
        let reader = fs::File::open(configuration_path)?;
        let configuration = serde_json::from_reader(reader)?;
        Ok(configuration::Configuration {
            unstable_features,
            ..configuration
        })
    } else {
        Ok(configuration::Configuration {
            allow_unknown_subgraphs: false,
            unstable_features,
        })
    }
}
