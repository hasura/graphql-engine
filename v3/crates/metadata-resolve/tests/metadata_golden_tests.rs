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

            let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value, jsonpath::JSONPath::new())
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
                    match open_dds::traits::OpenDd::deserialize(metadata_json_value, jsonpath::JSONPath::new()) {
                        Ok(metadata) => {
                            match metadata_resolve::resolve(metadata, &configuration) {
                                Ok(_) => {
                                    panic!("{}: Unexpected success when resolving {path:?}.", directory.display());
                                }
                                Err(msg) => {
                                    let config = ariadne::Config::new().with_color(false);

                                    match metadata_resolve::to_fancy_error(metadata_json_text.as_str(),&msg,config) {
                                        Some(report) =>
                                        {
                                            // write an ariadne error to a String
                                            let mut buf = Vec::new();
                                            let () = report.write(ariadne::Source::from(&metadata_json_text),&mut buf).unwrap();
                                            let string = String::from_utf8(buf).unwrap();
                                            insta::assert_snapshot!("resolve_error",string);
                                        }
                                        None => {
                                    insta::assert_snapshot!("resolve_error", msg);


                                        }
                                    }
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
        enable_aggregation_predicates: true,
    };

    let configuration_path = directory.join("configuration.json");
    if configuration_path.exists() {
        let reader = fs::File::open(configuration_path)?;
        let configuration = serde_json::from_reader(reader)?;
        Ok(configuration)
    } else {
        Ok(configuration::Configuration { unstable_features })
    }
}
