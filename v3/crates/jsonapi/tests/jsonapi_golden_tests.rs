//! Tests that run JSONAPI to see if it works

use std::path::PathBuf;
use std::str::FromStr;

#[test]
fn test_get_requests() {
    insta::glob!("fixtures/**/*.txt", |path| {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all() // this enables time and IO
            .build()
            .unwrap();

        runtime.block_on(async {
            let directory = path.parent().unwrap();
            let model_name = path.file_stem().unwrap().to_str().unwrap();

            let query_params = std::fs::read_to_string(path).unwrap_or_else(|error| {
                panic!(
                    "{}: Could not read file {path:?}: {error}",
                    directory.display()
                )
            });

            let jsonapi_query = jsonapi_library::query::Query::from_params(&query_params);

            // Setup test context
            let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
            let metadata_path = root_test_dir.join("static").join("metadata.json");

            let metadata_string =
                std::fs::read_to_string(metadata_path.clone()).unwrap_or_else(|error| {
                    panic!(
                        "{}: Could not read file {path:?}: {error}",
                        metadata_path.display()
                    )
                });

            let metadata_json_value = serde_json::from_str(&metadata_string).unwrap();

            let input_metadata: open_dds::Metadata = open_dds::traits::OpenDd::deserialize(
                metadata_json_value,
                jsonpath::JSONPath::new(),
            )
            .unwrap();

            let configuration = get_metadata_resolve_configuration();

            let (resolved_metadata, _) = metadata_resolve::resolve(input_metadata, &configuration)
                .unwrap_or_else(|error| {
                    panic!(
                        "{}: Could not resolve metadata: {error}",
                        directory.display()
                    )
                });

            let jsonapi_state = jsonapi::State::new(&resolved_metadata);

            // always test in `default` subgraph for now
            let path = format!("/default/{model_name}");

            let http_context = execute::HttpContext {
                client: reqwest::Client::new(),
                ndc_response_size_limit: None,
            };

            let result = jsonapi::handler_internal(
                &http_context,
                &jsonapi_state,
                &resolved_metadata,
                axum::http::method::Method::GET,
                axum::http::uri::Uri::from_str(&path).unwrap(),
                jsonapi_query,
            )
            .await;

            insta::assert_debug_snapshot!("result", result);
        });
    });
}

fn get_metadata_resolve_configuration() -> metadata_resolve::configuration::Configuration {
    let unstable_features = metadata_resolve::configuration::UnstableFeatures {
        enable_order_by_expressions: false,
        enable_ndc_v02_support: false,
        enable_subscriptions: true,
        enable_jsonapi: true,
    };

    metadata_resolve::configuration::Configuration {
        allow_unknown_subgraphs: false,
        unstable_features,
    }
}
