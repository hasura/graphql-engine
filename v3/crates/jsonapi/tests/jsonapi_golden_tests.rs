//! Tests that run JSONAPI to see if it works

use hasura_authn_core::{Identity, Role};
use reqwest::header::HeaderMap;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;

#[test]
fn test_get_succeeding_requests() {
    insta::glob!("passing/**/*.txt", |path| {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all() // this enables time and IO
            .build()
            .unwrap();

        runtime.block_on(async {
            let TestRequest {
                query,
                model_name,
                jsonapi_state,
                resolved_metadata,
            } = test_setup(path);

            // always test in `default` subgraph for now
            let path = format!("/default/{model_name}");

            let http_context = execute::HttpContext {
                client: reqwest::Client::new(),
                ndc_response_size_limit: None,
            };

            let result = jsonapi::handler_internal(
                Arc::new(HeaderMap::default()),
                Arc::new(http_context.clone()),
                Arc::new(create_default_session()),
                &jsonapi_state,
                &resolved_metadata,
                axum::http::method::Method::GET,
                axum::http::uri::Uri::from_str(&path).unwrap(),
                query,
            )
            .await;

            match result {
                Ok(result) => insta::assert_debug_snapshot!("result", result),
                Err(e) => panic!("expected success, instead got {e}"),
            }
        });
    });
}

#[test]
fn test_get_failing_requests() {
    insta::glob!("failing/**/*.txt", |path| {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all() // this enables time and IO
            .build()
            .unwrap();

        runtime.block_on(async {
            let TestRequest {
                query,
                model_name,
                jsonapi_state,
                resolved_metadata,
            } = test_setup(path);

            // always test in `default` subgraph for now
            let path = format!("/default/{model_name}");

            let http_context = execute::HttpContext {
                client: reqwest::Client::new(),
                ndc_response_size_limit: None,
            };

            let result = jsonapi::handler_internal(
                Arc::new(HeaderMap::default()),
                Arc::new(http_context.clone()),
                Arc::new(create_default_session()),
                &jsonapi_state,
                &resolved_metadata,
                axum::http::method::Method::GET,
                axum::http::uri::Uri::from_str(&path).unwrap(),
                query,
            )
            .await;

            match result {
                Ok(_) => panic!("expected failure"),
                Err(e) => insta::assert_debug_snapshot!("error", e),
            }
        });
    });
}

struct TestRequest {
    query: jsonapi_library::query::Query,
    model_name: String,
    jsonapi_state: jsonapi::State,
    resolved_metadata: metadata_resolve::Metadata,
}

fn test_setup(path: &Path) -> TestRequest {
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

    let metadata_string = std::fs::read_to_string(metadata_path.clone()).unwrap_or_else(|error| {
        panic!(
            "{}: Could not read file {path:?}: {error}",
            metadata_path.display()
        )
    });

    let metadata_json_value = serde_json::from_str(&metadata_string).unwrap();

    let input_metadata: open_dds::Metadata =
        open_dds::traits::OpenDd::deserialize(metadata_json_value, jsonpath::JSONPath::new())
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

    TestRequest {
        query: jsonapi_query,
        model_name: model_name.to_string(),
        jsonapi_state,
        resolved_metadata,
    }
}

// we will need to allow tests to define their own session options at some point
// the way that the GraphQL tests defined in `crates/engine/tests/execution.rs` do
fn create_default_session() -> hasura_authn_core::Session {
    //return an arbitrary identity with role emulation enabled
    let authorization = Identity::admin(Role::new("admin"));
    let role = Role::new("admin");
    let role_authorization = authorization.get_role_authorization(Some(&role)).unwrap();

    role_authorization.build_session(HashMap::new())
}

fn get_metadata_resolve_configuration() -> metadata_resolve::configuration::Configuration {
    let unstable_features = metadata_resolve::configuration::UnstableFeatures {
        enable_order_by_expressions: false,
        enable_ndc_v02_support: false,
        enable_subscriptions: true,
        enable_jsonapi: true,
        enable_aggregation_predicates: false,
    };

    metadata_resolve::configuration::Configuration { unstable_features }
}
