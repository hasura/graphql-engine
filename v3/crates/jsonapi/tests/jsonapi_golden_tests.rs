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
            let TestEnvironment {
                jsonapi_catalog,
                resolved_metadata,
            } = test_environment_setup();

            let TestRequest { query, model_name } = test_request_setup(path);

            // always test in `default` subgraph for now
            let path = format!("/default/{model_name}");

            let http_context = execute::HttpContext {
                client: reqwest::Client::new(),
                ndc_response_size_limit: None,
            };

            let session = create_default_session();

            let result = jsonapi::handler_internal(
                Arc::new(HeaderMap::default()),
                Arc::new(http_context.clone()),
                Arc::new(session.clone()),
                &jsonapi_catalog,
                &resolved_metadata,
                axum::http::method::Method::GET,
                axum::http::uri::Uri::from_str(&path).unwrap(),
                query,
            )
            .await;

            match result {
                Ok(result) => {
                    insta::assert_debug_snapshot!(
                        format!("result_for_role_{}", session.role),
                        result
                    );
                }
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
            let TestEnvironment {
                jsonapi_catalog,
                resolved_metadata,
            } = test_environment_setup();

            let TestRequest { query, model_name } = test_request_setup(path);

            // always test in `default` subgraph for now
            let path = format!("/default/{model_name}");

            let http_context = execute::HttpContext {
                client: reqwest::Client::new(),
                ndc_response_size_limit: None,
            };

            let session = create_default_session();

            let result = jsonapi::handler_internal(
                Arc::new(HeaderMap::default()),
                Arc::new(http_context.clone()),
                Arc::new(session.clone()),
                &jsonapi_catalog,
                &resolved_metadata,
                axum::http::method::Method::GET,
                axum::http::uri::Uri::from_str(&path).unwrap(),
                query,
            )
            .await;

            insta::assert_debug_snapshot!(format!("error_for_role_{}", session.role), result);
        });
    });
}

#[test]
fn test_openapi_generation() {
    let TestEnvironment {
        jsonapi_catalog,
        resolved_metadata: _,
    } = test_environment_setup();

    for (role, state) in &jsonapi_catalog.state_per_role {
        let generated_openapi = jsonapi::openapi_schema(state);

        // if the test fails, let's take a look at what was generated
        dbg!(&serde_json::to_value(&generated_openapi)
            .unwrap()
            .to_string());

        insta::assert_json_snapshot!(
            format!("generated_openapi_for_role_{role}"),
            generated_openapi
        );
    }
}

struct TestRequest {
    query: jsonapi_library::query::Query,
    model_name: String,
}

struct TestEnvironment {
    jsonapi_catalog: jsonapi::Catalog,
    resolved_metadata: metadata_resolve::Metadata,
}

fn trim_newline(s: &mut String) {
    if s.ends_with('\n') {
        s.pop();
        if s.ends_with('\r') {
            s.pop();
        }
    }
}

fn test_environment_setup() -> TestEnvironment {
    // Setup test context
    let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    let metadata_path = root_test_dir.join("static").join("metadata.json");

    let metadata_string = std::fs::read_to_string(metadata_path.clone()).unwrap_or_else(|error| {
        panic!("{}: Could not read file: {error}", metadata_path.display())
    });

    let metadata_json_value = serde_json::from_str(&metadata_string).unwrap();

    let input_metadata: open_dds::Metadata =
        open_dds::traits::OpenDd::deserialize(metadata_json_value, jsonpath::JSONPath::new())
            .unwrap();

    let configuration = get_metadata_resolve_configuration();

    let (resolved_metadata, _) = metadata_resolve::resolve(input_metadata, &configuration)
        .unwrap_or_else(|error| panic!("Could not resolve metadata: {error}",));

    let (jsonapi_catalog, _warnings) = jsonapi::Catalog::new(&resolved_metadata);

    TestEnvironment {
        jsonapi_catalog,
        resolved_metadata,
    }
}

fn test_request_setup(path: &Path) -> TestRequest {
    let directory = path.parent().unwrap();
    let model_name = path.file_stem().unwrap().to_str().unwrap();

    let mut query_params = std::fs::read_to_string(path).unwrap_or_else(|error| {
        panic!(
            "{}: Could not read file {path:?}: {error}",
            directory.display()
        )
    });

    // our input files contain trailing newlines that break the JSONAPI parser
    trim_newline(&mut query_params);

    let jsonapi_query = jsonapi_library::query::Query::from_params(&query_params);

    TestRequest {
        query: jsonapi_query,
        model_name: model_name.to_string(),
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
        enable_ndc_v02_support: false,
        enable_jsonapi: true,
        enable_aggregation_predicates: false,
    };

    metadata_resolve::configuration::Configuration { unstable_features }
}
