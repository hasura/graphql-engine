//! Tests that run JSONAPI to see if it works

use engine_types::HttpContext;
use hasura_authn_core::{Identity, Role};
use jsonapi_library::api::{DocumentData, IdentifierData, PrimaryData};
use metadata_resolve::{LifecyclePluginConfigs, ResolvedLifecyclePreResponsePluginHooks};
use reqwest::header::HeaderMap;
use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;

#[test]
fn test_get_succeeding_requests() {
    insta::glob!("passing/**/*.txt", |path| {
        let directory = path.parent().unwrap();
        insta::with_settings!({
            snapshot_path => directory,
            snapshot_suffix => "",
            prepend_module_to_snapshot => false,
        }, {
            let runtime = tokio::runtime::Builder::new_current_thread()
                .enable_all() // this enables time and IO
                .build()
                .unwrap();

            runtime.block_on(async {
                let TestEnvironment {
                    jsonapi_catalog,
                    metadata,
                } = test_environment_setup();

                let TestRequest { query, model_name } = test_request_setup(path);

                // always test in `default` subgraph for now
                let request_path = format!("/default/{model_name}");

                let http_context = HttpContext {
                    client: reqwest::Client::new(),
                    ndc_response_size_limit: None,
                };

                let session = create_default_session();

                let plugins = LifecyclePluginConfigs {
                    pre_parse_plugins: Vec::new(),
                    pre_response_plugins: ResolvedLifecyclePreResponsePluginHooks::new(),
                    pre_route_plugins: Vec::new(),
                    pre_ndc_request_plugins: BTreeMap::new(),
                    pre_ndc_response_plugins: BTreeMap::new(),
                };

                let result = jsonapi::handler_internal(
                    Arc::new(HeaderMap::default()),
                    Arc::new(http_context.clone()),
                    Arc::new(plugins.clone()),
                    Arc::new(session.clone()),
                    &jsonapi_catalog,
                    metadata.into(),
                    axum::http::method::Method::GET,
                    axum::http::uri::Uri::from_str(&request_path).unwrap(),
                    query,
                )
                    .await;

                match result {
                    Ok(result) => {
                        // Assert uniqueness of resources in the response
                        validate_resource_uniqueness(&result).unwrap();
                        // Assert all relationships have corresponding included resources
                        validate_relationships_in_included(&result).unwrap();
                        let file_name = path.file_name().unwrap().to_str().unwrap();
                        insta::assert_debug_snapshot!(
                            format!("result_for_role_{}__{file_name}", session.role),
                            result
                        );
                    }
                    Err(e) => panic!("expected success for {path:?}, instead got {e}"),
                }
            });
        });
    });
}

#[test]
fn test_get_failing_requests() {
    insta::glob!("failing/**/*.txt", |path| {
        let directory = path.parent().unwrap();
        insta::with_settings!({
            snapshot_path => directory,
            snapshot_suffix => "",
            prepend_module_to_snapshot => false,
        }, {
            let runtime = tokio::runtime::Builder::new_current_thread()
                .enable_all() // this enables time and IO
                .build()
                .unwrap();

            runtime.block_on(async {
                let TestEnvironment {
                    metadata,
                    jsonapi_catalog,
                } = test_environment_setup();

                let TestRequest { query, model_name } = test_request_setup(path);

                // always test in `default` subgraph for now
                let request_path = format!("/default/{model_name}");

                let http_context = HttpContext {
                    client: reqwest::Client::new(),
                    ndc_response_size_limit: None,
                };

                let session = create_default_session();

                let plugins = LifecyclePluginConfigs {
                    pre_parse_plugins: Vec::new(),
                    pre_response_plugins: ResolvedLifecyclePreResponsePluginHooks::new(),
                    pre_route_plugins: Vec::new(),
                    pre_ndc_request_plugins: BTreeMap::new(),
                    pre_ndc_response_plugins: BTreeMap::new()
                };

                let result = jsonapi::handler_internal(
                    Arc::new(HeaderMap::default()),
                    Arc::new(http_context.clone()),
                    Arc::new(plugins.clone()),
                    Arc::new(session.clone()),
                    &jsonapi_catalog,
                    metadata.into(),
                    axum::http::method::Method::GET,
                    axum::http::uri::Uri::from_str(&request_path).unwrap(),
                    query,
                )
                    .await;

                let file_name = path.file_name().unwrap().to_str().unwrap();
                insta::assert_debug_snapshot!(format!("error_for_role_{}__{file_name}", session.role), result);
            });
        });
    });
}

#[test]
#[allow(clippy::dbg_macro)]
fn test_openapi_generation() {
    let TestEnvironment {
        jsonapi_catalog,
        metadata: _,
    } = test_environment_setup();

    let tests_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
    insta::with_settings!({
        snapshot_path => tests_dir.join("openapi"),
        snapshot_suffix => "",
        prepend_module_to_snapshot => false,
    }, {
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
    });
}

struct TestRequest {
    query: jsonapi_library::query::Query,
    model_name: String,
}

struct TestEnvironment {
    jsonapi_catalog: jsonapi::Catalog,
    metadata: metadata_resolve::Metadata,
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
        metadata: resolved_metadata,
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

    role_authorization.build_session(BTreeMap::new())
}

fn get_metadata_resolve_configuration() -> metadata_resolve::configuration::Configuration {
    let unstable_features = metadata_resolve::configuration::UnstableFeatures {
        enable_aggregation_predicates: false,
        enable_authorization_rules: false,
    };

    metadata_resolve::configuration::Configuration { unstable_features }
}

/// A Result indicating whether resources are unique, with details of duplicates if not
fn validate_resource_uniqueness(
    document_data: &DocumentData,
) -> Result<(), Vec<(String, String, String)>> {
    // Collect all resources including primary and included
    let mut all_resources = Vec::new();

    // Add primary data resources
    match &document_data.data {
        Some(PrimaryData::Single(resource)) => all_resources.push(resource.as_ref()),
        Some(PrimaryData::Multiple(resources)) => all_resources.extend(resources),
        _ => {}
    }

    // Add included resources if present
    if let Some(included) = &document_data.included {
        all_resources.extend(included);
    }

    // Check uniqueness
    let mut seen = HashSet::new();
    let duplicates: Vec<_> = all_resources
        .iter()
        .filter(|r| !seen.insert((r._type.clone(), r.id.clone())))
        .map(|r| {
            (
                r._type.clone(),
                r.id.clone(),
                "Duplicate resource".to_string(),
            )
        })
        .collect();

    if duplicates.is_empty() {
        Ok(())
    } else {
        Err(duplicates)
    }
}

/// A Result indicating whether all relationships have corresponding included resources
fn validate_relationships_in_included(document_data: &DocumentData) -> Result<(), Vec<String>> {
    // Extract all resources to check
    let mut all_resources = Vec::new();
    match &document_data.data {
        Some(PrimaryData::Single(resource)) => all_resources.push(resource.as_ref()),
        Some(PrimaryData::Multiple(resources)) => all_resources.extend(resources),
        _ => return Ok(()),
    }

    // If no included resources, but relationships exist
    if document_data.included.is_none() {
        let resources_with_relationships: Vec<String> = all_resources
            .iter()
            .filter(|r| r.relationships.is_some())
            .map(|r| r._type.to_string())
            .collect();

        return if resources_with_relationships.is_empty() {
            Ok(())
        } else {
            Err(resources_with_relationships)
        };
    }

    // Get included resources as a lookup set
    let included_resources: HashSet<_> = document_data
        .included
        .as_ref()
        .unwrap()
        .iter()
        .map(|r| (&r._type, &r.id))
        .collect();

    // Check each resource's relationships
    let mut missing_relationships = Vec::new();

    for resource in &all_resources {
        if let Some(relationships) = &resource.relationships {
            for (rel_name, relationship) in relationships {
                match &relationship.data {
                    Some(IdentifierData::Single(identifier)) => {
                        if !included_resources.contains(&(&identifier._type, &identifier.id)) {
                            missing_relationships.push(format!(
                                "Resource type: {}, ID: {}, Missing relationship: {} (type: {}, id: {})",
                                resource._type,
                                resource.id,
                                rel_name,
                                identifier._type,
                                identifier.id
                            ));
                        }
                    }
                    Some(IdentifierData::Multiple(identifiers)) => {
                        for identifier in identifiers {
                            if !included_resources.contains(&(&identifier._type, &identifier.id)) {
                                missing_relationships.push(format!(
                                    "Resource type: {}, ID: {}, Missing relationship: {} (type: {}, id: {})",
                                    resource._type,
                                    resource.id,
                                    rel_name,
                                    identifier._type,
                                    identifier.id
                                ));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    if missing_relationships.is_empty() {
        Ok(())
    } else {
        Err(missing_relationships)
    }
}
