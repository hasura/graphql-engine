//! Tests that attempt to resolve different OpenDD IR queries and assert that they resolve successfully
//! or fail in the expected way.

use hasura_authn_core::{
    Role, RoleAuthorization, Session, SessionVariableList, SessionVariableName,
    SessionVariableValue,
};
use metadata_resolve::configuration;
use reqwest::header::HeaderMap;
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

#[test]
fn test_passing_plan() {
    insta::glob!("passing/**/query.json", |path| {
        let directory = path.parent().unwrap();
        insta::with_settings!({
            snapshot_path => directory,
            snapshot_suffix => "",
            prepend_module_to_snapshot => false,
        }, {

            let query_text = std::fs::read_to_string(path)
                .unwrap_or_else(|error| panic!("{}: Could not read file {path:?}: {error}", directory.display()));

            let query_request: open_dds::query::QueryRequest = serde_json::from_str(&query_text)
                .unwrap_or_else(|error| panic!("{}: Could not deserialize query request: {error}", directory.display()));

            let metadata = test_environment_setup();

            let session = make_test_session("admin",BTreeMap::new());
            let header_map = HeaderMap::new();

            let execution_plan = plan::plan_query_request(&query_request, &metadata,&session,&header_map )
                .unwrap_or_else(|error| panic!("{}: Could not plan query: {error}",directory.display()));

            insta::assert_debug_snapshot!("execution_plan", execution_plan);
        });
    });
}

#[test]
fn test_failing_plan() {
    insta::glob!("failing/**/query.json", |path| {
        let directory = path.parent().unwrap();
        insta::with_settings!({
             snapshot_path => directory,
             snapshot_suffix => "",
             prepend_module_to_snapshot => false,
         }, {

        let query_text = std::fs::read_to_string(path)
            .unwrap_or_else(|error| panic!("{}: Could not read file {path:?}: {error}", directory.display()));

        let query_request: open_dds::query::QueryRequest = serde_json::from_str(&query_text)
            .unwrap_or_else(|error| panic!("{}: Could not deserialize query request: {error}", directory.display()));

        let metadata = test_environment_setup();

        let session = make_test_session("admin",BTreeMap::new());
        let header_map = HeaderMap::new();

        match plan::plan_query_request(&query_request, &metadata,&session,&header_map ) {
            Ok(_execution_plan) => {
                panic!("{}: Unexpected success when resolving {path:?}.", directory.display());
            }
            Err(msg) => {insta::assert_snapshot!("resolve_error", msg);}}
         });
    });
}

fn test_environment_setup() -> metadata_resolve::Metadata {
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

    let configuration = configuration::Configuration {
        unstable_features: configuration::UnstableFeatures {
            enable_aggregation_predicates: true,
        },
    };

    let (resolved_metadata, _) = metadata_resolve::resolve(input_metadata, &configuration)
        .unwrap_or_else(|error| panic!("Could not resolve metadata: {error}",));

    resolved_metadata
}

fn make_test_session(
    role: &str,
    client_session_variables: BTreeMap<SessionVariableName, SessionVariableValue>,
) -> Session {
    let authenticated_session_variables = HashMap::new();

    let role_authorization = RoleAuthorization {
        role: Role::new(role),
        session_variables: authenticated_session_variables,
        allowed_session_variables_from_request: SessionVariableList::All,
    };

    role_authorization.build_session(client_session_variables)
}
