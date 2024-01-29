use goldenfile::{differs::text_diff, Mint};
use hasura_authn_core::{Identity, Role, Session, SessionVariableValue};
use lang_graphql::{http::RawRequest, schema::Schema};
use open_dds::session_variables::{SessionVariable, SESSION_VARIABLE_ROLE};
use serde_json as json;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::Path,
    path::PathBuf,
};

use engine::execute::execute_query;
use engine::schema::GDS;

extern crate json_value_merge;
use json_value_merge::Merge;
use serde_json::Value;

pub struct GoldenTestContext {
    http_client: reqwest::Client,
    mint: Mint,
}

pub fn setup(test_dir: &Path) -> GoldenTestContext {
    let http_client = reqwest::Client::new();
    let mint = Mint::new(test_dir);
    GoldenTestContext { http_client, mint }
}

fn resolve_session(session_variables: HashMap<SessionVariable, SessionVariableValue>) -> Session {
    //return an arbitrary identity with role emulation enabled
    let authorization = Identity::admin(Role::new("admin"));

    let role = session_variables
        .get(&SESSION_VARIABLE_ROLE)
        .map(|v| Role::new(&v.0));
    authorization
        .get_role_authorization(role.as_ref())
        .unwrap()
        .build_session(session_variables)
}

// This function is deprecated in favour of test_execution_expectation
// TODO: Remove this function after all tests are moved to use test_execution_expectation
pub fn test_execution_expectation_legacy(test_path_string: &str, common_metadata_paths: &[&str]) {
    tokio_test::block_on(async {
        // Setup test context
        let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut test_ctx = setup(&root_test_dir);
        let test_path = root_test_dir.join(test_path_string);

        let request_path = test_path.join("request.gql");
        let response_path = test_path_string.to_string() + "/expected.json";
        let metadata_path = test_path.join("metadata.json");

        let metadata = merge_with_common_metadata(
            &metadata_path,
            common_metadata_paths
                .iter()
                .map(|path| root_test_dir.join(path)),
        );

        let gds = GDS::new(serde_json::from_value(metadata).unwrap()).unwrap();
        let schema = GDS::build_schema(&gds).unwrap();

        // Ensure schema is serialized successfully.
        serde_json::to_string(&schema).unwrap();

        let query = fs::read_to_string(request_path).unwrap();

        let session = {
            let session_vars_path = &test_path.join("session_variables.json");
            let session_variables: HashMap<SessionVariable, SessionVariableValue> =
                json::from_str(fs::read_to_string(session_vars_path).unwrap().as_ref()).unwrap();
            resolve_session(session_variables)
        };

        let raw_request = RawRequest {
            operation_name: None,
            query,
            variables: None,
        };

        // Execute the test

        let response =
            execute_query(&test_ctx.http_client, &schema, &session, raw_request, None).await;

        let mut expected = test_ctx
            .mint
            .new_goldenfile_with_differ(
                response_path,
                Box::new(|file1, file2| {
                    let json1: serde_json::Value =
                        serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                    let json2: serde_json::Value =
                        serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                    if json1 != json2 {
                        text_diff(file1, file2)
                    }
                }),
            )
            .unwrap();
        write!(
            expected,
            "{}",
            serde_json::to_string_pretty(&response.0).unwrap()
        )
        .unwrap();
    });
}

pub fn test_execution_expectation(test_path_string: &str, common_metadata_paths: &[&str]) {
    tokio_test::block_on(async {
        // Setup test context
        let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut test_ctx = setup(&root_test_dir);
        let test_path = root_test_dir.join(test_path_string);

        let request_path = test_path.join("request.gql");
        let response_path = test_path_string.to_string() + "/expected.json";
        let metadata_path = test_path.join("metadata.json");

        let metadata = merge_with_common_metadata(
            &metadata_path,
            common_metadata_paths
                .iter()
                .map(|path| root_test_dir.join(path)),
        );

        let gds = GDS::new(serde_json::from_value(metadata).unwrap()).unwrap();
        let schema = GDS::build_schema(&gds).unwrap();

        // Verify successful serialization and deserialization of the schema.
        // Hasura V3 relies on the serialized schema for handling requests.
        // Therefore, it is crucial to ensure the functionality of both
        // deserialization and serialization.
        // Testing this within this function allows us to detect errors for any
        // future metadata tests that may be added.
        let serialized_metadata =
            serde_json::to_string(&schema).expect("Failed to serialize schema");
        let deserialized_metadata: Schema<GDS> =
            serde_json::from_str(&serialized_metadata).expect("Failed to deserialize metadata");
        assert_eq!(
            schema, deserialized_metadata,
            "initial built metadata does not match deserialized metadata"
        );

        let query = fs::read_to_string(request_path).unwrap();

        let session_vars_path = &test_path.join("session_variables.json");
        let sessions: Vec<HashMap<SessionVariable, SessionVariableValue>> =
            json::from_str(fs::read_to_string(session_vars_path).unwrap().as_ref()).unwrap();
        let sessions: Vec<Session> = sessions.into_iter().map(resolve_session).collect();

        assert!(
            sessions.len() > 1,
            "Found less than 2 roles in test scenario"
        );

        let raw_request = RawRequest {
            operation_name: None,
            query,
            variables: None,
        };

        // Execute the test
        let mut responses = Vec::new();
        for session in sessions.iter() {
            let response = execute_query(
                &test_ctx.http_client,
                &schema,
                session,
                raw_request.clone(),
                None,
            )
            .await;
            responses.push(response.0);
        }

        let mut expected = test_ctx
            .mint
            .new_goldenfile_with_differ(
                response_path,
                Box::new(|file1, file2| {
                    let json1: serde_json::Value =
                        serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                    let json2: serde_json::Value =
                        serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                    if json1 != json2 {
                        text_diff(file1, file2)
                    }
                }),
            )
            .unwrap();
        write!(
            expected,
            "{}",
            serde_json::to_string_pretty(&responses).unwrap()
        )
        .unwrap();
    });
}

fn read_json(path: &Path) -> Value {
    let json_string = fs::read_to_string(path).unwrap();
    serde_json::from_str(&json_string).unwrap()
}

pub fn merge_with_common_metadata<T: Iterator<Item = PathBuf>>(
    metadata_path: &Path,
    common_metadata_paths: T,
) -> Value {
    let mut metadata = read_json(metadata_path);
    common_metadata_paths.for_each(|path| metadata.merge(&read_json(&path)));
    metadata
}

#[allow(dead_code)]
pub fn test_execute_explain(
    test_path_string: &str,
    test_metadata_path: &str,
    common_metadata_paths: &[&str],
) {
    tokio_test::block_on(async {
        let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut test_ctx = setup(&root_test_dir);
        let test_path = root_test_dir.join(test_path_string);
        let gql_request_file_path = test_path.join("request.gql");
        let expected_response_file = test_path_string.to_string() + "/expected.json";

        let test_metadata_path = root_test_dir.join(test_metadata_path);
        let metadata = merge_with_common_metadata(
            &test_metadata_path,
            common_metadata_paths
                .iter()
                .map(|path| root_test_dir.join(path)),
        );
        let gds = GDS::new(serde_json::from_value(metadata).unwrap()).unwrap();
        let schema = GDS::build_schema(&gds).unwrap();
        let session = {
            let session_variables_raw = r#"{
                "x-hasura-role": "admin"
            }"#;
            let session_variables: HashMap<SessionVariable, SessionVariableValue> =
                serde_json::from_str(session_variables_raw).unwrap();
            resolve_session(session_variables)
        };
        let query = std::fs::read_to_string(root_test_dir.join(gql_request_file_path)).unwrap();
        let raw_request = lang_graphql::http::RawRequest {
            operation_name: None,
            query,
            variables: None,
        };
        let response = engine::execute::explain::execute_explain(
            &test_ctx.http_client,
            &schema,
            &session,
            raw_request,
        )
        .await;

        let mut expected = test_ctx
            .mint
            .new_goldenfile_with_differ(
                expected_response_file,
                Box::new(|file1, file2| {
                    let json1: serde_json::Value =
                        serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                    let json2: serde_json::Value =
                        serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                    if json1 != json2 {
                        text_diff(file1, file2)
                    }
                }),
            )
            .unwrap();
        write!(
            expected,
            "{}",
            serde_json::to_string_pretty(&response).unwrap()
        )
        .unwrap();
    })
}
