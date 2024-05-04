use goldenfile::{differs::text_diff, Mint};
use hasura_authn_core::{Identity, Role, Session, SessionError, SessionVariableValue};
use lang_graphql::ast::common as ast;
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

use engine::execute::{execute_query, HttpContext};
use engine::schema::GDS;

extern crate json_value_merge;
use json_value_merge::Merge;
use serde_json::Value;

pub struct GoldenTestContext {
    http_context: HttpContext,
    mint: Mint,
}

pub fn setup(test_dir: &Path) -> GoldenTestContext {
    let http_context = HttpContext {
        client: reqwest::Client::new(),
        ndc_response_size_limit: None,
    };
    let mint = Mint::new(test_dir);
    GoldenTestContext { http_context, mint }
}

fn resolve_session(
    session_variables: HashMap<SessionVariable, SessionVariableValue>,
) -> Result<Session, SessionError> {
    //return an arbitrary identity with role emulation enabled
    let authorization = Identity::admin(Role::new("admin"));
    let role = session_variables
        .get(&SESSION_VARIABLE_ROLE)
        .map(|v| Role::new(&v.0));
    let role_authorization = authorization.get_role_authorization(role.as_ref())?;
    let session = role_authorization.build_session(session_variables);
    Ok(session)
}

// This function is deprecated in favour of test_execution_expectation
// TODO: Remove this function after all tests are moved to use test_execution_expectation
pub fn test_execution_expectation_legacy(
    test_path_string: &str,
    common_metadata_paths: &[&str],
) -> anyhow::Result<()> {
    tokio_test::block_on(async {
        // Setup test context
        let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut test_ctx = setup(&root_test_dir);
        let test_path = root_test_dir.join(test_path_string);

        let request_path = test_path.join("request.gql");
        let response_path = test_path_string.to_string() + "/expected.json";
        let metadata_path = test_path.join("metadata.json");

        let metadata_json_value = merge_with_common_metadata(
            &metadata_path,
            common_metadata_paths
                .iter()
                .map(|path| root_test_dir.join(path)),
        )?;

        let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value)?;

        // TODO: remove this assert once we have stopped manually implementing Serialize for OpenDD types.
        assert_eq!(
            open_dds::Metadata::from_json_str(&serde_json::to_string(&metadata)?)?,
            metadata
        );

        let gds = GDS::new(metadata)?;
        let schema = GDS::build_schema(&gds)?;

        // Ensure schema is serialized successfully.
        serde_json::to_string(&schema)?;

        let query = fs::read_to_string(request_path)?;

        let session = {
            let session_vars_path = &test_path.join("session_variables.json");
            let session_variables: HashMap<SessionVariable, SessionVariableValue> =
                json::from_str(fs::read_to_string(session_vars_path)?.as_ref())?;
            resolve_session(session_variables)
        }?;

        let raw_request = RawRequest {
            operation_name: None,
            query,
            variables: None,
        };

        // Execute the test

        let response =
            execute_query(&test_ctx.http_context, &schema, &session, raw_request, None).await;

        let mut expected = test_ctx.mint.new_goldenfile_with_differ(
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
        )?;
        write!(expected, "{}", serde_json::to_string_pretty(&response.0)?)?;
        Ok(())
    })
}

#[allow(dead_code)]
pub(crate) fn test_introspection_expectation(
    test_path_string: &str,
    common_metadata_paths: &[&str],
) -> anyhow::Result<()> {
    tokio_test::block_on(async {
        // Setup test context
        let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut test_ctx = setup(&root_test_dir);
        let test_path = root_test_dir.join(test_path_string);

        let request_path = test_path.join("introspection_request.gql");
        let response_path = test_path_string.to_string() + "/introspection_expected.json";
        let metadata_path = test_path.join("metadata.json");

        let metadata_json_value = merge_with_common_metadata(
            &metadata_path,
            common_metadata_paths
                .iter()
                .map(|path| root_test_dir.join(path)),
        )?;

        let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value)?;

        // TODO: remove this assert once we have stopped manually implementing Serialize for OpenDD types.
        assert_eq!(
            open_dds::Metadata::from_json_str(&serde_json::to_string(&metadata)?)?,
            metadata
        );

        let gds = GDS::new(metadata)?;
        let schema = GDS::build_schema(&gds)?;

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

        let query = fs::read_to_string(request_path)?;

        let session_vars_path = &test_path.join("session_variables.json");
        let sessions: Vec<HashMap<SessionVariable, SessionVariableValue>> =
            json::from_str(fs::read_to_string(session_vars_path)?.as_ref())?;
        let sessions: Vec<Session> = sessions
            .into_iter()
            .map(resolve_session)
            .collect::<Result<_, _>>()?;

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
                &test_ctx.http_context,
                &schema,
                session,
                raw_request.clone(),
                None,
            )
            .await;
            responses.push(response.0);
        }

        let mut expected = test_ctx.mint.new_goldenfile_with_differ(
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
        )?;
        write!(expected, "{}", serde_json::to_string_pretty(&responses)?)?;
        Ok(())
    })
}

pub fn test_execution_expectation(
    test_path_string: &str,
    common_metadata_paths: &[&str],
) -> anyhow::Result<()> {
    tokio_test::block_on(async {
        // Setup test context
        let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut test_ctx = setup(&root_test_dir);
        let test_path = root_test_dir.join(test_path_string);

        let request_path = test_path.join("request.gql");
        let variables_path = test_path.join("variables.json");
        let response_path = test_path_string.to_string() + "/expected.json";
        let metadata_path = test_path.join("metadata.json");

        let metadata_json_value = merge_with_common_metadata(
            &metadata_path,
            common_metadata_paths
                .iter()
                .map(|path| root_test_dir.join(path)),
        )?;

        let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value)?;

        // TODO: remove this assert once we have stopped manually implementing Serialize for OpenDD types.
        assert_eq!(
            open_dds::Metadata::from_json_str(&serde_json::to_string(&metadata)?)?,
            metadata
        );

        let gds = GDS::new(metadata)?;
        let schema = GDS::build_schema(&gds)?;

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

        let query = fs::read_to_string(request_path)?;

        // Read optional GQL query variables.
        // NOTE: It is expected the variables.json file contains a list of
        // variables. Each item in the list corresponding to a session in
        // session_variables.json
        let query_vars: Option<Vec<HashMap<ast::Name, serde_json::Value>>> =
            match fs::read_to_string(variables_path) {
                Ok(query_vars_str) => Some(json::from_str(&query_vars_str)?),
                Err(_) => None,
            };

        let session_vars_path = &test_path.join("session_variables.json");
        let sessions: Vec<HashMap<SessionVariable, SessionVariableValue>> =
            json::from_str(fs::read_to_string(session_vars_path)?.as_ref())?;
        let sessions: Vec<Session> = sessions
            .into_iter()
            .map(resolve_session)
            .collect::<Result<_, _>>()?;

        assert!(
            sessions.len() > 1,
            "Found less than 2 roles in test scenario"
        );

        // Execute the test
        let mut responses = Vec::new();

        match query_vars {
            None => {
                let raw_request = RawRequest {
                    operation_name: None,
                    query,
                    variables: None,
                };
                for session in sessions.iter() {
                    let response = execute_query(
                        &test_ctx.http_context,
                        &schema,
                        session,
                        raw_request.clone(),
                        None,
                    )
                    .await;
                    responses.push(response.0);
                }
            }
            Some(vars) => {
                for (session, variables) in sessions.iter().zip(vars) {
                    let raw_request = RawRequest {
                        operation_name: None,
                        query: query.clone(),
                        variables: Some(variables),
                    };
                    let response = execute_query(
                        &test_ctx.http_context,
                        &schema,
                        session,
                        raw_request.clone(),
                        None,
                    )
                    .await;
                    responses.push(response.0);
                }
            }
        }

        let mut expected = test_ctx.mint.new_goldenfile_with_differ(
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
        )?;
        write!(expected, "{}", serde_json::to_string_pretty(&responses)?)?;
        Ok(())
    })
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

#[allow(dead_code)]
pub fn test_execute_explain(
    test_path_string: &str,
    test_metadata_path: &str,
    common_metadata_paths: &[&str],
) -> anyhow::Result<()> {
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
        )?;
        let gds = GDS::new(open_dds::traits::OpenDd::deserialize(metadata)?)?;
        let schema = GDS::build_schema(&gds)?;
        let session = {
            let session_variables_raw = r#"{
                "x-hasura-role": "admin"
            }"#;
            let session_variables: HashMap<SessionVariable, SessionVariableValue> =
                serde_json::from_str(session_variables_raw)?;
            resolve_session(session_variables)
        }?;
        let query = std::fs::read_to_string(root_test_dir.join(gql_request_file_path))?;
        let raw_request = lang_graphql::http::RawRequest {
            operation_name: None,
            query,
            variables: None,
        };
        let raw_response = engine::execute::explain::execute_explain(
            &test_ctx.http_context,
            &schema,
            &session,
            raw_request,
        )
        .await;

        let response = engine::execute::explain::types::redact_ndc_explain(raw_response);

        let mut expected = test_ctx.mint.new_goldenfile_with_differ(
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
        )?;
        write!(expected, "{}", serde_json::to_string_pretty(&response)?)?;
        Ok(())
    })
}
