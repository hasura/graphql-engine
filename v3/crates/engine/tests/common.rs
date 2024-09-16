use anyhow::anyhow;
use goldenfile::{differs::text_diff, Mint};
use hasura_authn_core::{Identity, Role, Session, SessionError, SessionVariableValue};
use lang_graphql::ast::common as ast;
use lang_graphql::{http::RawRequest, schema::Schema};
use metadata_resolve::data_connectors::NdcVersion;
use open_dds::session_variables::{SessionVariable, SESSION_VARIABLE_ROLE};
use serde_json as json;
use sql::execute::SqlRequest;
use std::collections::BTreeMap;
use std::iter;
use std::sync::Arc;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::Path,
    path::PathBuf,
};

use execute::HttpContext;
use graphql_frontend::execute_query;
use schema::GDS;

extern crate json_value_merge;
use json_value_merge::Merge;
use serde_json::Value;

pub struct GoldenTestContext {
    pub(crate) http_context: HttpContext,
    pub(crate) mint: Mint,
}

pub fn setup(test_dir: &Path) -> GoldenTestContext {
    let http_context = HttpContext {
        client: reqwest::Client::new(),
        ndc_response_size_limit: None,
    };
    let mint = Mint::new(test_dir);
    GoldenTestContext { http_context, mint }
}

pub(crate) fn resolve_session(
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
#[allow(dead_code)]
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

        let gds = GDS::new(metadata, &test_metadata_resolve_configuration())?;
        let schema = GDS::build_schema(&gds)?;

        // Ensure schema is serialized successfully.
        serde_json::to_string(&schema)?;

        let query = read_to_string(&request_path)?;

        let request_headers = reqwest::header::HeaderMap::new();
        let session = {
            let session_vars_path = &test_path.join("session_variables.json");
            let session_variables: HashMap<SessionVariable, SessionVariableValue> =
                json::from_str(read_to_string(session_vars_path)?.as_ref())?;
            resolve_session(session_variables)
        }?;

        let raw_request = RawRequest {
            operation_name: None,
            query,
            variables: None,
        };

        // Execute the test

        let (_, response) = execute_query(
            execute::ExposeInternalErrors::Expose,
            &test_ctx.http_context,
            &schema,
            &session,
            &request_headers,
            raw_request,
            None,
        )
        .await;

        let mut expected = test_ctx.mint.new_goldenfile_with_differ(
            response_path,
            Box::new(|file1, file2| {
                let json1: serde_json::Value =
                    serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                let json2: serde_json::Value =
                    serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                if json1 != json2 {
                    text_diff(file1, file2);
                }
            }),
        )?;
        write!(
            expected,
            "{}",
            serde_json::to_string_pretty(&response.inner())?
        )?;
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

        let gds = GDS::new(metadata, &test_metadata_resolve_configuration())?;

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

        let query = read_to_string(&request_path)?;

        let request_headers = reqwest::header::HeaderMap::new();
        let session_vars_path = &test_path.join("session_variables.json");
        let sessions: Vec<HashMap<SessionVariable, SessionVariableValue>> =
            json::from_str(read_to_string(session_vars_path)?.as_ref())?;
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
        for session in &sessions {
            let (_, response) = execute_query(
                execute::ExposeInternalErrors::Expose,
                &test_ctx.http_context,
                &schema,
                session,
                &request_headers,
                raw_request.clone(),
                None,
            )
            .await;
            responses.push(response.inner());
        }

        let mut expected = test_ctx.mint.new_goldenfile_with_differ(
            response_path,
            Box::new(|file1, file2| {
                let json1: serde_json::Value =
                    serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                let json2: serde_json::Value =
                    serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                if json1 != json2 {
                    text_diff(file1, file2);
                }
            }),
        )?;
        write!(expected, "{}", serde_json::to_string_pretty(&responses)?)?;
        Ok(())
    })
}

#[allow(dead_code)]
pub fn test_execution_expectation(
    test_path_string: &str,
    common_metadata_paths: &[&str],
) -> anyhow::Result<()> {
    test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        common_metadata_paths,
        BTreeMap::new(),
    )
}

#[allow(clippy::print_stdout, dead_code)]
pub fn test_execution_expectation_for_multiple_ndc_versions(
    test_path_string: &str,
    common_metadata_paths: &[&str],
    common_metadata_paths_per_ndc_version: BTreeMap<NdcVersion, Vec<&str>>,
) -> anyhow::Result<()> {
    tokio_test::block_on(async {
        let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let test_path = root_test_dir.join(test_path_string);

        let metadata_path = test_path.join("metadata.json");
        let request_path = test_path.join("request.gql");
        let variables_path = test_path.join("variables.json");
        let response_path = test_path_string.to_string() + "/expected.json";
        let response_headers_path = test_path.join("expected_headers.json");

        let ndc_version_test_iterations = if common_metadata_paths_per_ndc_version.is_empty() {
            vec![(None, common_metadata_paths.to_vec())]
        } else {
            common_metadata_paths_per_ndc_version
                .into_iter()
                .map(|(ndc_version, version_common_metadata_paths)| {
                    (
                        Some(ndc_version),
                        common_metadata_paths
                            .iter()
                            .copied()
                            .chain(version_common_metadata_paths)
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>()
        };

        for (ndc_version, common_metadata_paths) in ndc_version_test_iterations {
            // Setup test context
            let mut test_ctx = setup(&root_test_dir);

            if let Some(ndc_version) = ndc_version {
                println!("Testing with connectors for NDC version {ndc_version}");
            }

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

            let gds = GDS::new(metadata, &test_metadata_resolve_configuration())?;
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

            let query = read_to_string(&request_path)?;

            // Read optional GQL query variables.
            // NOTE: It is expected the variables.json file contains a list of
            // variables. Each item in the list corresponding to a session in
            // session_variables.json
            let query_vars: Option<Vec<HashMap<ast::Name, serde_json::Value>>> =
                match read_to_string(&variables_path) {
                    Ok(query_vars_str) => Some(json::from_str(&query_vars_str)?),
                    Err(_) => None,
                };

            let request_headers = reqwest::header::HeaderMap::new();
            let session_vars_path = &test_path.join("session_variables.json");
            let sessions: Vec<HashMap<SessionVariable, SessionVariableValue>> =
                json::from_str(read_to_string(session_vars_path)?.as_ref())?;
            let sessions: Vec<Session> = sessions
                .into_iter()
                .map(resolve_session)
                .collect::<Result<_, _>>()?;

            assert!(
                sessions.len() > 1,
                "Found less than 2 roles in test scenario"
            );

            // expected response headers are a `Vec<String>`; one set for each
            // session/role.
            let expected_headers: Option<Vec<Vec<String>>> =
                match read_to_string(&response_headers_path) {
                    Ok(response_headers_str) => Some(json::from_str(&response_headers_str)?),
                    Err(_) => None,
                };

            // Execute the test
            let mut responses = Vec::new();

            match query_vars {
                None => {
                    let raw_request = RawRequest {
                        operation_name: None,
                        query,
                        variables: None,
                    };
                    for session in &sessions {
                        let (_, response) = execute_query(
                            execute::ExposeInternalErrors::Expose,
                            &test_ctx.http_context,
                            &schema,
                            session,
                            &request_headers,
                            raw_request.clone(),
                            None,
                        )
                        .await;
                        responses.push(response.inner());
                    }
                }
                Some(vars) => {
                    for (session, variables) in sessions.iter().zip(vars) {
                        let raw_request = RawRequest {
                            operation_name: None,
                            query: query.clone(),
                            variables: Some(variables),
                        };
                        let (_, response) = execute_query(
                            execute::ExposeInternalErrors::Expose,
                            &test_ctx.http_context,
                            &schema,
                            session,
                            &request_headers,
                            raw_request.clone(),
                            None,
                        )
                        .await;
                        responses.push(response.inner());
                    }
                }
            }

            // assert response headers matches
            if let Some(expected_headers) = expected_headers {
                for (response, expected_response_headers) in responses.iter().zip(expected_headers)
                {
                    for header_name in &expected_response_headers {
                        assert!(
                            response.headers.contains_key(header_name),
                            "Header {header_name:} not found in response headers."
                        );
                    }
                }
            }

            // assert response body matches
            let mut expected = test_ctx.mint.new_goldenfile_with_differ(
                &response_path,
                Box::new(|file1, file2| {
                    let json1: serde_json::Value =
                        serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                    let json2: serde_json::Value =
                        serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                    if json1 != json2 {
                        text_diff(file1, file2);
                    }
                }),
            )?;
            write!(expected, "{}", serde_json::to_string_pretty(&responses)?)?;
        }

        Ok(())
    })
}

fn read_json(path: &Path) -> anyhow::Result<Value> {
    let json_string = read_to_string(path)?;
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

        let configuration = metadata_resolve::configuration::Configuration {
            unstable_features: metadata_resolve::configuration::UnstableFeatures {
                enable_order_by_expressions: false,
                enable_ndc_v02_support: true,
                enable_subscriptions: false,
                enable_jsonapi: false,
            },
            ..Default::default()
        };
        let gds = GDS::new(
            open_dds::traits::OpenDd::deserialize(metadata)?,
            &configuration,
        )?;

        let schema = GDS::build_schema(&gds)?;
        let request_headers = reqwest::header::HeaderMap::new();
        let session = {
            let session_variables_raw = r#"{
                "x-hasura-role": "admin"
            }"#;
            let session_variables: HashMap<SessionVariable, SessionVariableValue> =
                serde_json::from_str(session_variables_raw)?;
            resolve_session(session_variables)
        }?;
        let query = read_to_string(&root_test_dir.join(gql_request_file_path))?;
        let raw_request = lang_graphql::http::RawRequest {
            operation_name: None,
            query,
            variables: None,
        };
        let (_, raw_response) = graphql_frontend::execute_explain(
            execute::ExposeInternalErrors::Expose,
            &test_ctx.http_context,
            &schema,
            &session,
            &request_headers,
            raw_request,
        )
        .await;

        let response = graphql_frontend::redact_ndc_explain(raw_response);

        let mut expected = test_ctx.mint.new_goldenfile_with_differ(
            expected_response_file,
            Box::new(|file1, file2| {
                let json1: serde_json::Value =
                    serde_json::from_reader(File::open(file1).unwrap()).unwrap();
                let json2: serde_json::Value =
                    serde_json::from_reader(File::open(file2).unwrap()).unwrap();
                if json1 != json2 {
                    text_diff(file1, file2);
                }
            }),
        )?;
        write!(expected, "{}", serde_json::to_string_pretty(&response)?)?;
        Ok(())
    })
}

// This is where we'll want to enable pre-release features in tests
pub(crate) fn test_metadata_resolve_configuration() -> metadata_resolve::configuration::Configuration
{
    metadata_resolve::configuration::Configuration {
        allow_unknown_subgraphs: false,
        unstable_features: metadata_resolve::configuration::UnstableFeatures {
            enable_order_by_expressions: false,
            enable_ndc_v02_support: true,
            enable_subscriptions: true,
            enable_jsonapi: false,
        },
    }
}

#[allow(dead_code)]
pub(crate) fn test_sql(test_path_string: &str) -> anyhow::Result<()> {
    tokio_test::block_on(async {
        // Setup test context
        let root_test_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests");
        let mut test_ctx = setup(&root_test_dir);
        let test_path = root_test_dir.join(test_path_string);

        let request_path = test_path.join("query.sql");
        let request_path_json = test_path.join("query.json");
        let headers_path_json = test_path.join("headers.json");
        let response_path = test_path_string.to_string() + "/expected.json";
        let explain_path = test_path_string.to_string() + "/plan.json";
        let metadata_path = root_test_dir.join("sql/metadata.json");

        let metadata_json_value = merge_with_common_metadata(&metadata_path, iter::empty())?;

        let metadata = open_dds::traits::OpenDd::deserialize(metadata_json_value)?;

        // TODO: remove this assert once we have stopped manually implementing Serialize for OpenDD types.
        assert_eq!(
            open_dds::Metadata::from_json_str(&serde_json::to_string(&metadata)?)?,
            metadata
        );

        let gds = GDS::new(metadata, &test_metadata_resolve_configuration())?;
        let schema = GDS::build_schema(&gds)?;

        // Ensure schema is serialized successfully.
        serde_json::to_string(&schema)?;

        let request = if let Ok(content) = read_to_string(&request_path) {
            SqlRequest::new(content)
        } else {
            let json_content = read_to_string(&request_path_json)?;
            serde_json::from_str(&json_content)?
        };

        let header_map = if let Ok(content) = read_to_string(&headers_path_json) {
            let header_map: HashMap<String, String> = serde_json::from_str(&content)?;
            Arc::new(reqwest::header::HeaderMap::try_from(&header_map)?)
        } else {
            Arc::new(reqwest::header::HeaderMap::new())
        };

        let session = Arc::new({
            let session_vars_path = &test_path.join("session_variables.json");
            let session_variables: HashMap<SessionVariable, SessionVariableValue> =
                serde_json::from_str(read_to_string(session_vars_path)?.as_ref())?;
            resolve_session(session_variables)
        }?);

        let catalog = Arc::new(sql::catalog::Catalog::from_metadata(gds.metadata));
        let http_context = Arc::new(test_ctx.http_context);

        // Execute the test

        snapshot_sql(
            &catalog,
            &session,
            &http_context,
            &mut test_ctx.mint,
            explain_path,
            &header_map,
            &SqlRequest::new(format!("EXPLAIN {}", request.sql)),
        )
        .await?;

        snapshot_sql(
            &catalog,
            &session,
            &http_context,
            &mut test_ctx.mint,
            response_path,
            &header_map,
            &request,
        )
        .await?;

        Ok(())
    })
}

async fn snapshot_sql(
    catalog: &Arc<sql::catalog::Catalog>,
    session: &Arc<hasura_authn_core::Session>,
    http_context: &Arc<execute::HttpContext>,
    mint: &mut Mint,
    response_path: String,
    request_headers: &Arc<reqwest::header::HeaderMap>,
    request: &SqlRequest,
) -> Result<(), anyhow::Error> {
    let response = sql::execute::execute_sql(
        request_headers.clone(),
        catalog.clone(),
        session.clone(),
        http_context.clone(),
        request,
    )
    .await;

    let response = match response {
        Ok(r) => r,
        Err(e) => serde_json::to_vec(&serde_json::json!({"error": e.to_string()}))?,
    };
    let response = serde_json::from_reader::<_, serde_json::Value>(response.as_slice())?;
    let mut expected = mint.new_goldenfile_with_differ(
        response_path,
        Box::new(|file1, file2| {
            let json1: serde_json::Value =
                serde_json::from_reader(File::open(file1).unwrap()).unwrap();
            let json2: serde_json::Value =
                serde_json::from_reader(File::open(file2).unwrap()).unwrap();
            if json1 != json2 {
                text_diff(file1, file2);
            }
        }),
    )?;
    write!(expected, "{}", serde_json::to_string_pretty(&response)?)?;
    Ok(())
}

/// A utility wrapper around std::read_to_string
/// Prints path on error to help debugging
fn read_to_string(path: &Path) -> anyhow::Result<String> {
    fs::read_to_string(path).map_err(|e| anyhow!("path: {}, error: {}", path.to_string_lossy(), e))
}
