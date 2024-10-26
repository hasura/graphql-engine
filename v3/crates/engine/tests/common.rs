use anyhow::anyhow;
use execute::{HttpContext, ProjectId};
use goldenfile::{differs::text_diff, Mint};
use graphql_frontend::execute_query;
use graphql_schema::GDS;
use hasura_authn_core::{
    Identity, JsonSessionVariableValue, Role, Session, SessionError, SessionVariableValue,
};
use lang_graphql::ast::common as ast;
use lang_graphql::{http::RawRequest, schema::Schema};
use metadata_resolve::{data_connectors::NdcVersion, LifecyclePluginConfigs};
use open_dds::session_variables::{SessionVariableName, SESSION_VARIABLE_ROLE};
use pretty_assertions::assert_eq;
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
    session_variables: HashMap<SessionVariableName, SessionVariableValue>,
) -> Result<Session, SessionError> {
    //return an arbitrary identity with role emulation enabled
    let authorization = Identity::admin(Role::new("admin"));
    let role = session_variables
        .get(&SESSION_VARIABLE_ROLE)
        .map(|v| {
            Ok(Role::new(v.as_str().ok_or_else(|| {
                SessionError::InvalidHeaderValue {
                    header_name: SESSION_VARIABLE_ROLE.to_string(),
                    error: "session variable value is not a string".to_owned(),
                }
            })?))
        })
        .transpose()?;
    let role_authorization = authorization.get_role_authorization(role.as_ref())?;
    let session = role_authorization.build_session(session_variables);
    Ok(session)
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

        let metadata =
            open_dds::traits::OpenDd::deserialize(metadata_json_value, jsonpath::JSONPath::new())?;

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
        let sessions: Vec<HashMap<SessionVariableName, JsonSessionVariableValue>> =
            json::from_str(read_to_string(session_vars_path)?.as_ref())?;
        let sessions: Vec<Session> = sessions
            .into_iter()
            .map(|session_vars| {
                resolve_session(
                    session_vars
                        .into_iter()
                        .map(|(k, v)| (k, v.into()))
                        .collect(),
                )
            })
            .collect::<Result<_, _>>()?;

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
    opendd_tests: TestOpenDDPipeline,
) -> anyhow::Result<()> {
    test_execution_expectation_for_multiple_ndc_versions(
        test_path_string,
        common_metadata_paths,
        BTreeMap::new(),
        opendd_tests,
    )
}

#[allow(clippy::print_stdout, dead_code)]
pub fn test_execution_expectation_for_multiple_ndc_versions(
    test_path_string: &str,
    common_metadata_paths: &[&str],
    common_metadata_paths_per_ndc_version: BTreeMap<NdcVersion, Vec<&str>>,
    opendd_tests: TestOpenDDPipeline,
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

            let metadata = open_dds::traits::OpenDd::deserialize(
                metadata_json_value,
                jsonpath::JSONPath::new(),
            )?;

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

            // Ensure sql_context can be serialized and deserialized
            let sql_context = sql::catalog::Catalog::from_metadata(gds.metadata.clone());
            let sql_context_str = serde_json::to_string(&sql_context)?;
            let sql_context_parsed = serde_json::from_str(&sql_context_str)?;
            assert_eq!(sql_context, sql_context_parsed);
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
            let sessions: Vec<HashMap<SessionVariableName, JsonSessionVariableValue>> =
                json::from_str(read_to_string(session_vars_path)?.as_ref())?;
            let sessions: Vec<Session> = sessions
                .into_iter()
                .map(|session_vars| {
                    resolve_session(
                        session_vars
                            .into_iter()
                            .map(|(k, v)| (k, v.into()))
                            .collect(),
                    )
                })
                .collect::<Result<_, _>>()?;

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
                        query: query.clone(),
                        variables: None,
                    };
                    for session in &sessions {
                        // attempt to create open ir for this request
                        open_dd_pipeline_test(
                            test_path_string,
                            opendd_tests,
                            &query,
                            &schema,
                            &gds.metadata,
                            session,
                            raw_request.clone(),
                            &test_ctx.http_context.clone().into(),
                            &request_headers,
                        )
                        .await;

                        // do actual test
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
                        let http_response = response.inner();
                        let graphql_ws_response = run_query_graphql_ws(
                            execute::ExposeInternalErrors::Expose,
                            &test_ctx.http_context,
                            &schema,
                            session,
                            &request_headers,
                            raw_request.clone(),
                            None,
                        )
                        .await;
                        compare_graphql_responses(&http_response, &graphql_ws_response);
                        responses.push(http_response);
                    }
                }
                Some(vars) => {
                    for (session, variables) in sessions.iter().zip(vars) {
                        let raw_request = RawRequest {
                            operation_name: None,
                            query: query.clone(),
                            variables: Some(variables),
                        };
                        // attempt to create open ir for this request
                        open_dd_pipeline_test(
                            test_path_string,
                            opendd_tests,
                            &query,
                            &schema,
                            &gds.metadata,
                            session,
                            raw_request.clone(),
                            &Arc::new(test_ctx.http_context.clone()),
                            &request_headers,
                        )
                        .await;
                        // do actual test
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
                        let http_response = response.inner();
                        let graphql_ws_response = run_query_graphql_ws(
                            execute::ExposeInternalErrors::Expose,
                            &test_ctx.http_context,
                            &schema,
                            session,
                            &request_headers,
                            raw_request.clone(),
                            None,
                        )
                        .await;
                        compare_graphql_responses(&http_response, &graphql_ws_response);
                        responses.push(http_response);
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
                enable_jsonapi: false,
                ..Default::default()
            },
        };
        let gds = GDS::new(
            open_dds::traits::OpenDd::deserialize(metadata, jsonpath::JSONPath::new())?,
            &configuration,
        )?;

        let schema = GDS::build_schema(&gds)?;
        let request_headers = reqwest::header::HeaderMap::new();
        let session = {
            let session_variables: HashMap<SessionVariableName, SessionVariableValue> =
                HashMap::from_iter([(
                    SESSION_VARIABLE_ROLE.clone(),
                    SessionVariableValue::Unparsed("admin".to_owned()),
                )]);
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
        unstable_features: metadata_resolve::configuration::UnstableFeatures {
            enable_order_by_expressions: false,
            enable_ndc_v02_support: true,
            enable_jsonapi: false,
            ..Default::default()
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

        let metadata =
            open_dds::traits::OpenDd::deserialize(metadata_json_value, jsonpath::JSONPath::new())?;

        // TODO: remove this assert once we have stopped manually implementing Serialize for OpenDD types.
        assert_eq!(
            open_dds::Metadata::from_json_str(&serde_json::to_string(&metadata)?)?,
            metadata
        );

        let gds = GDS::new(metadata, &test_metadata_resolve_configuration())?;
        let schema = GDS::build_schema(&gds)?;

        // Ensure schema is serialized successfully.
        serde_json::to_string(&schema)?;

        // Ensure sql_context can be serialized and deserialized
        let sql_context = sql::catalog::Catalog::from_metadata(gds.metadata.clone());
        let sql_context_str = serde_json::to_string(&sql_context)?;
        let sql_context_parsed = serde_json::from_str(&sql_context_str)?;
        assert_eq!(sql_context, sql_context_parsed);

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
            let session_variables: HashMap<SessionVariableName, JsonSessionVariableValue> =
                serde_json::from_str(read_to_string(session_vars_path)?.as_ref())?;
            resolve_session(
                session_variables
                    .into_iter()
                    .map(|(k, v)| (k, v.into()))
                    .collect(),
            )
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
        Err(e) => serde_json::to_vec(&e.to_error_response())?,
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

fn compare_graphql_responses(
    http_response: &lang_graphql::http::Response,
    ws_response: &lang_graphql::http::Response,
) {
    assert_eq!(
        http_response.status_code, ws_response.status_code,
        "Status Codes donot match {}, {}",
        http_response.status_code, ws_response.status_code,
    );

    assert_eq!(
        http_response.errors, ws_response.errors,
        "Errors do not match",
    );

    assert_eq!(http_response.data, ws_response.data, "Data do not match");
}

/// Execute a GraphQL query over a dummy WebSocket connection.
async fn run_query_graphql_ws(
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &Schema<GDS>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    request: RawRequest,
    project_id: Option<&ProjectId>,
) -> lang_graphql::http::Response {
    use graphql_ws;

    // Dummy auth config. We never use it in the test. It is only used to create a dummy connection.
    let dummy_auth_config = hasura_authn::AuthConfig::V1(hasura_authn::AuthConfigV1 {
        allow_role_emulation_by: None,
        mode: hasura_authn::AuthModeConfig::NoAuth(hasura_authn_noauth::NoAuthConfig {
            role: Role::new("admin"),
            session_variables: HashMap::new(),
        }),
    });

    let context = graphql_ws::Context {
        http_context: http_context.clone(),
        expose_internal_errors,
        project_id: project_id.cloned(),
        schema: Arc::new(schema.clone()),
        auth_config: Arc::new(dummy_auth_config),
        plugin_configs: Arc::new(LifecyclePluginConfigs {
            pre_parse_plugins: Vec::new(),
            pre_response_plugins: Vec::new(),
        }),
    };
    let (channel_sender, mut channel_receiver) =
        tokio::sync::mpsc::channel::<graphql_ws::Message>(10);
    let websocket_id = graphql_ws::WebSocketId::new();
    let dummy_conn = graphql_ws::Connection::new(websocket_id, context, channel_sender);
    let operation_id = graphql_ws::OperationId("some-operation-id".to_string());
    // Using the internal function. The actual 'execute_request' function from
    // graphl_ws crate needs a parent span context for linking purposes.
    // Traces are not considered in tests
    let result = graphql_ws::execute_query_internal(
        operation_id.clone(),
        session.clone(),
        request_headers.clone(),
        &dummy_conn,
        request,
    )
    .await;
    match result {
        Ok(()) => {}
        Err(e) => {
            graphql_ws::send_request_error(
                e,
                expose_internal_errors,
                operation_id.clone(),
                &dummy_conn,
            )
            .await;
        }
    }

    // Assert response
    let message = channel_receiver.recv().await.expect("Expected a message");
    let response = match message {
        graphql_ws::Message::Protocol(message) => match *message {
            graphql_ws::ServerMessage::Next { id, payload } => {
                assert_eq!(operation_id, id);
                payload
            }
            graphql_ws::ServerMessage::Error {
                id,
                payload: errors,
            } => {
                assert_eq!(operation_id, id);
                lang_graphql::http::Response::errors(errors)
            }
            _ => {
                panic!("Expected a Next or Error message")
            }
        },
        graphql_ws::Message::Raw(_) => panic!("Expected a Next or Error message"),
    };

    // Assert completion when no errors
    if response.errors.is_none() {
        let message = channel_receiver.recv().await.expect("Expected a message");
        match message {
            graphql_ws::Message::Protocol(message) => match *message {
                graphql_ws::ServerMessage::Complete { id } => {
                    assert_eq!(operation_id, id);
                }
                _ => {
                    panic!("Expected a Complete message")
                }
            },
            graphql_ws::Message::Raw(_) => {
                panic!("Expected a Complete message")
            }
        };
    }
    response
}

// which OpenDD IR pipeline tests should we include for this test?
#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum TestOpenDDPipeline {
    Skip,
    GenerateOpenDDQuery,
    TestNDCResponses,
    GenerateExecutionPlan,
}

// generate open_dd_ir for each test and see what happens
// eventually these tests will be deleted once the OpenDD pipeline becomes the main one
pub async fn open_dd_pipeline_test(
    test_path_string: &str,
    opendd_tests: TestOpenDDPipeline,
    query: &str,
    schema: &Schema<GDS>,
    metadata: &metadata_resolve::Metadata,
    session: &Session,
    raw_request: lang_graphql::http::RawRequest,
    http_context: &Arc<HttpContext>,
    request_headers: &reqwest::header::HeaderMap,
) {
    match opendd_tests {
        TestOpenDDPipeline::Skip => {}
        TestOpenDDPipeline::GenerateOpenDDQuery => {
            // parse the raw request into a GQL query
            let query = graphql_frontend::parse_query(query).unwrap();

            // normalize the parsed GQL query
            if let Ok(normalized_request) =
                graphql_frontend::normalize_request(schema, session, query, &raw_request)
            {
                // we can only generate for queries that would have worked,
                // `normalize_request` fails when we try and access a field we're not allowed to,
                // for instance
                let ir = graphql_frontend::to_opendd_ir(&normalized_request);

                insta::assert_debug_snapshot!(
                    format!("ir_{test_path_string}_{}", session.role),
                    ir
                );
            }
        }
        TestOpenDDPipeline::TestNDCResponses => {
            // test the partial NDC pipeline so we can sanity check the planning steps

            // parse the raw request into a GQL query
            let query = graphql_frontend::parse_query(query).unwrap();

            // normalize the parsed GQL query
            if let Ok(normalized_request) =
                graphql_frontend::normalize_request(schema, session, query, &raw_request)
            {
                // we can only generate for queries that would have worked,
                // `normalize_request` fails when we try and access a field we're not allowed to,
                // for instance
                let query_ir = graphql_frontend::to_opendd_ir(&normalized_request);

                // check IR is what we expect
                insta::assert_debug_snapshot!(
                    format!("ir_{test_path_string}_{}", session.role),
                    query_ir
                );

                // create a query execution plan for a single node with the new pipeline
                let (query_execution_plan, _) = plan::plan_query_request(
                    &query_ir,
                    metadata,
                    &Arc::new(session.clone()),
                    http_context,
                    request_headers,
                )
                .await
                .unwrap();

                match query_execution_plan {
                    plan::SingleNodeExecutionPlan::Mutation(_) => {
                        todo!("Executing mutations in OpenDD IR pipeline tests not implemented yet")
                    }
                    plan::SingleNodeExecutionPlan::Query(plan) => {
                        // run the pipeline using functions from GraphQL frontend
                        let rowsets =
                            graphql_frontend::resolve_ndc_query_execution(http_context, plan)
                                .await
                                .map_err(|e| e.to_string());

                        insta::assert_json_snapshot!(
                            format!("rowsets_{test_path_string}_{}", session.role),
                            rowsets
                        );
                    }
                }
            }
        }
        TestOpenDDPipeline::GenerateExecutionPlan => {
            todo!("GenerateExecutionPlan not implemented yet")
        }
    }
}
