use anyhow::anyhow;
use goldenfile::{Mint, differs::text_diff};
use graphql_frontend::execute_query;
use graphql_schema::GDS;
use hasura_authn_core::{
    Identity, JsonSessionVariableValue, Role, Session, SessionError, SessionVariableValue,
};
use lang_graphql::ast::common as ast;
use lang_graphql::{http::RawRequest, schema::Schema};
use metadata_resolve::data_connectors::NdcVersion;
use metadata_resolve::{LifecyclePluginConfigs, ResolvedLifecyclePreResponsePluginHooks};
use open_dds::session_variables::{SESSION_VARIABLE_ROLE, SessionVariableName};
use pretty_assertions::assert_eq;
use serde_json as json;
use std::collections::BTreeMap;
use std::sync::Arc;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::Path,
    path::PathBuf,
};
extern crate json_value_merge;
use axum::http::{HeaderMap, Method, Uri};
use engine_types::{ExposeInternalErrors, HttpContext, ProjectId};
use json_value_merge::Merge;
use jsonapi_library::query::Query;
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
    session_variables: BTreeMap<SessionVariableName, SessionVariableValue>,
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

        let (resolved_metadata, _) =
            metadata_resolve::resolve(metadata, &test_metadata_resolve_configuration())?;

        let arc_resolved_metadata = Arc::new(resolved_metadata);

        let gds = GDS {
            metadata: arc_resolved_metadata.clone(),
        };

        let schema = GDS::build_schema(&gds)?;

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

        let plugins = LifecyclePluginConfigs {
            pre_ndc_request_plugins: BTreeMap::new(),
            pre_ndc_response_plugins: BTreeMap::new(),
            pre_parse_plugins: Vec::new(),
            pre_response_plugins: ResolvedLifecyclePreResponsePluginHooks::new(),
            pre_route_plugins: Vec::new(),
        };

        let raw_request = RawRequest {
            operation_name: None,
            query,
            variables: None,
        };

        // Execute the test
        let mut responses = Vec::new();
        for session in &sessions {
            let (_, http_response) = execute_query(
                ExposeInternalErrors::Expose,
                &test_ctx.http_context,
                &schema,
                &arc_resolved_metadata,
                &plugins,
                session,
                &request_headers,
                raw_request.clone(),
                None,
            )
            .await;
            let response = http_response.inner();

            responses.push(response);
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

#[derive(Debug, serde::Deserialize)]
enum JsonApiRequest {
    #[serde(rename = "GET")]
    Get(String),
}

impl JsonApiRequest {
    fn to_method(&self) -> Method {
        match self {
            JsonApiRequest::Get(_) => Method::GET,
        }
    }

    fn to_path(&self) -> &str {
        match self {
            JsonApiRequest::Get(path) => path,
        }
    }
}

async fn test_jsonapi(
    test_path: &Path,
    test_path_string: &str,
    test_ctx: &mut GoldenTestContext,
    sessions: &[Session],
    resolved_metadata: Arc<metadata_resolve::Metadata>,
) -> anyhow::Result<()> {
    let request_path = test_path.join("request_jsonapi.json");
    let response_path = test_path_string.to_string() + "/expected_jsonapi.json";

    // Skip if no JSONAPI request file exists
    if !request_path.exists() {
        return Ok(());
    }

    // Get JSONAPI catalog
    let (catalog, _warnings) = jsonapi::Catalog::new(&resolved_metadata);

    // Read and parse requests
    let request_content = std::fs::read_to_string(&request_path)?;
    let requests: Vec<JsonApiRequest> = serde_json::from_str(&request_content).map_err(|e| {
        anyhow::anyhow!(
            "Failed to parse JSONAPI request file {}: {}",
            request_path.display(),
            e
        )
    })?;

    // For each session, execute all requests
    let mut all_responses = Vec::with_capacity(sessions.len());

    for session in sessions {
        let mut session_responses = Vec::with_capacity(requests.len());

        for request in &requests {
            let (path, query_params) = parse_jsonapi_request(request.to_path());

            let result = jsonapi::handler_internal(
                Arc::new(HeaderMap::default()),
                Arc::new(test_ctx.http_context.clone()),
                Arc::new(resolved_metadata.plugin_configs.clone()),
                Arc::new(session.clone()),
                &catalog,
                resolved_metadata.clone(),
                request.to_method(),
                Uri::try_from(&path).map_err(|e| anyhow::anyhow!("Invalid URI: {}", e))?,
                Query::from_params(&query_params),
            )
            .await;

            let response = match result {
                Ok(response) => serde_json::to_value(response)?,
                Err(e) => serde_json::to_value(
                    e.into_http_error(engine_types::ExposeInternalErrors::Expose)
                        .into_document_error(),
                )?,
            };

            session_responses.push(response);
        }

        all_responses.push(session_responses);
    }

    // Write JSONAPI responses
    let mut expected_jsonapi = test_ctx.mint.new_goldenfile_with_differ(
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
    write!(
        expected_jsonapi,
        "{}",
        serde_json::to_string_pretty(&all_responses)?
    )?;

    Ok(())
}

fn parse_jsonapi_request(content: &str) -> (String, String) {
    let parts: Vec<&str> = content.trim().split('?').collect();
    match parts.len() {
        1 => (parts[0].to_string(), String::new()),
        2 => (parts[0].to_string(), parts[1].to_string()),
        _ => panic!("Invalid JSONAPI request format"),
    }
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

            let metadata = open_dds::traits::OpenDd::deserialize(
                metadata_json_value,
                jsonpath::JSONPath::new(),
            )?;

            // TODO: remove this assert once we have stopped manually implementing Serialize for OpenDD types.
            assert_eq!(
                open_dds::Metadata::from_json_str(&serde_json::to_string(&metadata)?)?,
                metadata
            );

            let (resolved_metadata, _) =
                metadata_resolve::resolve(metadata, &test_metadata_resolve_configuration())?;

            let arc_resolved_metadata = Arc::new(resolved_metadata);

            let gds = GDS {
                metadata: arc_resolved_metadata.clone(),
            };

            let schema = GDS::build_schema(&gds)?;

            let query = read_to_string(&request_path)?;

            // Read optional GQL query variables.
            // NOTE: It is expected the variables.json file contains a list of
            // variables. Each item in the list corresponding to a session in
            // session_variables.json
            let query_vars: Option<Vec<BTreeMap<ast::Name, serde_json::Value>>> =
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

            let plugins = LifecyclePluginConfigs {
                pre_ndc_request_plugins: BTreeMap::new(),
                pre_ndc_response_plugins: BTreeMap::new(),
                pre_parse_plugins: Vec::new(),
                pre_response_plugins: ResolvedLifecyclePreResponsePluginHooks::new(),
                pre_route_plugins: Vec::new(),
            };

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
                        let (_, response) = execute_query(
                            ExposeInternalErrors::Expose,
                            &test_ctx.http_context,
                            &schema,
                            &arc_resolved_metadata.clone(),
                            &plugins,
                            session,
                            &request_headers,
                            raw_request.clone(),
                            None,
                        )
                        .await;
                        let http_response = response.inner();
                        let graphql_ws_response = run_query_graphql_ws(
                            ExposeInternalErrors::Expose,
                            &test_ctx.http_context,
                            &schema,
                            arc_resolved_metadata.clone(),
                            session,
                            &request_headers,
                            raw_request.clone(),
                            None,
                        )
                        .await;
                        compare_graphql_responses(
                            &http_response,
                            &graphql_ws_response,
                            "websockets",
                        );

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
                        // do actual test
                        let (_, response) = execute_query(
                            ExposeInternalErrors::Expose,
                            &test_ctx.http_context,
                            &schema,
                            &arc_resolved_metadata,
                            &plugins,
                            session,
                            &request_headers,
                            raw_request.clone(),
                            None,
                        )
                        .await;
                        let http_response = response.inner();
                        let graphql_ws_response = run_query_graphql_ws(
                            ExposeInternalErrors::Expose,
                            &test_ctx.http_context,
                            &schema,
                            arc_resolved_metadata.clone(),
                            session,
                            &request_headers,
                            raw_request.clone(),
                            None,
                        )
                        .await;
                        compare_graphql_responses(
                            &http_response,
                            &graphql_ws_response,
                            "websockets",
                        );

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

            // Run JSONAPI test
            test_jsonapi(
                &test_path,
                test_path_string,
                &mut test_ctx,
                &sessions,
                arc_resolved_metadata.clone(),
            )
            .await?;
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
                ..Default::default()
            },
        };
        let (resolved_metadata, _) = metadata_resolve::resolve(
            open_dds::traits::OpenDd::deserialize(metadata, jsonpath::JSONPath::new())?,
            &configuration,
        )?;

        let arc_resolved_metadata = Arc::new(resolved_metadata);

        let gds = GDS {
            metadata: arc_resolved_metadata.clone(),
        };

        let schema = GDS::build_schema(&gds)?;
        let request_headers = reqwest::header::HeaderMap::new();
        let session = {
            let session_variables: BTreeMap<SessionVariableName, SessionVariableValue> =
                BTreeMap::from_iter([(
                    SESSION_VARIABLE_ROLE.clone(),
                    SessionVariableValue::Unparsed("admin".to_owned()),
                )]);
            resolve_session(session_variables)
        }?;
        let plugins = LifecyclePluginConfigs {
            pre_ndc_request_plugins: BTreeMap::new(),
            pre_ndc_response_plugins: BTreeMap::new(),
            pre_parse_plugins: Vec::new(),
            pre_response_plugins: ResolvedLifecyclePreResponsePluginHooks::new(),
            pre_route_plugins: Vec::new(),
        };
        let query = read_to_string(&root_test_dir.join(gql_request_file_path))?;
        let raw_request = lang_graphql::http::RawRequest {
            operation_name: None,
            query,
            variables: None,
        };
        let (_, raw_response) = graphql_frontend::execute_explain(
            ExposeInternalErrors::Expose,
            &test_ctx.http_context,
            &plugins,
            &schema,
            &arc_resolved_metadata,
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
            enable_authorization_rules: true,
            ..Default::default()
        },
    }
}

/// A utility wrapper around std::read_to_string
/// Prints path on error to help debugging
fn read_to_string(path: &Path) -> anyhow::Result<String> {
    fs::read_to_string(path).map_err(|e| anyhow!("path: {}, error: {}", path.to_string_lossy(), e))
}

fn compare_graphql_responses(
    http_response: &lang_graphql::http::Response,
    other_response: &lang_graphql::http::Response,
    description: &str,
) {
    assert_eq!(
        http_response.status_code, other_response.status_code,
        "HTTP status codes for {description} do not match {}, {}",
        http_response.status_code, other_response.status_code,
    );

    assert_eq!(
        http_response.errors, other_response.errors,
        "Errors for {description} do not match regular queries",
    );

    assert_eq!(
        http_response.data, other_response.data,
        "Data for {description} does not match regular queries"
    );
}

/// Execute a GraphQL query over a dummy WebSocket connection.
async fn run_query_graphql_ws(
    expose_internal_errors: ExposeInternalErrors,
    http_context: &HttpContext,
    schema: &Schema<GDS>,
    metadata: Arc<metadata_resolve::Metadata>,
    session: &Session,
    request_headers: &reqwest::header::HeaderMap,
    request: RawRequest,
    project_id: Option<&ProjectId>,
) -> lang_graphql::http::Response {
    use graphql_ws;

    // Dummy auth config. We never use it in the test. It is only used to create a dummy connection.
    let dummy_auth_config = hasura_authn::ResolvedAuthConfig {
        auth_config: hasura_authn::AuthConfig::V1(hasura_authn::AuthConfigV1 {
            allow_role_emulation_by: None,
            mode: hasura_authn::AuthModeConfig::NoAuth(hasura_authn_noauth::NoAuthConfig {
                role: Role::new("admin"),
                session_variables: HashMap::new(),
            }),
        }),
        auth_config_flags: hasura_authn::AuthConfigFlags::default(),
    };

    let runtime_flags = metadata.runtime_flags.clone();

    let context = graphql_ws::Context {
        connection_expiry: graphql_ws::ConnectionExpiry::Never,
        http_context: http_context.clone(),
        expose_internal_errors,
        metadata,
        project_id: project_id.cloned(),
        schema: Arc::new(schema.clone()),
        auth_config: Arc::new(dummy_auth_config),
        metrics: graphql_ws::NoOpWebSocketMetrics,
        handshake_headers: Arc::new(request_headers.clone()),
        auth_mode_header: "x-hasura-auth-mode".to_string(),
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
        "127.0.0.1:8080".parse().unwrap(),
        operation_id.clone(),
        session.clone(),
        request_headers.clone(),
        &dummy_conn,
        request,
        &runtime_flags,
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
        }
    }
    response
}
