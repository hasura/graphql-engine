use axum::http::HeaderMap;
use axum::{extract::State, response::IntoResponse, routing::get};
use engine_types::{ExposeInternalErrors, HttpContext};
use futures_util::{SinkExt, StreamExt};
use graphql_ir::GraphqlRequestPipeline;
use graphql_ws::Context;
use graphql_ws::GRAPHQL_WS_PROTOCOL;
use std::{net, path::PathBuf, sync::Arc};
use tokio::{net::TcpStream, task::JoinHandle};
use tokio_tungstenite::{
    connect_async,
    tungstenite::{self, client::IntoClientRequest},
    MaybeTlsStream, WebSocketStream,
};

#[allow(dead_code)]
static METADATA_PATH: &str = "tests/static/metadata.json";

#[allow(dead_code)]
static AUTH_CONFIG_PATH: &str = "tests/static/auth_config_v3.json";

#[allow(dead_code)]
pub(crate) struct ServerState<M> {
    pub(crate) ws_server: graphql_ws::WebSocketServer<M>,
    pub(crate) context: Context<M>,
}

#[allow(dead_code)]
pub(crate) struct TestServer {
    pub(crate) connections: graphql_ws::Connections<graphql_ws::NoOpWebSocketMetrics>,
    pub(crate) socket: WebSocketStream<MaybeTlsStream<TcpStream>>,
    pub(crate) server_handle: JoinHandle<()>,
}

#[allow(dead_code)]
pub(crate) async fn ws_handler(
    headers: axum::http::header::HeaderMap,
    State(state): State<Arc<ServerState<graphql_ws::NoOpWebSocketMetrics>>>,
    ws: axum::extract::ws::WebSocketUpgrade,
) -> impl IntoResponse {
    let mut context = state.context.clone();
    context.handshake_headers = Arc::new(headers);
    state
        .ws_server
        .upgrade_and_handle_websocket("127.0.0.1:8080".parse().unwrap(), ws, context)
        .into_response()
}

#[allow(dead_code)]
pub(crate) async fn start_websocket_server() -> TestServer {
    start_websocket_server_inner(graphql_ws::ConnectionExpiry::Never, HeaderMap::new()).await
}

#[allow(dead_code)]
pub(crate) async fn start_websocket_server_expiry(
    expiry: graphql_ws::ConnectionExpiry,
) -> TestServer {
    start_websocket_server_inner(expiry, HeaderMap::new()).await
}

#[allow(dead_code)]
pub(crate) async fn start_websocket_server_headers(headers: HeaderMap) -> TestServer {
    start_websocket_server_inner(graphql_ws::ConnectionExpiry::Never, headers).await
}

#[allow(dead_code)]
async fn start_websocket_server_inner(
    expiry: graphql_ws::ConnectionExpiry,
    headers: HeaderMap,
) -> TestServer {
    // Create a TCP listener
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();

    // Metadata
    let metadata_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(METADATA_PATH);
    let raw_metadata = std::fs::read_to_string(metadata_path).unwrap();
    let metadata = open_dds::Metadata::from_json_str(&raw_metadata).unwrap();
    let flags = metadata.get_flags().into_owned();
    let metadata_resolve_configuration = metadata_resolve::configuration::Configuration::default();
    let (resolved_metadata, _warnings) =
        metadata_resolve::resolve(metadata, &metadata_resolve_configuration).unwrap();

    // Auth Config
    let auth_config_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(AUTH_CONFIG_PATH);
    let raw_auth_config = std::fs::read_to_string(auth_config_path).unwrap();
    let auth_config = hasura_authn::parse_auth_config(&raw_auth_config).unwrap();
    let (auth_config, _auth_warnings) =
        hasura_authn::resolve_auth_config(auth_config, &flags).unwrap();

    let schema = graphql_schema::GDS {
        metadata: resolved_metadata.clone().into(),
    }
    .build_schema()
    .unwrap();

    // Init context
    let http_context = HttpContext {
        client: reqwest::Client::new(),
        ndc_response_size_limit: None,
    };
    let plugin_configs = metadata_resolve::LifecyclePluginConfigs {
        pre_parse_plugins: Vec::new(),
        pre_response_plugins: Vec::new(),
        pre_route_plugins: Vec::new(),
    };
    let context = Context {
        connection_expiry: expiry,
        http_context,
        metadata: resolved_metadata.into(),
        request_pipeline: GraphqlRequestPipeline::Old,
        expose_internal_errors: ExposeInternalErrors::Expose,
        project_id: None,
        schema: Arc::new(schema),
        auth_config: Arc::new(auth_config),
        plugin_configs: Arc::new(plugin_configs),
        metrics: graphql_ws::NoOpWebSocketMetrics,
        handshake_headers: Arc::new(HeaderMap::new()), // Will be populated in "ws_handler"
    };

    let connections = graphql_ws::Connections::new();
    let ws_server = graphql_ws::WebSocketServer {
        connections: connections.clone(),
    };
    // Spawn a server
    let state = ServerState { ws_server, context };
    let server_handle = tokio::spawn(async move {
        let app = axum::Router::new()
            .route("/ws", get(ws_handler))
            .with_state(Arc::new(state));

        axum::serve(
            listener,
            app.into_make_service_with_connect_info::<net::SocketAddr>(),
        )
        .await
        .unwrap();
    });

    let url = format!("ws://{addr}/ws");
    let mut request = url.into_client_request().unwrap();
    request.headers_mut().insert(
        graphql_ws::SEC_WEBSOCKET_PROTOCOL,
        GRAPHQL_WS_PROTOCOL.parse().unwrap(),
    );
    request.headers_mut().extend(headers);
    let (socket, _response) = connect_async(request)
        .await
        .expect("Failed to connect to WebSocket server");

    TestServer {
        connections,
        socket,
        server_handle,
    }
}

#[allow(dead_code)]
pub(crate) async fn assert_zero_connections_timeout<M>(connections: graphql_ws::Connections<M>) {
    // Closure of a websocket connection is not immediate. So, we keep checking zero connections
    // for at most 5 seconds.
    let result = tokio::time::timeout(tokio::time::Duration::from_secs(5), async {
        loop {
            let conns = connections.0.read().await.len();
            if conns == 0 {
                break;
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;
        }
    })
    .await;
    assert!(result.is_ok(), "Connections are not empty");
}

#[allow(dead_code)]
pub(crate) async fn assert_zero_operations_timeout<M>(connections: &graphql_ws::Connections<M>) {
    // One connection should be present in an active test
    let connections = connections.0.read().await;
    let (_, connection) = connections.iter().next().unwrap();
    // Removal of an operation is not immediate. So, we keep checking zero operations
    // for at most 5 seconds.
    let result = tokio::time::timeout(tokio::time::Duration::from_secs(5), async {
        loop {
            let operations = connection.pollers.read().await.len();
            if operations == 0 {
                break;
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;
        }
    })
    .await;
    assert!(result.is_ok(), "Operations are not empty");
}

#[allow(dead_code)]
pub(crate) async fn expect_close_message(
    socket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
) -> tungstenite::Message {
    let message = socket.next().await.unwrap();
    let message = message.unwrap();
    // Check close message
    assert!(message.is_close(), "Expected close message");
    message
}

#[allow(dead_code)]
pub(crate) async fn expect_text_message(
    socket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
) -> String {
    let message = socket.next().await.unwrap();
    let message = message.unwrap();
    // Check text message
    let tungstenite::Message::Text(text_message) = message else {
        panic!("Expected text message");
    };
    text_message
}

#[allow(dead_code)]
pub(crate) fn connection_init_admin() -> serde_json::Value {
    serde_json::json!(
        {
            "type": "connection_init",
            "payload": {
                "headers": {
                    "x-hasura-role": "admin"
                }
            }
        }
    )
}

#[allow(dead_code)]
pub(crate) fn connection_init_user_1_id_2() -> serde_json::Value {
    serde_json::json!(
        {
            "type": "connection_init",
            "payload": {
                "headers": {
                    "x-hasura-role": "user_1",
                    "x-hasura-user-id": "2"
                }
            }
        }
    )
}

#[allow(dead_code)]
pub(crate) fn subscribe_article_by_id(operation_id: &str) -> serde_json::Value {
    let query = r"
          subscription {
            ArticleByID(article_id: 1) {
              article_id
              title
              Author {
                author_id
                first_name
              }
            }
          }
    ";
    serde_json::json!({
        "type": "subscribe",
        "id": operation_id,
        "payload": {
            "operationName": null,
            "query": query
        }
    })
}

#[allow(dead_code)]
pub(crate) async fn assert_graphql_ws_connection_init(
    socket: &mut WebSocketStream<MaybeTlsStream<TcpStream>>,
    init_payload: serde_json::Value,
) {
    // Send connection init with required headers for authentication.
    let json_message = serde_json::to_string(&init_payload).unwrap();
    socket
        .send(tungstenite::Message::Text(json_message))
        .await
        .unwrap();
    // Wait for a text message
    let message = expect_text_message(socket).await;

    // Check for connection_ack message
    let message_json: serde_json::Value =
        serde_json::from_str(message.as_str()).expect("Expected a valid JSON");
    assert_eq!(message_json, serde_json::json!({"type": "connection_ack"}));
}

#[allow(dead_code)]
pub(crate) async fn check_operation_id<M>(
    operation_id: &str,
    connections: &graphql_ws::Connections<M>,
) {
    let operation_id = graphql_ws::OperationId(operation_id.to_string());
    // One connection should be present in an active test
    let connections = connections.0.read().await;
    let (_, connection) = connections.iter().next().unwrap();
    assert!(connection.pollers.read().await.contains_key(&operation_id));
}
