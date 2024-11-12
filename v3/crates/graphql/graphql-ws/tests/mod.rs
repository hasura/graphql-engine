mod common;

use common::*;
use futures_util::SinkExt;
use tokio_tungstenite::tungstenite;

#[tokio::test]
async fn test_graphql_ws_connection_init_timeout() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Wait for the connection to be timed out and closed by the server
    let message = expect_close_message(&mut socket).await;
    // Check close code
    let close_code = tungstenite::protocol::frame::coding::CloseCode::from(4408);
    if let tungstenite::Message::Close(Some(close_frame)) = message {
        assert_eq!(close_frame.code, close_code);
        assert_eq!(close_frame.reason, "Connection initialization timeout");
    }

    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_invalid_message_format() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Only JSON text messages are allowed. Sending non-JSON messages result in websocket closure.
    let text_message = "Hello!";
    socket
        .send(tungstenite::Message::Text(text_message.into()))
        .await
        .unwrap();
    // Wait for a close message
    let message = expect_close_message(&mut socket).await;

    // Check close code
    let close_code = tungstenite::protocol::frame::coding::CloseCode::from(4400);
    if let tungstenite::Message::Close(Some(close_frame)) = message {
        assert_eq!(close_frame.code, close_code);
        assert_eq!(
            close_frame.reason,
            "Invalid message format: expected value at line 1 column 1"
        );
    }
    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_invalid_json() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Send JSON message not supported by the graphql-ws protocol
    let json_message = "{\"hello\": \"world\"}";
    socket
        .send(tungstenite::Message::Text(json_message.into()))
        .await
        .unwrap();
    // Wait for a close message
    let message = expect_close_message(&mut socket).await;

    // Check close code
    let close_code_4400 = tungstenite::protocol::frame::coding::CloseCode::from(4400);
    if let tungstenite::Message::Close(Some(close_frame)) = message {
        assert_eq!(close_frame.code, close_code_4400);
        assert_eq!(
            close_frame.reason,
            "Invalid message format: missing field `type` at line 1 column 18"
        );
    }
    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_connection_init_no_headers() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Send connection init without headers. Connection initialization fails with a forbidden message.
    let connection_init_no_headers = serde_json::json!({
        "type": "connection_init",
        "payload": {
            "headers": {}
        }
    });
    let json_message = serde_json::to_string(&connection_init_no_headers).unwrap();
    socket
        .send(tungstenite::Message::Text(json_message))
        .await
        .unwrap();
    // Wait for a close message
    let message = expect_close_message(&mut socket).await;

    // Check close code
    let close_code = tungstenite::protocol::frame::coding::CloseCode::from(4403);
    if let tungstenite::Message::Close(Some(close_frame)) = message {
        assert_eq!(close_frame.code, close_code);
        assert_eq!(close_frame.reason, "Forbidden");
    }
    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_too_many_connection_inits() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Send connection_init and check ack
    graphql_ws_connection_init(&mut socket, connection_init_admin()).await;
    // Sending connection_init again results in connection closure
    let json_message = serde_json::to_string(&connection_init_admin()).unwrap();
    socket
        .send(tungstenite::Message::Text(json_message))
        .await
        .unwrap();

    // Wait for a close message
    let message = expect_close_message(&mut socket).await;

    // Check close code
    let close_code = tungstenite::protocol::frame::coding::CloseCode::from(4429);
    if let tungstenite::Message::Close(Some(close_frame)) = message {
        assert_eq!(close_frame.code, close_code);
        assert_eq!(close_frame.reason, "Too many initialization requests");
    }
    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_subscribe_admin() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Send connection_init and check ack
    graphql_ws_connection_init(&mut socket, connection_init_admin()).await;

    // Send a subscription
    let operation_id = "some-operation-id";
    let json_message = serde_json::to_string(&subscribe_article_by_id(operation_id)).unwrap();
    socket
        .send(tungstenite::Message::Text(json_message))
        .await
        .unwrap();

    // Wait for a text message
    let message = expect_text_message(&mut socket).await;

    // Check message
    let message_json: serde_json::Value =
        serde_json::from_str(message.as_str()).expect("Expected a valid JSON");
    let expected = serde_json::json!({
        "type": "next",
        "id": operation_id,
        "payload": {
            "data": {
                "ArticleByID": {
                    "article_id": 1,
                    "title": "The Next 700 Programming Languages",
                    "Author": {
                        "author_id": 1,
                        "first_name": "Peter"
                    }
                }
            }
        }
    });
    assert_eq!(message_json, expected);

    // Check operation id
    check_operation_id(operation_id, &connections).await;

    // Send another subscription with same operation_id
    let json_message = serde_json::to_string(&subscribe_article_by_id(operation_id)).unwrap();
    socket
        .send(tungstenite::Message::Text(json_message))
        .await
        .unwrap();

    // Wait for a close message
    let message = expect_close_message(&mut socket).await;

    // Check close code
    let close_code = tungstenite::protocol::frame::coding::CloseCode::from(4409);
    if let tungstenite::Message::Close(Some(close_frame)) = message {
        assert_eq!(close_frame.code, close_code);
        assert_eq!(
            close_frame.reason,
            "Subscriber for some-operation-id already exists"
        );
    }
    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_subscribe_user_1() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Send connection_init and check ack
    graphql_ws_connection_init(&mut socket, connection_init_user_1_id_2()).await;

    // Send a subscription
    let operation_id = "some-operation-id";
    let query = r"
          subscription {
            ArticleMany{
              article_id
              author_id
            }
          }
    ";
    let subscribe_message = serde_json::json!({
        "type": "subscribe",
        "id": operation_id,
        "payload": {
            "query": query,
            "variables": {}
        }
    });

    let json_message = serde_json::to_string(&subscribe_message).unwrap();
    socket
        .send(tungstenite::Message::Text(json_message))
        .await
        .unwrap();

    // Wait for a text message
    let message = expect_text_message(&mut socket).await;

    // Check message
    let message_json: serde_json::Value =
        serde_json::from_str(message.as_str()).expect("Expected a valid JSON");
    // Expects data with author_id = 2
    let expected = serde_json::json!({
        "type": "next",
        "id": operation_id,
        "payload": {
            "data": {
                "ArticleMany": [
                    {
                        "article_id": 2,
                        "author_id": 2
                    },
                    {
                        "article_id": 3,
                        "author_id": 2
                    },
                    {
                        "article_id": 5,
                        "author_id": 2
                    }
                ]
            }
        }
    });
    assert_eq!(message_json, expected);
    // Check operation id
    check_operation_id(operation_id, &connections).await;

    // stop subscription
    let stop_message = serde_json::json!({
        "id": operation_id,
        "type": "complete"
    });
    socket
        .send(tungstenite::Message::Text(
            serde_json::to_string(&stop_message).unwrap(),
        ))
        .await
        .unwrap();

    // Assert zero operations
    assert_zero_operations_timeout(&connections).await;

    // Send close frame from client
    socket
        .send(tungstenite::Message::Close(None))
        .await
        .unwrap();
    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_subscribe_user_1_validation_error() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Send connection_init and check ack
    graphql_ws_connection_init(&mut socket, connection_init_user_1_id_2()).await;

    // Send a subscription
    let operation_id = "some-operation-id";
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
    let subscribe_message = serde_json::json!({
        "type": "subscribe",
        "id": operation_id,
        "payload": {
            "query": query,
            "variables": {}
        }
    });
    let json_message = serde_json::to_string(&subscribe_message).unwrap();
    socket
        .send(tungstenite::Message::Text(json_message))
        .await
        .unwrap();

    // Wait for a text message
    let message = expect_text_message(&mut socket).await;

    // Check message
    let message_json: serde_json::Value =
        serde_json::from_str(message.as_str()).expect("Expected a valid JSON");
    // Expects data with author_id = 2
    let expected = serde_json::json!({
        "type": "error",
        "id": operation_id,
        "payload": [
            {
                "message": "validation failed: no such field on type Article: title"
            }
        ]
    });
    assert_eq!(message_json, expected);
    // The above operation resulted in an error.
    // Assert zero operations
    assert_zero_operations_timeout(&connections).await;

    // Send close frame from client
    socket
        .send(tungstenite::Message::Close(None))
        .await
        .unwrap();

    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_connection_expiry() {
    // Expiry in 4 seconds
    let expiry = graphql_ws::ConnectionExpiry::After(std::time::Duration::from_secs(4));
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server_expiry(expiry).await;
    // Send connection_init and check ack
    graphql_ws_connection_init(&mut socket, connection_init_admin()).await;
    // Wait for the connection to expire
    tokio::time::sleep(std::time::Duration::from_secs(5)).await;
    // Wait for a close message
    let message = expect_close_message(&mut socket).await;
    // Check close code
    let close_code = tungstenite::protocol::frame::coding::CloseCode::Again;
    if let tungstenite::Message::Close(Some(close_frame)) = message {
        assert_eq!(close_frame.code, close_code);
        assert_eq!(close_frame.reason, "WebSocket session expired");
    }
    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}

#[tokio::test]
async fn test_graphql_ws_keepalive() {
    let TestServer {
        connections,
        mut socket,
        server_handle,
    } = start_websocket_server().await;
    // Send connection_init and check ack
    graphql_ws_connection_init(&mut socket, connection_init_admin()).await;
    // Wait for a text message
    let message = expect_text_message(&mut socket).await;
    // Check for a keepalive message
    let message_json: serde_json::Value =
        serde_json::from_str(message.as_str()).expect("Expected a valid JSON");
    assert_eq!(
        message_json,
        serde_json::json!({"type": "ping", "payload": {"message": "keepalive"}})
    );
    // Close the connection
    socket.close(None).await.unwrap();
    // Assert zero connections
    assert_zero_connections_timeout(connections).await;
    server_handle.abort();
}
