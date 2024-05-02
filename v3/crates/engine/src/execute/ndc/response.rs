use super::client as ndc_client;

/// Handle response return from an NDC request by applying the size limit and
/// deserializing into a JSON value
pub(crate) async fn handle_response_with_size_limit<T: for<'de> serde::Deserialize<'de>>(
    response: reqwest::Response,
    size_limit: usize,
) -> Result<T, ndc_client::Error> {
    if let Some(content_length) = &response.content_length() {
        // Check with content length
        if *content_length > size_limit as u64 {
            Err(ndc_client::Error::ResponseTooLarge(format!(
                "Received content length {} exceeds the limit {}",
                content_length, size_limit
            )))
        } else {
            Ok(response.json().await?)
        }
    } else {
        // If no content length found, then check chunk-by-chunk
        handle_response_by_chunks_with_size_limit(response, size_limit).await
    }
}

/// Handle response by chunks. For each chunk consumed, check if the total size exceeds the limit.
///
/// This logic is separated in a function to allow testing.
async fn handle_response_by_chunks_with_size_limit<T: for<'de> serde::Deserialize<'de>>(
    response: reqwest::Response,
    size_limit: usize,
) -> Result<T, ndc_client::Error> {
    let mut size = 0;
    let mut buf = bytes::BytesMut::new();
    let mut response = response;
    while let Some(chunk) = response.chunk().await? {
        size += chunk.len();
        if size > size_limit {
            return Err(ndc_client::Error::ResponseTooLarge(format!(
                "Size exceeds the limit {}",
                size_limit
            )));
        } else {
            buf.extend_from_slice(&chunk);
        }
    }
    Ok(serde_json::from_slice(&buf)?)
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    #[tokio::test]
    async fn test_content_length() {
        let mut server = mockito::Server::new_async().await;
        let test_api = server
            .mock("GET", "/test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"message": "hello"}"#)
            .create();
        let response = reqwest::get(server.url() + "/test").await.unwrap();
        test_api.assert();
        let err = super::handle_response_with_size_limit::<serde_json::Value>(response, 10)
            .await
            .unwrap_err();
        assert_eq!(
            err.to_string(),
            "response received from connector is too large: Received content length 20 exceeds the limit 10"
        )
    }

    #[tokio::test]
    async fn test_chunk_by_chunk() {
        let mut server = mockito::Server::new_async().await;
        let test_api = server
            .mock("GET", "/test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"message": "hello"}"#)
            .create();
        let response = reqwest::get(server.url() + "/test").await.unwrap();
        test_api.assert();
        let err =
            super::handle_response_by_chunks_with_size_limit::<serde_json::Value>(response, 5)
                .await
                .unwrap_err();
        assert_eq!(
            err.to_string(),
            "response received from connector is too large: Size exceeds the limit 5"
        )
    }

    #[tokio::test]
    async fn test_success() {
        let json = serde_json::json!(
            [
                {"name": "Alice"},
                {"name": "Bob"},
                {"name": "Charlie"}
            ]
        );
        let mut server = mockito::Server::new_async().await;
        let test_api = server
            .mock("GET", "/test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(serde_json::to_vec(&json).unwrap())
            .create();
        let response = reqwest::get(server.url() + "/test").await.unwrap();
        test_api.assert();
        let res = super::handle_response_with_size_limit::<serde_json::Value>(response, 100)
            .await
            .unwrap();
        assert_eq!(json, res)
    }

    #[tokio::test]
    async fn test_success_by_chunks() {
        let json = serde_json::json!(
            [
                {"name": "Alice"},
                {"name": "Bob"},
                {"name": "Charlie"}
            ]
        );
        let mut server = mockito::Server::new_async().await;
        let test_api = server
            .mock("GET", "/test")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(serde_json::to_vec(&json).unwrap())
            .create();
        let response = reqwest::get(server.url() + "/test").await.unwrap();
        test_api.assert();
        let res =
            super::handle_response_by_chunks_with_size_limit::<serde_json::Value>(response, 100)
                .await
                .unwrap();
        assert_eq!(json, res)
    }
}
