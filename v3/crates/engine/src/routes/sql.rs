use crate::EngineState;
use axum::{extract::State, response::IntoResponse, Extension, Json};
use reqwest::header::CONTENT_TYPE;
use std::sync::Arc;

use hasura_authn_core::Session;
use tracing_util::{set_status_on_current_span, SpanVisibility};

/// Handle a SQL request and execute it.
pub async fn handle_sql_request(
    headers: axum::http::header::HeaderMap,
    State(state): State<EngineState>,
    Extension(session): Extension<Session>,
    Json(request): Json<sql::execute::SqlRequest>,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "handle_sql_request",
            "Handle SQL Request",
            SpanVisibility::User,
            || {
                Box::pin(async {
                    sql::execute::execute_sql(
                        Arc::new(headers),
                        state.sql_context.clone(),
                        state.resolved_metadata.clone(),
                        Arc::new(session),
                        Arc::new(state.http_context.clone()),
                        &request,
                    )
                    .await
                })
            },
        )
        .await;

    // Set the span as error if the response contains an error
    set_status_on_current_span(&response);

    match response {
        Ok(r) => {
            let mut response = (axum::http::StatusCode::OK, r).into_response();
            response.headers_mut().insert(
                CONTENT_TYPE,
                axum::http::HeaderValue::from_static("application/json"),
            );
            response
        }
        Err(e) => (
            axum::http::StatusCode::BAD_REQUEST,
            Json(e.to_error_response()),
        )
            .into_response(),
    }
}
