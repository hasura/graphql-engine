use std::{borrow::Borrow, sync::Arc};

use axum::{
    extract::State,
    http::StatusCode,
    routing::{get, post},
    Json, Router,
};

use custom_connector::state::AppState;
use ndc_client::models;

type Result<A> = std::result::Result<A, (StatusCode, Json<models::ErrorResponse>)>;

#[tokio::main]
async fn main() {
    let app_state = Arc::new(custom_connector::state::init_app_state());

    let app = Router::new()
        .route("/healthz", get(get_healthz))
        .route("/capabilities", get(get_capabilities))
        .route("/schema", get(get_schema))
        .route("/query", post(post_query))
        .route("/mutation", post(post_mutation))
        .route("/explain", post(post_explain))
        .with_state(app_state);

    // run it with hyper on localhost:8101
    axum::Server::bind(&"0.0.0.0:8101".parse().unwrap())
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn get_healthz() -> StatusCode {
    StatusCode::NO_CONTENT
}

async fn get_capabilities() -> Json<models::CapabilitiesResponse> {
    Json(custom_connector::schema::get_capabilities())
}

async fn get_schema() -> Json<models::SchemaResponse> {
    Json(custom_connector::schema::get_schema())
}

pub async fn post_query(
    State(state): State<Arc<AppState>>,
    Json(request): Json<models::QueryRequest>,
) -> Result<Json<models::QueryResponse>> {
    custom_connector::query::execute_query_request(state.borrow(), request).map(Json)
}

async fn post_mutation(
    State(state): State<Arc<AppState>>,
    Json(request): Json<models::MutationRequest>,
) -> Result<Json<models::MutationResponse>> {
    custom_connector::mutation::execute_mutation_request(state.borrow(), request).map(Json)
}

async fn post_explain(
    Json(_request): Json<models::QueryRequest>,
) -> Result<Json<models::ExplainResponse>> {
    Err((
        StatusCode::NOT_IMPLEMENTED,
        Json(models::ErrorResponse {
            message: "explain is not supported".into(),
            details: serde_json::Value::Null,
        }),
    ))
}
