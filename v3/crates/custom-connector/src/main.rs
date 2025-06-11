use std::borrow::Borrow;
use std::net;
use std::sync::Arc;

use axum::{
    Json, Router,
    extract::State,
    http::StatusCode,
    routing::{get, post},
};

use custom_connector::state::AppState;
use ndc_models::{RelationalQuery, RelationalQueryResponse};

type Result<A> = std::result::Result<A, (StatusCode, Json<ndc_models::ErrorResponse>)>;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    env_logger::init();

    let app_state = Arc::new(custom_connector::state::init_app_state()?);

    let app = Router::new()
        .route("/healthz", get(get_healthz))
        .route("/capabilities", get(get_capabilities))
        .route("/schema", get(get_schema))
        .route("/query", post(post_query))
        .route("/query/relational", post(post_query_relational))
        .route("/mutation", post(post_mutation))
        .route("/explain", post(post_explain))
        .route("/mutation/rel/insert", post(post_mutation_rel_insert))
        .route("/mutation/rel/update", post(post_mutation_rel_update))
        .route("/mutation/rel/delete", post(post_mutation_rel_delete))
        .with_state(app_state);

    // run it with hyper on localhost:8102
    let host = net::IpAddr::V6(net::Ipv6Addr::UNSPECIFIED);
    let port = 8102;
    let socket_addr = net::SocketAddr::new(host, port);
    let listener = tokio::net::TcpListener::bind(socket_addr).await.unwrap();

    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<net::SocketAddr>(),
    )
    .with_graceful_shutdown(axum_ext::shutdown_signal())
    .await?;
    Ok(())
}

async fn get_healthz() -> StatusCode {
    StatusCode::NO_CONTENT
}

async fn get_capabilities(
    State(state): State<Arc<AppState>>,
) -> Json<ndc_models::CapabilitiesResponse> {
    Json(custom_connector::schema::get_capabilities(state.borrow()))
}

async fn get_schema() -> Json<ndc_models::SchemaResponse> {
    Json(custom_connector::schema::get_schema())
}

pub async fn post_query(
    State(state): State<Arc<AppState>>,
    Json(request): Json<ndc_models::QueryRequest>,
) -> Result<Json<ndc_models::QueryResponse>> {
    custom_connector::query::execute_query_request(state.borrow(), &request).map(Json)
}

async fn post_mutation(
    State(state): State<Arc<AppState>>,
    Json(request): Json<ndc_models::MutationRequest>,
) -> Result<Json<ndc_models::MutationResponse>> {
    custom_connector::mutation::execute_mutation_request(state.borrow(), &request).map(Json)
}

async fn post_explain(
    Json(_request): Json<ndc_models::QueryRequest>,
) -> Result<Json<ndc_models::ExplainResponse>> {
    Err((
        StatusCode::NOT_IMPLEMENTED,
        Json(ndc_models::ErrorResponse {
            message: "explain is not supported".into(),
            details: serde_json::Value::Null,
        }),
    ))
}

async fn post_query_relational(
    State(state): State<Arc<AppState>>,
    Json(request): Json<RelationalQuery>,
) -> Result<Json<RelationalQueryResponse>> {
    custom_connector::query::relational::execute_relational_query(state.borrow(), &request)
        .await
        .map(|rows| Json(RelationalQueryResponse { rows }))
}

async fn post_mutation_rel_insert(
    State(state): State<Arc<AppState>>,
    Json(request): Json<ndc_models::RelationalInsertRequest>,
) -> Result<Json<ndc_models::RelationalInsertResponse>> {
    custom_connector::mutation::execute_relational_insert(state.borrow(), &request).map(Json)
}

async fn post_mutation_rel_update(
    State(state): State<Arc<AppState>>,
    Json(request): Json<ndc_models::RelationalUpdateRequest>,
) -> Result<Json<ndc_models::RelationalUpdateResponse>> {
    custom_connector::mutation::execute_relational_update(state.borrow(), &request).map(Json)
}

async fn post_mutation_rel_delete(
    State(state): State<Arc<AppState>>,
    Json(request): Json<ndc_models::RelationalDeleteRequest>,
) -> Result<Json<ndc_models::RelationalDeleteResponse>> {
    custom_connector::mutation::execute_relational_delete(state.borrow(), &request).map(Json)
}
