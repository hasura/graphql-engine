use std::borrow::Borrow;
use std::net;
use std::sync::Arc;

use axum::{
    extract::State,
    http::StatusCode,
    routing::{get, post},
    Json, Router,
};

use custom_connector::state::AppState;

type Result<A> = std::result::Result<A, (StatusCode, Json<ndc_models::ErrorResponse>)>;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let app_state = Arc::new(custom_connector::state::init_app_state()?);

    let app = Router::new()
        .route("/healthz", get(get_healthz))
        .route("/capabilities", get(get_capabilities))
        .route("/schema", get(get_schema))
        .route("/query", post(post_query))
        .route("/mutation", post(post_mutation))
        .route("/explain", post(post_explain))
        .with_state(app_state);

    // run it with hyper on localhost:8101
    let host = net::IpAddr::V6(net::Ipv6Addr::UNSPECIFIED);
    let port = 8101;
    let socket_addr = net::SocketAddr::new(host, port);
    axum::Server::bind(&socket_addr)
        .serve(app.into_make_service())
        .with_graceful_shutdown(async {
            // wait for a SIGINT, i.e. a Ctrl+C from the keyboard
            let sigint = async {
                tokio::signal::ctrl_c()
                    .await
                    .expect("failed to install signal handler")
            };
            // wait for a SIGTERM, i.e. a normal `kill` command
            #[cfg(unix)]
            let sigterm = async {
                tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
                    .expect("failed to install signal handler")
                    .recv()
                    .await
            };
            // block until either of the above happens
            #[cfg(unix)]
            tokio::select! {
                () = sigint => (),
                _ = sigterm => (),
            }
            #[cfg(windows)]
            tokio::select! {
                _ = sigint => (),
            }
        })
        .await?;
    Ok(())
}

async fn get_healthz() -> StatusCode {
    StatusCode::NO_CONTENT
}

async fn get_capabilities() -> Json<ndc_models::CapabilitiesResponse> {
    Json(custom_connector::schema::get_capabilities())
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
