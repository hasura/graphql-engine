use std::fmt::Display;
use std::net;
use std::path::PathBuf;
use std::sync::Arc;

use axum::{
    body::HttpBody,
    extract::{DefaultBodyLimit, State},
    http::{HeaderMap, Request},
    middleware::Next,
    response::{Html, IntoResponse},
    routing::{get, post},
    Extension, Json, Router,
};
use clap::Parser;
use tower_http::trace::TraceLayer;
use tracing_util::{
    add_event_on_active_span, set_status_on_current_span, ErrorVisibility, SpanVisibility,
    TraceableError, TraceableHttpResponse,
};

use base64::engine::Engine;
use engine::authentication::{AuthConfig, AuthConfig::V1 as V1AuthConfig, AuthModeConfig};
use engine::VERSION;
use execute::HttpContext;
use hasura_authn_core::Session;
use hasura_authn_jwt::auth as jwt_auth;
use hasura_authn_jwt::jwt;
use hasura_authn_webhook::webhook;
use lang_graphql as gql;
use schema::GDS;
use std::hash;
use std::hash::{Hash, Hasher};

mod cors;

const DEFAULT_PORT: u16 = 3000;

const MB: usize = 1_048_576;

#[derive(Parser)]
#[command(version = VERSION)]
struct ServerOptions {
    /// The path to the metadata file, used to construct the schema.g
    #[arg(long, value_name = "PATH", env = "METADATA_PATH")]
    metadata_path: PathBuf,
    /// An introspection metadata file, served over `/metadata` if provided.
    #[arg(long, value_name = "PATH", env = "INTROSPECTION_METADATA_FILE")]
    introspection_metadata: Option<PathBuf>,
    /// The OpenTelemetry collector endpoint.
    #[arg(long, value_name = "URL", env = "OTLP_ENDPOINT")]
    otlp_endpoint: Option<String>,
    /// The configuration file used for authentication.
    #[arg(long, value_name = "PATH", env = "AUTHN_CONFIG_PATH")]
    authn_config_path: PathBuf,
    /// The host IP on which the server listens, defaulting to all IPv4 and IPv6 addresses.
    #[arg(long, value_name = "HOST", env = "HOST", default_value_t = net::IpAddr::V6(net::Ipv6Addr::UNSPECIFIED))]
    host: net::IpAddr,
    /// The port on which the server listens.
    #[arg(long, value_name = "PORT", env = "PORT", default_value_t = DEFAULT_PORT)]
    port: u16,
    /// Enable CORS. Support preflight request and include related headers in responses.
    #[arg(long, env = "ENABLE_CORS")]
    enable_cors: bool,
    /// The list of allowed origins for CORS. If not provided, all origins are allowed.
    /// Requires `--enable-cors` to be set.
    #[arg(
        long,
        value_name = "ORIGIN_LIST",
        env = "CORS_ALLOW_ORIGIN",
        requires = "enable_cors",
        value_delimiter = ','
    )]
    cors_allow_origin: Vec<String>,
}

struct EngineState {
    http_context: HttpContext,
    schema: gql::schema::Schema<GDS>,
    auth_config: AuthConfig,
}

#[tokio::main]
async fn main() {
    let server = ServerOptions::parse();

    let tracer = tracing_util::start_tracer(
        server.otlp_endpoint.as_deref(),
        "graphql-engine",
        env!("CARGO_PKG_VERSION"),
    )
    .unwrap();

    if let Err(e) = tracer
        .in_span_async(
            "app init",
            "App initialization",
            SpanVisibility::Internal,
            || Box::pin(start_engine(&server)),
        )
        .await
    {
        println!("Error while starting up the engine: {e}");
    }

    tracing_util::shutdown_tracer();
}

// Connects a signal handler for the unix SIGTERM signal. (This is the standard signal that unix
// systems send when pressing ctrl+c or running `kill`. It is distinct from the "force kill" signal
// which is SIGKILL.) This function produces a future that resolves when a SIGTERM is received. We
// pass the future to axum's `with_graceful_shutdown` method to instruct axum to start a graceful
// shutdown when the signal is received.
//
// Listening for SIGTERM specifically avoids a 10-second delay when stopping the process.
//
// Also listens for tokio's cross-platform `ctrl_c` signal polyfill.
//
// copied from https://github.com/davidB/axum-tracing-opentelemetry/blob/main/examples/otlp/src/main.rs
async fn shutdown_signal() {
    let ctrl_c = async {
        tokio::signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        () = ctrl_c => {},
        () = terminate => {},
    }
}

#[derive(thiserror::Error, Debug)]
enum StartupError {
    #[error("could not read the auth config - {0}")]
    ReadAuth(anyhow::Error),
    #[error("could not read the schema - {0}")]
    ReadSchema(anyhow::Error),
}

impl TraceableError for StartupError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        ErrorVisibility::User
    }
}

async fn start_engine(server: &ServerOptions) -> Result<(), StartupError> {
    let auth_config =
        read_auth_config(&server.authn_config_path).map_err(StartupError::ReadAuth)?;
    let schema = read_schema(&server.metadata_path).map_err(StartupError::ReadSchema)?;
    let http_context = HttpContext {
        client: reqwest::Client::new(),
        ndc_response_size_limit: None,
    };
    let state = Arc::new(EngineState {
        http_context,
        schema,
        auth_config,
    });

    let graphql_route = Router::new()
        .route("/graphql", post(handle_request))
        .layer(axum::middleware::from_fn(
            hasura_authn_core::resolve_session,
        ))
        .layer(axum::middleware::from_fn_with_state(
            state.clone(),
            authentication_middleware,
        ))
        .layer(axum::middleware::from_fn(
            graphql_request_tracing_middleware,
        ))
        // *PLEASE DO NOT ADD ANY MIDDLEWARE
        // BEFORE THE `graphql_request_tracing_middleware`*
        // Refer to it for more details.
        .layer(TraceLayer::new_for_http())
        .with_state(state.clone());

    let explain_route = Router::new()
        .route("/v1/explain", post(handle_explain_request))
        .layer(axum::middleware::from_fn(
            hasura_authn_core::resolve_session,
        ))
        .layer(axum::middleware::from_fn_with_state(
            state.clone(),
            authentication_middleware,
        ))
        .layer(axum::middleware::from_fn(
            explain_request_tracing_middleware,
        ))
        // *PLEASE DO NOT ADD ANY MIDDLEWARE
        // BEFORE THE `explain_request_tracing_middleware`*
        // Refer to it for more details.
        .layer(TraceLayer::new_for_http())
        .with_state(state.clone());

    let health_route = Router::new().route("/health", get(handle_health));

    let app = Router::new()
        // serve graphiql at root
        .route("/", get(graphiql))
        .merge(graphql_route)
        .merge(explain_route)
        .merge(health_route)
        .layer(DefaultBodyLimit::max(10 * MB)); // Set request payload limit to 10 MB

    // If `--introspection-metadata` is specified we also serve the file indicated on `/metadata`
    // and its hash on `/metadata-hash`.
    let app = match &server.introspection_metadata {
        None => app,
        Some(path) => add_metadata_routes(app, path).await?,
    };

    let address = net::SocketAddr::new(server.host, server.port);
    let log = format!("starting server on {address}");
    println!("{log}");
    add_event_on_active_span(log);

    // If `--enable-cors` is specified, we add a CORS layer to the app.
    // It is important that this layer is added last, since it only affects the layers that precede
    // it.
    let app = if server.enable_cors {
        cors::add_cors_layer(app, &server.cors_allow_origin)
    } else {
        app
    };

    // run it with hyper on `addr`
    axum::Server::bind(&address)
        .serve(app.into_make_service())
        .with_graceful_shutdown(shutdown_signal())
        .await
        .unwrap();

    Ok(())
}

/// Health check endpoint
async fn handle_health() -> reqwest::StatusCode {
    reqwest::StatusCode::OK
}

/// Middleware to start tracing of the `/graphql` request.
/// This middleware must be active for the entire duration
/// of the request i.e. this middleware should be the
/// entry point and the exit point of the GraphQL request.
async fn graphql_request_tracing_middleware<B: Send>(
    request: Request<B>,
    next: Next<B>,
) -> axum::response::Response {
    use tracing_util::*;
    let tracer = global_tracer();
    let path = "/graphql";
    tracer
        .in_span_async_with_parent_context(
            path,
            path,
            SpanVisibility::User,
            &request.headers().clone(),
            || {
                set_attribute_on_active_span(AttributeVisibility::Internal, "version", VERSION);
                Box::pin(async move {
                    let mut response = next.run(request).await;
                    TraceContextResponsePropagator::new().inject_context(
                        &Context::current(),
                        &mut HeaderInjector(response.headers_mut()),
                    );
                    TraceableHttpResponse::new(response, path)
                })
            },
        )
        .await
        .response
}

/// Middleware to start tracing of the `/v1/explain` request.
/// This middleware must be active for the entire duration
/// of the request i.e. this middleware should be the
/// entry point and the exit point of the GraphQL request.
async fn explain_request_tracing_middleware<B: Send>(
    request: Request<B>,
    next: Next<B>,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let path = "/v1/explain";
    tracer
        .in_span_async_with_parent_context(
            path,
            path,
            SpanVisibility::User,
            &request.headers().clone(),
            || {
                Box::pin(async move {
                    let response = next.run(request).await;
                    TraceableHttpResponse::new(response, path)
                })
            },
        )
        .await
        .response
}

#[derive(Debug, thiserror::Error)]
enum AuthError {
    #[error("JWT auth error: {0}")]
    Jwt(#[from] jwt::Error),
    #[error("Webhook auth error: {0}")]
    Webhook(#[from] webhook::Error),
}

impl TraceableError for AuthError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        match self {
            AuthError::Jwt(e) => e.visibility(),
            AuthError::Webhook(e) => e.visibility(),
        }
    }
}

impl IntoResponse for AuthError {
    fn into_response(self) -> axum::response::Response {
        match self {
            AuthError::Jwt(e) => e.into_response(),
            AuthError::Webhook(e) => e.into_response(),
        }
    }
}

/// This middleware authenticates the incoming GraphQL request according to the
/// authentication configuration present in the `auth_config` of `EngineState`. The
/// result of the authentication is `hasura-authn-core::Identity`, which is then
/// made available to the GraphQL request handler.
async fn authentication_middleware<'a, B>(
    State(engine_state): State<Arc<EngineState>>,
    headers_map: HeaderMap,
    mut request: Request<B>,
    next: Next<B>,
) -> axum::response::Result<axum::response::Response>
where
    B: HttpBody,
    B::Error: Display,
{
    let tracer = tracing_util::global_tracer();

    let resolved_identity = tracer
        .in_span_async(
            "authentication_middleware",
            "Authentication middleware",
            SpanVisibility::Internal,
            || {
                Box::pin(async {
                    match &engine_state.auth_config {
                        V1AuthConfig(auth_config) => match &auth_config.mode {
                            AuthModeConfig::Webhook(webhook_config) => {
                                webhook::authenticate_request(
                                    &engine_state.http_context.client,
                                    webhook_config,
                                    &headers_map,
                                    auth_config.allow_role_emulation_by.as_ref(),
                                )
                                .await
                                .map_err(AuthError::from)
                            }
                            AuthModeConfig::Jwt(jwt_secret_config) => {
                                jwt_auth::authenticate_request(
                                    &engine_state.http_context.client,
                                    *jwt_secret_config.clone(),
                                    auth_config.allow_role_emulation_by.as_ref(),
                                    &headers_map,
                                )
                                .await
                                .map_err(AuthError::from)
                            }
                        },
                    }
                })
            },
        )
        .await?;

    request.extensions_mut().insert(resolved_identity);
    Ok(next.run(request).await)
}

async fn graphiql() -> Html<&'static str> {
    Html(include_str!("index.html"))
}

async fn handle_request(
    State(state): State<Arc<EngineState>>,
    Extension(session): Extension<Session>,
    Json(request): Json<gql::http::RawRequest>,
) -> gql::http::Response {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "handle_request",
            "Handle request",
            SpanVisibility::User,
            || {
                Box::pin(execute::execute_query(
                    &state.http_context,
                    &state.schema,
                    &session,
                    request,
                    None,
                ))
            },
        )
        .await;

    // Set the span as error if the response contains an error
    // NOTE: Ideally, we should mark the root span as error in `graphql_request_tracing_middleware` function,
    // the tracing middleware, where the span is initialized. It is possible by completing the implementation
    // of `Traceable` trait for `AxumResponse` struct. The said struct just wraps the `axum::response::Response`.
    // The only way to determine the error is to inspect the status code from the `Response` struct.
    // In `/graphql` API, all responses are sent with `200` OK including errors, which leaves no way to deduce errors in the tracing middleware.
    set_status_on_current_span(&response);
    response.0
}

async fn handle_explain_request(
    State(state): State<Arc<EngineState>>,
    Extension(session): Extension<Session>,
    Json(request): Json<gql::http::RawRequest>,
) -> execute::ExplainResponse {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "handle_explain_request",
            "Handle explain request",
            SpanVisibility::User,
            || {
                Box::pin(execute::execute_explain(
                    &state.http_context,
                    &state.schema,
                    &session,
                    request,
                ))
            },
        )
        .await;

    // Set the span as error if the response contains an error
    set_status_on_current_span(&response);
    response
}

fn read_schema(metadata_path: &PathBuf) -> Result<gql::schema::Schema<GDS>, anyhow::Error> {
    let raw_metadata = std::fs::read_to_string(metadata_path)?;
    let metadata = open_dds::Metadata::from_json_str(&raw_metadata)?;
    Ok(engine::build::build_schema(metadata)?)
}

fn read_auth_config(path: &PathBuf) -> Result<AuthConfig, anyhow::Error> {
    let raw_auth_config = std::fs::read_to_string(path)?;
    Ok(open_dds::traits::OpenDd::deserialize(
        serde_json::from_str(&raw_auth_config)?,
    )?)
}

/// Serve the introspection metadata file and its hash at `/metadata` and `/metadata-hash` respectively.
/// This is a temporary workaround to enable the console to interact with an engine process running locally.
async fn add_metadata_routes(
    app: Router,
    introspection_metadata_path: &PathBuf,
) -> Result<Router, StartupError> {
    let file_contents = tokio::fs::read_to_string(introspection_metadata_path)
        .await
        .map_err(|err| StartupError::ReadSchema(err.into()))?;
    let mut hasher = hash::DefaultHasher::new();
    file_contents.hash(&mut hasher);
    let hash = hasher.finish();
    let base64_hash = base64::engine::general_purpose::STANDARD.encode(hash.to_ne_bytes());
    let new_app = app
        .merge(Router::new().route("/metadata", get(|| async { file_contents })))
        .merge(Router::new().route("/metadata-hash", get(|| async { base64_hash })));
    Ok(new_app)
}
