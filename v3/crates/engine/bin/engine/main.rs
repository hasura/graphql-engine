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
use pre_execution_plugin::{
    configuration::PrePluginConfig, execute::pre_execution_plugins_handler,
};
use reqwest::header::CONTENT_TYPE;
use serde::Serialize;
use tower_http::cors::CorsLayer;
use tower_http::trace::TraceLayer;
use tracing_util::{
    add_event_on_active_span, set_attribute_on_active_span, set_status_on_current_span,
    ErrorVisibility, SpanVisibility, TraceableError, TraceableHttpResponse,
};

use base64::engine::Engine;
use engine::internal_flags::{resolve_unstable_features, UnstableFeature};
use engine::VERSION;
use engine::{
    authentication::{
        AuthConfig::{self, V1 as V1AuthConfig},
        AuthModeConfig,
    },
    plugins::read_pre_execution_plugins_config,
};
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

#[allow(clippy::struct_excessive_bools)] // booleans are pretty useful here
#[derive(Parser, Serialize)]
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
    /// Enables the '/v1/sql' endpoint
    #[arg(long, env = "ENABLE_SQL_INTERFACE")]
    enable_sql_interface: bool,
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
    /// Allow unknown subgraphs, pruning relationships that refer to them.
    /// Useful when working with part of a supergraph.
    #[arg(long, env = "PARTIAL_SUPERGRAPH")]
    partial_supergraph: bool,
    /// List of internal unstable features to enable, separated by commas
    #[arg(
        long = "unstable-feature",
        value_name = "UNSTABLE_FEATURES",
        env = "UNSTABLE_FEATURES",
        value_delimiter = ','
    )]
    unstable_features: Vec<UnstableFeature>,
    /// The configuration file used for authentication.
    #[arg(long, value_name = "PATH", env = "pre_execution_plugins_path")]
    pre_execution_plugins_path: Option<PathBuf>,

    /// Whether internal errors should be shown or censored.
    /// It is recommended to only show errors while developing since internal errors may contain
    /// sensitve information.
    #[arg(long, env = "EXPOSE_INTERNAL_ERRORS")]
    expose_internal_errors: bool,
}

struct EngineState {
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: HttpContext,
    schema: gql::schema::Schema<GDS>,
    auth_config: AuthConfig,
    pre_execution_plugins_config: Vec<PrePluginConfig>,
    sql_context: sql::catalog::Context,
}

#[tokio::main]
#[allow(clippy::print_stdout)]
async fn main() {
    let server = ServerOptions::parse();

    tracing_util::initialize_tracing(
        server.otlp_endpoint.as_deref(),
        "graphql-engine",
        Some(VERSION),
    )
    .unwrap();

    if let Err(e) = tracing_util::global_tracer()
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
#[allow(clippy::enum_variant_names)]
enum StartupError {
    #[error("could not read the auth config - {0}")]
    ReadAuth(anyhow::Error),
    #[error("failed to build engine state - {0}")]
    ReadSchema(anyhow::Error),
    #[error("could not read the pre-execution plugins config - {0}")]
    ReadPrePlugin(anyhow::Error),
}

impl TraceableError for StartupError {
    fn visibility(&self) -> tracing_util::ErrorVisibility {
        ErrorVisibility::User
    }
}

/// The main router for the engine.
struct EngineRouter {
    /// The base router for the engine.
    /// Contains /, /graphql, /v1/explain and /health routes.
    base_router: Router,
    /// The metadata routes for the introspection metadata file.
    /// Contains /metadata and /metadata-hash routes.
    metadata_routes: Option<Router>,
    /// Routes for the SQL interface
    sql_routes: Option<Router>,
    /// The CORS layer for the engine.
    cors_layer: Option<CorsLayer>,
}

impl EngineRouter {
    fn new(state: Arc<EngineState>) -> Self {
        let graphql_route = Router::new()
            .route("/graphql", post(handle_request))
            .layer(axum::middleware::from_fn_with_state(
                state.clone(),
                pre_execution_plugins_middleware,
            ))
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
            .with_state(state);

        let health_route = Router::new().route("/health", get(handle_health));

        let base_routes = Router::new()
            // serve graphiql at root
            .route("/", get(graphiql))
            // The '/graphql' route
            .merge(graphql_route)
            // The '/v1/explain' route
            .merge(explain_route)
            // The '/health' route
            .merge(health_route)
            // Set request payload limit to 10 MB
            .layer(DefaultBodyLimit::max(10 * MB));

        Self {
            base_router: base_routes,
            metadata_routes: None,
            sql_routes: None,
            cors_layer: None,
        }
    }

    /// Serve the introspection metadata file and its hash at `/metadata` and `/metadata-hash` respectively.
    /// This is a temporary workaround to enable the console to interact with an engine process running locally.
    async fn add_metadata_routes(
        &mut self,
        introspection_metadata_path: &PathBuf,
    ) -> Result<(), StartupError> {
        let file_contents = tokio::fs::read_to_string(introspection_metadata_path)
            .await
            .map_err(|err| StartupError::ReadSchema(err.into()))?;
        let mut hasher = hash::DefaultHasher::new();
        file_contents.hash(&mut hasher);
        let hash = hasher.finish();
        let base64_hash = base64::engine::general_purpose::STANDARD.encode(hash.to_ne_bytes());
        let metadata_routes = Router::new()
            .route("/metadata", get(|| async { file_contents }))
            .route("/metadata-hash", get(|| async { base64_hash }));
        self.metadata_routes = Some(metadata_routes);
        Ok(())
    }

    fn add_sql_route(&mut self, state: Arc<EngineState>) {
        let sql_routes = Router::new()
            .route("/v1/sql", post(handle_sql_request))
            .layer(axum::middleware::from_fn(
                hasura_authn_core::resolve_session,
            ))
            .layer(axum::middleware::from_fn_with_state(
                state.clone(),
                authentication_middleware,
            ))
            .layer(axum::middleware::from_fn(sql_request_tracing_middleware))
            // *PLEASE DO NOT ADD ANY MIDDLEWARE
            // BEFORE THE `explain_request_tracing_middleware`*
            // Refer to it for more details.
            .layer(TraceLayer::new_for_http())
            .with_state(state);
        self.sql_routes = Some(sql_routes);
    }

    fn add_cors_layer(&mut self, allow_origin: &[String]) {
        self.cors_layer = Some(cors::build_cors_layer(allow_origin));
    }

    fn into_make_service(self) -> axum::routing::IntoMakeService<Router> {
        let mut app = self.base_router;
        // Merge the metadata routes if they exist.
        if let Some(sql_routes) = self.sql_routes {
            app = app.merge(sql_routes);
        }
        // Merge the metadata routes if they exist.
        if let Some(metadata_routes) = self.metadata_routes {
            app = app.merge(metadata_routes);
        }
        // Add the CORS layer if it exists.
        if let Some(cors_layer) = self.cors_layer {
            // It is important that this layer is added last, since it only affects
            // the layers that precede it.
            app = app.layer(cors_layer);
        }
        app.into_make_service()
    }
}

#[allow(clippy::print_stdout)]
async fn start_engine(server: &ServerOptions) -> Result<(), StartupError> {
    let metadata_resolve_configuration = metadata_resolve::configuration::Configuration {
        allow_unknown_subgraphs: server.partial_supergraph,
        unstable_features: resolve_unstable_features(&server.unstable_features),
    };

    let expose_internal_errors = if server.expose_internal_errors {
        execute::ExposeInternalErrors::Expose
    } else {
        execute::ExposeInternalErrors::Censor
    };

    let state = build_state(
        expose_internal_errors,
        &server.authn_config_path,
        &server.metadata_path,
        &server.pre_execution_plugins_path,
        metadata_resolve_configuration,
    )
    .map_err(StartupError::ReadSchema)?;

    let mut engine_router = EngineRouter::new(state.clone());

    if server.enable_sql_interface {
        engine_router.add_sql_route(state.clone());
    }

    // If `--introspection-metadata` is specified we also serve the file indicated on `/metadata`
    // and its hash on `/metadata-hash`.
    if let Some(path) = &server.introspection_metadata {
        engine_router.add_metadata_routes(path).await?;
    }

    // If `--enable-cors` is specified, we add a CORS layer to the app.
    if server.enable_cors {
        engine_router.add_cors_layer(&server.cors_allow_origin);
    }

    let address = net::SocketAddr::new(server.host, server.port);
    let log = format!("starting server on {address}");
    println!("{log}");
    add_event_on_active_span(log);

    set_attribute_on_active_span(
        tracing_util::AttributeVisibility::Internal,
        "server_options",
        serde_json::to_string_pretty(server).unwrap_or_else(|err| err.to_string()),
    );

    // run it with hyper on `addr`
    axum::Server::bind(&address)
        .serve(engine_router.into_make_service())
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

/// Middleware to start tracing of the `/v1/sql` request.
/// This middleware must be active for the entire duration
/// of the request i.e. this middleware should be the
/// entry point and the exit point of the SQL request.
async fn sql_request_tracing_middleware<B: Send>(
    request: Request<B>,
    next: Next<B>,
) -> axum::response::Response {
    let tracer = tracing_util::global_tracer();
    let path = "/v1/sql";
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
    headers: axum::http::header::HeaderMap,
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
                    state.expose_internal_errors,
                    &state.http_context,
                    &state.schema,
                    &session,
                    &headers,
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
    response.inner()
}

async fn handle_explain_request(
    headers: axum::http::header::HeaderMap,
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
                    state.expose_internal_errors,
                    &state.http_context,
                    &state.schema,
                    &session,
                    &headers,
                    request,
                ))
            },
        )
        .await;

    // Set the span as error if the response contains an error
    set_status_on_current_span(&response);
    response
}

async fn pre_execution_plugins_middleware<'a, B>(
    State(engine_state): State<Arc<EngineState>>,
    Extension(session): Extension<Session>,
    headers_map: HeaderMap,
    request: Request<B>,
    next: Next<axum::body::Body>,
) -> axum::response::Result<axum::response::Response>
where
    B: HttpBody,
    B::Error: Display,
{
    let (request, response) = pre_execution_plugins_handler(
        &engine_state.pre_execution_plugins_config,
        &engine_state.http_context.client,
        session,
        request,
        headers_map,
    )
    .await?;

    match response {
        Some(response) => Ok(response),
        None => Ok(next.run(request).await),
    }
}

/// Handle a SQL request and execute it.
async fn handle_sql_request(
    State(state): State<Arc<EngineState>>,
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
                        &state.sql_context,
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
            Json(serde_json::json!({"error": e.to_string()})),
        )
            .into_response(),
    }
}

/// Build the engine state - include auth, metadata, and sql context.
fn build_state(
    expose_internal_errors: execute::ExposeInternalErrors,
    authn_config_path: &PathBuf,
    metadata_path: &PathBuf,
    pre_execution_plugins_path: &Option<PathBuf>,
    metadata_resolve_configuration: metadata_resolve::configuration::Configuration,
) -> Result<Arc<EngineState>, anyhow::Error> {
    let auth_config = read_auth_config(authn_config_path).map_err(StartupError::ReadAuth)?;
    let pre_execution_plugins_config =
        read_pre_execution_plugins_config(pre_execution_plugins_path)
            .map_err(StartupError::ReadPrePlugin)?;
    let raw_metadata = std::fs::read_to_string(metadata_path)?;
    let metadata = open_dds::Metadata::from_json_str(&raw_metadata)?;
    let resolved_metadata = metadata_resolve::resolve(metadata, metadata_resolve_configuration)?;
    let http_context = HttpContext {
        client: reqwest::Client::new(),
        ndc_response_size_limit: None,
    };
    let sql_context = sql::catalog::Context::from_metadata(&resolved_metadata);
    let schema = schema::GDS {
        metadata: resolved_metadata,
    }
    .build_schema()?;
    let state = Arc::new(EngineState {
        expose_internal_errors,
        http_context,
        schema,
        auth_config,
        pre_execution_plugins_config,
        sql_context,
    });
    Ok(state)
}

fn read_auth_config(path: &PathBuf) -> Result<AuthConfig, anyhow::Error> {
    let raw_auth_config = std::fs::read_to_string(path)?;
    Ok(open_dds::traits::OpenDd::deserialize(
        serde_json::from_str(&raw_auth_config)?,
    )?)
}
