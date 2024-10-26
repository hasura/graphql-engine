use futures_util::FutureExt;
use hasura_authn::{authenticate, resolve_auth_config, AuthConfig};
use json_api::create_json_api_router;
use std::fmt::Display;
use std::hash;
use std::hash::{Hash, Hasher};
use std::net;
use std::path::PathBuf;
use std::sync::Arc;

use axum::{
    extract::{DefaultBodyLimit, State},
    http::{HeaderMap, Request},
    middleware::Next,
    response::{Html, IntoResponse},
    routing::{get, post},
    Extension, Json, Router,
};
use axum_core::body::Body;
use base64::engine::Engine;
use clap::Parser;
use http_body_util::BodyExt;
use metadata_resolve::LifecyclePluginConfigs;
use pre_parse_plugin::execute::pre_parse_plugins_handler;
use pre_response_plugin::execute::pre_response_plugins_handler;
use reqwest::header::CONTENT_TYPE;
use serde::Serialize;
use tower_http::cors::CorsLayer;
use tower_http::trace::TraceLayer;

use engine::{
    internal_flags::{resolve_unstable_features, UnstableFeature},
    VERSION,
};
use execute::HttpContext;
use graphql_schema::GDS;
use hasura_authn_core::Session;
use lang_graphql as gql;
use tracing_util::{
    add_event_on_active_span, set_attribute_on_active_span, set_status_on_current_span,
    ErrorVisibility, SpanVisibility, TraceableError, TraceableHttpResponse,
};

mod cors;
mod json_api;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

const DEFAULT_PORT: u16 = 3000;

const MB: usize = 1_048_576;

#[allow(clippy::struct_excessive_bools)] // booleans are pretty useful here
#[derive(Parser, Serialize)]
#[command(version = VERSION)]
struct ServerOptions {
    /// The path to the metadata file, used to construct the schema.
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
    /// List of internal unstable features to enable, separated by commas
    #[arg(
        long = "unstable-feature",
        value_name = "UNSTABLE_FEATURES",
        env = "UNSTABLE_FEATURES",
        value_delimiter = ','
    )]
    unstable_features: Vec<UnstableFeature>,

    /// Whether internal errors should be shown or censored.
    /// It is recommended to only show errors while developing since internal errors may contain
    /// sensitve information.
    #[arg(long, env = "EXPOSE_INTERNAL_ERRORS")]
    expose_internal_errors: bool,

    /// Log traces to stdout.
    #[arg(long, env = "EXPORT_TRACES_STDOUT")]
    export_traces_stdout: bool,
}

#[derive(Clone)] // Cheap to clone as heavy fields are wrapped in `Arc`
pub struct EngineState {
    expose_internal_errors: execute::ExposeInternalErrors,
    http_context: HttpContext,
    graphql_state: Arc<gql::schema::Schema<GDS>>,
    resolved_metadata: Arc<metadata_resolve::Metadata>,
    jsonapi_catalog: Arc<jsonapi::Catalog>,
    auth_config: Arc<AuthConfig>,
    sql_context: Arc<sql::catalog::Catalog>,
    plugin_configs: Arc<LifecyclePluginConfigs>,
    graphql_websocket_server: Arc<graphql_ws::WebSocketServer>,
}

#[tokio::main]
#[allow(clippy::print_stdout)]
async fn main() {
    let server_options = ServerOptions::parse();
    let export_traces_stdout = if server_options.export_traces_stdout {
        tracing_util::ExportTracesStdout::Enable
    } else {
        tracing_util::ExportTracesStdout::Disable
    };

    tracing_util::initialize_tracing(
        server_options.otlp_endpoint.as_deref(),
        "ddn-engine",
        Some(VERSION),
        tracing_util::PropagateBaggage::Disable,
        export_traces_stdout,
    )
    .unwrap();

    if let Err(e) = tracing_util::global_tracer()
        .in_span_async(
            "app init",
            "App initialization",
            SpanVisibility::Internal,
            || Box::pin(start_engine(&server_options)),
        )
        .await
    {
        println!("Error while starting up the engine: {e}");
    }

    tracing_util::shutdown_tracer();
}

#[derive(thiserror::Error, Debug)]
#[allow(clippy::enum_variant_names)]
enum StartupError {
    #[error("could not read the auth config - {0}")]
    ReadAuth(anyhow::Error),
    #[error("failed to build engine state - {0}")]
    ReadSchema(anyhow::Error),
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
    /// Routes for the JSON:API interface
    jsonapi_routes: Option<Router>,
    /// The CORS layer for the engine.
    cors_layer: Option<CorsLayer>,
}

impl EngineRouter {
    fn new(state: EngineState) -> Self {
        let graphql_ws_route = Router::new()
            .route("/graphql", get(handle_websocket_request))
            .layer(axum::middleware::from_fn(
                graphql_request_tracing_middleware,
            ))
            // *PLEASE DO NOT ADD ANY MIDDLEWARE
            // BEFORE THE `graphql_request_tracing_middleware`*
            // Refer to it for more details.
            .layer(TraceLayer::new_for_http())
            .with_state(state.clone());

        let graphql_route = Router::new()
            .route("/graphql", post(handle_request))
            .layer(axum::middleware::from_fn_with_state(
                state.clone(),
                plugins_middleware,
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
            // The '/graphql' route for websocket
            .merge(graphql_ws_route)
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
            jsonapi_routes: None,
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

    fn add_sql_route(&mut self, state: EngineState) {
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

    fn add_jsonapi_route(&mut self, state: EngineState) {
        let jsonapi_routes = create_json_api_router(state);
        self.jsonapi_routes = Some(jsonapi_routes);
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
        if let Some(jsonapi_routes) = self.jsonapi_routes {
            app = app.merge(jsonapi_routes);
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
        server.enable_sql_interface,
        &metadata_resolve_configuration,
    )
    .map_err(StartupError::ReadSchema)?;

    let mut engine_router = EngineRouter::new(state.clone());

    if server.enable_sql_interface {
        engine_router.add_sql_route(state.clone());
    }

    if metadata_resolve_configuration
        .unstable_features
        .enable_jsonapi
    {
        engine_router.add_jsonapi_route(state.clone());
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
    let listener = tokio::net::TcpListener::bind(address).await.unwrap();

    axum::serve(listener, engine_router.into_make_service())
        .with_graceful_shutdown(axum_ext::shutdown_signal_with_handler(|| async move {
            state
                .graphql_websocket_server
                .shutdown("Shutting server down")
                .await;
        }))
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
async fn graphql_request_tracing_middleware(
    request: Request<Body>,
    next: Next,
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
                    get_text_map_propagator(|propagator| {
                        propagator.inject_context(
                            &Context::current(),
                            &mut HeaderInjector(response.headers_mut()),
                        );
                    });
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
async fn explain_request_tracing_middleware(
    request: Request<Body>,
    next: Next,
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
async fn sql_request_tracing_middleware(
    request: Request<Body>,
    next: Next,
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

/// This middleware authenticates the incoming GraphQL request according to the
/// authentication configuration present in the `auth_config` of `EngineState`. The
/// result of the authentication is `hasura-authn-core::Identity`, which is then
/// made available to the GraphQL request handler.
pub async fn authentication_middleware<'a>(
    State(engine_state): State<EngineState>,
    headers_map: HeaderMap,
    mut request: Request<Body>,
    next: Next,
) -> axum::response::Result<axum::response::Response> {
    let tracer = tracing_util::global_tracer();

    let resolved_identity = tracer
        .in_span_async(
            "authentication_middleware",
            "Authentication middleware",
            SpanVisibility::Internal,
            || {
                Box::pin(authenticate(
                    &headers_map,
                    &engine_state.http_context.client,
                    &engine_state.auth_config,
                ))
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
    State(state): State<EngineState>,
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
                {
                    Box::pin(
                        graphql_frontend::execute_query(
                            state.expose_internal_errors,
                            &state.http_context,
                            &state.graphql_state,
                            &session,
                            &headers,
                            request,
                            None,
                        )
                        .map(|(_operation_type, graphql_response)| graphql_response),
                    )
                }
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
    State(state): State<EngineState>,
    Extension(session): Extension<Session>,
    Json(request): Json<gql::http::RawRequest>,
) -> graphql_frontend::ExplainResponse {
    let tracer = tracing_util::global_tracer();
    let response = tracer
        .in_span_async(
            "handle_explain_request",
            "Handle explain request",
            SpanVisibility::User,
            || {
                Box::pin(
                    graphql_frontend::execute_explain(
                        state.expose_internal_errors,
                        &state.http_context,
                        &state.graphql_state,
                        &session,
                        &headers,
                        request,
                    )
                    .map(|(_operation_type, graphql_response)| graphql_response),
                )
            },
        )
        .await;

    // Set the span as error if the response contains an error
    set_status_on_current_span(&response);
    response
}

async fn plugins_middleware(
    State(engine_state): State<EngineState>,
    Extension(session): Extension<Session>,
    headers_map: HeaderMap,
    request: Request<axum::body::Body>,
    next: Next,
) -> axum::response::Result<axum::response::Response<Body>> {
    let (parts, body) = request.into_parts();
    let bytes = body
        .collect()
        .await
        .map_err(|err| {
            (reqwest::StatusCode::INTERNAL_SERVER_ERROR, err.to_string()).into_response()
        })?
        .to_bytes();
    let raw_request = bytes.clone();

    // Check if the pre_parse_plugins_config is empty
    let response =
        match nonempty::NonEmpty::from_slice(&engine_state.plugin_configs.pre_parse_plugins) {
            None => {
                // If empty, do nothing and pass the request to the next middleware
                let recreated_request = Request::from_parts(parts, axum::body::Body::from(bytes));
                Ok::<_, axum::response::ErrorResponse>(next.run(recreated_request).await)
            }
            Some(pre_parse_plugins) => {
                let response = pre_parse_plugins_handler(
                    &pre_parse_plugins,
                    &engine_state.http_context.client,
                    session.clone(),
                    &bytes,
                    headers_map.clone(),
                )
                .await?;

                if let Some(response) = response {
                    Ok(response)
                } else {
                    let recreated_request =
                        Request::from_parts(parts, axum::body::Body::from(bytes));
                    Ok(next.run(recreated_request).await)
                }
            }
        }?;

    let (parts, body) = response.into_parts();
    let response_bytes = body
        .collect()
        .await
        .map_err(|err| {
            (reqwest::StatusCode::INTERNAL_SERVER_ERROR, err.to_string()).into_response()
        })?
        .to_bytes();

    if let Some(pre_response_plugins) =
        nonempty::NonEmpty::from_slice(&engine_state.plugin_configs.pre_response_plugins)
    {
        pre_response_plugins_handler(
            &pre_response_plugins,
            &engine_state.http_context.client,
            session,
            &raw_request,
            &response_bytes,
            headers_map,
        )?;
    }
    let recreated_response =
        axum::response::Response::from_parts(parts, axum::body::Body::from(response_bytes));
    Ok(recreated_response)
}

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

#[allow(clippy::print_stdout)]
/// Print any build warnings to stdout
fn print_warnings<T: Display>(warnings: Vec<T>) {
    for warning in warnings {
        println!("Warning: {warning}");
    }
}

/// Build the engine state - include auth, metadata, and sql context.
fn build_state(
    expose_internal_errors: execute::ExposeInternalErrors,
    authn_config_path: &PathBuf,
    metadata_path: &PathBuf,
    enable_sql_interface: bool,
    metadata_resolve_configuration: &metadata_resolve::configuration::Configuration,
) -> Result<EngineState, anyhow::Error> {
    // Auth Config
    let raw_auth_config = std::fs::read_to_string(authn_config_path)?;
    let (auth_config, auth_warnings) =
        resolve_auth_config(&raw_auth_config).map_err(StartupError::ReadAuth)?;

    // Metadata
    let raw_metadata = std::fs::read_to_string(metadata_path)?;
    let metadata = open_dds::Metadata::from_json_str(&raw_metadata)?;
    let (resolved_metadata, warnings) =
        metadata_resolve::resolve(metadata, metadata_resolve_configuration)?;
    let resolved_metadata = Arc::new(resolved_metadata);

    print_warnings(auth_warnings);
    print_warnings(warnings);

    let http_context = HttpContext {
        client: reqwest::Client::new(),
        ndc_response_size_limit: None,
    };
    let plugin_configs = resolved_metadata.plugin_configs.clone();
    let sql_context = if enable_sql_interface {
        sql::catalog::Catalog::from_metadata(resolved_metadata.clone())
    } else {
        sql::catalog::Catalog::empty_from_metadata(resolved_metadata.clone())
    };

    let schema = graphql_schema::GDS {
        metadata: resolved_metadata.clone(),
    }
    .build_schema()?;

    let (jsonapi_catalog, _json_api_warnings) = jsonapi::Catalog::new(&resolved_metadata);

    let state = EngineState {
        expose_internal_errors,
        http_context,
        graphql_state: Arc::new(schema),
        jsonapi_catalog: Arc::new(jsonapi_catalog),
        resolved_metadata,
        auth_config: Arc::new(auth_config),
        sql_context: sql_context.into(),
        plugin_configs: Arc::new(plugin_configs),
        graphql_websocket_server: Arc::new(graphql_ws::WebSocketServer::new()),
    };
    Ok(state)
}

async fn handle_websocket_request(
    headers: axum::http::header::HeaderMap,
    State(engine_state): State<EngineState>,
    ws: axum::extract::ws::WebSocketUpgrade,
) -> impl IntoResponse {
    // Create the context for the websocket server
    let context = graphql_ws::Context {
        http_context: engine_state.http_context,
        project_id: None, // project_id is not needed for OSS v3-engine.
        expose_internal_errors: engine_state.expose_internal_errors,
        schema: engine_state.graphql_state,
        auth_config: engine_state.auth_config,
        plugin_configs: engine_state.plugin_configs,
    };

    engine_state
        .graphql_websocket_server
        .upgrade_and_handle_websocket(ws, &headers, context)
}
