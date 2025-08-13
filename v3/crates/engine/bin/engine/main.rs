use clap::Parser;
use engine::{
    StartupError, VERSION, get_base_routes, get_cors_layer, get_jsonapi_route, get_metadata_routes,
    internal_flags::{UnstableFeature, resolve_unstable_features},
};
use engine_types::ExposeInternalErrors;
use serde::Serialize;
use std::net;
use std::path::PathBuf;
use tower_http::compression::CompressionLayer;
use tracing_util::{SpanVisibility, add_event_on_active_span, set_attribute_on_active_span};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

static MB: usize = 1_048_576;

static DEFAULT_OTEL_SERVICE_NAME: &str = "ddn-engine";
const DEFAULT_PORT: u16 = 3000;

#[allow(clippy::struct_excessive_bools)] // booleans are pretty useful here
#[derive(Parser, Serialize)]
#[command(version = VERSION)]
struct ServerOptions {
    /// The path to the OpenDD metadata file, used to construct the schema.
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

    /// Service name output in OpenTelemetry traces
    #[arg(long, env = "OTEL_SERVICE_NAME")]
    otel_service_name: Option<String>,

    /// The name of the header used to specify the auth mode when using alternative Auth Modes.
    /// Defaults to "X-Hasura-Auth-Mode" if not specified.
    #[arg(long, env = "AUTH_MODE_HEADER", default_value = "X-Hasura-Auth-Mode")]
    auth_mode_header: String,

    /// Maximum size of response for NDC responses in bytes
    #[arg(long, value_name = "NDC_RESPONSE_SIZE_LIMIT in bytes", env = "NDC_RESPONSE_SIZE_LIMIT", default_value_t = 30 * MB)]
    ndc_response_size_limit: usize,
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

    let otel_service_name = match &server_options.otel_service_name {
        Some(otel_service_name) => otel_service_name,
        None => DEFAULT_OTEL_SERVICE_NAME,
    };

    tracing_util::initialize_tracing(
        server_options.otlp_endpoint.as_deref(),
        otel_service_name.to_string(),
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

#[allow(clippy::print_stdout)]
async fn start_engine(server: &ServerOptions) -> Result<(), StartupError> {
    let metadata_resolve_configuration = metadata_resolve::configuration::Configuration {
        unstable_features: resolve_unstable_features(&server.unstable_features),
    };

    let expose_internal_errors = if server.expose_internal_errors {
        ExposeInternalErrors::Expose
    } else {
        ExposeInternalErrors::Censor
    };

    let raw_auth_config =
        std::fs::read_to_string(&server.authn_config_path).expect("could not read auth config");
    let opendd_metadata_json =
        std::fs::read_to_string(&server.metadata_path).expect("could not read metadata");

    let (resolved_metadata, auth_config) = engine::resolve_metadata(
        &opendd_metadata_json,
        &raw_auth_config,
        &metadata_resolve_configuration,
    )
    .map_err(StartupError::ReadSchema)?;

    let state = engine::build_state(
        expose_internal_errors,
        auth_config,
        resolved_metadata,
        server.auth_mode_header.clone(),
        server.ndc_response_size_limit,
    )
    .map_err(StartupError::ReadSchema)?;

    let mut app = get_base_routes(state.clone());

    app = app.merge(get_jsonapi_route(state.clone()));

    // If `--introspection-metadata` is specified we also serve the file indicated on `/metadata`
    // and its hash on `/metadata-hash`.
    if let Some(path) = &server.introspection_metadata {
        app = app.merge(get_metadata_routes(path).await?);
    }

    // If `--enable-cors` is specified, we add a CORS layer to the app.
    if server.enable_cors {
        app = app.layer(get_cors_layer(&server.cors_allow_origin));
    }

    // Add compression layer to support zstd and gzip response compression
    // Use fastest compression level (1) based on experiments in crates/cloud/build-artifacts/src/encode.rs
    // which showed level 1 provides good compression with minimal performance impact
    // NOTE: Fastest can't be used here, see: https://github.com/tower-rs/tower-http/issues/590
    app = app.layer(CompressionLayer::new().quality(tower_http::CompressionLevel::Precise(1)));

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

    axum::serve(
        listener,
        app.into_make_service_with_connect_info::<net::SocketAddr>(),
    )
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
