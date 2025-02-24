pub mod build;
mod cors;
pub mod internal_flags;
mod middleware;

mod routes;
mod state;
mod types;
pub use cors::build_cors_layer;
pub use middleware::{
    authentication_middleware, explain_request_tracing_middleware,
    graphql_request_tracing_middleware, plugins_middleware,
};
pub use routes::{get_base_routes, get_cors_layer, get_jsonapi_route, get_metadata_routes};
pub use state::{build_state, print_warnings, resolve_metadata};
pub use types::{EngineState, RequestType, StartupError};

// This is set by the build.rs script.
/// The version of the v3-engine release.
pub static VERSION: &str = env!(
    "CARGO_V3_ENGINE_VERSION",
    "Unable to start engine: unable to fetch the current git hash to use as version in traces"
);
