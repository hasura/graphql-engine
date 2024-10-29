mod sql;
pub use sql::handle_sql_request;
mod graphql;
pub use graphql::{handle_explain_request, handle_request, handle_websocket_request};
mod jsonapi;
pub use jsonapi::create_json_api_router;

use axum::{
    extract::DefaultBodyLimit,
    response::Html,
    routing::{get, post},
    Router,
};
use base64::engine::Engine;
use std::hash;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use tower_http::cors::CorsLayer;
use tower_http::trace::TraceLayer;

use crate::{
    authentication_middleware, build_cors_layer, explain_request_tracing_middleware,
    graphql_request_tracing_middleware, plugins_middleware, sql_request_tracing_middleware,
    EngineState, StartupError,
};

const MB: usize = 1_048_576;

/// The main router for the engine.
pub struct EngineRouter {
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
    pub fn new(state: EngineState) -> Self {
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
    pub async fn add_metadata_routes(
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

    pub fn add_sql_route(&mut self, state: EngineState) {
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

    pub fn add_jsonapi_route(&mut self, state: EngineState) {
        let jsonapi_routes = create_json_api_router(state);
        self.jsonapi_routes = Some(jsonapi_routes);
    }

    pub fn add_cors_layer(&mut self, allow_origin: &[String]) {
        self.cors_layer = Some(build_cors_layer(allow_origin));
    }

    pub fn into_make_service(self) -> axum::routing::IntoMakeService<Router> {
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

/// Health check endpoint
async fn handle_health() -> reqwest::StatusCode {
    reqwest::StatusCode::OK
}

async fn graphiql() -> Html<&'static str> {
    Html(include_str!("index.html"))
}
