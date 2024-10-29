use hasura_authn::AuthConfig;
use metadata_resolve::LifecyclePluginConfigs;
use std::sync::Arc;

use execute::HttpContext;
use graphql_schema::GDS;
use lang_graphql as gql;
use tracing_util::{ErrorVisibility, TraceableError};

#[derive(Clone)] // Cheap to clone as heavy fields are wrapped in `Arc`
pub struct EngineState {
    pub expose_internal_errors: execute::ExposeInternalErrors,
    pub http_context: HttpContext,
    pub graphql_state: Arc<gql::schema::Schema<GDS>>,
    pub resolved_metadata: Arc<metadata_resolve::Metadata>,
    pub jsonapi_catalog: Arc<jsonapi::Catalog>,
    pub auth_config: Arc<AuthConfig>,
    pub sql_context: Arc<sql::catalog::Catalog>,
    pub plugin_configs: Arc<LifecyclePluginConfigs>,
    pub graphql_websocket_server:
        Arc<graphql_ws::WebSocketServer<graphql_ws::NoOpWebSocketMetrics>>,
}

#[derive(thiserror::Error, Debug)]
#[allow(clippy::enum_variant_names)]
pub enum StartupError {
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
