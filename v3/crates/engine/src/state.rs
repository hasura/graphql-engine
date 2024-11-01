use crate::{EngineState, StartupError};
use hasura_authn::resolve_auth_config;
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;

use execute::HttpContext;

#[allow(clippy::print_stdout)]
/// Print any build warnings to stdout
fn print_warnings<T: Display>(warnings: Vec<T>) {
    for warning in warnings {
        println!("Warning: {warning}");
    }
}

/// Build the engine state - include auth, metadata, and sql context.
pub fn build_state(
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
        sql::catalog::Catalog::from_metadata(&resolved_metadata)
    } else {
        sql::catalog::Catalog::empty()
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
