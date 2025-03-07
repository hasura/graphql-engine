use crate::{EngineState, StartupError};
use engine_types::{ExposeInternalErrors, HttpContext};
use graphql_ir::GraphqlRequestPipeline;
use std::fmt::Display;
use std::sync::Arc;

#[allow(clippy::print_stdout)]
/// Print any build warnings to stdout
pub fn print_warnings<T: Display>(warnings: Vec<T>) {
    for warning in warnings {
        println!("Warning: {warning}");
    }
}

pub fn resolve_metadata(
    opendd_metadata_json: &str,
    raw_auth_config: &str,
    metadata_resolve_configuration: &metadata_resolve::configuration::Configuration,
) -> Result<(metadata_resolve::Metadata, hasura_authn::ResolvedAuthConfig), anyhow::Error> {
    // Metadata
    let metadata = open_dds::Metadata::from_json_str(opendd_metadata_json)?;
    let flags = metadata.get_flags();

    // Auth Config
    let auth_config =
        hasura_authn::parse_auth_config(raw_auth_config).map_err(StartupError::ReadAuth)?;
    let (resolved_auth_config, auth_warnings) =
        hasura_authn::resolve_auth_config(auth_config, flags.as_ref())?;

    let (resolved_metadata, warnings) =
        metadata_resolve::resolve(metadata, metadata_resolve_configuration).map_err(|error| {
            match metadata_resolve::to_fancy_error(
                opendd_metadata_json,
                &error,
                ariadne::Config::new(),
            ) {
                Some(report) => {
                    report
                        .eprint(ariadne::Source::from(opendd_metadata_json))
                        .unwrap();

                    // return empty error to stop printing twice
                    anyhow::anyhow!("error building metadata")
                }
                None => anyhow::anyhow!(error),
            }
        })?;

    print_warnings(auth_warnings);
    print_warnings(warnings);

    Ok((resolved_metadata, resolved_auth_config))
}

/// Build the engine state - include auth, metadata, and jsonapi context.
pub fn build_state(
    request_pipeline: GraphqlRequestPipeline,
    expose_internal_errors: ExposeInternalErrors,
    auth_config: hasura_authn::ResolvedAuthConfig,
    resolved_metadata: metadata_resolve::Metadata,
) -> Result<EngineState, anyhow::Error> {
    // Metadata
    let resolved_metadata = Arc::new(resolved_metadata);

    let http_context = HttpContext {
        client: reqwest::Client::new(),
        ndc_response_size_limit: None,
    };
    let plugin_configs = resolved_metadata.plugin_configs.clone();

    let schema = graphql_schema::GDS {
        metadata: resolved_metadata.clone(),
    }
    .build_schema()?;

    let (jsonapi_catalog, _json_api_warnings) = jsonapi::Catalog::new(&resolved_metadata);

    let state = EngineState {
        request_pipeline,
        expose_internal_errors,
        http_context,
        graphql_state: Arc::new(schema),
        jsonapi_catalog: Arc::new(jsonapi_catalog),
        resolved_metadata,
        auth_config: Arc::new(auth_config),
        plugin_configs: Arc::new(plugin_configs),
        graphql_websocket_server: Arc::new(graphql_ws::WebSocketServer::new()),
    };
    Ok(state)
}
