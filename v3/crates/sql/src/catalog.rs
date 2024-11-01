use std::{any::Any, sync::Arc};

use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved};
use model::WithSession;
use serde::{Deserialize, Serialize};
use types::TypeRegistry;

use crate::execute::optimizer;

mod datafusion {
    pub(super) use datafusion::{
        catalog::{CatalogProvider, SchemaProvider},
        execution::{session_state::SessionStateBuilder, SessionStateDefaults},
        prelude::{SessionConfig, SessionContext},
        scalar::ScalarValue,
    };
}

pub mod command;
pub mod introspection;
pub mod mem_table;
pub mod model;
pub mod subgraph;
pub mod types;

/// The context in which to compile and execute SQL queries.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Catalog {
    pub(crate) subgraphs: IndexMap<String, Arc<subgraph::Subgraph>>,
    pub(crate) table_valued_functions: IndexMap<String, Arc<command::Command>>,
    pub(crate) introspection: Arc<introspection::IntrospectionSchemaProvider>,
    pub(crate) default_schema: Option<String>,
}

impl Catalog {
    /// Create a no-op Catalog, used when `sql` layer is disabled
    pub fn empty() -> Self {
        Catalog {
            subgraphs: IndexMap::default(),
            table_valued_functions: IndexMap::default(),
            introspection: Arc::new(introspection::IntrospectionSchemaProvider::new(
                &introspection::Introspection::default(),
            )),
            default_schema: None,
        }
    }
    /// Derive a SQL Context from resolved Open DDS metadata.
    pub fn from_metadata(metadata: &Arc<resolved::Metadata>) -> Self {
        let type_registry = TypeRegistry::build_type_registry(metadata);
        // process models
        let mut subgraphs = IndexMap::new();
        let mut unsupported_models = IndexMap::new();
        for (model_name, model) in &metadata.models {
            match model::Model::from_resolved_model(&type_registry, model) {
                Ok(table) => {
                    let schema_name = &model_name.subgraph;
                    let table_name = &model_name.name;
                    let subgraph = subgraphs.entry(schema_name.to_string()).or_insert_with(|| {
                        subgraph::Subgraph {
                            metadata: metadata.clone(),
                            tables: IndexMap::new(),
                        }
                    });
                    subgraph
                        .tables
                        .insert(table_name.to_string(), Arc::new(table));
                }
                Err(unsupported_model) => {
                    unsupported_models.insert(model_name.clone(), unsupported_model);
                }
            }
        }

        // derive default schema
        let default_schema = type_registry.default_schema().map(ToString::to_string);
        // process commands
        let mut table_valued_functions = IndexMap::new();
        let mut unsupported_commands = IndexMap::new();
        for (command_name, command) in &metadata.commands {
            match command::Command::from_resolved_command(&type_registry, command) {
                Ok(command) => {
                    let schema_name = command_name.subgraph.to_string();
                    let command_name = &command_name.name;
                    let table_valued_function_name =
                        if Some(&schema_name) == default_schema.as_ref() {
                            format!("{command_name}")
                        } else {
                            format!("{schema_name}_{command_name}")
                        };
                    table_valued_functions.insert(table_valued_function_name, Arc::new(command));
                }
                Err(unsupported_command) => {
                    unsupported_commands.insert(command_name.clone(), unsupported_command);
                }
            }
        }

        let introspection = introspection::IntrospectionSchemaProvider::new(
            &introspection::Introspection::from_metadata(
                metadata,
                &type_registry,
                &subgraphs,
                &table_valued_functions,
                &unsupported_models,
                &unsupported_commands,
            ),
        );

        Catalog {
            subgraphs: subgraphs
                .into_iter()
                .map(|(k, v)| (k, Arc::new(v)))
                .collect(),
            table_valued_functions,
            introspection: Arc::new(introspection),
            default_schema,
        }
    }
}

impl datafusion::CatalogProvider for model::WithSession<Catalog> {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn schema_names(&self) -> Vec<String> {
        let mut schema_names: Vec<String> = self.value.subgraphs.keys().cloned().collect();
        schema_names.push(introspection::HASURA_METADATA_SCHEMA.to_string());
        schema_names
    }

    fn schema(&self, name: &str) -> Option<Arc<dyn datafusion::SchemaProvider>> {
        let subgraph_provider = self.value.subgraphs.get(name).cloned().map(|schema| {
            Arc::new(model::WithSession {
                value: schema,
                session: self.session.clone(),
            }) as Arc<dyn datafusion::SchemaProvider>
        });
        if subgraph_provider.is_none() && name == introspection::HASURA_METADATA_SCHEMA {
            Some(self.value.introspection.clone() as Arc<dyn datafusion::SchemaProvider>)
        } else {
            subgraph_provider
        }
    }
}

impl Catalog {
    pub fn create_session_context(
        self: Arc<Self>,
        metadata: Arc<resolved::Metadata>,
        request_headers: &Arc<reqwest::header::HeaderMap>,
        session: &Arc<Session>,
        http_context: &Arc<execute::HttpContext>,
    ) -> datafusion::SessionContext {
        let session_config = datafusion::SessionConfig::new()
            .set(
                "datafusion.catalog.default_catalog",
                datafusion::ScalarValue::Utf8(Some("default".to_string())),
            )
            .set(
                "datafusion.catalog.information_schema",
                datafusion::ScalarValue::Boolean(Some(true)),
            )
            .set(
                "datafusion.execution.target_partitions",
                datafusion::ScalarValue::Int32(Some(1)),
            )
            .set(
                "datafusion.execution.planning_concurrency",
                datafusion::ScalarValue::Int32(Some(1)),
            )
            .set(
                "datafusion.sql_parser.enable_ident_normalization",
                datafusion::ScalarValue::Boolean(Some(false)),
            );

        let session_config = if let Some(default_schema_name) = &self.default_schema {
            session_config.set(
                "datafusion.catalog.default_schema",
                datafusion::ScalarValue::Utf8(Some(default_schema_name.clone())),
            )
        } else {
            session_config
        };
        let query_planner = Arc::new(super::execute::planner::OpenDDQueryPlanner {
            session: session.clone(),
            http_context: http_context.clone(),
            request_headers: request_headers.clone(),
            metadata,
        });
        let session_state = datafusion::SessionStateBuilder::new()
            .with_config(session_config)
            .with_query_planner(query_planner)
            .with_optimizer_rule(Arc::new(optimizer::ReplaceTableScan {}))
            .with_optimizer_rule(Arc::new(optimizer::NDCPushDownSort {}))
            .with_optimizer_rule(Arc::new(optimizer::NDCPushDownLimit {}))
            .with_optimizer_rule(Arc::new(optimizer::NDCPushDownAggregate {}))
            .with_expr_planners(datafusion::SessionStateDefaults::default_expr_planners())
            .with_scalar_functions(datafusion::SessionStateDefaults::default_scalar_functions())
            .with_aggregate_functions(
                datafusion::SessionStateDefaults::default_aggregate_functions(),
            )
            .build();
        let session_context = datafusion::SessionContext::new_with_state(session_state);
        for (function_name, function) in &self.table_valued_functions {
            let function_impl = WithSession {
                session: session.clone(),
                value: function.clone(),
            };
            session_context.register_udtf(function_name, Arc::new(function_impl));
        }
        session_context.register_catalog(
            "default",
            Arc::new(model::WithSession {
                session: session.clone(),
                value: self,
            }) as Arc<dyn datafusion::CatalogProvider>,
        );
        session_context
    }
}
