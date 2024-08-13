use std::{any::Any, sync::Arc};

use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved};
use serde::{Deserialize, Serialize};

mod datafusion {
    pub(super) use datafusion::{
        catalog::{CatalogProvider, SchemaProvider},
        execution::{session_state::SessionStateBuilder, SessionStateDefaults},
        prelude::{SessionConfig, SessionContext},
        scalar::ScalarValue,
    };
}

pub mod introspection;
pub mod mem_table;
pub mod model;
pub mod subgraph;

/// The context in which to compile and execute SQL queries.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Catalog {
    pub(crate) metadata: Arc<resolved::Metadata>,
    pub(crate) subgraphs: IndexMap<String, Arc<subgraph::Subgraph>>,
    pub(crate) introspection: Arc<introspection::IntrospectionSchemaProvider>,
    pub(crate) default_schema: Option<String>,
}

impl Catalog {
    /// Derive a SQL Context from resolved Open DDS metadata.
    pub fn from_metadata(metadata: Arc<resolved::Metadata>) -> Self {
        let mut subgraphs = IndexMap::new();
        for (model_name, model) in &metadata.models {
            let schema_name = &model_name.subgraph;
            let table_name = &model_name.name;
            let subgraph =
                subgraphs
                    .entry(schema_name.to_string())
                    .or_insert_with(|| subgraph::Subgraph {
                        tables: IndexMap::new(),
                    });
            subgraph.tables.insert(
                table_name.to_string(),
                Arc::new(model::Model::from_resolved_model(&metadata, model)),
            );
        }

        let introspection = introspection::IntrospectionSchemaProvider::new(
            &introspection::Introspection::from_metadata(&metadata, &subgraphs),
        );

        let default_schema = if subgraphs.len() == 1 {
            subgraphs.get_index(0).map(|v| v.0.clone())
        } else {
            None
        };
        Catalog {
            metadata,
            subgraphs: subgraphs
                .into_iter()
                .map(|(k, v)| (k, Arc::new(v)))
                .collect(),
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
            catalog: self.clone(),
            session: session.clone(),
            http_context: http_context.clone(),
        });
        let session_state = datafusion::SessionStateBuilder::new()
            .with_config(session_config)
            .with_query_planner(query_planner)
            .with_optimizer_rule(Arc::new(super::execute::optimizer::ReplaceTableScan {}))
            .with_expr_planners(datafusion::SessionStateDefaults::default_expr_planners())
            .with_scalar_functions(datafusion::SessionStateDefaults::default_scalar_functions())
            .with_aggregate_functions(
                datafusion::SessionStateDefaults::default_aggregate_functions(),
            )
            .build();
        let session_context = datafusion::SessionContext::new_with_state(session_state);
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
