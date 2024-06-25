use std::{any::Any, collections::HashMap, sync::Arc};

use ::datafusion::{
    execution::{context::SessionState, runtime_env::RuntimeEnv},
    sql::TableReference,
};
use async_trait::async_trait;
use hasura_authn_core::Session;
use indexmap::IndexMap;
use metadata_resolve::{self as resolved};
use open_dds::permissions::Role;
use schema::OpenDDSchemaProvider;
use serde::{Deserialize, Serialize};

mod datafusion {
    pub(super) use datafusion::{
        catalog::{schema::SchemaProvider, CatalogProvider},
        datasource::TableProvider,
        error::Result,
        prelude::{SessionConfig, SessionContext},
        scalar::ScalarValue,
    };
}

pub mod introspection;
pub mod schema;
pub mod table;

/// The context in which to compile and execute SQL queries.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Context {
    pub(crate) subgraphs: IndexMap<String, schema::Subgraph>,
    pub(crate) type_permissions: HashMap<Role, Arc<table::TypePermissionsOfRole>>,
    pub(crate) introspection: introspection::Introspection,
}

impl Context {
    /// Derive a SQL Context from resolved Open DDS metadata.
    pub fn from_metadata(metadata: &resolved::Metadata) -> Self {
        let mut subgraphs = IndexMap::new();
        for (model_name, model) in &metadata.models {
            let schema_name = &model_name.subgraph;
            let table_name = &model_name.name;
            let subgraph =
                subgraphs
                    .entry(schema_name.clone())
                    .or_insert_with(|| schema::Subgraph {
                        models: IndexMap::new(),
                    });
            subgraph.models.insert(
                table_name.to_string(),
                table::Model::from_resolved_model(model),
            );
        }

        let mut type_permissions = HashMap::new();
        for (type_name, object_type) in &metadata.object_types {
            for (role, output_permission) in &object_type.type_output_permissions {
                let output_permission = table::TypePermission {
                    output: output_permission.clone(),
                };
                let role_permissions =
                    type_permissions
                        .entry(role)
                        .or_insert_with(|| table::TypePermissionsOfRole {
                            permissions: HashMap::new(),
                        });
                role_permissions
                    .permissions
                    .insert(type_name.clone(), output_permission);
            }
        }
        let introspection = introspection::Introspection::from_metadata(metadata, &subgraphs);
        Context {
            subgraphs,
            type_permissions: type_permissions
                .into_iter()
                .map(|(role, role_permissions)| (role.clone(), Arc::new(role_permissions)))
                .collect(),
            introspection,
        }
    }
}

pub struct OpenDDCatalogProvider {
    schemas: IndexMap<String, Arc<HasuraSchemaProvider>>,
}

impl OpenDDCatalogProvider {
    fn new(
        session: &Arc<Session>,
        http_context: &Arc<execute::HttpContext>,
        context: &Context,
    ) -> Self {
        let type_permissions = context.type_permissions.get(&session.role).cloned();
        let mut schemas = IndexMap::new();
        for (subgraph_name, subgraph) in &context.subgraphs {
            let mut tables = IndexMap::new();
            for model in subgraph.models.values() {
                let select_permission = model.permissions.get(&session.role).cloned();
                let provider = table::OpenDDTableProvider {
                    session: session.clone(),
                    http_context: http_context.clone(),
                    name: model.name.clone(),
                    data_type: model.data_type.clone(),
                    source: model.source.clone(),
                    schema: model.schema.clone(),
                    select_permission,
                    type_permissions: type_permissions.clone(),
                };
                tables.insert(model.name.to_string(), Arc::new(provider));
            }
            let provider = HasuraSchemaProvider::OpenDD(schema::OpenDDSchemaProvider { tables });
            schemas.insert(subgraph_name.clone(), Arc::new(provider));
        }
        schemas.insert(
            introspection::HASURA_METADATA_SCHEMA.to_string(),
            Arc::new(HasuraSchemaProvider::Introspection(
                introspection::IntrospectionSchemaProvider::new(&context.introspection),
            )),
        );
        OpenDDCatalogProvider { schemas }
    }
    pub(crate) fn get(
        &self,
        default_schema: Option<&str>,
        table: &TableReference,
    ) -> Option<&table::OpenDDTableProvider> {
        let schema = table.schema().or(default_schema);
        let table = table.table();
        if let Some(schema) = schema {
            if let HasuraSchemaProvider::OpenDD(schema) = self.schemas.get(schema)?.as_ref() {
                schema.tables.get(table).map(std::convert::AsRef::as_ref)
            } else {
                None
            }
        } else {
            None
        }
    }
}

enum HasuraSchemaProvider {
    OpenDD(OpenDDSchemaProvider),
    Introspection(introspection::IntrospectionSchemaProvider),
}

#[async_trait]
impl datafusion::SchemaProvider for HasuraSchemaProvider {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn table_names(&self) -> Vec<String> {
        match self {
            HasuraSchemaProvider::OpenDD(schema) => schema.table_names(),
            HasuraSchemaProvider::Introspection(schema) => schema.table_names(),
        }
    }

    async fn table(
        &self,
        name: &str,
    ) -> datafusion::Result<Option<Arc<dyn datafusion::TableProvider>>> {
        match self {
            HasuraSchemaProvider::OpenDD(schema) => schema.table(name).await,
            HasuraSchemaProvider::Introspection(schema) => schema.table(name).await,
        }
    }

    fn table_exist(&self, name: &str) -> bool {
        match self {
            HasuraSchemaProvider::OpenDD(schema) => schema.table_exist(name),
            HasuraSchemaProvider::Introspection(schema) => schema.table_exist(name),
        }
    }
}

impl datafusion::CatalogProvider for OpenDDCatalogProvider {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn schema_names(&self) -> Vec<String> {
        self.schemas.keys().cloned().collect()
    }

    fn schema(&self, name: &str) -> Option<Arc<dyn datafusion::SchemaProvider>> {
        self.schemas
            .get(name)
            .cloned()
            .map(|schema| schema as Arc<dyn datafusion::SchemaProvider>)
    }
}

impl Context {
    pub fn create_session_context(
        &self,
        session: &Arc<Session>,
        http_context: &Arc<execute::HttpContext>,
    ) -> datafusion::SessionContext {
        let default_schema_name = if self.subgraphs.len() == 1 {
            self.subgraphs.get_index(0).map(|v| v.0)
        } else {
            None
        };
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

        let session_config = if let Some(default_schema_name) = default_schema_name {
            session_config.set(
                "datafusion.catalog.default_schema",
                datafusion::ScalarValue::Utf8(Some(default_schema_name.clone())),
            )
        } else {
            session_config
        };
        let catalog = Arc::new(OpenDDCatalogProvider::new(session, http_context, self));
        let query_planner = Arc::new(super::execute::planner::NDCQueryPlanner {
            default_schema: default_schema_name.map(|s| Arc::new(s.clone())),
            catalog: catalog.clone(),
        });
        let session_state =
            SessionState::new_with_config_rt(session_config, Arc::new(RuntimeEnv::default()))
                .with_analyzer_rules(vec![Arc::new(
                    super::execute::analyzer::ReplaceTableScan::new(
                        default_schema_name.map(|s| Arc::new(s.clone())),
                        catalog.clone(),
                    ),
                )])
                .with_query_planner(query_planner)
                .add_optimizer_rule(Arc::new(
                    super::execute::optimizer::NDCPushDownProjection {},
                ));
        let session_context = datafusion::SessionContext::new_with_state(session_state);
        session_context
            .register_catalog("default", catalog as Arc<dyn datafusion::CatalogProvider>);
        session_context
    }
}
