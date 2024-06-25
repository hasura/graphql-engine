use async_trait::async_trait;
use std::{any::Any, sync::Arc};

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

mod df {
    pub(super) use datafusion::error::Result;
    pub(super) use datafusion::{catalog::schema::SchemaProvider, datasource::TableProvider};
}

use crate::catalog;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Subgraph {
    pub models: IndexMap<String, catalog::table::Model>,
}

pub struct OpenDDSchemaProvider {
    pub(crate) tables: IndexMap<String, Arc<catalog::table::OpenDDTableProvider>>,
}

#[async_trait]
impl df::SchemaProvider for OpenDDSchemaProvider {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn table_names(&self) -> Vec<String> {
        self.tables.keys().cloned().collect::<Vec<_>>()
    }

    async fn table(&self, name: &str) -> df::Result<Option<Arc<dyn df::TableProvider>>> {
        Ok(self
            .tables
            .get(name)
            .cloned()
            .map(|table| table as Arc<dyn df::TableProvider>))
    }

    fn table_exist(&self, name: &str) -> bool {
        self.tables.contains_key(name)
    }
}
