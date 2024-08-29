use async_trait::async_trait;
use metadata_resolve::Metadata;
use std::{any::Any, sync::Arc};

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

mod datafusion {
    pub(super) use datafusion::error::Result;
    pub(super) use datafusion::{catalog::SchemaProvider, datasource::TableProvider};
}

use crate::catalog;

use super::model;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub(crate) struct Subgraph {
    pub metadata: Arc<Metadata>,
    pub tables: IndexMap<String, Arc<catalog::model::Model>>,
}

#[async_trait]
impl datafusion::SchemaProvider for catalog::model::WithSession<Subgraph> {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn table_names(&self) -> Vec<String> {
        self.value.tables.keys().cloned().collect::<Vec<_>>()
    }

    async fn table(
        &self,
        name: &str,
    ) -> datafusion::Result<Option<Arc<dyn datafusion::TableProvider>>> {
        if let Some(model) = self.value.tables.get(name) {
            let table = model::Table::new_no_args(self.value.metadata.clone(), model.clone());
            Ok(Some(Arc::new(table) as Arc<dyn datafusion::TableProvider>))
        } else {
            Ok(None)
        }
    }

    fn table_exist(&self, name: &str) -> bool {
        self.value.tables.contains_key(name)
    }
}
