use crate::stages::graphql_config;
use crate::types::subgraph::Qualified;
use open_dds::types::CustomTypeName;

#[derive(Debug, thiserror::Error)]
pub enum ScalarTypesError {
    #[error("{0}")]
    GraphqlConfigError(#[from] graphql_config::GraphqlConfigError),
    #[error("the following type is defined more than once: {name:}")]
    DuplicateTypeDefinition { name: Qualified<CustomTypeName> },
}
