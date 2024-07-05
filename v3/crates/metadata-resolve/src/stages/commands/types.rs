use crate::stages::{data_connectors, object_types};
use crate::types::subgraph::{
    deserialize_qualified_btreemap, serialize_qualified_btreemap, ArgumentInfo, Qualified,
    QualifiedTypeReference,
};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::arguments::ArgumentName;
use open_dds::commands::{CommandName, DataConnectorCommand, GraphQlRootFieldKind};

use open_dds::types::{CustomTypeName, DataConnectorArgumentName, Deprecated};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandGraphQlApi {
    pub root_field_kind: GraphQlRootFieldKind,
    pub root_field_name: ast::Name,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CommandSource {
    pub data_connector: data_connectors::DataConnectorLink,
    pub source: DataConnectorCommand,
    // Is the output type of this command in OpenDD and NDC same. This can be
    // different in the case when `CommandsResponseConfig` is set
    pub ndc_type_opendd_type_same: bool,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub type_mappings: BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    pub argument_mappings: BTreeMap<ArgumentName, DataConnectorArgumentName>,
    pub source_arguments: BTreeMap<DataConnectorArgumentName, ndc_models::Type>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Command {
    pub name: Qualified<CommandName>,
    pub output_type: QualifiedTypeReference,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub graphql_api: Option<CommandGraphQlApi>,
    pub source: Option<CommandSource>,
    pub description: Option<String>,
}
