use crate::metadata::resolved::stages::{data_connector_type_mappings, data_connectors};
use crate::metadata::resolved::subgraph::{
    deserialize_qualified_btreemap, serialize_qualified_btreemap, ArgumentInfo, Qualified,
    QualifiedTypeReference,
};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::arguments::ArgumentName;
use open_dds::commands::{CommandName, DataConnectorCommand, GraphQlRootFieldKind};

use open_dds::permissions::Role;
use open_dds::types::{CustomTypeName, Deprecated};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

use crate::metadata::resolved::permission::ValueExpression;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandGraphQlApi {
    pub root_field_kind: GraphQlRootFieldKind,
    pub root_field_name: ast::Name,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandSource {
    pub data_connector: data_connectors::DataConnectorLink,
    pub source: DataConnectorCommand,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub type_mappings:
        BTreeMap<Qualified<CustomTypeName>, data_connector_type_mappings::TypeMapping>,
    pub argument_mappings: HashMap<ArgumentName, String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct Command {
    pub name: Qualified<CommandName>,
    pub output_type: QualifiedTypeReference,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub graphql_api: Option<CommandGraphQlApi>,
    pub source: Option<CommandSource>,
    pub permissions: Option<HashMap<Role, CommandPermission>>,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandPermission {
    pub allow_execution: bool,
    pub argument_presets: BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpression)>,
}
