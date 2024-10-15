use std::sync::Arc;

use crate::data_connectors::ArgumentPresetValue;
use crate::helpers::argument::ArgumentMappingIssue;
use crate::stages::{data_connectors, object_types};
use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::{
    deserialize_qualified_btreemap, serialize_qualified_btreemap, ArgumentInfo, Qualified,
    QualifiedTypeReference,
};

use indexmap::IndexMap;
use lang_graphql::ast::common as ast;
use open_dds::arguments::ArgumentName;
use open_dds::commands::{
    CommandName, DataConnectorCommand, FunctionName, GraphQlRootFieldKind, ProcedureName,
};
use open_dds::data_connector::DataConnectorName;
use open_dds::types::{CustomTypeName, DataConnectorArgumentName, Deprecated};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

pub struct CommandsOutput {
    pub commands: IndexMap<Qualified<CommandName>, Command>,
    pub issues: Vec<CommandsIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandGraphQlApi {
    pub root_field_kind: GraphQlRootFieldKind,
    pub root_field_name: ast::Name,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CommandSource {
    pub data_connector: Arc<data_connectors::DataConnectorLink>,
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
    pub data_connector_link_argument_presets:
        BTreeMap<DataConnectorArgumentName, ArgumentPresetValue>,
    pub source_arguments: BTreeMap<DataConnectorArgumentName, ndc_models::Type>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct Command {
    pub name: Qualified<CommandName>,
    pub output_type: QualifiedTypeReference,
    pub arguments: IndexMap<ArgumentName, ArgumentInfo>,
    pub graphql_api: Option<CommandGraphQlApi>,
    pub source: Option<Arc<CommandSource>>,
    pub description: Option<String>,
}

#[derive(Debug, thiserror::Error)]
pub enum CommandsIssue {
    #[error("An issue occurred while mapping arguments in the command {command_name:} to the function {function_name:} in the data connector {data_connector_name:}: {issue:}")]
    FunctionArgumentMappingIssue {
        data_connector_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        function_name: FunctionName,
        issue: ArgumentMappingIssue,
    },
    #[error("An issue occurred while mapping arguments in the command {command_name:} to the procedure {procedure_name:} in the data connector {data_connector_name:}: {issue:}")]
    ProcedureArgumentMappingIssue {
        data_connector_name: Qualified<DataConnectorName>,
        command_name: Qualified<CommandName>,
        procedure_name: ProcedureName,
        issue: ArgumentMappingIssue,
    },
    #[error("Cannot add command {command_name:} to GraphQL schema as the root field name {graphql_name:} is already in use")]
    GraphQlNameAlreadyInUse {
        command_name: Qualified<CommandName>,
        graphql_name: ast::Name,
    },
}

impl ShouldBeAnError for CommandsIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::Flags) -> bool {
        match self {
            CommandsIssue::GraphQlNameAlreadyInUse { .. } => {
                flags.require_unique_command_graphql_names
            }
            _ => false,
        }
    }
}
