use std::{collections::BTreeMap, ops::Deref};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    arguments::{ArgumentDefinition, ArgumentName},
    data_connector::DataConnectorName,
    identifier::Identifier,
    impl_JsonSchema_with_OpenDd_for,
    types::{Deprecated, GraphQlFieldName, TypeReference},
};

/// The name of a command.
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    Hash,
    derive_more::Display,
    opendds_derive::OpenDd,
)]
pub struct CommandName(pub Identifier);

impl_JsonSchema_with_OpenDd_for!(CommandName);

/// The name of a function backing the command.
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
    JsonSchema,
)]
pub struct FunctionName(pub String);

/// The name of a procedure backing the command.
#[derive(
    Serialize,
    Deserialize,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
    JsonSchema,
)]
pub struct ProcedureName(pub String);

#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, JsonSchema, opendds_derive::OpenDd,
)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "DataConnectorCommand")]
pub enum DataConnectorCommand {
    #[schemars(title = "Function")]
    Function(FunctionName),
    #[schemars(title = "Procedure")]
    Procedure(ProcedureName),
}

/// The definition of a command.
/// A command is a user-defined operation which can take arguments and returns an output.
/// The semantics of a command are opaque to the Open DD specification.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[opendd(
    as_versioned_with_definition,
    json_schema(title = "Command", example = "Command::example")
)]
pub enum Command {
    V1(CommandV1),
}

impl Command {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "kind": "Command",
            "version": "v1",
            "definition": {
                "name": "get_latest_article",
                "outputType": "commandArticle",
                "arguments": [],
                "source": {
                    "dataConnectorName": "data_connector",
                    "dataConnectorCommand": {
                        "function": "latest_article"
                    },
                    "argumentMapping": {}
                },
                "graphql": {
                    "rootFieldName": "getLatestArticle",
                    "rootFieldKind": "Query"
                },
                "description": "Get the latest article",
            }
        })
    }

    pub fn upgrade(self) -> CommandV1 {
        match self {
            Command::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "CommandV1"))]
/// Definition of an OpenDD Command, which is a custom operation that can take arguments and
/// returns an output. The semantics of a command are opaque to OpenDD.
pub struct CommandV1 {
    /// The name of the command.
    pub name: CommandName,
    /// The return type of the command.
    pub output_type: TypeReference,
    /// The list of arguments accepted by this command. Defaults to no arguments.
    #[opendd(default, json_schema(default_exp = "serde_json::json!([])"))]
    pub arguments: Vec<ArgumentDefinition>,
    /// The source configuration for this command.
    pub source: Option<CommandSource>,
    /// Configuration for how this command should appear in the GraphQL schema.
    pub graphql: Option<CommandGraphQlDefinition>,
    /// The description of the command.
    /// Gets added to the description of the command's root field in the GraphQL schema.
    pub description: Option<String>,
}

#[derive(Default, Serialize, opendds_derive::OpenDd, Clone, Debug, PartialEq)]
/// Mapping of a comand or model argument name to the corresponding argument name used in the data connector.
/// The key of this object is the argument name used in the command or model and the value
/// is the argument name used in the data connector.
// We wrap maps into newtype structs so that we have a type and title for them in the JSONSchema which
// makes it easier to auto-generate documentation.
pub struct ArgumentMapping(pub BTreeMap<ArgumentName, String>);

impl Deref for ArgumentMapping {
    type Target = BTreeMap<ArgumentName, String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(title = "CommandSource", example = "CommandSource::example"))]
/// Description of how a command maps to a particular data connector
pub struct CommandSource {
    /// The name of the data connector backing this command.
    pub data_connector_name: DataConnectorName,

    /// The function/procedure in the data connector that backs this command.
    pub data_connector_command: DataConnectorCommand,

    /// Mapping from command argument names to data connector function or procedure argument names.
    #[opendd(default)]
    pub argument_mapping: ArgumentMapping,
}

impl CommandSource {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "dataConnectorName": "data_connector",
            "dataConnectorCommand": {
                "function": "latest_article"
            },
            "argumentMapping": {}
        })
    }
}

#[derive(
    Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema, opendds_derive::OpenDd,
)]
#[schemars(title = "GraphQlRootFieldKind")]
pub enum GraphQlRootFieldKind {
    Query,
    Mutation,
}

#[derive(Serialize, Clone, Debug, PartialEq, Eq, opendds_derive::OpenDd)]
#[serde(rename_all = "camelCase")]
#[opendd(json_schema(
    title = "CommandGraphQlDefinition",
    example = "CommandGraphQlDefinition::example"
))]
/// The definition of how a command should appear in the GraphQL API.
pub struct CommandGraphQlDefinition {
    /// The name of the graphql root field to use for this command.
    pub root_field_name: GraphQlFieldName,
    /// Whether to put this command in the Query or Mutation root of the GraphQL API.
    pub root_field_kind: GraphQlRootFieldKind,
    /// Whether this command root field is deprecated.
    /// If set, this will be added to the graphql schema as a deprecated field.
    pub deprecated: Option<Deprecated>,
}

impl CommandGraphQlDefinition {
    fn example() -> serde_json::Value {
        serde_json::json!({
            "rootFieldName": "getLatestArticle",
            "rootFieldKind": "Query"
        })
    }
}
