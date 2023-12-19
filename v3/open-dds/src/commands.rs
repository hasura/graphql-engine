use std::collections::HashMap;

use indexmap::IndexMap;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{
    arguments::{ArgumentDefinition, ArgumentName},
    data_connector::DataConnectorName,
    types::{CustomTypeName, FieldName, GraphQlFieldName, TypeReference},
};

/// The name of a command.
#[derive(
    Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display, JsonSchema,
)]
pub struct CommandName(pub String);

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(title = "DataConnectorCommand")]
pub enum DataConnectorCommand {
    #[schemars(title = "Function")]
    Function(String),
    #[schemars(title = "Procedure")]
    Procedure(String),
}

/// The definition of a command.
/// A command is a user-defined operation which can take arguments and returns an output.
/// The semantics of a command are opaque to the Open DD specification.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(tag = "version", content = "definition")]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "Command")]
pub enum Command {
    V1(CommandV1),
}

impl Command {
    pub fn upgrade(self) -> CommandV1 {
        match self {
            Command::V1(v1) => v1,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "CommandV1")]
#[schemars(example = "CommandV1::example")]
/// Definition of an OpenDD Command, which is a custom operation that can take arguments and
/// returns an output. The semantics of a command are opaque to OpenDD.
pub struct CommandV1 {
    /// The name of the command.
    pub name: CommandName,
    /// The return type of the command.
    pub output_type: TypeReference,
    #[serde(default)]
    /// The list of arguments accepted by this command. Defaults to no arguments.
    pub arguments: Vec<ArgumentDefinition>,
    /// The source configuration for this command.
    pub source: Option<CommandSource>,
    /// Configuration for how this command should appear in the GraphQL schema.
    pub graphql: Option<CommandGraphQlDefinition>,
}

impl CommandV1 {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "name": "get_latest_article",
                "arguments": [],
                "outputType": "commandArticle",
                "source": {
                  "dataConnectorName": "data_connector",
                  "dataConnectorCommand": {
                    "function": "latest_article"
                  },
                  "typeMapping": {
                    "commandArticle": {
                      "fieldMapping": {
                        "article_id": {
                          "column": "id"
                        }
                      }
                    }
                  }
                },
                "graphql": {
                  "rootFieldName": "getLatestArticle",
                  "rootFieldKind": "Query"
                }
            }
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "CommandSource")]
#[schemars(example = "CommandSource::example")]
/// Description of how a command maps to a particular data connector
pub struct CommandSource {
    /// The name of the data connector backing this command.
    pub data_connector_name: DataConnectorName,

    /// The function/procedure in the data connector that backs this command.
    pub data_connector_command: DataConnectorCommand,

    /// How the various types used in this command correspond to
    /// entities in the data connector.
    #[serde(default)]
    pub type_mapping: HashMap<CustomTypeName, TypeMapping>,

    /// Mapping from command argument names to data connector table argument names.
    #[serde(default)]
    pub argument_mapping: HashMap<ArgumentName, String>,
}

impl CommandSource {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "dataConnectorName": "data_connector",
                "dataConnectorCommand": {
                  "function": "latest_article"
                }
            }
        "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Eq, PartialEq, JsonSchema)]
#[schemars(title = "GraphQlRootFieldKind")]
pub enum GraphQlRootFieldKind {
    Query,
    Mutation,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema, Eq)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "CommandGraphQlDefinition")]
#[schemars(example = "CommandGraphQlDefinition::example")]
/// The definition of how a command should appear in the GraphQL API.
pub struct CommandGraphQlDefinition {
    /// The name of the graphql root field to use for this command.
    pub root_field_name: GraphQlFieldName,
    /// Whether to put this command in the Query or Mutation root of the GraphQL API.
    pub root_field_kind: GraphQlRootFieldKind,
}

impl CommandGraphQlDefinition {
    fn example() -> Self {
        serde_json::from_str(
            r#"
            {
                "rootFieldName": "getLatestArticle",
                "rootFieldKind": "Query"
            }
      "#,
        )
        .unwrap()
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "TypeMapping")]
pub struct TypeMapping {
    pub field_mapping: IndexMap<FieldName, FieldMapping>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, JsonSchema)]
#[serde(rename_all = "camelCase")]
#[serde(deny_unknown_fields)]
#[schemars(title = "ObjectFieldMapping")]
pub struct FieldMapping {
    pub column: String,
    // TODO: Map field arguments
}
