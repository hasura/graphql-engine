use crate::types::subgraph::QualifiedTypeReference;
use crate::ValueExpressionOrPredicate;
use crate::{deserialize_non_string_key_btreemap, serialize_non_string_key_btreemap};
use open_dds::{data_connector::DataConnectorColumnName, types::DataConnectorArgumentName};
use std::collections::BTreeMap;
use std::fmt::Display;

use crate::stages::{command_permissions, commands, model_permissions, models, models_graphql};
use crate::types::subgraph::Qualified;
use hasura_authn_core::Role;
use indexmap::IndexMap;
use open_dds::{commands::CommandName, models::ModelName};
use serde::{Deserialize, Serialize};

// Model plus resolved argument presets
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelWithArgumentPresets {
    pub model: models::Model,
    pub argument_presets: BTreeMap<Role, ArgumentPresets>,
    pub select_permissions: BTreeMap<Role, model_permissions::SelectPermission>,
    pub filter_expression_type: Option<models_graphql::ModelExpressionType>,
    pub graphql_api: models_graphql::ModelGraphQlApi,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CommandWithArgumentPresets {
    pub command: commands::Command,
    pub permissions: BTreeMap<Role, command_permissions::CommandPermission>,
    pub argument_presets: BTreeMap<Role, ArgumentPresets>,
}

// output from this stage
pub struct ArgumentPresetsOutput {
    pub models: IndexMap<Qualified<ModelName>, ModelWithArgumentPresets>,
    pub commands: IndexMap<Qualified<CommandName>, CommandWithArgumentPresets>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
/// Argument name with optional field path, if part of the argument has to be
/// preset
pub struct ArgumentNameAndPath {
    /// Name of the ndc function/procedure argument
    pub ndc_argument_name: Option<DataConnectorArgumentName>,
    /// Optional path of field names to traverse to get to a field, in case of
    /// complex input object types
    pub field_path: Vec<DataConnectorColumnName>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Default)]
/// Preset arguments for models or commands
pub struct ArgumentPresets {
    #[serde(
        serialize_with = "serialize_non_string_key_btreemap",
        deserialize_with = "deserialize_non_string_key_btreemap"
    )]
    pub argument_presets:
        BTreeMap<ArgumentNameAndPath, (QualifiedTypeReference, ValueExpressionOrPredicate)>,
}

impl Display for ArgumentPresets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.argument_presets, f)
    }
}
