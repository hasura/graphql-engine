use serde::Serialize;

use crate::{identifier::Identifier, spanned::Spanned, str_newtype, types::TypeReference};

str_newtype!(ArgumentName over Identifier | doc "The name of an argument.");

/// The definition of an argument for a field, command, or model.
#[derive(Serialize, Clone, Debug, PartialEq, opendds_derive::OpenDd)]
#[opendd(json_schema(title = "ArgumentDefinition"))]
pub struct ArgumentDefinition {
    pub name: Spanned<ArgumentName>,
    #[serde(rename = "type")]
    #[opendd(rename = "type")]
    pub argument_type: TypeReference,
    pub description: Option<String>,
}
