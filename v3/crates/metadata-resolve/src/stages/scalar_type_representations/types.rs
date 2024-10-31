use crate::{deserialize_non_string_key_btreemap, serialize_non_string_key_btreemap, Qualified};
use lang_graphql::ast::common as ast;
use open_dds::data_connector::DataConnectorName;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
// what type should we advertise for this value? useful for generating schema for
// JSONAPI and SQL frontends
pub enum ValueRepresentation {
    FromDataConnectorSchema(ndc_models::TypeRepresentation),
    AssumeJson,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarTypeRepresentation {
    pub graphql_type_name: Option<ast::TypeName>,
    pub description: Option<String>,
    #[serde(
        serialize_with = "serialize_non_string_key_btreemap",
        deserialize_with = "deserialize_non_string_key_btreemap"
    )]
    pub representations: BTreeMap<Qualified<DataConnectorName>, ValueRepresentation>,
}
