use crate::{Qualified, deserialize_non_string_key_btreemap, serialize_non_string_key_btreemap};
use lang_graphql::ast::common as ast;
use open_dds::data_connector::DataConnectorName;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ScalarTypeRepresentation {
    pub graphql_type_name: Option<ast::TypeName>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    #[serde(
        serialize_with = "serialize_non_string_key_btreemap",
        deserialize_with = "deserialize_non_string_key_btreemap"
    )]
    pub representations: BTreeMap<Qualified<DataConnectorName>, ndc_models::TypeRepresentation>,
}
