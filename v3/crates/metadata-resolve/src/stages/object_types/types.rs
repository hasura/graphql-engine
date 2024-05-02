use crate::types::error::Error;
use crate::types::subgraph::QualifiedTypeReference;
use indexmap::IndexMap;

use open_dds::types::{CustomTypeName, Deprecated, FieldName};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};

use open_dds::models::ModelName;

use crate::types::subgraph::Qualified;

use lang_graphql::ast::common as ast;
use open_dds::data_connector::DataConnectorName;

/// A mapping from a data connector to their objects, which contain field types.
#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct DataConnectorTypeMappingsForObject(
    HashMap<Qualified<DataConnectorName>, HashMap<String, TypeMapping>>,
);

impl Default for DataConnectorTypeMappingsForObject {
    fn default() -> Self {
        Self::new()
    }
}

impl DataConnectorTypeMappingsForObject {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(
        &self,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &str,
    ) -> Option<&TypeMapping> {
        self.0
            .get(data_connector_name)
            .and_then(|data_connector_object_types| {
                data_connector_object_types.get(data_connector_object_type)
            })
    }

    pub fn insert(
        &mut self,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &str,
        type_mapping: TypeMapping,
    ) -> Result<(), Error> {
        if self
            .0
            .entry(data_connector_name.clone())
            .or_default()
            .insert(data_connector_object_type.to_string(), type_mapping)
            .is_some()
        {
            return Err(Error::DuplicateDataConnectorObjectTypeMapping {
                data_connector: data_connector_name.clone(),
                data_connector_object_type: data_connector_object_type.to_string(),
            });
        }
        Ok(())
    }
}

/// output of `object_types` step
pub struct DataConnectorTypeMappingsOutput {
    pub graphql_types: HashSet<ast::TypeName>,
    pub global_id_enabled_types: HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    pub apollo_federation_entity_enabled_types:
        HashMap<Qualified<CustomTypeName>, Option<Qualified<open_dds::models::ModelName>>>,
    pub object_types: HashMap<Qualified<CustomTypeName>, ObjectTypeWithTypeMappings>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ObjectTypeRepresentation {
    pub fields: IndexMap<FieldName, FieldDefinition>,
    pub global_id_fields: Vec<FieldName>,
    pub apollo_federation_config: Option<ResolvedObjectApolloFederationConfig>,
    pub graphql_output_type_name: Option<ast::TypeName>,
    pub graphql_input_type_name: Option<ast::TypeName>,
    pub description: Option<String>,
    // TODO: add graphql_output_type_kind if we support creating interfaces.
}

pub struct ObjectTypeWithTypeMappings {
    pub object_type: ObjectTypeRepresentation,
    pub type_mappings: DataConnectorTypeMappingsForObject,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct FieldDefinition {
    pub field_type: QualifiedTypeReference,
    pub description: Option<String>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ResolvedObjectApolloFederationConfig {
    pub keys: nonempty::NonEmpty<ResolvedApolloFederationObjectKey>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, derive_more::Display)]
#[display(fmt = "Display")]
pub struct ResolvedApolloFederationObjectKey {
    pub fields: nonempty::NonEmpty<FieldName>,
}

/// Mapping from a column to its type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldMapping {
    pub column: String,
    pub column_type: ndc_models::Type,
}

/// Mapping from an object to their fields, which contain types.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum TypeMapping {
    /// Mapping from an object to their fields, which contain the types of fields.
    Object {
        ndc_object_type_name: String,
        field_mappings: BTreeMap<FieldName, FieldMapping>,
    },
}
