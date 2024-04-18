use std::collections::{HashMap, HashSet};

use open_dds::{models::ModelName, types::CustomTypeName};

use crate::metadata::resolved::types::ObjectTypeRepresentation;

use crate::metadata::resolved::types::TypeMapping;

use crate::metadata::resolved::error::Error;
use crate::metadata::resolved::subgraph::Qualified;

use lang_graphql::ast::common as ast;
use open_dds::data_connector::DataConnectorName;

pub type DataConnectorTypeMappingsForObjectType =
    HashMap<Qualified<DataConnectorName>, HashMap<String, TypeMapping>>;

#[derive(Debug)]
pub struct DataConnectorTypeMappings(
    HashMap<Qualified<CustomTypeName>, DataConnectorTypeMappingsForObjectType>,
);

impl Default for DataConnectorTypeMappings {
    fn default() -> Self {
        Self::new()
    }
}

impl DataConnectorTypeMappings {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(
        &self,
        object_type_name: &Qualified<CustomTypeName>,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &str,
    ) -> Option<&TypeMapping> {
        self.0
            .get(object_type_name)
            .and_then(|connectors| {
                connectors
                    .get(data_connector_name)
                    .map(|data_connector_object_types| {
                        data_connector_object_types.get(data_connector_object_type)
                    })
            })
            .flatten()
    }

    pub fn insert(
        &mut self,
        object_type_name: &Qualified<CustomTypeName>,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &str,
        type_mapping: TypeMapping,
    ) -> Result<(), Error> {
        if self
            .0
            .entry(object_type_name.clone())
            .or_default()
            .entry(data_connector_name.clone())
            .or_default()
            .insert(data_connector_object_type.to_string(), type_mapping)
            .is_some()
        {
            return Err(Error::DuplicateDataConnectorTypeMapping {
                type_name: object_type_name.clone(),
                data_connector: data_connector_name.clone(),
                data_connector_object_type: data_connector_object_type.to_string(),
            });
        }
        Ok(())
    }
}

/// output of `data_connector_type_mappings` step
pub struct DataConnectorTypeMappingsOutput {
    pub existing_graphql_types: HashSet<ast::TypeName>,
    pub global_id_enabled_types: HashMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    pub apollo_federation_entity_enabled_types:
        HashMap<Qualified<CustomTypeName>, Option<Qualified<open_dds::models::ModelName>>>,
    pub data_connector_type_mappings: DataConnectorTypeMappings,
    pub object_types: HashMap<Qualified<CustomTypeName>, ObjectTypeRepresentation>,
}
