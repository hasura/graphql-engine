use super::error::ObjectTypesError;
use crate::types::subgraph::QualifiedTypeReference;
use indexmap::IndexMap;
use open_dds::arguments::ArgumentName;
use open_dds::models::ModelName;
use open_dds::types::{CustomTypeName, DataConnectorArgumentName, Deprecated, FieldName};
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};
use std::ops::Deref;

use crate::types::subgraph::Qualified;

use lang_graphql::ast::common as ast;
use open_dds::data_connector::{
    DataConnectorColumnName, DataConnectorName, DataConnectorObjectType, DataConnectorOperatorName,
};

/// A mapping from a data connector to their objects, which contain field types.
#[derive(Serialize, Deserialize, Debug, Clone, Eq, PartialEq)]
pub struct DataConnectorTypeMappingsForObject {
    mappings:
        BTreeMap<Qualified<DataConnectorName>, BTreeMap<DataConnectorObjectType, TypeMapping>>,
    // Possible scalar representations for a field that are collected across all data connectors
    // and across all data connector type mappings. Ideally, this should be a set of one and we
    // should throw an error but to not break things, we'll only collect all the representations.
    // This is currently used only in the SQL layer
    scalar_representations: BTreeMap<FieldName, BTreeSet<ndc_models::TypeRepresentation>>,
}

impl Default for DataConnectorTypeMappingsForObject {
    fn default() -> Self {
        Self::new()
    }
}

impl DataConnectorTypeMappingsForObject {
    pub fn new() -> Self {
        Self {
            mappings: BTreeMap::new(),
            scalar_representations: BTreeMap::new(),
        }
    }
    pub fn scalar_representations(
        &self,
    ) -> &BTreeMap<FieldName, BTreeSet<ndc_models::TypeRepresentation>> {
        &self.scalar_representations
    }
    pub fn get<TObjectTypeName>(
        &self,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &TObjectTypeName,
    ) -> Option<&TypeMapping>
    where
        DataConnectorObjectType: Borrow<TObjectTypeName>,
        TObjectTypeName: Ord + ?Sized,
    {
        self.mappings
            .get(data_connector_name)
            .and_then(|data_connector_object_types| {
                data_connector_object_types.get(data_connector_object_type)
            })
    }

    pub fn insert(
        &mut self,
        data_connector_name: &Qualified<DataConnectorName>,
        data_connector_object_type: &DataConnectorObjectType,
        type_mapping: TypeMapping,
    ) -> Result<(), ObjectTypesError> {
        let TypeMapping::Object {
            ndc_object_type_name: _,
            field_mappings,
        } = &type_mapping;

        for (field_name, field_mapping) in field_mappings {
            if let Some(type_representation) = &field_mapping.column_type_representation {
                self.scalar_representations
                    .entry(field_name.clone())
                    .or_default()
                    .insert(type_representation.clone());
            }
        }

        if self
            .mappings
            .entry(data_connector_name.clone())
            .or_default()
            .insert(data_connector_object_type.clone(), type_mapping)
            .is_some()
        {
            return Err(ObjectTypesError::DuplicateDataConnectorObjectTypeMapping {
                data_connector: data_connector_name.clone(),
                data_connector_object_type: data_connector_object_type.to_string(),
            });
        }
        Ok(())
    }

    pub fn data_connector_names(&self) -> impl Iterator<Item = &Qualified<DataConnectorName>> {
        self.mappings.keys()
    }

    pub fn data_connector_mappings(
        &self,
    ) -> impl Iterator<Item = &BTreeMap<DataConnectorObjectType, TypeMapping>> {
        self.mappings.values()
    }

    pub fn object_types_for_data_connector(
        &self,
        data_connector_name: &Qualified<DataConnectorName>,
    ) -> Vec<DataConnectorObjectType> {
        match self.mappings.get(data_connector_name) {
            Some(map) => map.keys().cloned().collect(),
            None => vec![],
        }
    }
}

pub struct ObjectTypesWithTypeMappings(
    pub BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithTypeMappings>,
);

impl Deref for ObjectTypesWithTypeMappings {
    type Target = BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithTypeMappings>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// output of `object_types` step
pub struct ObjectTypesOutput {
    pub graphql_types: BTreeSet<ast::TypeName>,
    pub global_id_enabled_types: BTreeMap<Qualified<CustomTypeName>, Vec<Qualified<ModelName>>>,
    pub apollo_federation_entity_enabled_types:
        BTreeMap<Qualified<CustomTypeName>, Option<Qualified<open_dds::models::ModelName>>>,
    pub object_types: ObjectTypesWithTypeMappings,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
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
    pub field_arguments: IndexMap<ArgumentName, FieldArgumentInfo>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct FieldArgumentInfo {
    pub argument_type: QualifiedTypeReference,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedObjectApolloFederationConfig {
    pub keys: nonempty::NonEmpty<ResolvedApolloFederationObjectKey>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedApolloFederationObjectKey {
    pub fields: nonempty::NonEmpty<FieldName>,
}

/// Mapping from a column to its type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FieldMapping {
    pub column: DataConnectorColumnName,
    pub column_type: ndc_models::Type,
    pub column_type_representation: Option<ndc_models::TypeRepresentation>,
    pub comparison_operators: Option<ComparisonOperators>,
    pub argument_mappings: BTreeMap<ArgumentName, DataConnectorArgumentName>,
}

/// Mapping from a column to its type.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ComparisonOperators {
    pub equality_operators: Vec<DataConnectorOperatorName>,
    pub in_operators: Vec<DataConnectorOperatorName>,

    // TODO: for now, put other operators here. Later, once we have NDC
    // operator meanings, categorize these by their meaning:
    pub other_operators: Vec<DataConnectorOperatorName>,
}

/// Mapping from an object to their fields, which contain types.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum TypeMapping {
    /// Mapping from an object to their fields, which contain the types of fields.
    Object {
        ndc_object_type_name: DataConnectorObjectType,
        field_mappings: BTreeMap<FieldName, FieldMapping>,
    },
}
