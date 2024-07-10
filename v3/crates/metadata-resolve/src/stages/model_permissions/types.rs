use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use open_dds::{
    arguments::ArgumentName,
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    models::ModelName,
    permissions::Role,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, FieldName},
};

use crate::stages::{data_connectors, models, models_graphql, object_types, relationships};
use crate::types::error::{Error, RelationshipError};
use crate::types::permission::{ValueExpression, ValueExpressionOrPredicate};
use crate::types::subgraph::{deserialize_qualified_btreemap, serialize_qualified_btreemap};
use crate::types::subgraph::{Qualified, QualifiedTypeReference};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ModelWithPermissions {
    pub model: models::Model,
    pub select_permissions: BTreeMap<Role, SelectPermission>,
    pub filter_expression_type: Option<models_graphql::ModelExpressionType>,
    pub graphql_api: models_graphql::ModelGraphQlApi,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum FilterPermission {
    AllowAll,
    Filter(ModelPredicate),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SelectPermission {
    pub filter: FilterPermission,
    // pub allow_aggregations: bool,
    pub argument_presets:
        BTreeMap<ArgumentName, (QualifiedTypeReference, ValueExpressionOrPredicate)>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelPredicate {
    UnaryFieldComparison {
        field: FieldName,
        field_parent_type: Qualified<CustomTypeName>,
        ndc_column: DataConnectorColumnName,
        operator: UnaryComparisonOperator,
    },
    BinaryFieldComparison {
        field: FieldName,
        field_parent_type: Qualified<CustomTypeName>,
        ndc_column: DataConnectorColumnName,
        operator: DataConnectorOperatorName,
        argument_type: QualifiedTypeReference,
        value: ValueExpression,
    },
    Relationship {
        relationship_info: PredicateRelationshipInfo,
        predicate: Box<ModelPredicate>,
    },
    And(Vec<ModelPredicate>),
    Or(Vec<ModelPredicate>),
    Not(Box<ModelPredicate>),
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryComparisonOperator {
    IsNull,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PredicateRelationshipInfo {
    pub relationship_name: RelationshipName,
    pub relationship_type: RelationshipType,
    pub source_type: Qualified<CustomTypeName>,
    pub source_data_connector: data_connectors::DataConnectorLink,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub source_type_mappings: BTreeMap<Qualified<CustomTypeName>, object_types::TypeMapping>,
    pub target_source: ModelTargetSource,
    pub target_type: Qualified<CustomTypeName>,
    pub target_model_name: Qualified<ModelName>,
    pub mappings: Vec<relationships::RelationshipModelMapping>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelTargetSource {
    pub model: models::ModelSource,
    pub capabilities: relationships::RelationshipCapabilities,
}

impl ModelTargetSource {
    pub fn new(
        model: &ModelWithPermissions,
        relationship: &relationships::RelationshipField,
    ) -> Result<Option<Self>, Error> {
        model
            .model
            .source
            .as_ref()
            .map(|model_source| Self::from_model_source(model_source, relationship))
            .transpose()
    }

    pub fn from_model_source(
        model_source: &models::ModelSource,
        relationship: &relationships::RelationshipField,
    ) -> Result<Self, Error> {
        Ok(Self {
            model: model_source.clone(),
            capabilities: relationship
                .target_capabilities
                .as_ref()
                .ok_or_else(|| Error::RelationshipError {
                    relationship_error: RelationshipError::NoRelationshipCapabilitiesDefined {
                        type_name: relationship.source.clone(),
                        relationship_name: relationship.relationship_name.clone(),
                        data_connector_name: model_source.data_connector.name.clone(),
                    },
                })?
                .clone(),
        })
    }
}
