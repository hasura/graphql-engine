use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::sync::Arc;

use open_dds::{
    arguments::ArgumentName,
    data_connector::{DataConnectorColumnName, DataConnectorOperatorName},
    models::ModelName,
    permissions::Role,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, Deprecated, FieldName},
};

use crate::stages::{
    argument_presets, data_connectors, models, models_graphql, object_relationships, object_types,
};
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
    pub allow_subscriptions: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum ModelPredicate {
    UnaryFieldComparison {
        field: FieldName,
        field_parent_type: Qualified<CustomTypeName>,
        ndc_column: DataConnectorColumnName,
        operator: UnaryComparisonOperator,
        /// To mark a field as deprecated in the field usage while reporting query usage analytics.
        deprecated: Option<Deprecated>,
    },
    BinaryFieldComparison {
        field: FieldName,
        field_parent_type: Qualified<CustomTypeName>,
        ndc_column: DataConnectorColumnName,
        operator: DataConnectorOperatorName,
        argument_type: QualifiedTypeReference,
        value: ValueExpression,
        /// To mark a field as deprecated in the field usage while reporting query usage analytics.
        deprecated: Option<Deprecated>,
    },
    Relationship {
        relationship_info: PredicateRelationshipInfo,
        predicate: Box<ModelPredicate>,
    },
    And(Vec<ModelPredicate>),
    Or(Vec<ModelPredicate>),
    Not(Box<ModelPredicate>),
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
    pub mappings: Vec<object_relationships::RelationshipModelMapping>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelTargetSource {
    pub model: Arc<models::ModelSource>,
    pub capabilities: object_relationships::RelationshipCapabilities,
}

impl ModelTargetSource {
    pub fn new(
        model: &argument_presets::ModelWithArgumentPresets,
        relationship: &object_relationships::RelationshipField,
    ) -> Result<Option<Self>, Error> {
        model
            .model
            .source
            .as_ref()
            .map(|model_source| Self::from_model_source(&model_source.clone(), relationship))
            .transpose()
    }

    pub fn from_model_source(
        model_source: &Arc<models::ModelSource>,
        relationship: &object_relationships::RelationshipField,
    ) -> Result<Self, Error> {
        Ok(Self {
            model: model_source.clone(),
            capabilities: relationship
                .target_capabilities
                .as_ref()
                .ok_or_else(|| Error::ObjectRelationshipError {
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
