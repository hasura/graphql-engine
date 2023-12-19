use open_dds::{
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::CustomTypeName,
};

use serde::{Deserialize, Serialize};

use crate::{
    metadata::resolved::{self, subgraph::Qualified},
    schema,
};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelRelationshipAnnotation {
    pub source_type: Qualified<CustomTypeName>,
    pub relationship_name: RelationshipName,
    pub model_name: Qualified<ModelName>,
    pub target_source: Option<ModelTargetSource>,
    pub target_type: Qualified<CustomTypeName>,
    pub relationship_type: RelationshipType,
    pub mappings: Vec<resolved::relationship::RelationshipModelMapping>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelTargetSource {
    pub(crate) model: resolved::model::ModelSource,
    pub(crate) capabilities: resolved::relationship::RelationshipCapabilities,
}

impl ModelTargetSource {
    pub fn new(
        model: &resolved::model::Model,
        relationship: &resolved::relationship::Relationship,
    ) -> Result<Option<Self>, schema::Error> {
        model
            .source
            .as_ref()
            .map(|model_source| {
                Ok(Self {
                    model: model_source.clone(),
                    capabilities: relationship
                        .target_capabilities
                        .as_ref()
                        .ok_or_else(|| schema::Error::InternalMissingRelationshipCapabilities {
                            type_name: relationship.source.clone(),
                            relationship: relationship.name.clone(),
                        })?
                        .clone(),
                })
            })
            .transpose()
    }
}
