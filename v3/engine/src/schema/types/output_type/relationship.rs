use open_dds::{
    commands::CommandName,
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::CustomTypeName,
};

use serde::{Deserialize, Serialize};

use crate::{
    metadata::resolved::{
        self,
        subgraph::{Qualified, QualifiedTypeReference},
    },
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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CommandRelationshipAnnotation {
    pub source_type: Qualified<CustomTypeName>,
    pub relationship_name: RelationshipName,
    pub command_name: Qualified<CommandName>,
    pub target_source: Option<CommandTargetSource>,
    pub target_type: QualifiedTypeReference,
    pub underlying_object_typename: Option<Qualified<CustomTypeName>>,
    pub mappings: Vec<resolved::relationship::RelationshipCommandMapping>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CommandTargetSource {
    pub(crate) command: resolved::command::CommandSource,
    pub(crate) capabilities: resolved::relationship::RelationshipCapabilities,
}

impl CommandTargetSource {
    pub fn new(
        command: &resolved::command::Command,
        relationship: &resolved::relationship::Relationship,
    ) -> Result<Option<Self>, schema::Error> {
        command
            .source
            .as_ref()
            .map(|command_source| {
                Ok(Self {
                    command: command_source.clone(),
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
