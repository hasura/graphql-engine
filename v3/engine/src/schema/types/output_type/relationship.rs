use open_dds::{
    commands::{CommandName, FunctionName},
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
    schema::{self, types::CommandSourceDetail},
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
    pub(crate) details: CommandSourceDetail,
    pub(crate) function_name: FunctionName,
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
                    details: CommandSourceDetail {
                        data_connector: command_source.data_connector.clone(),
                        type_mappings: command_source.type_mappings.clone(),
                        argument_mappings: command_source.argument_mappings.clone(),
                    },
                    function_name: match &command_source.source {
                        schema::types::output_type::DataConnectorCommand::Function(
                            function_name,
                        ) => function_name.clone(),
                        schema::types::output_type::DataConnectorCommand::Procedure(_) => Err(
                            schema::Error::RelationshipsToProcedureBasedCommandsAreNotSupported,
                        )?,
                    },
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
