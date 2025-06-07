use open_dds::{
    commands::{CommandName, FunctionName},
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, Deprecated},
};

use serde::{Deserialize, Serialize};

use metadata_resolve::{self, Qualified, QualifiedTypeReference, RelationshipCapabilities};

use crate::types::TypeKind;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelRelationshipAnnotation {
    pub source_type: Qualified<CustomTypeName>,
    pub relationship_name: RelationshipName,
    pub target_model_name: Qualified<ModelName>,
    pub target_capabilities: Option<RelationshipCapabilities>,
    pub target_type: Qualified<CustomTypeName>,
    pub relationship_type: RelationshipType,
    pub mappings: Vec<metadata_resolve::RelationshipModelMapping>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelAggregateRelationshipAnnotation {
    pub source_type: Qualified<CustomTypeName>,
    pub relationship_name: RelationshipName,
    pub target_model_name: Qualified<ModelName>,
    pub target_type: Qualified<CustomTypeName>,
    pub mappings: Vec<metadata_resolve::RelationshipModelMapping>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FilterRelationshipAnnotation {
    pub relationship_name: RelationshipName,
    pub relationship_type: RelationshipType,
    pub source_type: Qualified<CustomTypeName>,
    pub target_source: metadata_resolve::ModelTargetSource,
    pub target_type: Qualified<CustomTypeName>,
    pub target_model_name: Qualified<ModelName>,
    pub mappings: Vec<metadata_resolve::RelationshipModelMapping>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OrderByRelationshipAnnotation {
    pub relationship_name: RelationshipName,
    pub relationship_type: RelationshipType,
    pub source_type: Qualified<CustomTypeName>,
    pub target_source: metadata_resolve::ModelTargetSource,
    pub target_type: Qualified<CustomTypeName>,
    pub target_model_name: Qualified<ModelName>,
    pub mappings: Vec<metadata_resolve::RelationshipModelMapping>,
    pub deprecated: Option<Deprecated>,
    pub multiple_input_properties: metadata_resolve::MultipleOrderByInputObjectFields,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CommandRelationshipAnnotation {
    pub source_type: Qualified<CustomTypeName>,
    pub relationship_name: RelationshipName,
    pub command_name: Qualified<CommandName>,
    pub target_source: Option<CommandTargetSource>,
    pub target_type: QualifiedTypeReference,
    pub target_base_type_kind: TypeKind,
    pub mappings: Vec<metadata_resolve::RelationshipCommandMapping>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CommandTargetSource {
    pub function_name: FunctionName,
}

impl CommandTargetSource {
    pub fn new(
        command: &metadata_resolve::CommandWithPermissions,
    ) -> Result<Option<Self>, crate::Error> {
        command
            .command
            .source
            .as_ref()
            .map(|command_source| {
                Ok(Self {
                    function_name: match &command_source.source {
                        crate::types::output_type::DataConnectorCommand::Function(
                            function_name,
                        ) => function_name.clone(),
                        crate::types::output_type::DataConnectorCommand::Procedure(_) => {
                            Err(crate::Error::RelationshipsToProcedureBasedCommandsAreNotSupported)?
                        }
                    },
                })
            })
            .transpose()
    }
}
