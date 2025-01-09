use std::collections::BTreeMap;
use std::sync::Arc;

use open_dds::{
    commands::{CommandName, FunctionName},
    models::ModelName,
    relationships::{RelationshipName, RelationshipType},
    types::{CustomTypeName, Deprecated},
};

use serde::{Deserialize, Serialize};

use metadata_resolve::{
    self, deserialize_qualified_btreemap, serialize_qualified_btreemap, Qualified,
    QualifiedTypeReference,
};

use crate::types::{CommandSourceDetail, TypeKind};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelRelationshipAnnotation {
    pub source_type: Qualified<CustomTypeName>,
    pub relationship_name: RelationshipName,
    pub model_name: Qualified<ModelName>,
    pub target_source: Option<metadata_resolve::ModelTargetSource>,
    pub target_type: Qualified<CustomTypeName>,
    pub relationship_type: RelationshipType,
    pub mappings: Vec<metadata_resolve::RelationshipModelMapping>,
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModelAggregateRelationshipAnnotation {
    pub source_type: Qualified<CustomTypeName>,
    pub relationship_name: RelationshipName,
    pub model_name: Qualified<ModelName>,
    pub target_source: Option<metadata_resolve::ModelTargetSource>,
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
    pub source_data_connector: Arc<metadata_resolve::DataConnectorLink>,
    #[serde(
        serialize_with = "serialize_qualified_btreemap",
        deserialize_with = "deserialize_qualified_btreemap"
    )]
    pub source_type_mappings: BTreeMap<Qualified<CustomTypeName>, metadata_resolve::TypeMapping>,
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
    pub details: CommandSourceDetail,
    pub function_name: FunctionName,
    pub capabilities: metadata_resolve::RelationshipCapabilities,
}

impl CommandTargetSource {
    pub fn new(
        command: &metadata_resolve::CommandWithArgumentPresets,
        relationship: &metadata_resolve::RelationshipField,
    ) -> Result<Option<Self>, crate::Error> {
        command
            .command
            .source
            .as_ref()
            .map(|command_source| {
                Ok(Self {
                    details: CommandSourceDetail {
                        data_connector: command_source.data_connector.clone(),
                        type_mappings: command_source.type_mappings.clone(),
                        argument_mappings: command_source.argument_mappings.clone(),
                        data_connector_link_argument_presets: command_source
                            .data_connector_link_argument_presets
                            .clone(),
                        ndc_type_opendd_type_same: command_source.ndc_type_opendd_type_same,
                    },
                    function_name: match &command_source.source {
                        crate::types::output_type::DataConnectorCommand::Function(
                            function_name,
                        ) => function_name.clone(),
                        crate::types::output_type::DataConnectorCommand::Procedure(_) => {
                            Err(crate::Error::RelationshipsToProcedureBasedCommandsAreNotSupported)?
                        }
                    },
                    capabilities: relationship
                        .target_capabilities
                        .as_ref()
                        .ok_or_else(|| crate::Error::InternalMissingRelationshipCapabilities {
                            type_name: relationship.source.clone(),
                            relationship: relationship.relationship_name.clone(),
                        })?
                        .clone(),
                })
            })
            .transpose()
    }
}
