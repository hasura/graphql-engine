use crate::stages::{data_connectors, object_types, type_permissions};
use crate::types::error::ShouldBeAnError;
use crate::types::subgraph::{Qualified, QualifiedTypeReference};
use indexmap::IndexMap;
use open_dds::aggregates::AggregateExpressionName;
use open_dds::data_connector::DataConnectorName;
use open_dds::{commands::CommandName, models::ModelName, types::CustomTypeName};
use serde::{Deserialize, Serialize};

use crate::helpers::types::NdcColumnForComparison;
use lang_graphql::ast::common as ast;
use open_dds::arguments::ArgumentName;
use std::collections::BTreeMap;

use open_dds::relationships::{RelationshipName, RelationshipType};
use open_dds::types::{Deprecated, FieldName};

pub struct ObjectRelationshipsOutput {
    pub object_types: BTreeMap<Qualified<CustomTypeName>, ObjectTypeWithRelationships>,
    pub issues: Vec<ObjectRelationshipsIssue>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ObjectTypeWithRelationships {
    pub object_type: object_types::ObjectTypeRepresentation,
    /// permissions on this type, when it is used in an output context (e.g. as
    /// a return type of Model or Command)
    pub type_output_permissions: type_permissions::TypeOutputPermissions,
    /// permissions on this type, when it is used in an input context (e.g. in
    /// an argument type of Model or Command)
    pub type_input_permissions: type_permissions::TypeInputPermissions,
    /// any relationship fields defined on this object, indexed by relationship name
    pub relationship_fields: IndexMap<RelationshipName, RelationshipField>,
    /// type mappings for each data connector
    pub type_mappings: object_types::DataConnectorTypeMappingsForObject,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RelationshipTarget {
    Model(Box<ModelRelationshipTarget>),
    Command(CommandRelationshipTarget),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct ModelRelationshipTarget {
    // TODO(Abhinav): Refactor resolved types to contain denormalized data (eg: actual resolved model)
    pub model_name: Qualified<ModelName>,
    pub relationship_type: RelationshipType,
    pub target_typename: Qualified<CustomTypeName>,
    pub mappings: Vec<RelationshipModelMapping>,
    pub relationship_aggregate: Option<AggregateRelationship>, // only applicable to array relationships
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct AggregateRelationship {
    pub field_name: ast::Name,
    pub aggregate_expression: Qualified<AggregateExpressionName>,
    pub filter_input_field_name: ast::Name,
    pub description: Option<String>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct CommandRelationshipTarget {
    pub command_name: Qualified<CommandName>,
    pub target_type: QualifiedTypeReference,
    pub mappings: Vec<RelationshipCommandMapping>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RelationshipTargetName {
    Model(Qualified<ModelName>),
    Command(Qualified<CommandName>),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipModelMapping {
    pub source_field: RelationshipFieldAccess,
    pub target: RelationshipModelMappingTarget,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipFieldAccess {
    pub field_name: FieldName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum RelationshipModelMappingTarget {
    ModelField(RelationshipModelMappingFieldTarget),
    Argument(ArgumentName),
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipModelMappingFieldTarget {
    pub target_field: RelationshipFieldAccess,
    // Optional because we allow building schema without specifying a data source
    pub target_ndc_column: Option<NdcColumnForComparison>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipCommandMapping {
    pub source_field: RelationshipFieldAccess,
    pub argument_name: ArgumentName,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipField {
    pub field_name: ast::Name,
    pub relationship_name: RelationshipName,
    pub source: Qualified<CustomTypeName>,
    pub target: RelationshipTarget,
    pub target_capabilities: Option<RelationshipCapabilities>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub description: Option<String>,
    #[serde(default = "serde_ext::ser_default")]
    #[serde(skip_serializing_if = "serde_ext::is_ser_default")]
    pub deprecated: Option<Deprecated>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct RelationshipCapabilities {
    // TODO: We don't handle relationships without foreach.
    // Change this to a bool, when we support that
    pub foreach: (),
    pub supports_relationships: Option<data_connectors::DataConnectorRelationshipCapabilities>,
}

pub enum RelationshipExecutionCategory {
    // Push down relationship definition to the data connector
    Local,
    // Use foreach in the data connector to fetch related rows for multiple objects in a single request
    RemoteForEach,
}

/// Indicates the 'nestedness' of fields, ie. whether they are at the root of a model/command
/// or if they are nested inside an object field, or nested inside an object inside an array.
///
/// Each nestedness level is stronger than the last (ie. ordering is important). Fields on an object
/// that is nested within an object that is nested within an array is still regarded to be
/// array nested. Fields on an object that is nested inside an object that is not nested inside anything
/// is regarded as object nested.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[allow(clippy::enum_variant_names)]
pub enum FieldNestedness {
    /// The fields are at the root of the model/command (ie. on the object type of the model/command)
    NotNested,
    /// The fields are on an object type that is nested
    ObjectNested,
    /// The fields are on an object type that that has a nested array ancestor
    ArrayNested,
}

#[derive(Debug, thiserror::Error)]
pub enum ObjectRelationshipsIssue {
    #[error(
        "The data connector {data_connector_name} does not support relationships or variables, so it cannot be used for relationship {relationship_name} on type {type_name}"
    )]
    LocalRelationshipDataConnectorDoesNotSupportRelationshipsOrVariables {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        data_connector_name: Qualified<DataConnectorName>,
    },
    #[error(
        "Procedure based commands are not supported in relationships. \
         Relationship '{relationship_name}' on type '{type_name}' targets \
         command '{command_name}' which is backed by procedure '{procedure_name}'"
    )]
    ProcedureCommandRelationshipsNotSupported {
        type_name: Qualified<CustomTypeName>,
        relationship_name: RelationshipName,
        command_name: Qualified<CommandName>,
        procedure_name: open_dds::commands::ProcedureName,
    },
}

impl ShouldBeAnError for ObjectRelationshipsIssue {
    fn should_be_an_error(&self, flags: &open_dds::flags::OpenDdFlags) -> bool {
        match self {
            ObjectRelationshipsIssue::LocalRelationshipDataConnectorDoesNotSupportRelationshipsOrVariables { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowLocalRelationshipsOnDataConnectorsWithoutRelationshipsOrVariables)
            }
            ObjectRelationshipsIssue::ProcedureCommandRelationshipsNotSupported { .. } => {
                flags.contains(open_dds::flags::Flag::DisallowProcedureCommandRelationships)
            }
        }
    }
}
